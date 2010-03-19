{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, RankNTypes, FlexibleInstances #-}
{- a simple http server, not meant to be fast or very good, just easy to use -}
module Network.EasyHttp.Server (module Network.EasyHttp.Types
                   , httpServe
                   , startHTTP
                   , getReq
                   , limitClients
                   , debug
                   , debugs
                   ) where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.State

import Data.Char
import Data.Maybe
import Data.Time

import Network.EasyHttp.Types

import Network.Socket
import Network.BSD
import Network.Socket.SendFile

import System.Directory
import System.FilePath
import System.IO

import Text.Printf
import System.Posix.Files
import Data.MIME.Types
import Data.IORef

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.Map as M
import qualified Network as N
import qualified Network.Socket.ByteString as NB

-- Classes ----------------------------------------------------

-- Things that have HTTP headers...

instance HasHeaders Request where
    getHeaders = getReqHeaders
    putHeaders rq h = rq { getReqHeaders = h }

instance HasHeaders Response where
    getHeaders = getRespHeaders 
    putHeaders (Response a b c) h = Response a h c

-- Things that can be written to a socket

instance Servable C.ByteString where
    serve bs s = NB.sendAll s bs 

instance Servable L.ByteString where
    serve (LI.Chunk bs x) s = NB.sendAll s bs >> serve x s
    serve (LI.Empty) s = return ()

instance Servable File where
    serve (File fp _ _) s = sendFile s fp

instance Servable SC where
    serve (SC a) s = serve a s
 
instance Servable a => Servable (HTML a) where
    serve (HTML a) s = serve a s

instance Servable Response where
    serve (Response c h b) s = do 
      let a  = ["HTTP/1.1 ",(showbs c),"\n"]
      let ct = ((showbs) 
                (fromMaybe (CT "text/plain") 
                (getContentType b)))  
      let cl = getContentLength b 
      let hdrs = M.insert "Connection" "Keep-Alive" $ M.insert  "Content-type" ct $ (M.insert "Content-length" (showbs cl) h)
      NB.sendAll s $ foldl C.append "" a
      NB.sendAll s (C.unlines . map showbs . toHeaders $ hdrs) 
      NB.sendAll s "\n"
      serve (getContent b) s


-- Things that can be served via HTTP...

instance (HttpContent a) => HttpContent (HTML a) where
    getContentType   _ = Just $ CT "text/html"
    getContentLength (HTML a) = getContentLength a
    getContent       (HTML a) = getContent a

instance HttpContent String where
    getContentLength = length
    getContent a     = SC (C.pack a)

instance HttpContent C.ByteString where
    getContentLength = C.length
    getContent       = SC

instance HttpContent File where
    getContentType   f = let a = getMimeType f in Just (CT a)
    getContentLength f = sizeFromFp f
    getContent       f = SC f

instance HttpContent ImageData where
    getContentType _   = Just (CT "image/jpeg")
    getContentLength (ImageData x) = C.length x
    getContent (ImageData x) = SC x

sizeFromFp = fromIntegral . fileSize . getFileFileStatus
fromFilePath fp = do fs   <- lift $ getFileStatus fp
                     let mime = checkMime (guessType defaultmtd False fp)
                     return $ File fp fs mime
    where checkMime (m,n) | isNothing m = "application/octet-stream"
                          | otherwise = C.pack . fromMaybe "" $ m


sendfile fp = do
  exist <- lift $ doesFileExist fp
  if exist then fromFilePath fp >>= httpServe 
           else putResp (resp404)

httpServe a = putCode Found >> putBody a

-- Server -----------------------------------------------------------

startSocket = do
  proto <- getProtocolNumber "tcp"
  socket AF_INET Stream proto

startServer addr port hndl = do
  addr' <- inet_addr addr
  bracket (do sock <- startSocket
              bindSocket sock (SockAddrInet (fromInteger port) (addr'))
              listen sock maxListenQueue
              return sock)
          (\s-> N.sClose s)
          (\s-> doAccept s)
  where doAccept sock = do 
          (s,sa) <- accept sock
          forkIO (hdlRequest s sa)
          doAccept sock
        hdlRequest s sa =
          (do a <- hndl s sa (hdlRequest s sa)
              catch (a s) (\e->putStrLn (show e) >> sClose s)
              return ()
          )


startHTTP :: String -> Integer -> ServerMonad () -> IO ()
startHTTP addr port = startServer addr port . httpService 
    where httpService f sock sa next = 
            withHeaders sa $ \hds-> do 
              debugServer (getReqPath hds)
              rsp <- fmap _getResp (runHttpHandler hds)
              return (\s->serve rsp s >> next)
          
           where runHttpHandler hds = do 
                   catch (execStateT f (ServerState hds emptyResponse ))
                         (\e->return $ ServerState hds (resp500 (C.pack . show $ e)) )

                 withHeaders sockAddr next = do
                   h  <- readTillEnd ""
                   if C.length h == 0 
                     then return sClose
                     else next . snd $ execState parse (h,emptyRequest sockAddr)
                   where 
                     readTillEnd x = do i <- catch (NB.recv sock 8192) (\_->return "")
                                        if C.length i == 0
                                          then return ""
                                          else do
                                            let j = (C.append x i)
                                            if C.null i || hasEnd j
                                              then return j
                                              else print i >> readTillEnd j
                         where hasEnd i = C.isInfixOf "\n\n" i
                                          || C.isInfixOf "\r\n\r\n" i
                                          || C.isInfixOf "\r\r" i

                     parse :: State (C.ByteString,Request) ()
                     parse = do parseRqType 
                                parseRqPath 
                                parseUrlParams
                                parseProtocol                              
                                parseHeaders 

                     ws f = do st <- get
                               put (f st)

                     parseRqType = ws $ \(s,rq) ->
                                      let (x,y) = C.break (== ' ') s
                                      in (C.tail y
                                         ,rq { getReqType = 
                                                   case x of
                                                     "GET" -> GET
                                                     "POST"-> POST 
                                                     "HEAD"-> HEAD
                                             })

                     parseRqPath   = ws $ \(s,rq) ->
                                        let (x,y) = C.break (\x-> x == '?' || x == ' ') s
                                        in (C.dropWhile (== '?') y,rq { getReqPath = x })

                     parseUrlParams= ws $ \(s,rq) ->
                                     let (x,y) = C.break isSpace s ;
                                         args  = if C.null x 
                                                   then [] 
                                                   else map a2tup $ map (splitup (== '=') []) (splitup (== '&') [] x) in
                                     (y,rq { getGetParams = args })
                                     

                     parseProtocol = ws $ \(s,rq) ->
                                        let (x,y) = (C.break (== ' ') . dropTillWS) $ s
                                        in (y,rq)

                     parseHeaders  = ws $ \(s,rq) -> 
                                     let l = (tail . lines') s
                                     in ("",rq { getReqHeaders = 
                                                 M.fromList . parseHeaders' $ l })

                     parseHeaders' []     = []
                     parseHeaders' (x:xs) = let (key,value) = C.break (== ':') x in
                                           (key,(C.dropWhile isSpace . C.drop 1 $ value)) : parseHeaders' xs

                     
------------------------------------------------------------------------

emptyRequest = Request GET "/" (M.fromList []) []

toHeaders = map (\(k,v)->Header k v) . M.toList

resp500 str = Response InternalError (M.fromList [("Content-type","text/plain")
                                                 ,("Content-length",len)]) (str :: C.ByteString)
              where len = C.pack . show $ C.length str


resp404 = Response NotFound (M.fromList [("Content-type","text/plain")
                                        ,("Content-length","9")]) ("Not Found" :: C.ByteString)
emptyResponse = resp404
resp403 = Response Forbidden (M.fromList [("Content-type","text/plain")
                                         ,("Content-length","9")]) ("Forbidden" :: C.ByteString)

putResp :: Response -> ServerMonad ()
putResp rsp = do { st <- get ; put st {_getResp = rsp}; return () }

updateResp f = do rsp <- getResp
                  putResp $ f rsp

putBody :: (HttpContent a) => a -> ServerMonad ()
putBody bd = updateResp $ \(Response c h b) -> Response c h bd
putCode :: Code -> ServerMonad ()
putCode cd = updateResp $ \(Response a h b) -> Response cd h b
putHeader k v = updateResp $ \rsp -> putHeaders rsp (M.insert k v $  getHeaders rsp)

putCT,putCL :: C.ByteString -> ServerMonad ()
putCT = putHeader "Content-type"
putCL = putHeader "Content-length"

getResp = fmap _getResp get

getReq :: ServerMonad Request 
getReq  = fmap _getReq  get 


limitClients clients app = do
  rq <- getReq 
  ip <- lift $ getIP $ getClientAddr rq
  if ip `elem` clients then app else debug ("Reject request from " ++ ip) >> putResp resp403

getIP (SockAddrInet pn ha) = inet_ntoa ha


-- Utils -------------------------------------------------------

lines' "" = []
lines' x  = let bl x = x `elem` "\r\n"
                (a,b) = C.break bl x
            in  a:(lines' $ C.drop 1 b)

dropTillWS = C.dropWhile (not . isSpace)
takeTillWS = C.takeWhile (not . isSpace)

showbs :: Show a => a -> C.ByteString
showbs = C.pack . show

stepPath = C.dropWhile (/= '/') . C.tail

debugServer s = do 
   tn <- getZonedTime
   putStrLn $ printf "[%s] %s" (show tn) (C.unpack s)


splitup :: (Char -> Bool) -> [C.ByteString] -> C.ByteString -> [C.ByteString]
splitup fn s x = next (C.break fn x)
    where next (a,b) | b == C.empty = (a:s) 
          next (a,b)  = splitup fn (a:s) (C.drop 1 b)

a2tup y = let x = reverse y in (head x,head $ drop 1 x)
-- debugServer _ = return ()

debug :: (Debug a) => a -> ServerMonad ()
debug = lift . debugServer . debugShow

debugs :: String -> ServerMonad ()
debugs = debug