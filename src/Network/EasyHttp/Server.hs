{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, RankNTypes, FlexibleInstances #-}
{- a simple http server, not meant to be fast or very good, just easy to use -}
module Network.EasyHttp.Server (module Network.EasyHttp.Types
                               , ok
                               , serverFail
                               , notFound
                               , redirect
                               , startHTTP
                               , startHTTP'
                               , getReq
                               , getParams
                               , lookupHeader
                               , lookupParam
                               , lookupParam'
                               , limitClients
                               , dispatch
                               , fileServer
                               , debug
                               , debugs
                               , getSession
                               , withSession
                               ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad.State

import Data.Attoparsec.Char8 
import Data.Char
import Data.List
import Data.Maybe
import Data.Time

import Network.EasyHttp.Types

import Network.Socket
import Network.BSD
import Network.Socket.SendFile
import Network.URI (unEscapeString)

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Signals

import Text.Printf
import System.Posix.Files
import Data.MIME.Types
import Data.IORef
import Data.Dynamic

import Text.Regex.Posix
import Text.Regex.Posix.Wrap

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.Map as M
import qualified Network as N
import qualified Network.EasyHttp.RFC2616 as R
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
    serve (SC a) = serve a
 
instance Servable a => Servable (HTML a) where
    serve (HTML a) = serve a

instance Servable Response where
    serve (Response c h b) s = do 
      let a  = ["HTTP/1.1 ",showbs c,"\r\n"]
      let ct = (showbs 
                (fromMaybe (CT "text/plain") 
                (getContentType b)))  
      let cl = getContentLength b 
      let hdrs = M.insert "Server" "Easy-HTTP"
                 $ M.insert "Content-Type" ct 
                 $ M.insert "Content-Length" (showbs cl) h
      let h = intersperse "\r\n" (map showbs . toHeaders $ hdrs)
      NB.sendMany s (a ++ h ++ ["\r\n\r\n"])
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
    getContentLength   = sizeFromFp 
    getContent         = SC 

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
  if exist then fromFilePath fp >>= ok
           else putResp resp404

ok a = putCode Found >> putBody a
serverFail a = putCode InternalError >> putBody a
notFound = putResp resp404
redirect a = putCode MovedPermanently >> putHeader "Location" a
-- Server -----------------------------------------------------------

startSocket = do
  proto <- getProtocolNumber "tcp"
  socket AF_INET Stream proto

startServer addr port postconn mkstate hndl = do
  addr' <- inet_addr addr
  bracket (do sock <- startSocket
              setSocketOption sock ReuseAddr 1
              bindSocket sock (SockAddrInet (fromInteger port) addr')
              listen sock maxListenQueue
              return sock)
          N.sClose
          (\s-> do case postconn of 
                     (Just pc) -> pc s
                     Nothing   -> return ()
                   state <- mkstate
                   let end = Catch (putStrLn "Shutting Down Server..." >> sClose s >> exitSuccess)
                   installHandler sigTERM end Nothing
                   installHandler sigHUP  end Nothing
                   installHandler sigKILL end Nothing
                   installHandler sigQUIT end Nothing
                   installHandler sigINT  end Nothing
                   doAccept s state)
  where doAccept sock state = do 
          (s,sa) <- accept sock
          forkIO (hdlRequest s sa state)
          doAccept sock state
        hdlRequest s sa state =
            do a <- hndl s sa state (hdlRequest s sa state)
               catch (a s) print >> sClose s
               return ()

startHTTP addr port = startHTTP' addr port Nothing

startHTTP' :: String                 -- Host
          -> Integer                 -- Port
          -> Maybe (Socket -> IO ()) -- Post connect callback
          -> ServerMonad ()          -- Request handler
          -> IO () 
startHTTP' addr port pc d = do let st = (print "CREATE MVAR" >> newMVar 0)
                               startServer addr port pc st (httpService d)
    where httpService f sock sa m next = 
            withHeaders sa $ \hds-> do 
              debugServer (getReqPath hds)
              hndl <- runHttpHandler m hds
              let rsp  = _getResp hndl
              let mvar = _getSession hndl
              readMVar mvar >>= print
              let conn = fromMaybe "Keep-Alive" (M.lookup "Connection" $ getReqHeaders hds)
              if conn == "close"
                 then return (\s->serve (putHeaders rsp $ M.insert "Connection" "close" (getHeaders rsp)) s >> sClose s)
                 else return (\s->serve (putHeaders rsp $ M.insert "Connection" "keep-alive" (getHeaders rsp) ) s >> next)
          
           where runHttpHandler m hds =
                   catch (execStateT f (ServerState hds emptyResponse m))
                         (\e->return $ ServerState hds (resp500 (C.pack . show $ e)) m)

                 withHeaders sockAddr next = do
                   h  <- readTillEnd ""
                   if C.null h 
                     then return sClose
                     else do let (leftovers,request) = parse' h (emptyRequest sockAddr)
                             post <- readPost request leftovers 
                             print post
                             next (request { getParams = getParams request ++ post })
                   where 
                     readTillEnd x = do i <- catch (NB.recv sock 8192) (\_->return "")
                                        if C.null i
                                          then return ""
                                          else do
                                            let j = (C.append x i)
                                            if C.null i || hasEnd j
                                              then return j
                                              else readTillEnd j
                         where hasEnd i = C.isInfixOf "\n\n" i
                                          || C.isInfixOf "\r\n\r\n" i
                                          || C.isInfixOf "\r\r" i

                     readPost rq cur = 
                         case M.lookup "Content-Length" $ getReqHeaders rq of
                           Nothing -> return []
                           Just n  -> if n == "0" 
                                        then return []
                                        else let n' = read . C.unpack $ n in                                           
                                             do dat <- if C.length cur >= n'
                                                         then return cur
                                                         else do ex <- recvTill sock (n' - C.length cur)
                                                                 return (cur `C.append` ex)
                                                return $ parseUrlParams' dat
                           
                     --parse :: State (C.ByteString,Request) ()
                     parse' str rq = let Done left (prq,headers) = parse R.request str
                                         (rp,rg) = C.break (== '?') (R.requestUri prq)
                                    in (left,rq { getReqType    = toRqType (R.requestMethod prq)
                                                , getReqPath    = rp
                                                , getReqHeaders = M.fromList (map simHeaders headers)
                                                , getParams  = parseUrlParams rg
                                                }) --TODO FIX THIS

                     toRqType x | x == "GET"  = GET
                                | x == "POST" = POST
                                | x == "HEAD" = HEAD
                     toRqType x = undefined
                     simHeaders (R.Header a b) = (a,head b)

                     parseUrlParams s = let x = C.dropWhile (/= '?') s in 
                                        if C.null x then [] else parseUrlParams' $ C.tail x
                     parseUrlParams' x = map (\(x,y)-> let x' = C.unpack x
                                                           y' = C.unpack y in (C.pack $ esc x', C.pack $ esc y')) $
                                         map (a2tup . splitup (== '=') []) (splitup (== '&') [] x) 
                         where esc = unEscapeString . map (\x->if x == '+' then ' ' else x)

------------------------------------------------------------------------

emptyRequest = Request GET "/" (M.fromList []) []

toHeaders = map (uncurry Header) . M.toList



resp500 str = Response InternalError (M.fromList [("Content-Type","text/plain")
                                                 ,("Content-Length",len)]) (str :: C.ByteString)
              where len = C.pack . show $ C.length str


resp404 = Response NotFound (M.fromList [("Content-Type","text/plain")
                                        ,("Content-Length","9")]) ("Not Found" :: C.ByteString)
emptyResponse = resp404
resp403 = Response Forbidden (M.fromList [("Content-Type","text/plain")
                                         ,("Content-Length","9")]) ("Forbidden" :: C.ByteString)

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
putCT = putHeader "Content-Type"
putCL = putHeader "Content-Length"

getResp = fmap _getResp get

getReq :: ServerMonad Request 
getReq  = fmap _getReq  get 

lookupHeader k = do headers <- fmap getHeaders getReq
                    return (M.lookup k headers)

lookupParam k = do params <- fmap getParams getReq
                   return (lookup k params)

lookupParam' k = do params <- fmap getParams getReq
                    return (aux k params [])
    where aux _ [] ls = ls
          aux k ((a,b):xs) ls = let nls = if k == a then (b:ls) else ls
                                 in aux k xs nls

limitClients clients app = do
  rq <- getReq 
  ip <- lift $ getIP $ getClientAddr rq
  if ip `elem` clients then app else debug ("Reject request from " ++ ip) >> putResp resp403

getIP (SockAddrInet pn ha) = inet_ntoa ha


dispatch :: [(String,ServerMonad ())] -> ServerMonad ()
dispatch urls = do
  rq <- getReq 
  match' (getReqPath rq) urls >> return ()
  where match' path (x:xs) = if path =~ fst x
                              then snd x
                              else match' path xs
        match' _ [] = putResp resp403


fileServer :: String -> FilePath -> ServerMonad ()
fileServer uri path = do
  rq <- fmap (C.unpack . getReqPath) getReq
  let fp = makeRelative uri rq
  debug (path </> fp)
  sendfile (path </> fp)

-- Utils -------------------------------------------------------

recvTill s n = do r <- NB.recv s n
                  if C.length r < n then do rr <- recvTill s (n - C.length r)
                                            return (r `C.append` rr)
                                    else return r

isAtEOL x = C.isPrefixOf (C.pack "\r\n") x || C.isPrefixOf (C.pack "\n") x

lines' "" = []
lines' x  = let bl x = x `elem` "\r\n"
                (a,b) = C.break bl x
            in  a: (lines' $ C.drop 1 b)

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
    where next (a,b) | b == C.empty = a:s
          next (a,b)  = splitup fn (a:s) (C.drop 1 b)

a2tup y = let x = reverse y in if length x > 1 then (head x,x !! 1) else (head x,"")
-- debugServer _ = return ()

debug :: (Debug a) => a -> ServerMonad ()
debug = lift . debugServer . debugShow

debugs :: String -> ServerMonad ()
debugs = debug


------------------------------------------------------------------------
-- Sessions
------------------------------------------------------------------------

getSession :: ServerMonad Int
getSession = do m <- fmap _getSession get
                x <- liftIO $ readMVar m
                return x
withSession f = do st <- get
                   let session = _getSession st
                   x <- liftIO $ modifyMVar_ session f
                   return x