{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, 
  ExistentialQuantification, FlexibleInstances,
  UndecidableInstances, OverlappingInstances #-}
module Network.EasyHttp.Types where
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Control.Monad.State
import System.IO
import Network
import Network.Socket
import System.Posix.Files
import Data.IORef
-- Types -----------------------------------------------------

data Request = Request { getReqType :: RqType
                       , getReqPath :: RqPath 
                       , getReqHeaders :: Headers
                       , getGetParams :: [(C.ByteString,C.ByteString)]
                       , getClientAddr :: SockAddr } deriving (Show) 

data Header  = Header C.ByteString C.ByteString
type Headers = M.Map C.ByteString C.ByteString

data RqType = GET | POST | HEAD deriving (Show)
type RqPath = C.ByteString

data SC = forall a. Servable a => SC a
data Response = forall a. HttpContent a => Response Code Headers a

getRespHeaders (Response _ h _) = h
getRespCode    (Response c _ _) = c                


data Code = NotFound | Found | Forbidden | InternalError

data ContentType = CT C.ByteString 

data ServerState = ServerState { _getReq :: Request , _getResp :: Response }
type ServerMonad b = StateT ServerState IO b

data ImageFile = JpegFile File | PngFile File

data ImageData = ImageData C.ByteString

data File = File { getFilePath :: FilePath
                 , getFileFileStatus :: FileStatus 
                 , getMimeType :: C.ByteString }

data HTML a = HTML a

class Servable a where
    serve :: a -> Socket -> IO ()
    serveChunked :: a -> Socket -> IO ()    
    serveChunked a s = serve a s

class HasHeaders a where
    getHeaders :: a -> Headers
    putHeaders :: a -> Headers -> a

class HttpContent a where
    getContentType   :: a -> Maybe (ContentType)
    getContentLength :: a -> Int
    getContent       :: a -> SC
    getContentType  _ = Nothing

-- Things we can turn into a string...

instance Show Code where
    show NotFound  = "404 Not Found"
    show Found     = "200 OK"
    show Forbidden = "403 Forbidden"
    show InternalError = "500 Internal Error"

instance Show Header where
    show (Header a b) = C.unpack c
        where c = (a `C.append` ": " `C.append` b)

instance Show ContentType where
    show (CT a) = C.unpack $ a 


-- For debugging purposes ---------

class Debug a where
    debugShow :: a -> C.ByteString

instance (Show a) => Debug a where
    debugShow = C.pack . show

instance Debug String where
    debugShow = C.pack

instance Debug C.ByteString where
    debugShow = id

-----------------------------------