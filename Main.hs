module Main where

import Control.Concurrent.Async (async)
import Control.Concurrent.Chan (dupChan, newChan, readChan, writeChan, Chan)
import Control.Monad (forever, void, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, decode)
import Data.ByteString (ByteString(..))
import Data.ByteString.Builder (string8)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics (Generic)
import Hasql.Connection (acquire, Connection)
import Hasql.Decoders (rowVector, column)
import Hasql.Encoders (param, foldableArray, nonNullable, text)
import Hasql.Notifications (listen, unlisten, toPgIdentifier, waitForNotifications)
import Hasql.Statement (Statement(..))
import Network.Wai (Application, Middleware, pathInfo)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Gzip (gzip, def)
import System.IO (hPutStrLn, stderr)
import qualified Hasql.Decoders as D
import qualified Hasql.Session as Session


data Query = Query {
    sql :: Text
  , params :: [Text]
  --, token :: Token
} deriving stock    (Generic, Show)
  deriving anyclass FromJSON

listenPostgres :: Text -> Connection -> Connection -> Chan Text -> IO ()
listenPostgres topic connListen conn chan = waitForNotifications (handler conn) connListen
    where
        handler conn channel payload = do
            when (channel /= encodeUtf8 topic) $ return ()
            let query = decode (fromStrict payload) :: Maybe Query
            case query of
                Nothing -> do
                    hPutStrLn stderr "invalid payload"
                    print payload
                Just query' -> do
                    result <- Session.run (Session.statement (params query') (select $ sql query')) conn
                    case result of
                        Left e -> do
                            hPutStrLn stderr "invalid result"
                            print e
                        Right rows -> do
                            forM_ rows $ writeChan chan

        select sql = Statement (encodeUtf8 sql) encoder decoder True where
          encoder = param $ nonNullable $ foldableArray $ nonNullable text
          decoder = rowVector $ column $ D.nonNullable D.text

main :: IO ()
main = do
    Right conn <- acquire ""
    hPutStrLn stderr "listening http on 0.0.0.0:8080"
    run 8080 $ gzip def $ headers $ eventSourceAppChan conn
    where
        eventSourceAppChan conn req sendResponse = do
            chan <- newChan
            forM_ (pathInfo req) $ \topic -> do
                Right connListen <- acquire ""
                listen connListen $ toPgIdentifier topic
                print $ "listening topic: "
                print topic
                void $ async $ listenPostgres topic connListen conn chan
            eventSourceAppIO (event chan) req sendResponse
        event chan = do
            payload <- readChan chan
            return $ ServerEvent Nothing Nothing [string8 $ unpack payload]

        headers :: Middleware
        headers = addHeaders [
            ("X-Accel-Buffering", "no")
          , ("Cache-Control", "no-cache")
          ]
