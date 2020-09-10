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
import Data.Pool (createPool, withResource)
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics (Generic)
import Hasql.Connection (acquire, release, Connection)
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

listenPostgres :: Text -> Connection -> Chan Query -> IO ()
listenPostgres topic connListen chan = waitForNotifications handler connListen
    where
        handler channel payload = do
            when (channel /= encodeUtf8 topic) $ return ()
            let query = decode (fromStrict payload) :: Maybe Query
            maybe id (writeChan chan)
            -- case query of
            --     Nothing -> do
            --         hPutStrLn stderr "invalid payload"
            --         print payload
            --     Just query' -> do
            --         result <- Session.run (Session.statement (params query') (select $ sql query')) conn
            --         either print (\rows -> forM_ rows $ writeChan chan) result

        select sql = Statement (encodeUtf8 sql) encoder decoder True where
          encoder = param $ nonNullable $ foldableArray $ nonNullable text
          decoder = rowVector $ column $ D.nonNullable D.text

main :: IO ()
main = do
    pool <- createPool (acquire "")
        (either (const (pure ())) release)
        1 -- stripes
        60 -- unused connections are kept open for a minute
        100 -- max. 10 connections open per stripe
    withResource pool $ either print $ \conn -> do
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
                void $ async $ listenPostgres topic connListen chan
            eventSourceAppIO (event conn chan) req sendResponse
        event conn chan = do
            query <- readChan chan
            result <- Session.run (Session.statement (params query) (select $ sql query)) conn
            either print (\rows -> forM_ rows $ \payload -> ServerEvent Nothing Nothing [string8 $ unpack payload]) result

        headers :: Middleware
        headers = addHeaders [
            ("X-Accel-Buffering", "no")
          , ("Cache-Control", "no-cache")
          ]
