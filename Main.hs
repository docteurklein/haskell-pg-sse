module Main where

import Control.Concurrent.Async (async)
import Control.Concurrent.Chan
import Control.Monad (forever, void, forM_)
import Data.Aeson (FromJSON, decode)
import Data.ByteString (ByteString(..))
import Data.ByteString.Builder (string8)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Hasql.Connection (acquire, Connection)
import Hasql.Decoders (rowVector, column)
import Hasql.Encoders (param, foldableArray, nonNullable, text)
import Hasql.Notifications (listen, unlisten, toPgIdentifier, waitForNotifications)
import Hasql.Statement (Statement(..))
import Network.Wai (Application, Middleware)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Gzip (gzip, def)
import System.IO (hPutStrLn, stderr)
import qualified Hasql.Decoders as D
import qualified Hasql.Session as Session


data Query = Query {
    sql :: Text
  , params :: [Text]
  , role :: Maybe Text
} deriving stock    (Generic, Show)
  deriving anyclass FromJSON

eventChan :: Connection -> Connection -> Chan ServerEvent -> IO ()
eventChan connListen conn chan = waitForNotifications (handler conn) connListen
    where
        handler conn channel payload = do
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
                            forM_ rows $ \x -> writeChan chan (ServerEvent Nothing Nothing [string8 $ unpack x])

        select sql = Statement (encodeUtf8 sql) encoder decoder True where
          encoder = param $ nonNullable $ foldableArray $ nonNullable text
          decoder = rowVector $ column $ D.nonNullable D.text

main :: IO ()
main = do
    chan <- newChan
    Right conn <- acquire ""
    Right connListen <- acquire ""
    listen connListen $ toPgIdentifier "*"
    async $ (eventChan connListen conn) chan
    run 8080 (gzip def $ headers $ eventSourceAppChan chan)
    where
      headers :: Middleware
      headers = addHeaders [
          ("X-Accel-Buffering", "no")
        , ("Cache-Control", "no-cache")
        ]
