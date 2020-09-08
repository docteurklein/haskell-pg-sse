module Main where

import Data.ByteString.Builder (string8)
import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString(..))
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (encodeUtf8)
import Control.Concurrent.Async (async)
import Control.Concurrent.Chan
import Control.Monad (forever, void, forM_)
import Network.Wai (Application, Middleware)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Gzip (gzip, def)
import Hasql.Connection (acquire, Connection)
import Hasql.Statement (Statement(..))
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Session as Session
import Hasql.Notifications (listen, unlisten, toPgIdentifier, waitForNotifications)
import GHC.Generics
import Data.Aeson
import System.IO (hPutStrLn, stderr)


data Query = Query {
    sql :: Text
  , params :: [Text]
  , role :: Maybe Text
} deriving (Generic, Show)

instance FromJSON Query

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
          encoder = E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text)))
          decoder = D.rowVector (D.column (D.nonNullable D.text))

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
