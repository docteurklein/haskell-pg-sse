module Main where

import Data.ByteString.Builder (string8)
import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString(..))
import Data.Text (Text(..), unpack)
import Data.Text.Encoding (encodeUtf8)
import Control.Concurrent.Async (async, withAsync)
-- import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (forever, void)
import Network.Wai (Application, Middleware)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Gzip (gzip, def)
import Hasql.Connection (acquire, Connection)
import Hasql.Statement (Statement(..))
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Session
import Hasql.Notifications (listen, unlisten, toPgIdentifier, waitForNotifications)
import GHC.Generics
import Data.Aeson


data Query = Query {
    sql :: Text
  , params :: [Text]
} deriving (Generic, Show)

instance FromJSON Query

eventChan :: Connection -> Connection -> Chan ServerEvent -> IO ()
eventChan connListen conn chan = waitForNotifications (handler conn) connListen
    where
        handler conn channel payload = do
            let query = decode (fromStrict payload) :: Maybe Query
            case query of
                Just query' -> do
                    result <- Session.run (Session.statement (head $ params query') (select $ sql query')) conn
                    case result of
                        Left e -> print e
                        Right content -> do
                            print $ content
                            writeChan chan (ServerEvent Nothing Nothing [string8 $ unpack content])
                Nothing -> print payload

        select sql = Statement (encodeUtf8 sql) encoder decoder True where
          encoder = Encoders.param (Encoders.nonNullable Encoders.text)
          decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text))

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
        , ("Access-Control-Allow-Origin", "*")
        , ("Access-Control-Allow-Methods", "OPTIONS, GET, POST, PUT, PATCH, DELETE")
        , ("Access-Control-Allow-Headers", "Authorization, Accept")
        , ("Access-Control-Expose-Headers", "Cache-Control, Content-Language, Content-Type, Expires, Last-Modified, Pragma, Link")
        ]
