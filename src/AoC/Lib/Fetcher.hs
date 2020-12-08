module AoC.Lib.Fetcher where

import AoC.Lib.Day
import AoC.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client
import Network.HTTP.Req as Req
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import Web.Cookie (SetCookie (..), defaultSetCookie)

fetchDay :: String -> Day -> IO ()
fetchDay year day = do
  fp <- getDataFileName $ "input/" <> displayDayFile day
  alreadyFetched <- doesFileExist fp
  when alreadyFetched $ error "Input file already fetched"
  session <- getEnv "AOC_SESSION"
  now <- getCurrentTime
  bs <- fetchFile year session now day
  writeFile fp bs
  success "Successfully fetched input file"

fetchFile :: String -> String -> UTCTime -> Day -> IO Text
fetchFile year session now day = do
  let host = "adventofcode.com"
      url = Req.https (toS host) /: toS year /: "day" /: show (getDay day) /: "input"
  bs <-
    Req.runReq Req.defaultHttpConfig $
      Req.req Req.GET url Req.NoReqBody Req.bsResponse $
        Req.cookieJar (mkSessionCookie host session now)
  pure $ decodeUtf8 $ Req.responseBody bs

mkSessionCookie :: String -> String -> UTCTime -> CookieJar
mkSessionCookie host session now =
  createCookieJar $
    maybeToList $
      generateCookie
        defaultSetCookie
          { setCookieName = "session",
            setCookieValue = BS.pack session
          }
        defaultRequest {host = BS.pack host}
        now
        True
