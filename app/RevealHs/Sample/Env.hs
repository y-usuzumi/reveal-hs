module RevealHs.Sample.Env where

import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime

today :: IO String
today = do
  tz <- getCurrentTimeZone
  t <- fmap (utcToLocalTime tz) getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) t
