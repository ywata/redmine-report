module Redmine.Utils (parseRHTime, parseShortTime) where
import Data.Time.Format     (parseTime)
import Data.Time.Clock      (UTCTime)
import Data.Time.Calendar (Day)
import System.Locale        (defaultTimeLocale)


parseRHTime :: String -> Maybe UTCTime
parseRHTime = parseTime defaultTimeLocale "%FT%X%QZ"

parseShortTime :: String -> Maybe Day
parseShortTime = parseTime defaultTimeLocale "%F"

a = parseRHTime "2015-10-11T02:00:00Z"
b = parseRHTime "2015-10-11T03:00:00Z"
x = parseShortTime "2015-10-11"

