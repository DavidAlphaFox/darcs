--  Copyright (C) 2011 Eric Sessoms
--
--  BSD3

{-# LANGUAGE CPP #-}
#if MIN_VERSION_time(1,5,0)
-- for parseTime, which we need to use while we support time-1.4
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif
module Darcs.Util.DateTime
    ( getCurrentTime, toSeconds
    , formatDateTime, fromClockTime, parseDateTime, startOfTime
    ) where

import qualified Data.Time.Calendar as Calendar ( fromGregorian )
import Data.Time.Clock
    ( UTCTime(UTCTime), UniversalTime(ModJulianDate)
    , getModJulianDate, secondsToDiffTime, getCurrentTime
    )
import Data.Time.Format ( formatTime, parseTime )
import Data.Time.LocalTime
    ( utc
    , localTimeToUT1, ut1ToLocalTime
    , localTimeToUTC, utcToLocalTime
    )
#if MIN_VERSION_time(1,5,0)
import Data.Time ( defaultTimeLocale )
#else
import System.Locale ( defaultTimeLocale )
#endif
import System.Time ( ClockTime(TOD) )

toSeconds    :: UTCTime -> Integer
toSeconds dt = floor $
    (86400.0 :: Double) * fromRational (toMJD dt - startOfTimeMJD)

toMJD :: UTCTime -> Rational
toMJD = getModJulianDate . toUniversalTime

startOfTimeMJD :: Rational
startOfTimeMJD = toMJD startOfTime

startOfTime :: UTCTime
startOfTime = fromGregorian' 1970 1 1

fromGregorian'       :: Integer -> Int -> Int -> UTCTime
fromGregorian' y m d = fromGregorian y m d 0 0 0

fromGregorian :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
fromGregorian year month day hours minutes seconds =
    UTCTime day' (secondsToDiffTime . fromIntegral $ seconds')
  where
    day'     = Calendar.fromGregorian year month day
    seconds' = 3600 * hours + 60 * minutes + seconds

toUniversalTime :: UTCTime -> UniversalTime
toUniversalTime = localTimeToUT1 0 . utcToLocalTime utc

formatDateTime :: String -> UTCTime -> String
formatDateTime = formatTime defaultTimeLocale

parseDateTime :: String -> String -> Maybe UTCTime
parseDateTime = parseTime defaultTimeLocale

fromClockTime           :: ClockTime -> UTCTime
fromClockTime (TOD s _) = fromSeconds s

fromSeconds   :: Integer -> UTCTime
fromSeconds s = fromMJD $
    fromIntegral s / 86400 + startOfTimeMJD

fromMJD :: Rational -> UTCTime
fromMJD = fromUniversalTime . ModJulianDate

fromUniversalTime :: UniversalTime -> UTCTime
fromUniversalTime = localTimeToUTC utc . ut1ToLocalTime 0
