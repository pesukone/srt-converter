{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.List (intercalate)
import Data.List.Split
import Data.Maybe
import Numeric.Natural
import System.Environment
import Text.Printf

main :: IO ()
main = do
  (fileIn : fileOut : _) <- getArgs

  contents <- readFile fileIn
  let subtitles = map toSubtitle . splitToBlocks $ contents

  writeFile fileOut (unlines . toLines . map convertTimes $ subtitles)

data Subtitle = Subtitle
  { number :: Natural,
    from :: Time,
    to :: Time,
    texts :: (String, Maybe String)
  }
  deriving (Show)

-- Number of milliseconds
type Time = Natural

data Timestamp = Timestamp
  { hours :: Natural,
    minutes :: Natural,
    seconds :: Natural,
    milliseconds :: Natural
  }

instance Show Timestamp where
  show Timestamp {hours, minutes, seconds, milliseconds} =
    printf "%02d" hours
      ++ ":"
      ++ printf "%02d" minutes
      ++ ":"
      ++ printf "%02d" seconds
      ++ ","
      ++ printf "%03d" milliseconds

toMilliseconds :: Timestamp -> Time
toMilliseconds Timestamp {milliseconds, seconds, minutes, hours} =
  milliseconds
    + 1000 * seconds
    + 60 * 1000 * minutes
    + 60 * 60 * 1000 * hours

splitToBlocks :: String -> [[String]]
splitToBlocks = map (take 4) . linesBy (== "") . lines

-- 12:34:56,789
stringToTimestamp :: String -> Timestamp
stringToTimestamp str =
  Timestamp
    { hours = read . take 2 $ str :: Natural,
      minutes = read . take 2 . drop 3 $ str :: Natural,
      seconds = read . take 2 . drop 6 $ str :: Natural,
      milliseconds = read . take 3 . drop 9 $ str :: Natural
    }

toSubtitle :: [String] -> Subtitle
toSubtitle [] = undefined
toSubtitle (i : times : ws) =
  Subtitle
    { number = read i :: Natural,
      to = toMilliseconds to,
      from = toMilliseconds from,
      texts
    }
  where
    (from, to) = fromDuration times
    texts =
      if length ws == 1
        then (head ws, Nothing)
        else (head ws, Just $ ws !! 1)
toSubtitle (_ : _) = undefined

-- Reduces times by 4 milliseconds (1 frame in 25fps) for every
-- second that has passed, converting 25fps timings to 24fps
convertTimes :: Subtitle -> Subtitle
convertTimes Subtitle {number, from, to, texts} =
  Subtitle
    { number,
      from = from - 4 * (from `div` 1000),
      to = to - 4 * (to `div` 1000),
      texts
    }

-- 12:34:56,789 --> 12:34:56,789
fromDuration :: String -> (Timestamp, Timestamp)
fromDuration s =
  ( stringToTimestamp $ take 12 s,
    stringToTimestamp . take 12 . drop 17 $ s
  )

toLines :: [Subtitle] -> [String]
toLines = intercalate [""] . map toLines'

toLines' :: Subtitle -> [String]
toLines' Subtitle {number, from, to, texts} =
  [ show number,
    show (toTimestamp from) ++ " --> " ++ show (toTimestamp to),
    t1
  ]
    ++ maybeToList t2
  where
    (t1, t2) = texts

toTimestamp :: Time -> Timestamp
toTimestamp t = do
  let hours = t `div` (60 * 60 * 1000)
  let remain = t `mod` (60 * 60 * 1000)

  let minutes = remain `div` (60 * 1000)
  let remain' = remain `mod` (60 * 1000)

  let seconds = remain' `div` 1000
  let milliseconds = remain' `mod` 1000

  Timestamp {hours, minutes, seconds, milliseconds}
