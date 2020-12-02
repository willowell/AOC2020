module Days.Day02 (part01, part02) where

import System.IO

import qualified Data.Algebra.Boolean as Boolean

import Text.Regex.PCRE
import Text.Regex.PCRE ()

getLinesFrom :: String -> IO String
getLinesFrom path = do
  handle <- openFile path ReadMode
  hGetContents handle


sumTrues :: [Bool] -> Int
sumTrues xs = 
  sum $ map fromEnum xs


parsePassword :: String -> [String]
parsePassword = 
  concat . getPasswordParts
  where
    getPasswordParts :: String -> [[String]]
    getPasswordParts str =
      str =~ "\\w+"


part01 :: IO ()
part01 = do
  ls <- lines <$> getLinesFrom "data/passwords.txt"

  let result = sumTrues $ map (isValid . parsePassword) ls

  print result

  where
    isValid :: [String] -> Bool
    isValid [s, e, l, p] =
      let count = length $ filter (== (head l)) p;
          start = (read :: String -> Int) s;
          end = (read :: String -> Int) e
      in  start <= count && count <= end


part02 :: IO ()
part02 = do
  ls <- lines <$> getLinesFrom "data/passwords.txt"

  let result = sumTrues $ map (isValid . parsePassword) ls

  print result

  where
    isValid :: [String] -> Bool
    isValid [f, s, n, p] = 
      let indexFromOne xs i = xs !! (i - 1); 
          first = indexFromOne p $ (read :: String -> Int) f;
          second = indexFromOne p $ (read :: String -> Int) s;
          needle = head n
      in  first == needle `Boolean.xor` second == needle 
