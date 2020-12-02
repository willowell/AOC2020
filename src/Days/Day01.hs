module Days.Day01 (part01, part02) where

import System.IO

part01 :: IO ()
part01 = do
  handle <- openFile "data/expenses.txt" ReadMode
  contents <- hGetContents handle

  let numList = (map (read :: String -> Int) $ lines contents) :: [Int]

  print $ head $ [ a * b | a <- numList, b <- numList, (a + b) == 2020 ]


part02 :: IO ()
part02 = do
  handle <- openFile "data/expenses.txt" ReadMode
  contents <- hGetContents handle

  let numList = (map (read :: String -> Int) $ lines contents) :: [Int]

  print $ head $ [ a * b * c | a <- numList, b <- numList, c <- numList, (a + b + c) == 2020 ]