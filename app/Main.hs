import Data.List.Split
import Data.Maybe
import System.IO
import Text.Read
import Prelude
import Data.List (isInfixOf)

main :: IO ()
main = do
  rulesH <- openFile "rules.txt" ReadMode
  inputH <- openFile "input.txt" ReadMode
  hSetEncoding rulesH utf8
  hSetEncoding inputH utf8
  rulesStr <- readString rulesH []
  inputStr <- readString inputH []
  let rules = parse rulesStr "|"
  let input = parse inputStr ","
  print $ solve rules input
  hClose inputH

solve :: [[Integer]] -> [[Integer]] -> Integer
solve rules = sum . map (solve' rules) 

solve' :: [[Integer]] -> [Integer] -> Integer
solve' rules input = getMiddle $ snd $ foldl go (0, tail input) input 
    where go :: (Integer, [Integer]) -> Integer -> (Integer, [Integer])
          go acc@(valid, xs) x
                | valid /= 0 = acc 
                | wrongOrder = (valid+1, []) 
                | otherwise  = (valid, tail xs)
                where wrongOrder = any (`elem` xs) prevPage
                      prevPage = map head $ filter ((==x) . last) rules

getMiddle :: [Integer] -> Integer
getMiddle pages = pages !! max 0 (length pages `div` 2)

parse:: [String] -> String -> [[Integer]]
parse x delim = map (map read . splitOn delim) x 

readString:: Handle -> [String] -> IO [String]
readString fh acc = do
  ineof <- hIsEOF fh
  if ineof
    then return acc
    else do
      inpStr <- hGetLine fh
      readString fh (acc ++[inpStr])

