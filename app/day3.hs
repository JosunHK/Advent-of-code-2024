import Data.List.Split
import Data.Maybe
import System.IO
import Text.Read
import Prelude
import Data.List (isInfixOf)

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  hSetEncoding input utf8
  str <- readString input
  let splitStr = splitOn "don't()" str
  let filterStr = concat $ concatMap (tail . splitOn "do()") $ filter (isInfixOf "do()") $ tail splitStr
  let parsedStr = filter (elem ',') $ map (takeWhile (/=')')) $ splitOn "mul(" (head splitStr ++ filterStr)
  print $ sum $ map trySum parsedStr
  hClose input

trySum :: String -> Integer
trySum s 
       | length ints /= 2 = 0
       | isNothing num1 || isNothing num2 = 0
       | otherwise = fromJust num1 * fromJust num2
    where ints = splitOn "," s
          num1 = readMaybe $ head ints
          num2 = readMaybe $ last ints

readString:: Handle -> IO String
readString fh = do
  ineof <- hIsEOF fh
  if ineof
    then return ""
    else do hGetLine fh

