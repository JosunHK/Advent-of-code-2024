import Data.List.Split
import Data.List (sort, group)
import Data.Maybe
import System.IO
import Text.Read
import Prelude

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  hSetEncoding input utf8
  reports <- readReport input [[]]
  print $ evalReports $ init reports
  hClose input

readReport :: Handle -> [[Integer]] -> IO [[Integer]]
readReport fh acc = do
  ineof <- hIsEOF fh
  if ineof
    then return acc
    else do
      inpStr <- hGetLine fh
      let row = map (\x -> (readMaybe x) :: Maybe Integer) $ splitOn " " inpStr
      if length (filter (isNothing) row) > 0 
        then return acc
        else readReport fh ((map fromJust row):acc)

evalReports :: [[Integer]] -> Integer
evalReports = fromIntegral . length . filter id . map eval

eval :: [Integer] -> Bool
eval xs = getRes $ foldl go (0, True, head xs) $ tail xs
    where go :: (Integer, Bool, Integer) -> Integer -> (Integer, Bool, Integer)      
          go acc@(sign, eval, prev) x 
            | eval == False = acc
            | prev == x = (newSign, False, x)
            | newSign + sign == 0 = (newSign, False, x)
            | abs (prev - x) < 1 || abs (prev - x) > 3 = (newSign, False, x) 
            | otherwise = (newSign, True, x)
                where newSign = signum $ prev - x 


getRes :: (a, b, c) -> b
getRes (_, b, _) = b
