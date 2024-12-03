import Data.List.Split
import Data.List (sort, group)
import Data.Maybe
import System.IO
import Text.Read
import Prelude
import Control.Applicative 

main :: IO ()
main = do
  input <- openFile "input.txt" ReadMode
  hSetEncoding input utf8
  location <- readLocation input ([], [])
  print $ findDistance location
  print $ findSimilarity location
  hClose input

readLocation :: Handle -> ([Integer], [Integer]) -> IO ([Integer], [Integer])
readLocation fh acc@(x1, y1) = do
  ineof <- hIsEOF fh
  if ineof
    then return acc
    else do
      inpStr <- hGetLine fh
      let row = map (\x -> (readMaybe x) :: Maybe Integer) $ splitOn "   " inpStr
      let x2 = head row
      let y2 = last row
      if isNothing x2 || isNothing y2
        then return acc
        else readLocation fh (fromJust x2 : x1, fromJust y2 : y1)

findDistance :: ([Integer],[Integer]) -> Integer
findDistance (l1, l2) = sum $ map abs $ zipWith (subtract) (sort l1) (sort l2) 

findSimilarity :: ([Integer],[Integer]) -> Integer
findSimilarity (l1, l2) = foldl go 0 l1
            where score = map (\x -> (head x, length x)) $ group $ sort l2
                  go acc x = acc + (fst key) * (fromIntegral (snd key))
                    where key = if length keyList == 0 then (0, 0) else head keyList
                            where keyList = filter (\(y1, y2) -> x == y1) score
