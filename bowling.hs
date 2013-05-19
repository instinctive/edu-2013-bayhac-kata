
module Bowling where

import Control.Monad (liftM2, foldM)
import Control.Monad.Writer (Writer, runWriter, writer)
import Data.Char (intToDigit)
import Text.Printf (printf)

data Roll = Strike | Spare Int | One Int | Two Int deriving (Eq, Show)

toRoll :: Roll -> Int -> Roll
toRoll (One x)  y | x+y == 10 = Spare y
                  | otherwise = Two y
toRoll _       10             = Strike
toRoll _        x             = One x

toRolls :: [Int] -> [Roll]
toRolls xs = rs where rs = zipWith toRoll (Strike:rs) xs
                      
type Frame = [Roll]

toFrames :: [Roll] -> [Frame]
toFrames = go 1 where
  go _ []               = []
  go 10 rs              = [rs]
  go n rs@(Strike:_)    = take 3 rs : go (n+1) (drop 1 rs)
  go n rs@(_:Spare _:_) = take 3 rs : go (n+1) (drop 2 rs)
  go n rs               = take 2 rs : go (n+1) (drop 2 rs)

rollScore :: Roll -> Int
rollScore Strike    = 10
rollScore (Spare x) = x
rollScore (One x)   = x
rollScore (Two x)   = x

frameScore :: Frame -> Maybe Int
frameScore rs@[Strike,_      ,_] = Just $ sum $ map rollScore rs
frameScore rs@[_     ,Spare _,_] = Just $ sum $ map rollScore rs
frameScore rs@[One _ ,Two _  ]   = Just $ sum $ map rollScore rs
frameScore _                     = Nothing

pinChar :: Int -> Char
pinChar 0 = '-'
pinChar x = intToDigit x

rollChar :: Roll -> Char
rollChar Strike    = 'X'
rollChar (Spare _) = '/'
rollChar (One x)   = pinChar x
rollChar (Two x)   = pinChar x

frameText :: Bool -> Frame -> String
frameText False (Strike:_) = "  |  | X|"
frameText final rs = printf " %c| %c| %c|" a b c
  where (a:b:c:_) = if final then cs else ' ' : cs
        cs = map rollChar rs ++ repeat ' '

type Output = (String, String, String, String, String)

bowlFrame :: (Maybe Int) -> (Bool, Frame) -> Writer Output (Maybe Int)
bowlFrame score (final, frame) = writer (score', output)
  where score' = liftM2 (+) score (frameScore frame)
        output = (top, frameText final frame, mid final, scoreline, bot)
        top        = "--+--+--+"
        mid False  = "  +--+--+"  
        mid True   = "--+--+--+"  
        bot        = "--------+"
        scoreline = case score' of
          Nothing -> "        |"
          Just x  -> printf "%8d|" x

bowlWriter :: [Frame] -> Writer Output (Maybe Int)
bowlWriter fs = foldM bowlFrame (Just 0) (zip (map (==10) [1..10]) fs')
  where fs' = fs ++ repeat []
        
-- | Given a raw list of pin scores, output a display of the complete
-- bowling display, as it might appear in an alley.
--
-- >>> bowl [1,3,6,4,10,3,2,10,5,5,10,10,3,5,10,7,3]
-- +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
-- |  | 1| 3|  | 6| /|  |  | X|  | 3| 2|  |  | X|  | 5| /|  |  | X|  |  | X|  | 3| 5| X| 7| /|
-- |  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+  +--+--+--+--+--+
-- |       4|      24|      39|      44|      64|      84|     107|     125|     133|     153|
-- +--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+

bowl :: [Int] -> IO ()
bowl xs = do
  putStrLn ('+' : a)
  putStrLn ('|' : b)
  putStrLn ('|' : c)
  putStrLn ('|' : d)
  putStrLn ('+' : e)
  where (_, (a,b,c,d,e)) = runWriter (bowlWriter fs)
        fs = toFrames $ toRolls xs
