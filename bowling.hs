
module Bowling where

import Data.List (inits, tails)

data Frame
  = Strike
  | Spare Int Int
  | Plain Int Int
  | Partial Int
  deriving (Show, Eq)

first :: Frame -> Int
first Strike = 10
first (Spare x _) = x
first (Plain x _) = x
first (Partial x) = x

both :: Frame -> Int
both Strike = error "wtf"
both (Spare x y) = x+y
both (Plain x y) = x+y
both (Partial x) = x            -- must be the final value

prettyInt :: Int -> String
prettyInt 0 = "-"
prettyInt x = show x

pretty :: Frame -> [Char]
pretty Strike = " X"
pretty (Spare x y) = prettyInt x ++ "/"
pretty (Plain x y) = prettyInt x ++ prettyInt y
pretty (Partial x) = prettyInt x ++ " "

-- | Pretty print the top line of frames.
--
-- >>> prettyFrames (toFrames [3,4,10,5,5,10,10,3]) == "34 X5/ X X3 "
-- True

prettyFrames :: [Frame] -> String
prettyFrames fs = concatMap pretty fs

-- | List of raw pin scores to frames.
--
-- >>> toFrames [4,6,3] == [Spare 4 6, Partial 3]
-- True
--
-- >>> toFrames []
-- []
--
-- >>> toFrames [4]
-- [Partial 4]
--
-- >>> toFrames [1,3]
-- [Plain 1 3]
--
-- >>> toFrames [3,7]
-- [Spare 3 7]
--
-- >>> toFrames [10]
-- [Strike]
--
-- >>> toFrames [3,4,7,3,10,3,4] == [Plain 3 4, Spare 7 3, Strike, Plain 3 4]
-- True
--
-- >>> toFrames (replicate 12 10) == replicate 12 Strike
-- True

toFrames :: [Int] -> [Frame]
toFrames [] = []
toFrames (10:xs) = Strike : toFrames xs
toFrames [x] = [Partial x]
toFrames (x:y:zs)
  | x + y == 10 = Spare x y : toFrames zs
  | otherwise   = Plain x y : toFrames zs


-- | Convert a list of frames to a score.
--
-- >>> toScore []
-- 0
--
-- >>> toScore [Strike]
-- 10
--
-- >>> toScore [Spare 3 7]
-- 10
--
-- >>> toScore [Plain 3 6]
-- 9
--
-- >>> toScore [Partial 3]
-- 3
--
-- >>> toScore [Plain 5 2, Partial 3]
-- 10
--
-- >>> toScore [Plain 5 2, Plain 0 3]
-- 10
--
-- >>> toScore [Strike, Plain 2 3] == 15 + 5
-- True
--
-- >>> toScore [Spare 5 5, Plain 2 3] == 12 + 5
-- True
--
-- >>> toScore [Strike, Strike] == 20 + 10
-- True
--
-- >>> toScore [Strike, Strike, Strike] == 30 + 20 + 10
-- True
--
-- >>> toScore [Strike, Strike, Strike, Strike] == 30 + 30 + 20 + 10
-- True
--
-- >>> toScore [Spare 5 5, Spare 7 3, Plain 3 6] == 17 + 13 + 9
-- True

toScore :: [Frame] -> Int
toScore [] = 0
toScore (Partial x : zs) = x + toScore zs
toScore (Plain x y : zs) = x + y + toScore zs
toScore [Spare x y] = x + y
toScore (Spare x y : next : zs) = 10 + first next + toScore (next : zs)
toScore [Strike] = 10
toScore [Strike, Strike] = 30
toScore (Strike : Strike : next : zs)
  = 20 + first next + toScore (Strike : next : zs)
toScore (Strike : next : zs)    -- next cannot be a Strike
  = 10 + both next + toScore (next : zs)

-- | Bowling Code Kata from BayHac 2013
--
-- >>> bowl [1,2,3,4]    -- [1 2] [3 4]
-- 10
--
-- >>> bowl [6]
-- 6
--
-- >>> bowl [3,4,10,5,5,10,10,3] == 7 + 20 + 20 + 23 + 13 + 3
-- True
--

bowl :: [Int] -> Int
bowl = toScore . toFrames

-- | Cumulative frame scores.
--
-- >>> cumulativeScores (toFrames [3,4,10,5,5,10,10,3]) == [7,27,47,70,83,86]
-- True

cumulativeScores :: [Frame] -> [Int]
cumulativeScores fs = tail $ scanl (+) 0 perFrameScores
  where perFrameScores = zipWith (-) tailScores (tail tailScores)
        tailScores = map toScore $ tails fs
        
bowlDisplay :: [Int] -> String
bowlDisplay xs = prettyFrames fs ++ "\n" ++ show (cumulativeScores fs)
  where fs = toFrames xs
        
testdata :: [Int]
testdata = [3,4,10,5,5,10,10,3]

