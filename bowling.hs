
module Bowling where

toFrames :: [Int] -> [[Int]]
toFrames (10:b:c:xs)               = [10,b,c]:toFrames (b:c:xs)
toFrames ( a:b:c:xs) | a + b == 10 = [ a,b,c]:toFrames (  c:xs)
                     | otherwise   = [ a,b  ]:toFrames (  c:xs)
toFrames [10,b] = [[10,b],[b]]
toFrames xs     = [xs]

showRoll :: Int -> String
showRoll 0 = "-"
showRoll a = show a

showFrame :: [Int] -> String
showFrame (10:_) = " X"
showFrame [a] = showRoll a ++ " "
showFrame (a:b:_) | a + b == 10 = showRoll a ++ "/"
                  | otherwise   = showRoll a ++ showRoll b

completeFrame :: [Int] -> Bool
completeFrame [_,_,_]  = True
completeFrame [a,b]    = a+b < 10
completeFrame _        = False

-- | Score a game represented as a list of the number of pins knocked
-- down by each roll.
--
-- >>> score [3,4,10,5,5,10,10,3] == 7 + 20 + 20 + 23 + 13 + 3
-- True

score :: [Int] -> Int
score = sum . map sum . toFrames

-- | Return a string of two lines. The first line is how the
-- individual rolls are marked, and the second line is a
-- comma-separated list of displayed frame scores. Note that partial
-- frame scores are not displayed.
--
-- >>> putStrLn $ showFrames [3,4,10,5,5,10,10,3]
-- 34 X5/ X X3 
-- [7,27,47,70]

showFrames :: [Int] -> String
showFrames xs = concatMap showFrame frames ++ "\n" ++ show scores
  where scores = tail $ scanl (+) 0 $ map sum $ takeWhile completeFrame frames
        frames = toFrames xs

