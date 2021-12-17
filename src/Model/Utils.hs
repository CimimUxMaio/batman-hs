module Model.Utils where

import Data.List.Extra (takeEnd)

indexEnd :: Int -> [a] -> a
indexEnd i = head . takeEnd i

current :: [a] -> a
current = indexEnd 1

previous :: [a] -> a
previous = indexEnd 2

average :: (Fractional a) => [a] -> a
average nums = (/ listSize) . sum $ nums
    where listSize = fromIntegral . length $ nums