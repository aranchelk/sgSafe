-- Generate random valid combinations for Sargent & Greenleaf 6730 locks --

module Main where

import Lib (shuffle)

main :: IO ()
main = do
    c <- genValidCombo
    putStrLn $ show $ c


genValidCombo = do
    let r = [0..99]
    xs <- shuffle r
    ys <- shuffle r
    zs <- shuffle r

    return $ head $ filter validateAll $ allCombos xs ys zs


-- Lazily build list of all possible combinations
-- Input lists must be pre-shuffled for randomly ordered output
allCombos xs ys zs = concatMap (\x -> allYsAndZs x ys zs) xs
    where 
        allYsAndZs x ys zs = concatMap (\y -> allZs x y zs) ys
            where
                allZs x y zs = map (\z -> (x, y, z)) zs 


countValidCombos = length $ filter validateAll $ allCombos [0..99] [0..99] [0..99]

----------------------
-- Validation Rules --
----------------------
validateAll combo = reduceAnd $ map ($ combo) vRules

vRules = [ inDialRange
         , hasDriveCamPinClearence
         , hasWheelPackPinClearence
         , notSequential
         ] 

inDialRange (x, y, z) = reduceAnd $ map (inRange 0 99) [x, y, z]

hasDriveCamPinClearence (_, _, z) =  z > 20 && z < 95

hasWheelPackPinClearence (x, y, z) = (clearOfWheel2 x) && (clearOfWheel2 z)
    where clearOfWheel2 c = (minDialDistance 100 y c) > 10

notSequential (x, y, z) = not $ seqUp || seqDown
    where seqUp = x < y && y < z
          seqDown = x > y && y > z


----------------------
-- Helper functions --
----------------------
inRange a b c = (c >= a) && (c <= b)
reduceAnd = foldr (&&) True
-- Find the shortest distance between 2 numbers on dial (right or left)
minDialDistance c x y = min d d'
    where d = (x - y + c) `mod` c
          d' = (y - x + c) `mod` c
