--Poker.hs

module Poker where

import Card

data Pair = Pair {pairC1 :: Card, 
                  pairC2 :: Card} deriving(Show, Eq)
data Triple = Triple {triC1 :: Card, 
                      triC2 :: Card, 
                      triC3 :: Card} deriving(Show, Eq)
data TriplePair = TriPair {tpC1 :: Card,
                           tpC2 :: Card,
                           tpC3 :: Card,
                           tpC4 :: Card,
                           tpC5 :: Card} deriving(Show, Eq)

data CommonSuit = ComSuit {csC1 :: Card,
                           csC2 :: Card,
                           csC3 :: Card,
                           csC4 :: Card,
                           csC5 :: Card} deriving(Show, Eq)

data ContinuousRank = ConRank {crC1 :: Card,
                               crC2 :: Card,
                               crC3 :: Card,
                               crC4 :: Card,
                               crC5 :: Card} deriving(Show, Eq)

combination2 :: (Eq a, Ord a) => [a] -> [[a]]
combination2 xs = [[x, y] | x <- xs, y <- xs, x < y]

combination3 :: (Eq a, Ord a) => [a] -> [[a]]
combination3 xs = [[x, y, z] | x <- xs, y <- xs, z <- xs, x < y, y < z]

getPairList :: [Card] -> [Pair]
getPairList xs = getPairList' (combination2 xs) []
    where getPairList' [] res = res
          getPairList' (x:xs) res = Pair (x !! 0) (x !! 1) : getPairList' xs res

getPairs :: [[Card]] -> [Pair]
getPairs xss = let css = filter (\xs -> length xs >= 2) xss
               in getPair css []
                  where getPair [] res = res
                        getPair (ys:yss) res = (getPairList ys) ++ (getPair yss res)

getTripleList :: [Card] -> [Triple]
getTripleList xs = getTripleList' (combination3 xs) []
    where getTripleList' [] res = res
          getTripleList' (x:xs) res = Triple (x !! 0) (x !! 1) (x !! 2) : getTripleList' xs res

getTriples :: [[Card]] -> [Triple]
getTriples xss = let css = filter (\xs -> length xs >= 3) xss
               in getTriples' css []
                  where getTriples' [] res = res
                        getTriples' (ys:yss) res = (getTripleList ys) ++ (getTriples' yss res)

getTriplePairs :: [[Card]] -> [TriplePair]
getTriplePairs xss = let triples = getTriples xss
                         pairs = getPairs xss
                     in [TriPair t1 t2 t3 p1 p2 | Triple t1 t2 t3 <- triples, Pair p1 p2 <- pairs, not (p1 `elem` [t1,t2,t3]), not (p2 `elem` [t1,t2,t3])]

comN :: [a] -> Int -> [[a]]
comN xs n = filter (\x -> length x > 0) $ comN' xs n [] [[]]
    where comN' _ 0 cur total = cur:total
          comN' all@(y:ys) c cur total 
              | lenAll <  c = total
              | lenAll == c =  (all ++ cur):total
              | lenAll >  c = l1 ++ l2
              where lenAll = length all
                    l1 = comN' ys (c - 1) (y:cur) total
                    l2 = comN' ys c cur total

getCommonSuits :: [[Card]] -> [CommonSuit]
getCommonSuits xss = getCommonSuits' xss []
    where getCommonSuits' [] res = res
          getCommonSuits' (x:xs) res = getSuitList (comN x 5) ++ getCommonSuits' xs res
              where getSuitList yss = getSuitList' yss []
                        where getSuitList' [] ss = ss
                              getSuitList' (y:ys) ss = ComSuit (y!!0) (y!!1) (y!!2) (y!!3) (y!!4) : getSuitList' ys ss

main = do
  let str1 = "AS QH QC QD JS JH JC 9D 9S 8H 8C 7H 7S 5H 5C 4D 4S 3H"
      str2 = "KS KH TC TD"
      dr = groupByRank.sortByRankSuit.parseCardString $ str1
      ds = groupBySuit.sortBySuitRank.parseCardString $ str1
  --putStrLn $ show c1
  --putStrLn $ show c2
  putStrLn $ show dr
  --putStrLn $ show ds
  putStrLn $ show (getPairs dr)
  putStrLn $ show (getTriples dr)
  putStrLn $ show (getTriplePairs dr)
  putStrLn $ show ds
  putStrLn $ show.getCommonSuits $ ds           
