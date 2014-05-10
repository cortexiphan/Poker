--Card.hs

module Card where

import Data.List

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
     | Ten | Jack | Queen | King | Ace deriving(Show, Eq, Ord, Enum)
data Suit = Diamond | Club | Heart | Spade deriving(Show, Eq, Ord, Enum)
data Card = Card {rank :: Rank, suit :: Suit} deriving(Eq)

instance Show Card where
         show (Card r s) = show r ++ "-" ++ show s

parseCard :: String -> Card
parseCard str = let r = case head str of
                             'A' -> Ace
                             'K' -> King
                             'Q' -> Queen
                             'J' -> Jack
                             'T' -> Ten
                             n   -> toEnum (read [n] - 2)
                    s = case tail str of
                             "S" -> Spade
                             "H" -> Heart
                             "C" -> Club
                             "D" -> Diamond
                in Card r s

parseCardList :: [String] -> [Card]
parseCardList (x:xs) =parseCard x : parseCardList xs
parseCardList [] = []

parseCardString :: String -> [Card]
parseCardString = parseCardList.words

compareRank :: Card -> Card -> Ordering
compareRank c1 c2 = compare (rank c1) (rank c2)

compareSuit :: Card -> Card -> Ordering
compareSuit c1 c2 = compare (suit c1) (suit c2)

compareRankThenSuit :: Card -> Card -> Ordering
compareRankThenSuit c1 c2 = let ret = compareRank c1 c2
                            in case ret of
                              EQ -> compareSuit c1 c2
                              _  -> ret

compareSuitThenRank :: Card -> Card -> Ordering
compareSuitThenRank c1 c2 = let ret = compareSuit c1 c2
                            in case ret of
                              EQ -> compareRank c1 c2
                              _  -> ret

sortByRank :: [Card] -> [Card]
sortByRank = sortBy compareRank

sortBySuit :: [Card] -> [Card]
sortBySuit = sortBy compareSuit

sortByRankSuit :: [Card] -> [Card]
sortByRankSuit = sortBy compareRankThenSuit

sortBySuitRank :: [Card] -> [Card]
sortBySuitRank = sortBy compareSuitThenRank

equalRank :: Card -> Card -> Bool
equalRank c1 c2 = rank c1 == rank c2

equalSuit :: Card -> Card -> Bool
equalSuit c1 c2 = suit c1 == suit c2

dupGroups :: (Card -> Card -> Bool) -> [Card] -> [[Card]]
dupGroups f xs = dup' xs [] [[]]
  where dup' [] [] res = res
        dup' [] (g:gs) res = (g:gs):res
        dup' (c:cs) [] res = dup' cs [c] res
        dup' (c:cs) (g:gs) res
          | f c g = dup' cs (c:g:gs) res
          | otherwise = dup' cs [c] ((g:gs):res)

groupByRank :: [Card] -> [[Card]]
groupByRank = groupBy equalRank

groupBySuit :: [Card] -> [[Card]]
groupBySuit = groupBy equalSuit

instance Ord Card where
    compare = compareRankThenSuit
    c1 < c2 = compare c1 c2 == LT
    c1 <= c2 = (c1 < c2) || (c1 == c2)
