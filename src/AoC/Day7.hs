{-# LANGUAGE TupleSections #-}
{-
   --- Day 7: Handy Haversacks ---

You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab some food: all flights are currently delayed due to issues in luggage processing.

Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!

For example, consider the following rules:

light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.

These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.

You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)

In the above rules, the following options would be available to you:

    A bright white bag, which can hold your shiny gold bag directly.
    A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
    A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
    A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.

So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.

How many bag colors can eventually contain at least one shiny gold bag? (The list of rules is quite long; make sure you get all of it.)

-}
module AoC.Day7 where

import Control.Arrow ((&&&))
import Data.Containers.ListUtils (nubOrd)
import Data.Functor (($>))
import Data.Map (findWithDefault)
import Data.Map as M (Map)
import Data.Map.Internal (fromListWith)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, runParser, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, space, string)

type Bag = String

type NumBag = (Int, Bag)

data Entry = Entry Bag [NumBag] deriving (Show)

type Parser = Parsec Void String

type Input = [Entry]

type Output = Int

file :: String
file = "src/input/day7"

elfBag :: Bag
elfBag = "shiny gold"

------------
-- Parser --
------------

sepWith :: String -> Parser a -> Parser [a]
sepWith sep p = do
  fstEntry <- p
  restEntries <- many . try $ string sep *> p
  return $ fstEntry : restEntries

bag :: Parser Bag
bag = do
  adjective <- some letterChar <* space
  color <- some letterChar
  return $ adjective <> (' ' : color)

emptyNumBag :: Parser ()
emptyNumBag = string "no other bags" $> ()

numBag :: Parser NumBag
numBag = do
  no <- some digitChar <* space
  ntry <- bag <* space <* (try (string "bags") <|> string "bag")
  return (read no, ntry)

numBags :: Parser [NumBag]
numBags = (emptyNumBag $> []) <|> sepWith ", " numBag

entry :: Parser Entry
entry = do
  theBag <- bag <* string " bags contain "
  theNumBags <- numBags <* char '.'
  return $ Entry theBag theNumBags

entries :: Parser [Entry]
entries = sepWith "\n" entry <* char '\n' <* eof --we need the '\n' as it is also a separator

---------------
-- Algorithm --
---------------

entryToPair :: Entry -> [(Bag, [Bag])]
entryToPair (Entry a b) = fmap ((,[a]) . snd) b

parentPairs :: [Entry] -> Map Bag [Bag]
parentPairs = fromListWith (<>) . concatMap entryToPair

ancestors :: Bag -> Map Bag [Bag] -> [Bag]
ancestors b mp =
  let parents = findWithDefault [] b mp
      ansestors' = parents >>= (`ancestors` mp)
   in nubOrd $ parents ++ ansestors'

fstStar :: Input -> Output
fstStar = length . ancestors elfBag . parentPairs

sndStar :: Input -> Output
sndStar = undefined

main :: IO ()
main = do
  Right entries' <- runParser entries file <$> readFile file
  print . (fstStar &&& sndStar) $ entries'
