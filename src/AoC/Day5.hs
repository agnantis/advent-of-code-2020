{-# LANGUAGE TypeApplications #-}
{-
   --- Day 5: Binary Boarding ---

You board your plane only to discover a new problem: you dropped your boarding pass! You aren't sure which seat is yours, and all of the flight attendants are busy with the flood of people that suddenly made it through passport control.

You write a quick program to use your phone's camera to scan all of the nearby boarding passes (your puzzle input); perhaps you can find your seat through process of elimination.

Instead of zones or groups, this airline uses binary space partitioning to seat people. A seat might be specified like FBFBBFFRLR, where F means "front", B means "back", L means "left", and R means "right".

The first 7 characters will either be F or B; these specify exactly one of the 128 rows on the plane (numbered 0 through 127). Each letter tells you which half of a region the given seat is in. Start with the whole list of rows; the first letter indicates whether the seat is in the front (0 through 63) or the back (64 through 127). The next letter indicates which half of that region the seat is in, and so on until you're left with exactly one row.

For example, consider just the first seven characters of FBFBBFFRLR:

    Start by considering the whole range, rows 0 through 127.
    F means to take the lower half, keeping rows 0 through 63.
    B means to take the upper half, keeping rows 32 through 63.
    F means to take the lower half, keeping rows 32 through 47.
    B means to take the upper half, keeping rows 40 through 47.
    B keeps rows 44 through 47.
    F keeps rows 44 through 45.
    The final F keeps the lower of the two, row 44.

The last three characters will be either L or R; these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7). The same process as above proceeds again, this time with only three steps. L means to keep the lower half, while R means to keep the upper half.

For example, consider just the last 3 characters of FBFBBFFRLR:

    Start by considering the whole range, columns 0 through 7.
    R means to take the upper half, keeping columns 4 through 7.
    L means to take the lower half, keeping columns 4 through 5.
    The final R keeps the upper of the two, column 5.

So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

Every seat also has a unique seat ID: multiply the row by 8, then add the column. In this example, the seat has ID 44 * 8 + 5 = 357.

Here are some other boarding passes:

    BFFFBBFRRR: row 70, column 7, seat ID 567.
    FFFBBBFRRR: row 14, column 7, seat ID 119.
    BBFFBBFRLL: row 102, column 4, seat ID 820.

As a sanity check, look through your list of boarding passes. What is the highest seat ID on a boarding pass?

-}
module AoC.Day5 where

import Control.Arrow ((&&&))

data ColPos = F | B deriving (Show, Read, Enum)
data RowPos = L | R deriving (Show, Read, Enum)

data Entry = Entry Int Int deriving (Show, Read)
type Input = [Entry]

type Output = Int

file :: String
file = "src/input/day5"

fstStar :: Input -> Output
fstStar = maximum . fmap encode
 where encode (Entry x y) = x * 8 + y

sndStar :: Input -> Output
sndStar = undefined

parseEntry :: String -> Entry
parseEntry str = 
  let (rowS, colS) = splitAt 7 str
      row = toBinary $ fmap (read @ColPos. pure) rowS
      col = toBinary $ fmap (read @RowPos . pure) colS
  in Entry row col

toBinary :: Enum a => [a] -> Int
toBinary xs =
  let binData = reverse $ fmap fromEnum xs :: [Int]
   in sum $ zipWith (\a e -> a * 2^e) binData [0::Int ..]

main :: IO ()
main = do
  input <- fmap parseEntry . lines <$> readFile file
  print . (fstStar &&& sndStar) $ input