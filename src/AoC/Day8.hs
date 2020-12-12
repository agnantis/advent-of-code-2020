{-
   --- Day 8: Handheld Halting ---

Your flight to the major airline hub reaches cruising altitude without incident. While you consider checking the in-flight menu for one of those drinks that come with a little umbrella, you are interrupted by the kid sitting next to you.

Their handheld game console won't turn on! They ask if you can take a look.

You narrow the problem down to a strange infinite loop in the boot code (your puzzle input) of the device. You should be able to fix it, but first you need to be able to run the code in isolation.

The boot code is represented as a text file with one instruction per line of text. Each instruction consists of an operation (acc, jmp, or nop) and an argument (a signed number like +4 or -20).

    acc increases or decreases a single global value called the accumulator by the value given in the argument. For example, acc +7 would increase the accumulator by 7. The accumulator starts at 0. After an acc instruction, the instruction immediately below it is executed next.
    jmp jumps to a new instruction relative to itself. The next instruction to execute is found using the argument as an offset from the jmp instruction; for example, jmp +2 would skip the next instruction, jmp +1 would continue to the instruction immediately below it, and jmp -20 would cause the instruction 20 lines above to be executed next.
    nop stands for No OPeration - it does nothing. The instruction immediately below it is executed next.

For example, consider the following program:

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6

These instructions are visited in this order:

nop +0  | 1
acc +1  | 2, 8(!)
jmp +4  | 3
acc +3  | 6
jmp -3  | 7
acc -99 |
acc +1  | 4
jmp -4  | 5
acc +6  |

First, the nop +0 does nothing. Then, the accumulator is increased from 0 to 1 (acc +1) and jmp +4 sets the next instruction to the other acc +1 near the bottom. After it increases the accumulator from 1 to 2, jmp -4 executes, setting the next instruction to the only acc +3. It sets the accumulator to 5, and jmp -3 causes the program to continue back at the first acc +1.

This is an infinite loop: with this sequence of jumps, the program will run forever. The moment the program tries to run any instruction a second time, you know it will never terminate.

Immediately before the program would run an instruction a second time, the value in the accumulator is 5.

Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?

 -}
module AoC.Day8 where

import Control.Arrow ((&&&))
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Vector ((!), fromList, Vector)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, runParser, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, space, string)

-----------
-- Types --
-----------
data CommandType = ACC | JMP | NOP deriving (Show, Eq, Ord)

data Command = Command CommandType Int deriving (Show, Eq, Ord)

type Index = Int

type Value = Int

-- current pos, accumulated value, visited positions
type State = (Index, Value, S.Set Index)

type Parser = Parsec Void String

type Input = [Command]

type Output = Int

file :: String
file = "src/input/day8"

------------
-- Parser --
------------

sepWith :: String -> Parser a -> Parser [a]
sepWith sep p = do
  fstEntry <- p
  restEntries <- many . try $ string sep *> p
  return $ fstEntry : restEntries

commandP :: Parser Command
commandP =
  let accP = string "acc" $> ACC
      jmpP = string "jmp" $> JMP
      nopP = string "nop" $> NOP
   in do
        cmdType <- accP <|> jmpP <|> nopP
        _ <- space
        sign <- (char '+' $> 1) <|> (char '-' $> (-1))
        num <- some digitChar
        pure $ Command cmdType (sign * read num)

commandsP :: Parser [Command]
commandsP = sepWith "\n" commandP <* char '\n' <* eof --we need the '\n' as it is also a separator

---------------
-- Algorithm --
---------------

fstStar :: Input -> Output
fstStar input =
  let vec = fromList input
      initState = (-1, 0, S.empty)
      initCommand = Command NOP 0
      Left out = runProgram vec initState initCommand
   in out

runProgram :: Vector Command -> State -> Command -> Either Value State
runProgram vec s@(idx, _, _) cmd =
  let (idx', acc', set') = exec cmd s
      set'' = idx `S.insert` set'
   in if idx' `S.member` set'
        then Left acc'
        else runProgram vec (idx', acc', set'') (vec ! idx')

exec :: Command -> State -> State
exec (Command NOP _) (idx, acc, set) = (idx + 1, acc, set)
exec (Command ACC x) (idx, acc, set) = (idx + 1, acc + x, set)
exec (Command JMP x) (idx, acc, set) = (idx + x, acc, set)

sndStar :: Input -> Output
sndStar = undefined

main :: IO ()
main = do
  Right entries' <- runParser commandsP file <$> readFile file
  --print entries'
  print . (fstStar &&& sndStar) $ entries'
