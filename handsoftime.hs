import Control.Monad (liftM)
import Data.Array.Unboxed

type Position = Int
type Board = (Int, UArray Position Int)
type Path = [Position]

makeBoard :: [Int] -> Board
makeBoard ns = (size, listArray (0,size - 1) ns) where size = length ns

data GameState = GameState [Position] Int

steps :: Board -> GameState -> [Position]
steps (size, _)     (GameState []     _) = [0..size - 1]
steps (size, steps) (GameState (p:ps) _) =
  map (`mod` size) [p + step, p + size - step] where step = steps!p

validPathsFrom :: Board -> GameState -> [Path]
validPathsFrom board state@(GameState ps numAvail)
  | numAvail == 0	= [reverse ps]
  | otherwise     =
    [ path | p <- steps board state, not (p `elem` ps),
             path <- validPathsFrom board $ GameState (p:ps) (numAvail - 1) ]

main = do
  putStrLn "Input the puzzle as a list of Ints:"
  boardList <- liftM read getLine
  putStrLn "Calculating..."
  let board@(size, _) = makeBoard boardList
  print . head . validPathsFrom board $ GameState [] size
