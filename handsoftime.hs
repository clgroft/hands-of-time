import Prelude hiding (null)
import Control.Monad (liftM)
import Data.Array.Unboxed
import Data.Set hiding (filter)

type Position = Int -- technically, Int modulo size of board
type Board = (Int, UArray Position Int) -- = (size, step from each position)
type Path = [Position]

makeBoard :: [Int] -> Board
makeBoard ns = (size, listArray (0, size - 1) ns) where size = length ns

data GameState = GameState {
  currPos :: Maybe Position, -- there is no position at the beginning
  avail   :: Set Position    -- easier checking & updating than a list
}

startState :: Board -> GameState
-- all positions are free at the start
startState (size, steps) = GameState Nothing $ fromList (indices steps)

validSteps :: Board -> GameState -> [Position]
-- every move is valid at the beginning
validSteps (_,    steps) (GameState Nothing _) = indices steps
validSteps (size, steps) (GameState (Just p) avail) =
  filter (`member` avail) [(p + step) `mod` size, (p + size - step) `mod` size]
    where step = steps!p

moveTo :: Position -> GameState -> GameState
moveTo p (GameState _ avail) = GameState (Just p) $ delete p avail

validPathsFrom :: Board -> GameState -> [Path]
validPathsFrom board state@(GameState _ avail)
  | null avail = [[]]
  | otherwise  =
    [ p:path | p <- validSteps board state,
               path <- validPathsFrom board $ moveTo p state ]

main = do
  putStrLn "Input the puzzle as a list of Ints:"
  boardList <- liftM read getLine
  putStrLn "Calculating..."
  let board = makeBoard boardList
  print . head . validPathsFrom board $ startState board
