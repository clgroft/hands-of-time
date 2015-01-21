import Control.Monad (liftM)
import Data.Array.Unboxed
import Data.Ix (range)

type Position = Int
type Board = (Int, UArray Position Int)
type Path = [Position]

boardBounds :: Int -> (Position, Position)
boardBounds size = (0, size - 1)

makeBoard :: [Int] -> Board
makeBoard ns = (size, listArray (boardBounds size) ns) where size = length ns

data GameState = GameState (Maybe Position) (UArray Position Bool) Int

startState :: Int -> GameState
startState size =
  GameState Nothing (listArray (boardBounds size) (replicate size True)) size

validSteps :: Board -> GameState -> [Position]
validSteps (size, _) (GameState Nothing _ _) = range $ boardBounds size
validSteps (size, steps) (GameState (Just p) avail _) =
  filter (avail!) $ map (`mod` size) [p + step, p + size - step]
    where step = steps!p

moveTo :: Position -> GameState -> GameState
moveTo p (GameState _ avail numAvail) =
  GameState (Just p) (avail // [(p, False)]) (numAvail - 1)

validPathsFrom :: Board -> GameState -> [Path]
validPathsFrom board state@(GameState _ _ numAvail)
  | numAvail == 0	= [[]]
  | otherwise     =
    [ p:path | p <- validSteps board state,
               path <- validPathsFrom board $ moveTo p state ]

main = do
  putStrLn "Input the puzzle as a list of Ints:"
  boardList <- liftM read getLine
  putStrLn "Calculating..."
  let board@(size, _) = makeBoard boardList
  print . head . validPathsFrom board $ startState size
