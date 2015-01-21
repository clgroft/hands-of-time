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

data GameState = GameState [Position] (UArray Position Bool) Int

startState :: Int -> GameState
startState size =
  GameState [] (listArray (boardBounds size) (replicate size True)) size

validSteps :: Board -> GameState -> [Position]
validSteps (size, _    ) (GameState []     _     _) = range $ boardBounds size
validSteps (size, steps) (GameState (p:ps) avail _) =
  filter (avail!) $ map (`mod` size) [p + step, p + size - step]
    where step = steps!p

validPathsFrom :: Board -> GameState -> [Path]
validPathsFrom board state@(GameState ps avail numAvail)
  | numAvail == 0	= [reverse ps]
  | otherwise     =
    [ path | p <- validSteps board state,
             path <- validPathsFrom board $
                GameState (p:ps) (avail // [(p, False)]) (numAvail - 1) ]

main = do
  putStrLn "Input the puzzle as a list of Ints:"
  boardList <- liftM read getLine
  putStrLn "Calculating..."
  let board@(size, _) = makeBoard boardList
  print . head . validPathsFrom board $ startState size
