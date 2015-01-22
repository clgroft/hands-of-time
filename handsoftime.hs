import Control.Monad (liftM)
import Data.Array.Unboxed
import Data.Ix (range)

type Position = Int -- technically, Int modulo size of board
type Board = (Int, UArray Position Int) -- = (size, step from each position)
type Path = [Position]

makeBoard :: [Int] -> Board
makeBoard ns = (size, listArray (0, size - 1) ns) where size = length ns

data GameState = GameState {
  currPos  :: Maybe Position,       -- there is no position at the beginning
  avail    :: UArray Position Bool, -- easier checking & updating than a list
  numAvail :: Int                   -- for efficiency
}

startState :: Board -> GameState
startState (size, steps) =
  GameState Nothing
            -- all positions are free at the start
            (listArray (bounds steps) (repeat True))
            size

validSteps :: Board -> GameState -> [Position]
-- every move is valid at the beginning
validSteps (_,    steps) (GameState Nothing _ _) = range $ bounds steps
validSteps (size, steps) (GameState (Just p) avail _) =
  filter (avail!) $ map (`mod` size) [p + step, p + size - step]
    where step = steps!p

moveTo :: Position -> GameState -> GameState
moveTo p (GameState _ avail numAvail) =
  GameState (Just p) (avail // [(p, False)]) (numAvail - 1)

validPathsFrom :: Board -> GameState -> [Path]
validPathsFrom board state@(GameState _ _ numAvail)
  | numAvail == 0 = [[]]
  | otherwise     =
    [ p:path | p <- validSteps board state,
               path <- validPathsFrom board $ moveTo p state ]

main = do
  putStrLn "Input the puzzle as a list of Ints:"
  boardList <- liftM read getLine
  putStrLn "Calculating..."
  let board = makeBoard boardList
  print . head . validPathsFrom board $ startState board
