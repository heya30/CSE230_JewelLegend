module Game where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
-- import Lens.Micro.TH (makeLenses)
-- import System.Random (Random(..), newStdGen)

type BoardSize = Int
type JewelSize = Int
type JewelVal = Int

data Difficulty = Easy | Medium | Hard
  deriving (Eq, Show)

data Block = Block {val::JewelVal}
    deriving (Eq, Show)

type Board = [[Block]]

data Game = Game
    {
        board::Board,
        score::Int
    }
    deriving Show


initBoard :: BoardSize -> JewelSize -> Board
initBoard bsize gsize = error "TODO"

showBoard :: Board -> IO ()
showBoard b = error "TODO"

initGame :: Difficulty -> Board
initGame diff = error "TODO"

playGame :: Board -> IO ()
playGame b = error "TODO"
