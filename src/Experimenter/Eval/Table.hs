{-# LANGUAGE ExistentialQuantification #-}
module Experimenter.Eval.Table where

import           Control.Monad.Logger
import           Data.List                    (foldl')
import qualified Data.Text                    as T

import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Packages.AMSMath
import           Text.LaTeX.Packages.Inputenc

import           Experimenter.Util


data Table =
  Table Row
        [Row]
  deriving (Show, Eq)

newtype Row =
  Row [Cell]
  deriving (Show, Eq)


data Cell
  = CellT T.Text
  | CellD Double
  | CellL LaTeX
  | CellEmpty
  deriving (Show, Eq)

instance IsString Cell where
  fromString = CellT . T.pack


printTable :: (MonadLogger m) => Table -> LaTeXT m ()
printTable (Table headerInput rowsInput) =
  center $
  tabular Nothing (VerticalLine : replicate colLen LeftColumn ++ [VerticalLine]) $
    hline <>
    printRow textbf header <>
    hline <>
    mconcat (map (printRow id) rows) <>
    hline
  where
    printRow :: (LaTeXC l) => (l -> l) -> Row -> l
    printRow _ (Row []) = mempty
    printRow f (Row (c:cs)) = foldl' (&) (f $ printCell c) (map (f . printCell) cs) <> lnbk
    printCell :: (LaTeXC l) => Cell -> l
    printCell (CellT txt) = raw txt
    printCell (CellD nr)  = raw $ tshow nr
    printCell (CellL l)   = fromLaTeX l
    printCell CellEmpty   = mempty

    colLen = maximum $ map cellCount (headerInput : rowsInput)
    cellCount (Row xs) = length xs
    extendRow (Row xs)= Row $ xs ++ replicate (colLen - length xs) CellEmpty
    header = extendRow headerInput
    rows = map extendRow rowsInput
