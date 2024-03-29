{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Strict                    #-}
module Experimenter.Eval.Table where

import           Control.DeepSeq
import           Control.Monad                (forM_)
import           Control.Monad.Logger
import           Data.List                    (foldl')
import qualified Data.Text                    as T
import           GHC.Generics

import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Packages.TabularX
import           Text.Printf

data Table =
  Table !Row
        ![Row]
  deriving (Show, Eq, Generic, NFData)

newtype Row =
  Row [Cell]
  deriving (Show, Eq, Generic, NFData)


data Cell
  = CellT !Text
  | CellD !Double
  | CellL !LaTeX
  | CellEmpty
  deriving (Show, Eq, Generic)

instance NFData Cell where
  rnf (CellT !_) = ()
  rnf (CellD x)  = rnf x
  rnf (CellL !_) = ()
  rnf CellEmpty  = ()


instance IsString Cell where
  fromString = CellT . T.pack

dereferLatex :: T.Text -> T.Text
dereferLatex x = protectText $ foldl' (\x' (f, t) -> T.replace f t x') x list
  where
    list :: [(T.Text, T.Text)]
    list = [("{", "\\{"), ("}", "\\}"), ("_", "\\_"), ("_", "\\_"), ("%", "\\%"), ("^", "\\^"), ("~", "\\~"), ("$", "\\$")]

printTextwidthTable :: (MonadLogger m) => Table -> LaTeXT m ()
printTextwidthTable = printTableTabularX True

printTable :: (MonadLogger m) => Table -> LaTeXT m ()
printTable = printTableTabularX False


printTableTabularX :: (MonadLogger m) => Bool -> Table -> LaTeXT m ()
printTableTabularX tabX tbl@Table {} = forM_ (splitTable tbl) printTable'
  where
    printTable' (Table headerInput rowsInput)
      | tabX = center $ tabularx (CustomMeasure textwidth) Nothing (LeftColumn : replicate (cols - 1) (NameColumn "X")) content
      | otherwise = center $ tabular Nothing (replicate cols LeftColumn) content
      where
        content = hline <> printRow textbf header <> hline <> mconcat (map (printRow id) rows) <> hline
        printRow :: (LaTeXC l) => (l -> l) -> Row -> l
        printRow _ (Row [])     = mempty
        printRow f (Row (c:cs)) = foldl' (&) (f $ printCell c) (map (f . printCell) cs) <> lnbk
        printCell :: (LaTeXC l) => Cell -> l
        printCell (CellT txt) = raw (dereferLatex txt)
        printCell (CellD nr)  = raw $ printDouble nr
        printCell (CellL l)   = fromLaTeX l
        printCell CellEmpty   = mempty
        cols = maximum $ map cellCount (headerInput : rowsInput)
        cellCount (Row xs) = length xs
        extendRow (Row xs) = Row $ xs ++ replicate (cols - length xs) CellEmpty
        header = extendRow headerInput
        rows = map extendRow rowsInput

splitTable :: Table -> [Table]
splitTable tbl@(Table headerInput rowsInput)
  | colLen <= maxColLen = [tbl]
  | otherwise = takeCols maxColLen tbl : splitTable (dropCols maxColLen tbl)
  where
    colLen = maximum $ map cellCount (headerInput : rowsInput)
    cellCount (Row xs) = length xs
    takeCols n (Table (Row hs) rs) = Table (Row $ take n hs) (map (\(Row r) -> Row (take n r)) rs)
    dropCols n (Table (Row hs) rs) = Table (Row $ take 1 hs ++ drop n hs) (map (\(Row r) -> Row (take 1 r ++ drop n r)) rs)

maxColLen :: Int
maxColLen = 11

commas :: Int
commas = 3

printDouble :: Double -> T.Text
printDouble x = T.pack $ printf ("%." ++ show commas ++ "f") x
