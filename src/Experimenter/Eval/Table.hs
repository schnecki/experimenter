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
import           Data.ByteString              (ByteString)
import           Data.List                    (foldl')
import qualified Data.Text                    as T
import           GHC.Generics

import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Packages.AMSMath
import           Text.LaTeX.Packages.Inputenc
import           Text.Printf

import           Experimenter.Util


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
dereferLatex = protectText . T.replace "{" "\\{" . T.replace "}" "\\}" . T.replace "_" "\\_"


printTable :: (MonadLogger m) => Table -> LaTeXT m ()
printTable tbl@(Table header _) = forM_ (splitTable tbl) printTable'
  where
    printTable' (Table headerInput rowsInput) =
      center $
      tabular Nothing (replicate colLen LeftColumn) $ hline <> printRow textbf header <> hline <> mconcat (map (printRow id) rows) <> hline
      where
        maxColLens = map (map cellLength . fromRow) (header : rows)
        fromRow (Row []) = [CellT ""]
        fromRow (Row xs) = xs
        cellLength (CellT txt) = T.length txt
        cellLength (CellD dbl) = T.length (printDouble dbl)
        cellLength (CellL l)   = 0
        printRow :: (LaTeXC l) => (l -> l) -> Row -> l
        printRow _ (Row []) = mempty
        printRow f (Row (c:cs)) = foldl' (&) (f $ printCell c) (map (f . printCell) cs) <> lnbk
        printCell :: (LaTeXC l) => Cell -> l
        printCell (CellT txt) = raw (dereferLatex txt)
        printCell (CellD nr)  = raw $ printDouble nr
        printCell (CellL l)   = fromLaTeX l
        printCell CellEmpty   = mempty
        colLen = maximum $ map cellCount (headerInput : rowsInput)
        cellCount (Row xs) = length xs
        extendRow (Row xs) = Row $ xs ++ replicate (colLen - length xs) CellEmpty
        header = extendRow headerInput
        rows = map extendRow rowsInput

splitTable :: Table -> [Table]
splitTable tbl@(Table headerInput rowsInput)
  | colLen <= maxColLen = [tbl]
  | otherwise = takeCols maxColLen tbl : splitTable (dropCols maxColLen tbl)
  where
    colLen = maximum $ map cellCount (headerInput : rowsInput)
    cellCount (Row xs) = length xs
    takeCols n (Table (Row hs) rs) = Table (Row $ take n hs) (map (\(Row rs) -> Row (take n rs)) rs)
    dropCols n (Table (Row hs) rs) = Table (Row $ take 1 hs ++ drop n hs) (map (\(Row rs) -> Row (take 1 rs ++ drop n rs)) rs)

maxColLen :: Int
maxColLen = 11

commas :: Int
commas = 3

printDouble :: Double -> T.Text
printDouble x = T.pack $ printf ("%." ++ show commas ++ "f") x

