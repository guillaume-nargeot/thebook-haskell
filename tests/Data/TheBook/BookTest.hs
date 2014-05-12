-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.BookTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Data.TheBook.Book'.
-----------------------------------------------------------------------------
module Data.TheBook.BookTest (tests) where

import           Control.Arrow         ((&&&))
import           Data.List
import           Data.Maybe            (listToMaybe)
import           Data.Ord
import           Data.TheBook.Book     as Book
import           Data.TheBook.Types    as Types
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.TheBookTest" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "fromList buys == reverse stable sort by price . toList" isCorrectlySortedBuy
  , QC.testProperty "fromList sells == stable sort by price . toList" isCorrectlySortedSell
  , QC.testProperty "top buys == best buy" topGetsTopBuy
  , QC.testProperty "top sells == best sell" topGetsTopSell
  ]

sortBuy, sortSell :: [(Types.Price, Types.Qty)] -> [(Types.Price, Types.Qty)]
sortBuy  = sortBy (flip $ comparing fst)
sortSell = sortBy (comparing fst)

isCorrectlySortedSell :: [(Types.Price, Types.Qty)]
                      -> Bool
isCorrectlySortedSell entries = let book = Book.fromList entries :: Book.Book Book.Sell
                                in Book.toList book == sortSell entries
isCorrectlySortedBuy :: [(Types.Price, Types.Qty)]
                     -> Bool
isCorrectlySortedBuy entries = let book = Book.fromList entries :: Book.Book Book.Buy
                               in Book.toList book == sortBuy entries

topGetsTopSell :: [(Types.Price, Types.Qty)] -> Bool
topGetsTopSell entries = let book = Book.fromList entries :: Book.Book Book.Sell
                        in (fmap (price &&& qty) (Book.top book)) == (listToMaybe . sortSell $ entries)

topGetsTopBuy :: [(Types.Price, Types.Qty)] -> Bool
topGetsTopBuy entries = let book = Book.fromList entries :: Book.Book Book.Buy
                        in (fmap (price &&& qty) (Book.top book)) == (listToMaybe . sortBuy $ entries)
