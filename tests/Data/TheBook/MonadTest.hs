-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.MonadTest
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Tests for 'Data.TheBook.Monad'.
-----------------------------------------------------------------------------
module Data.TheBook.MonadTest (tests) where

import           Data.TheBook.Monad
import           Data.TheBook.Types    as Types
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.MonadTest" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "return in Monad should match" prop_return_should_match
  ]

type TestRule   a = Rule Int [String] String a
type TestResult a = Result Int [String] String a

s' :: Int
s' = 1

isMatch :: (Eq s, Eq w, Eq a) => s -> w -> a -> Result s w e a -> Bool
isMatch s w a r = case r of
  Match s'' w' a' -> s == s'' && w == w' && a == a'
  _              -> False

prop_return_should_match :: Int -> Bool
prop_return_should_match i = isMatch s' [] i (runRule retI s')
  where
    retI :: TestRule Int
    retI = return i
