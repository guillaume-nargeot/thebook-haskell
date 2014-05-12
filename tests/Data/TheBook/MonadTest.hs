{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Lens          (view)
import           Control.Monad         (mzero, (>>))
import           Data.TheBook.Engine
import           Data.TheBook.Monad
import           Data.TheBook.Rule
import           Data.TheBook.Types    as T
import           Data.TheBook.Types    (OrderRejectReason,
                                        OrderRejectReason (..), SessionID,
                                        WithDictionary, WithOrder, WithSession,
                                        dictL, orderL, sessionL)
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.TheBook.MonadTest" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "return in Monad should match" prop_return_should_match
  , QC.testProperty ">>= in Monad should stop at NoMatch" prop_bind_should_stop_at_no_match
  ]

type TestRule a = Rule Int [String] String a

s' :: Int
s' = 1

instrument :: T.Instrument
instrument = T.Instrument "VOD.L" "XLON"

currency :: T.Currency
currency = "GBP"

tickRules :: [T.TickRule]
tickRules = [
    T.TickRule { _tickPrice = 1.0    , _tickSize = 0.001 }
  , T.TickRule { _tickPrice = 100.0  , _tickSize = 0.1   }
  , T.TickRule { _tickPrice = 1000.0 , _tickSize = 1.0   }
  ]

dictionary :: T.Dictionary
dictionary = T.dictionary instrument currency tickRules

isMatch :: (Eq s, Eq w, Eq a) => s -> w -> a -> Result s w e a -> Bool
isMatch s w a r = case r of
  Match s'' w' a' -> s == s'' && w == w' && a == a'
  _              -> False

isNoMatch :: Result s w e a -> Bool
isNoMatch NoMatch = True
isNoMatch _       = False

prop_return_should_match :: Int -> Bool
prop_return_should_match i = isMatch s' [] i (runRule rule s')
  where rule :: TestRule Int
        rule = return i

prop_bind_should_stop_at_no_match :: Bool
prop_bind_should_stop_at_no_match = isNoMatch (runRule rule s')
  where rule :: TestRule Int
        rule = return (1 :: Int) >> mzero >> return 1

validatePrice :: (WithDictionary r, WithOrder r)
              => Validation r OrderRejectReason m
validatePrice = do
  -- Deps
  dict <- view dictL
  order <- condR orderL

  -- Validations
  require True OIncorrectPriceIncrement

