{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Engine
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Defines the main type for the state of the matching engine that can be
-- used in order to run most rules. All the instances are also provided here.
-----------------------------------------------------------------------------
module Data.TheBook.Engine (
    Engine, emptyEngine
) where

import           Control.Lens       (makeLenses)
import           Data.TheBook.Book  (Book)
import qualified Data.TheBook.Book  as Book
import qualified Data.TheBook.Types as Types

-- | Main type for the state of the matching engine.
data Engine = Engine {
    _dictionary :: Types.Dictionary
  , _session    :: Maybe Types.SessionID
  , _order      :: Maybe Types.Order
  , _buys       :: Book Book.Buy
  , _sells      :: Book Book.Sell
} deriving (Show, Eq)
$(makeLenses ''Engine)

instance Types.WithDictionary (Engine) where
  dictL = dictionary

instance Types.WithSession (Engine) where
  sessionL = session

instance Types.WithOrder (Engine) where
  orderL = order

-- | Creates an empty engine.
emptyEngine :: Types.Dictionary
            -> Engine
emptyEngine dictionary' = Engine {
    _dictionary = dictionary'
  , _session    = Nothing
  , _order      = Nothing
  , _buys       = Book.empty
  , _sells      = Book.empty
  }



