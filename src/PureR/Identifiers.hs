{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the types and conversions from PureScript/CoreFn identifiers to R identifiers.
-- This can be a tricky problem, since all three languages have different rules for what is and isn't allowed in certain kinds of identifiers.
-- The goal of this module is to provide an API that takes care of all those concerns.
-- In other words, the types in this module are as close to ready-to-print as possible.
module PureR.Identifiers
  ( Var(..)
  , mkVar
  , numberedVars
  , Key(..)
  , identKey
  , stringKey
  , moduleKey
  , binderKey
  , numberedKeys
  ) where

import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.String
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Language.PureScript           as PS
import qualified Language.PureScript.PSString  as PS

-- TODO rename to Binder, since this can occur in the LHS of a let-binding

-- | A valid (i.e. containing no illegal characters) variable binder.
-- Primarily constructed using 'mkVar'.
newtype Var = UnsafeVar {unVar :: Text}
  deriving newtype (IsString, Eq, Show)

identToText :: PS.Ident -> Text
identToText (PS.Ident t) = t
-- GenIdent is only used in PureScript for "unnamed" instances.
-- Originally, in PureScript, all instances needed to be named:
-- https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md#named-instances
-- This was relaxed in 0.14.2:
-- https://github.com/purescript/purescript/pull/4096
identToText (PS.GenIdent mvar n) =
  fromMaybe "instance__" mvar <> T.pack (show n)
identToText PS.UnusedIdent = error "impossible"

-- | Make an R variable binder from a CoreFn binder.
--
-- If a binder is an R keyword, we tick the binder with a prefix.
-- R variable names can't have ticks, so we also replace ticks with 'P'
--
-- Additionally, CoreFn can put dollar signs in generated names.
-- We simply drop leading dollar signs, and the rest we convert to hyphens.
mkVar :: PS.Ident -> Var
mkVar = UnsafeVar . noticks . removeDollarSigns . tickKeywords . identToText
 where
  tickKeywords w | Set.member w keywords || T.isPrefixOf "__" w = "k_" <> w
                 | otherwise = w
  removeDollarSigns w =
    T.map (\c -> if c == '$' then '.' else c)
      $ if T.isPrefixOf "$" w then T.tail w else w
  noticks = T.map (\c -> if c == '\'' then 'P' else c)

keywords :: Set Text
keywords = Set.fromList
  [ "library"
  , "source"
  , "list"
  , "cbind"
  , "rbind"
  , "do"
  , "do.call"
  , "call"
  , "require"
  , "c"
  , "T"
  , "F"
  , "TRUE"
  , "FALSE"
  , "NULL"
  , "Inf"
  , "NaN"
  , "NA"
  , "NA_integer_"
  , "NA_real_"
  , "NA_complex_"
  , "NA_character_"
  , "for"
  , "if"
  , "else"
  , "while"
  , "repeat"
  , "break"
  , "next"
  , "function"
  ]

-- | A valid R list key
newtype Key = UnsafeKey {unKey :: Text}
  deriving newtype (IsString, Eq, Show)

moduleKey :: PS.ModuleName -> Key
moduleKey (PS.ModuleName mdl) = UnsafeKey mdl

identKey :: PS.Ident -> Key
identKey = UnsafeKey . unVar . mkVar

stringKey :: PS.PSString -> Key
stringKey = UnsafeKey . PS.prettyPrintObjectKey

binderKey :: Var -> Key
binderKey = UnsafeKey . unVar

numberedText :: Text -> [Text]
numberedText prefix = fmap (\n -> prefix <> T.pack (show n)) [0 :: Int ..]

numberedVars :: Text -> [Var]
numberedVars = fmap UnsafeVar . numberedText

numberedKeys :: Text -> [Key]
numberedKeys = fmap UnsafeKey . numberedText
