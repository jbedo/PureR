{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | R expression types and auxiliary functions.
-- Since 'Expr' is actually a fixpoint over the 'ExprF' base functor, this
-- module also exposes auxiliary functions that serve as constructors.
module PureR.Expr where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import           PureR.Identifiers
import           PureR.Prelude

-- | The fixpoint over 'ExprF', see haddocks there for more information.
newtype Expr = Expr {unExpr :: ExprF Expr}
  -- Explicitly using the @newtype@ strategy here makes the 'Show' instance
  -- much nicer, since it makes it so you don't get all the @Expr@
  -- constructors.
  deriving newtype (Show)

-- | Base functor for an R expression.
-- We don't aim to be able to represent every valid R expression, just the
-- ones that are relevant for PureR.
--
-- 'ExprF' is the base functor for the 'Expr' fixpoint.
-- This allows us to easily annotate and consume it during pretty-printing.
--
-- Note that 'String', unlike 'Key' and 'Var', is a raw representation of the intended string, completely unquoted and unescaped.
-- That means that it might consist of, for example, a single '"'.
-- It is the job of the printer to figure out how to correctly escape those.
-- 
-- R doesn't have let bindings but they're useful so we model them here
-- and let the printers implement them
data ExprF f
  = Var Var
  | Lam Var f
  | App f f
  | Assoc [(Key, f)]
  | List [f]
  | Cond f f f
  | Bin Op f f
  | Not f
  | Sel f Key
  | Let (NonEmpty (Var, f)) f
  | Int Integer
  | Double Double
  | String Text
  | Index f Integer
  | Source Text
  | Update f f
  deriving stock (Functor, Foldable, Traversable, Show)

data Op = Equals | And
  deriving (Eq, Show)

foldExpr :: (ExprF r -> r) -> Expr -> r
foldExpr f = go where go = f . fmap go . unExpr

var :: Var -> Expr
var = Expr . Var

lam :: Var -> Expr -> Expr
lam arg body = Expr $ Lam arg body

app :: Expr -> Expr -> Expr
app f x = Expr $ App f x

assoc :: [(Key, Expr)] -> Expr
assoc = Expr . Assoc

cond :: Expr -> Expr -> Expr -> Expr
cond c true false = Expr $ Cond c true false

sel :: Expr -> Key -> Expr
sel e s = Expr $ Sel e s

let' :: [(Var, Expr)] -> Expr -> Expr
let' []      body = body
let' (h : t) body = Expr $ Let (h :| t) body

int :: Integer -> Expr
int = Expr . Int

double :: Double -> Expr
double = Expr . Double

string :: Text -> Expr
string = Expr . String

list :: [Expr] -> Expr
list = Expr . List

bin :: Op -> Expr -> Expr -> Expr
bin op a b = Expr $ Bin op a b


index :: Expr -> Integer -> Expr
index = (Expr .) . Index

source :: Text -> Expr
source = Expr . Source

update :: Expr -> Expr -> Expr
update = (Expr .) . Update

constructorFieldNames :: [Var]
constructorFieldNames = numberedVars "field__"

not' :: Expr -> Expr
not' = Expr . Not

constructor :: Text -> [Var] -> Expr
constructor conName fields = foldr
  lam
  (assoc
    ( ("tag__", string conName)
    : zipWith (\arg (UnsafeVar name) -> (UnsafeKey name, var arg))
              fields
              constructorFieldNames
    )
  )
  fields
