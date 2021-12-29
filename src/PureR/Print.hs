{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureR.Print
  ( renderExpr
  ) where

import           Data.Foldable                  ( toList )
import           Data.List                      ( intersperse )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Semigroup                 ( mtimesDefault )
import           Data.Text.Lazy.Builder         ( Builder )
import qualified Data.Text.Lazy.Builder        as TB
import           Lens.Micro.Platform
import           PureR.Expr            hiding ( string )
import           PureR.Identifiers
import           PureR.Prelude

newtype PrintState = PrintState {psBuilder :: Builder}

newtype Printer = Printer {_unPrinter :: State PrintState ()}

runPrinter :: Printer -> LText
runPrinter (Printer p) = TB.toLazyText $ psBuilder $ execState p ps0
  where ps0 = PrintState mempty

instance Semigroup Printer where
  Printer a <> Printer b = Printer (a >> b)

instance Monoid Printer where
  mempty = Printer (pure ())

instance IsString Printer where
  fromString = Printer . emit . fromString

delimit :: Char -> Char -> Printer -> Printer
delimit open close body = mconcat [char open, body, char close]

lparen :: Printer
lparen = char '('

rparen :: Printer
rparen = char ')'

comma :: Printer
comma = char ','

space :: Printer
space = char ' '

char :: Char -> Printer
char = Printer . emit . TB.singleton

emit :: Builder -> State PrintState ()
emit t = modify (\(PrintState s) -> PrintState $ s <> t)

text :: Text -> Printer
text = Printer . emit . TB.fromText

string :: String -> Printer
string = Printer . emit . TB.fromString

newline :: Printer
newline = Printer $ do
  emit "\n"

-- | Turn a Nix 'Expr' into an actual piece of text.
renderExpr :: Expr -> LText
renderExpr = runPrinter . foldExpr ppExpr

sepBy :: Foldable t => Printer -> t Printer -> Printer
sepBy sep ps = mconcat $ intersperse sep (toList ps)

binding :: (k -> Printer) -> (k, Printer) -> Printer
binding f (v, body) = f v <> " = " <> body

binder :: Var -> Printer
binder = text . unVar

key :: Key -> Printer
key = text . unKey

ppExpr :: ExprF Printer -> Printer
ppExpr (Var v          ) = binder v
ppExpr (Lam arg body) = "function(" <> text (unVar arg) <> "){" <> body <> "}"
ppExpr (App f   x      ) = f <> lparen <> x <> rparen
ppExpr (Assoc bs) = "list(" <> (sepBy comma $ binding key <$> bs) <> rparen
ppExpr (List  []       ) = "list()"
ppExpr (List  l        ) = "list(" <> sepBy comma l <> rparen
ppExpr (Int   n        ) = "as.integer(" <> string (show n) <> rparen
ppExpr (Cond c      t e) = "if(" <> c <> "){" <> t <> "} else {" <> e <> "}"
ppExpr (Bin  Equals l r) = lparen <> l <> " == " <> r <> rparen
ppExpr (Bin  And    l r) = lparen <> l <> " && " <> r <> rparen
ppExpr (Not body       ) = lparen <> "!" <> lparen <> body <> rparen <> rparen
ppExpr (Sel x k        ) = x <> "$`" <> key k <> "`"
ppExpr (Double n       ) = string (show n)
ppExpr (String s       ) = delimit '"' '"' $ text s
ppExpr (Index _ _      ) = error "index"
ppExpr (Source path    ) = "source(" <> text path <> ", chdir=T)$value"
ppExpr (Update x y     ) = "Map(function(a, b) b," <> x <> comma <> y <> rparen

ppExpr (Let binds body) =
  "(function(){"
    <> sepBy newline (dobind <$> binds)
    <> newline
    <> body
    <> "})()"
  where dobind (a, b) = binder a <> " = " <> b
