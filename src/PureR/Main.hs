{-# LANGUAGE NoImplicitPrelude #-}

module PureR.Main where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( parseEither )
import           Data.Foldable                  ( toList )
import           Data.List                      ( intercalate )
import qualified Data.Text.Lazy.IO             as TL
import qualified Language.PureScript.CoreFn    as P
import           Language.PureScript.CoreFn.FromJSON
                                                ( moduleFromJSON )
import           PureR.Convert                ( ModuleInfo(ModuleInfo)
                                                , convert
                                                )
import           PureR.Prelude
import           PureR.Print                  ( renderExpr )
import qualified System.Directory              as Dir
import qualified System.Exit                   as Sys
import           System.FilePath                ( (</>) )
import qualified System.FilePath               as FP
import           System.IO

defaultMain :: IO ()
defaultMain = do
  let workdir    = "."
  let moduleRoot = workdir </> "output"
  moduleDirs <- filter (/= "cache-db.json") <$> Dir.listDirectory moduleRoot
  forM_ moduleDirs $ \rel -> do
    let dir  = moduleRoot </> rel
    let file = dir </> "corefn.json"
    value <- Aeson.eitherDecodeFileStrict file >>= either Sys.die pure
    (_version, module') <- either Sys.die pure
      $ parseEither moduleFromJSON value
    let (r, ModuleInfo usesFFI interpolations) = convert module'
    TL.writeFile (dir </> "default.R") (renderExpr r)
    let modulePath = P.modulePath module'
        foreignSrc = workdir </> FP.replaceExtension modulePath "R"
        foreignTrg = dir </> "foreign.R"
    hasForeign <- Dir.doesFileExist foreignSrc
    case (hasForeign, usesFFI) of
      (True, True) -> Dir.copyFile foreignSrc foreignTrg
      (True, False) ->
        hPutStrLn stderr
          $  "Warning: "
          <> modulePath
          <> " has an FFI file, but does not use FFI!"
      (False, True) ->
        hPutStrLn stderr
          $  "Warning: "
          <> modulePath
          <> " calls foreign functions, but has no associated FFI file!"
      (False, False) -> pure ()
    unless (null interpolations) $ do
      hPutStrLn stderr $ unlines
        [ "Warning: "
        <> modulePath
        <> " appears to perform Nix string interpolation in the following locations:"
        , "  " <> intercalate ", " (show <$> toList interpolations)
        , "Nix string interpolations are currently not officially supported and may cause unexpected behavior."
        ]
