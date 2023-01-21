{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Options.Applicative

import Control.Exception(throwIO)
import Data.Function((&))
import Plutonomy.UPLC(optimizeUPLCWith, statsUPLC)
import Plutonomy.Optimize(defaultOptimizerOptions, aggressiveOptimizerOptions)
import PlutusCore.DeBruijn(FreeVariableError)
import System.IO(stderr,stdout)
import Text.PrettyBy.Default(display)
import UntypedPlutusCore.Core(Term(..), Program(..))
import UntypedPlutusCore.DeBruijn(deBruijnTerm)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified UntypedPlutusCore.Parser as UPLC

main :: IO ()
main = do
    customExecParser (prefs showHelpOnEmpty) commandParser >>= \case
        Optimize { input, mode } -> do
            bytes <- BL.readFile input
            pgrm  <- parseDeBruijnProgram bytes
            let opts = case mode of
                    DefaultMode -> defaultOptimizerOptions
                    AggressiveMode -> aggressiveOptimizerOptions
            let pgrm' = optimizeUPLCWith opts pgrm
            B8.hPutStrLn stderr (B8.pack (compare pgrm pgrm'))
            B8.hPutStrLn stdout (B8.pack (display pgrm'))
  where
    parseDeBruijnProgram bytes = do
        pgrm <- UPLC.parseProgram bytes & either throwIO (return . fmap (const ()))
        term  <- deBruijnTerm @FreeVariableError (_progTerm pgrm) & either throwIO return
        return pgrm { _progTerm = term }

    percentRatio num den =
        1000 * (1 - (fromIntegral num / fromIntegral den))
        & round @Double
        & fromIntegral @_ @Double
        & (/ 10)

    compare (statsUPLC -> (ast, size)) (statsUPLC -> (ast', size')) = do
        unlines
            [ "AST Nodes -" <> show (percentRatio ast' ast) <> "%"
            , "Size      -" <> show (percentRatio size' size) <> "%"
            ]

-- Quick command-line

data Command
    = Optimize
        { input :: FilePath,
          mode :: OptimizerMode
        }


commandParser :: ParserInfo Command
commandParser = info (helper <*> parser) $ mconcat
    [ progDesc "Optimize an Untyped Plutus Core (UPLC) program while preserving its semantic."
    ]
  where
    parser =
        Optimize <$> inputArg <*> optimizerModeParser

inputArg :: Parser FilePath
inputArg = argument str $ mconcat
    [ metavar "FILEPATH"
    , help "Path to a .uplc file"
    , completer (bashCompleter "file")
    ]

data OptimizerMode = DefaultMode | AggressiveMode

optimizerModeParser :: Parser OptimizerMode
optimizerModeParser = defaultModeParser <|> aggressiveModeParser
    where
        defaultModeParser = flag' DefaultMode (long "default" <> help "Optimize with default optimizations settings")
        aggressiveModeParser = flag' AggressiveMode (long "aggressive" <> help "Optimize with aggressive optimizations settings. May not preserve semantics.")
