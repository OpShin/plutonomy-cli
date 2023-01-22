{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Options.Applicative

import Control.Exception(throwIO, Exception)
import Control.Monad(void)
import Data.Function((&))
import Plutonomy.UPLC(optimizeUPLCWith, statsUPLC)
import Plutonomy.Optimize(defaultOptimizerOptions, aggressiveOptimizerOptions)
import PlutusCore.DeBruijn(FreeVariableError)
import PlutusCore.Error
import PlutusCore.Quote (Quote, runQuote, runQuoteT)
import System.IO(stderr,stdout)
import Text.PrettyBy.Default(display)
import UntypedPlutusCore.Core(Term(..), Program(..))
import UntypedPlutusCore.DeBruijn(deBruijnTerm)

import qualified Data.Text.IO as BT
import qualified Data.ByteString.Char8 as B8
import qualified UntypedPlutusCore.Parser as UPLC

instance Exception ParserErrorBundle

main :: IO ()
main = do
    customExecParser (prefs showHelpOnEmpty) commandParser >>= \case
        Optimize { input, mode } -> do
            text <- BT.readFile input
            pgrm  <- parseDeBruijnProgram text
            let opts = case mode of
                    DefaultMode -> Left defaultOptimizerOptions
                    AggressiveMode -> Left aggressiveOptimizerOptions
                    NoneMode -> Right ()
            let pgrm' = case opts of
                    Left optopts -> optimizeUPLCWith optopts pgrm
                    Right _ -> pgrm
            B8.hPutStrLn stderr (B8.pack (compare pgrm pgrm'))
            B8.hPutStrLn stdout (B8.pack (display pgrm'))
  where
    parseDeBruijnProgram text = do
        pgrm <- runQuoteT (UPLC.parseProgram @ParserErrorBundle text) & either throwIO return
        pgrmM <- (return . void) pgrm
        term  <- deBruijnTerm @FreeVariableError (_progTerm pgrmM) & either throwIO return
        return pgrmM { _progTerm = term }

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

data OptimizerMode = DefaultMode | AggressiveMode | NoneMode

optimizerModeParser :: Parser OptimizerMode
optimizerModeParser = defaultModeParser <|> aggressiveModeParser <|> noneModeParser
    where
        defaultModeParser = flag' DefaultMode (long "default" <> help "Optimize with default optimizations settings")
        aggressiveModeParser = flag' AggressiveMode (long "aggressive" <> help "Optimize with aggressive optimizations settings. May not preserve semantics.")
        noneModeParser = flag' NoneMode (long "none" <> help "Do not optimize. May be used for debugging purposes (should preserve code).")
