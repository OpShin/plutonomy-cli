module Main where
import System.IO
import Control.Monad

import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Parser as UPLC
import UntypedPlutusCore.Core.Type (Version)
import qualified Plutonomy

-- imported for parseTxt
import qualified Data.ByteString.Lazy.Internal as T
import PlutusCore.Quote (Quote, runQuote, runQuoteT)
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Name (Name)
import PlutusCore.Lexer (AlexPosn)

stripAnnotationsTerm :: UPLC.Term Name DefaultUni DefaultFun a -> UPLC.Term Name DefaultUni DefaultFun ()
stripAnnotationsTerm (UPLC.Var a n) = UPLC.Var () n


stripAnnotations :: UPLC.Program Name DefaultUni DefaultFun a -> UPLC.Program Name DefaultUni DefaultFun ()
stripAnnotations (UPLC.Program a v t) = UPLC.Program () (UPLC.Version () 1 0 0) (stripAnnotationsTerm t)

parseTxt ::
    T.ByteString
    -> IO (UPLC.Program Name DefaultUni DefaultFun AlexPosn)
parseTxt resTxt = runQuoteT $ UPLC.parseProgram resTxt

main :: IO ()
main = do
    contents <- readFile "contract.uplc"
    let contentsPacked = T.packChars contents
    parseRes <- parseTxt contentsPacked
    let optUPLC = Plutonomy.optimizeUPLC (stripAnnotations parseRes)
    print contents
