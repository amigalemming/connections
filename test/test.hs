import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified Test.Data.Connection as C
import qualified Test.Data.Connection.Fixed as CX
import qualified Test.Data.Connection.Float as CF
import qualified Test.Data.Connection.Int as CI
import qualified Test.Data.Connection.Ratio as CR
import qualified Test.Data.Connection.Word as CW
import qualified Test.Data.Lattice as L
import qualified Test.Data.Order as P

tests :: IO [Bool]
tests =
    sequence
        [ P.tests
        , L.tests
        , C.tests
        , CI.tests
        , CW.tests
        , CF.tests
        , CX.tests
        , CR.tests
        ]

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    results <- tests

    unless (and results) exitFailure
