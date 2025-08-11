import Test.Tasty
import TestUtils

import TestCombinations
import TestTichu

main :: IO ()
main =
    defaultMain
        ( testGroup
            "All Tests"
            [ combinationsTests
            , tichuTests
            , utilsTests
            ]
        )
