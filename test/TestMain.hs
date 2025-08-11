import Test.Tasty

import TestCombinations
import TestTichu
import TestUtils

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
