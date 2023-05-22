import           Test.Tasty
import           TestCombinations
import           TestTichu

main :: IO ()
main =
    defaultMain
        ( testGroup
            "All Tests"
            [ combinationsTests
            , tichuTests
            ]
        )
