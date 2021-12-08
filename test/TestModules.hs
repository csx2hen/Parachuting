module TestModules where
import Test.QuickCheck
-- import Core (Game (..), Tick(..), Name(..), direction, Direction (..), step, player, shouldRight, shouldLeft, movePlayerSingleStep, initState, maxDepth, Movement (Left, Right), changeDir)
import Core

prop_test_depth :: Depth -> Bool
prop_test_depth d = case findModes d of
                        Just index -> (d >= modesOfDepth !! index) && (index == 0 || (d < modesOfDepth !! (index - 1)))
                        Nothing -> d < 0

-- >>> prop_test_depth 5
-- True
--

quickCheckN n = quickCheckWith (stdArgs {maxSuccess = n})
-- >>> quickCheckN 100000 prop_test_depth
-- +++ OK, passed 100000 tests.
--
