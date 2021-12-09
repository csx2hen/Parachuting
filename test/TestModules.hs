module TestModules where
import Test.QuickCheck
import Linear.V2 (V2(..))
import Control.Lens (makeLenses, (^.), (.~), (%~), (&), _1, _2)

-- import Core (Game (..), Tick(..), Name(..), direction, Direction (..), step, player, shouldRight, shouldLeft, movePlayerSingleStep, initState, maxDepth, Movement (Left, Right), changeDir)
import Core

quickCheckN n = quickCheckWith (stdArgs {maxSuccess = n})
-- >>> quickCheckN 100000 prop_test_depth
-- +++ OK, passed 100000 tests.
--

prop_test_depth :: Depth -> Bool
prop_test_depth d = case findModes d of
                        Just index -> (d >= modesOfDepth !! index) && (index == 0 || (d < modesOfDepth !! (index - 1)))
                        Nothing -> d < 0

-- >>> prop_test_depth 5
-- True
--

prop_test_should_up :: Depth -> Bool
prop_test_should_up d = case shouldUp' d of
                            True -> d > 0
                            False -> d <=0 

-- >>> quickCheck prop_test_should_up
-- +++ OK, passed 100 tests.
--

prop_test_should_left :: [Int] -> Bool
prop_test_should_left xs = case shouldLeft' xs of
                            False -> xs==[]||(helper_left xs)==False
                            True -> (helper_left xs)==True
                    
prop_test_should_right :: [Int] -> Bool
prop_test_should_right xs = case shouldRight' xs of
                            False -> xs==[]||(helper_right xs)==False
                            True -> (helper_right xs)==True


prop_test_should_in_jellyfish :: (Int, Int) -> [(Int, Int)] -> Bool
prop_test_should_in_jellyfish c cs = case inJellyFish' c' (cs', Jellyfish) of
                                        True -> helper_obstacle c' cs' == True
                                        False -> helper_obstacle c' cs' == False
                                    where
                                        c' = (helper_transform c) 
                                        cs' = (helper_transform_list cs)

prop_test_should_in_mine :: (Int, Int) -> [(Int, Int)] -> Bool
prop_test_should_in_mine c cs = case inMine' c' (cs', Mine) of
                                        True -> helper_obstacle c' cs' == True
                                        False -> helper_obstacle c' cs' == False
                                    where
                                        c' = (helper_transform c) 
                                        cs' = (helper_transform_list cs)

prop_test_should_in_left_shark :: (Int, Int) -> [(Int, Int)] -> Bool
prop_test_should_in_left_shark c cs = case inLeftShark' c' (cs', Mine) of
                                        True -> helper_obstacle c' cs' == True
                                        False -> helper_obstacle c' cs' == False
                                    where
                                        c' = (helper_transform c) 
                                        cs' = (helper_transform_list cs)
                                    
prop_test_should_in_right_shark :: (Int, Int) -> [(Int, Int)] -> Bool
prop_test_should_in_right_shark c cs = case inRightShark' c' (cs', Mine) of
                                        True -> helper_obstacle c' cs' == True
                                        False -> helper_obstacle c' cs' == False
                                    where
                                        c' = (helper_transform c) 
                                        cs' = (helper_transform_list cs)

prop_test_crash :: (Int,Int) -> [(Int,Int)] -> Bool
prop_test_crash player cs = case crash' (helper_transform player) ((helper_transform_list cs),Jellyfish) of
                                True -> helper_crash c' cs' == True
                                False -> helper_crash c' cs' == False
                                where
                                    c' = (helper_transform player) 
                                    cs' = (helper_transform_list cs)
-- >>> quickCheck prop_test_should_left
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_test_should_right
-- +++ OK, passed 100 tests.
--

-- >>> 
-- >>> quickCheck prop_test_should_in_jellyfish
-- +++ OK, passed 100 tests.
--
-- >>> helper_obstacle (helper_transform (0, 0)) (helper_transform_list [(0, 1)])
-- False
--
-- >>> inJellyFish' (helper_transform (0, 0)) ((helper_transform_list [(0, 1)]), Jellyfish)
-- False
--

-- >>> quickCheck prop_test_crash
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_test_should_in_mine
-- +++ OK, passed 100 tests.
--


-- >>> quickCheck prop_test_should_in_left_shark
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_test_should_in_right_shark
-- +++ OK, passed 100 tests.
--


-- >>> prop_test_should_in_right_shark (1,0) [(1,0)]
-- False
--

-- >>> inLeftShark' (helper_transform (1,0)) ((helper_transform_list [(1,0)]), Mine)
-- False
--
helper_left :: [Int] -> Bool
helper_left [] = True
helper_left (x:xs) = (x>0) && (helper_left xs)

helper_right :: [Int] -> Bool
helper_right [] = False
helper_right (x:xs) = (x < gridWidth - 1) || (helper_right xs)

helper_obstacle :: Coordinate -> [Coordinate] -> Bool
helper_obstacle _ [] = False
helper_obstacle (c) (c':cs) = ((c^._1)==c'^._1 && c^._2==c'^._2)||helper_obstacle c (cs)

helper_transform :: (Int, Int) -> Coordinate
helper_transform (x, y) = V2 x y

helper_transform_list ::[(Int, Int)] -> [Coordinate]
helper_transform_list [] = []
helper_transform_list (c:cs) = (helper_transform c): (helper_transform_list cs)

helper_crash :: Coordinate -> [Coordinate] -> Bool
helper_crash _ [] = False
helper_crash (c) (c':cs) = ((c^._1)==c'^._1 && c^._2==c'^._2)||helper_crash c (cs)