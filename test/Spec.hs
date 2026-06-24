import Test.HUnit
import BSTv4

-- Test case

--              8
--           /    \
--         3       10
--       /  \       \
--     1     6       14
--         /  \     /
--       4     7  13

n1 = Node Nil 1 Nil
n4 = Node Nil 4 Nil
n7 = Node Nil 7 Nil
n6 = Node n4 6 n7
n3 = Node n1 3 n6
n13 = Node Nil 13 Nil
n14 = Node n13 14 Nil
n10 = Node Nil 10 n14
n8 = Node n3 8 n10

successorCases =
  [ ("successor(1) == 3", n1, n3)
  , ("successor(3) == 4", n3, n4)
  , ("successor(4) == 6", n4, n6)
  , ("successor(6) == 7", n6, n7)
  , ("successor(7) == 8", n7, n8)
  , ("successor(8) == 10", n8, n10)
  , ("successor(10) == 13", n10, n13)
  , ("successor(13) == 14", n13, n14)
  , ("successor(14) == Nil", n14, Nil)
  ]

mkCase (label, node, expected) =
  TestLabel label $ TestCase $ assertEqual label (next n8 node) expected

tests = TestList $ map mkCase successorCases

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then pure ()
    else error "Test suite failed"
