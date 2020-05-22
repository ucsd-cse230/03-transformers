{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import qualified CSE230.BST             as BST
import qualified CSE230.WhilePlus.Types as W
import qualified CSE230.WhilePlus.Eval  as W
import qualified Data.Map as M 

main :: IO ()
main = runTests 
  [ probWhile
  , probBST
  , probExtra
  ]

probWhile ::  Score -> TestTree
probWhile sc = testGroup "WhilePlus" 
  [ scoreTest ((\_ -> W.execute W.initStore W.test1),  (), W.out1, 10, "test-1")
  , scoreTest ((\_ -> W.execute W.initStore W.test2),  (), W.out2, 10, "test-2")
  , scoreTest ((\_ -> W.execute W.initStore W.test3),  (), W.out3, 15, "test-3")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

probBST :: Score -> TestTree
probBST sc = testGroup "BinSearchTree"
  [ scoreProp sc ("prop_insert_bso", BST.prop_insert_bso, 3) 
  , scoreProp sc ("prop_insert_map", BST.prop_insert_map, 4)
  , scoreProp sc ("prop_delete_bso", BST.prop_delete_bso, 6)
  , scoreProp sc ("prop_delete_map", BST.prop_delete_map, 6)
  , scoreProp sc ("prop_genBal"    , BST.prop_genBal    , 6)
  ]

probExtra :: Score -> TestTree
probExtra sc = testGroup "BalancedTree (EC)"
  [ scoreProp sc ("prop_insert_bal", BST.prop_insert_bal, 10)
  , scoreProp sc ("prop_delete_bal", BST.prop_delete_bal, 10) 
  ]

