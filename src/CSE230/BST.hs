module CSE230.BST where

import qualified Data.Map as Map
import Test.QuickCheck 

-- ============================================================================
-- | Binary Search Trees 
-- ============================================================================

{- For this problem, you will use Haskell's data types to
   implement an _Abstract Set_ datatype that implements
   the following API:

   -- | The Set data type
   data Set a

   -- | `contains x ys` returns `True` if and only if `x` is a member of the set `ys`
   contains :: (Ord a) => a -> Set a -> Bool

   -- | `add x xs` returns the (new) set obtained by inserting `x` into the set `xs`
   add :: (Ord a) => a -> Set a -> Set a

   -- | `remove x xs` returns the (new) set obtained by deleting `x` from the set `xs`
   remove :: (Ord a) => a -> Set a -> Set a

-}

-- | The BST Data Type ------------------------------------------------------------------ 

data BST k v 
  = Emp                          -- ^ Empty tree
  | Bind k v (BST k v) (BST k v) -- ^ Node with key=k, val=v, and left, right subtrees
  deriving (Show)

-- | `isBSO t` tests whether a tree satisfies the "binary-search-order" invariant -------
isBSO ::  Ord k => BST k v -> Bool
isBSO Emp            = True
isBSO (Bind k _ l r) = allKeys (\kl -> kl < k) l    -- all keys in `l` are less    than k
                    && allKeys (\kr -> k < kr) r    -- all keys in `r` are greater than k 
                    && isBSO l                      -- left-subtree is BS-ordered
                    && isBSO r                      -- right-subtree is BS-ordered

allKeys :: (k -> Bool) -> BST k v -> Bool
allKeys _ Emp            = True
allKeys p (Bind k _ l r) = p k && allKeys p l && allKeys p r

-- | To test your implementation, we will define a type of operations over trees.
--   This will let us randomly generate (programs) sequences of operations that 
--   manipulate the trees.

data BSTop k v 
  = BSTadd k v  -- "add key value" operation 
  | BSTdel k    -- "delete key" operation
  deriving (Eq, Show)

-- | A function that builds a tree from a list of operations

ofBSTops ::  Ord k => [BSTop k v] -> BST k v
ofBSTops ops            = foldr doOp Emp ops
  where 
      doOp (BSTadd k v) = bstInsert k v
      doOp (BSTdel k)   = bstDelete k

-- | We can also build a "golden" or "reference" implementation using the standard 
--   libraries `Map` datatype

mapOfBSTops ::  Ord k => [BSTop k a] -> Map.Map k a
mapOfBSTops ops         = foldr doOp Map.empty ops
  where 
      doOp (BSTadd k v) = Map.insert k v
      doOp (BSTdel k)   = Map.delete k

-- | The following functions will let us generate random BST operations 

keys :: [Int]
keys = [0..10]

genBSTadd :: Gen (BSTop Int Char)
genBSTadd = do 
  k <- elements keys
  v <- elements ['a'..'z']
  return (BSTadd k v) 

genBSTdel :: Gen (BSTop Int Char)
genBSTdel = do 
  k <- elements keys 
  return (BSTdel k)

genBSTop ::  Gen (BSTop Int Char)
genBSTop  = frequency [(5, genBSTadd), (1, genBSTdel)]

---------------------------------------------------------------------------------------------------
-- | Part (a) Insertion
--   Write a function`bstInsert k v t` that inserts a key `k` with value `v` into the tree `t`. 
--   If `k` already exists in `t` then its value should be *replaced* with `v`. 
---------------------------------------------------------------------------------------------------
bstInsert :: (Ord k) => k -> v -> BST k v -> BST k v
bstInsert = error "fill this in"

-- When you are done, your code should satisfy the following QC properties.

prop_insert_bso :: Property
prop_insert_bso = forAll (listOf genBSTadd) $ \ops ->
                    isBSO (ofBSTops ops)
-- >
prop_insert_map = forAll (listOf genBSTadd) $ \ops -> 
                    eqMap (ofBSTops ops) (mapOfBSTops ops)

-- >>> quickCheck prop_insert_bso
-- +++ OK, passed 100 tests.

-- >>> quickCheck prop_insert_map
-- +++ OK, passed 100 tests.


---------------------------------------------------------------------------------------------------
-- | (b) Deletion
--   Write a function `bstDelete k t` that removes the key `k` from the tree `t` 
--   and leaves the other key-values unchanged. If `k` is absent from the `t`, 
--   then the tree `t` is returned unchanged as the output. 
---------------------------------------------------------------------------------------------------

bstDelete :: (Ord k) => k -> BST k v -> BST k v
bstDelete = error "fill this in"

-- When you are done, your code should satisfy the following QC properties.

prop_delete_bso :: Property
prop_delete_bso = forAll (listOf genBSTop) $ \ops ->
                    isBSO (ofBSTops ops)

prop_delete_map = forAll (listOf genBSTop) $ \ops ->
                    eqMap (ofBSTops ops) (mapOfBSTops ops)

eqMap :: (Ord k, Eq v) => BST k v -> Map.Map k v -> Bool
eqMap bst map = toBinds bst == Map.toAscList map
   where
    toBinds ::  BST k v -> [(k, v)]
    toBinds Emp            = []
    toBinds (Bind k v l r) = toBinds l ++ [(k,v)] ++ toBinds r


-- >>> quickCheck prop_delete_bso
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_delete_map
-- +++ OK, passed 100 tests.
--


---------------------------------------------------------------------------------------------------
-- | (c) Balanced Trees [These are "AVL Trees" where the height difference between left/right <= 2]
---------------------------------------------------------------------------------------------------

-- | `height t` evaluates to the "height" of the BST `t` 
height :: BST k v -> Int
height (Bind _ _ l r) = 1 + max (height l) (height r)
height Emp            = 0

-- | `isBal t` returns `True` if the tree `t` is *balanced*
isBal :: BST k v -> Bool
isBal (Bind _ _ l r) = isBal l && isBal r && abs (height l - height r) <= 2
isBal Emp            = True

-- | Write a balanced tree generator
genBal :: Gen (BST Int Char)
genBal = error "fill this in"

-- such that
prop_genBal :: Property
prop_genBal = forAll genBal isBal

-- >>> quickCheck prop_genBal
-- +++ OK, passed 100 tests.


---------------------------------------------------------------------------------------------------
-- | (d) Height Balancing (** HARD: EXTRA CREDIT see [NOTE:Balancing] below  **) 
---------------------------------------------------------------------------------------------------
-- | Modify your `insert` and `delete` functions so that 
--   if given balanced trees as input, they return balanced trees as output.
--   That is, they satisfy the properties
---------------------------------------------------------------------------------------------------

prop_insert_bal :: Property
prop_insert_bal = forAll (listOf genBSTadd) (\ops -> isBal (ofBSTops ops))

prop_delete_bal ::  Property
prop_delete_bal = forAll (listOf genBSTop) (\ops -> isBal (ofBSTops ops))

-- >>> quickCheck prop_insert_bal
-- +++ OK, passed 100 tests.


-- >>> quickCheck prop_delete_bal 
-- +++ OK, passed 100 tests.

---------------------------------------------------------------------------------------------------
-- | [NOTE:Balancing] One "trivial" way to achieve this is to 
--   (1) convert the BST to a list of key-value pairs 
--   (2) perform the insertion or deletion on the list 
--   (3) convert the list back into a BST
--
--   However, this defeats the purpose of balancing which is to make `insert` and `delete` efficient
--   O(log n) operations instead of O(n). 
-- 
--   DO NOT CONVERT YOUR BST TO A LIST or similar or you WILL GET 0 for the assignment!
---------------------------------------------------------------------------------------------------




