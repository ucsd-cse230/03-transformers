module CSE230.BST where

import qualified Data.Map as Map
import Test.QuickCheck 

-- ============================================================================
-- | Binary Search Trees 
-- ============================================================================
-- In this task, you will implement a map (dictionary)
-- that maps unique keys to values.
-- The map will be represented as a binary search tree.

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
prop_insert_map :: Property
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
--   (Hint: feel free to look up the algorithm for BST deletion, e.g. on Wikipedia) 
---------------------------------------------------------------------------------------------------

bstDelete :: (Ord k) => k -> BST k v -> BST k v
bstDelete = error "fill this in"

-- When you are done, your code should satisfy the following QC properties.

prop_delete_bso :: Property
prop_delete_bso = forAll (listOf genBSTop) $ \ops ->
                    isBSO (ofBSTops ops)

prop_delete_map :: Property
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
-- | (c) Balanced Trees
--   Let us define a tree to be *balanced* 
--   if the height difference between the left and the right subtree of any node is <= 1
---------------------------------------------------------------------------------------------------

-- | The height of a tree
height :: BST k v -> Int
height (Bind _ _ l r) = 1 + max (height l) (height r)
height Emp            = 0

-- | `isBal t` returns `True` if the tree `t` is *balanced*
isBal :: BST k v -> Bool
isBal (Bind _ _ l r) = isBal l && isBal r && abs (height l - height r) <= 1
isBal Emp            = True

-- | Write a generator for *balanced* BSTs that have a given height.
--   Unlike previous generators that invoked the BST's operations (insert and delete),
--   this one is supposed to construct a BST explicitly using its constructors 
--   (since our insertion/deletion operations do not keep the tree balanced).
--
--   We expect your generator to have some variety:
--   - some trees it generates have to be non-*perfect*
--   - some trees it generates have to be non-*full*
genBal :: Int -> Gen (BST Int Char)
genBal h = error "fill this in"

-- Once you have implemented `genBal`, the following three properties should pass:

-- | `genBal` produces a tree of exactly the given height
prop_genBalHeight :: Property
prop_genBalHeight = forAll genHeightAndBal (\(h, t) -> height t == h)

-- | `genBal` produces a balanced tree
prop_genBalBalanced :: Property
prop_genBalBalanced = forAll genHeightAndBal (isBal . snd)

-- | `genBal` produces a BST
prop_genBalBSO :: Property
prop_genBalBSO = forAll genHeightAndBal (isBSO . snd)

-- | Generate a random height and a balanced BST of that height
genHeightAndBal :: Gen (Int, BST Int Char)
genHeightAndBal = do
  h <- chooseInt (0, 10)
  t <- genBal h
  return (h, t)

-- >>> quickCheck prop_genBalHeight
-- +++ OK, passed 100 tests.

-- >>> quickCheck prop_genBalBalanced
-- +++ OK, passed 100 tests.

-- >>> quickCheck prop_genBalBSO
-- +++ OK, passed 100 tests.
