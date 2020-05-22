-- | *******************************************************************************************
--  ** DO NOT MODIFY ANY CODE IN THIS FILE ****************************************************
--  *******************************************************************************************

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}

module CSE230.WhilePlus.Types where

import           Data.String
import           GHC.Exts
import           GHC.Generics
import qualified Data.Map as Map

-- ===========================================================================================
-- | An Interpreter for WHILE++
-- ===========================================================================================

-- Previously, you wrote a simple interpreter for *WHILE*.
-- For this problem, you will use monad transformers to build
-- an evaluator for *WHILE++* which, adds exceptions and I/O
-- to the original language.

----------------------------------------------------------------------------------------------
-- | Variables and expressions.
----------------------------------------------------------------------------------------------

type Variable = String

type Store    = Map.Map Variable Value


data Value 
  = IntVal Int
  | BoolVal Bool
  deriving (Eq, Generic)

instance Show Value where
  show (IntVal i) = show i
  show (BoolVal b) = show b

data Expression 
  = Var Variable
  | Val Value
  | Op  Bop Expression Expression
  deriving (Show)


data Bop 
  = Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  deriving (Show)

----------------------------------------------------------------------------------------------
-- | Programs in the language are values of the type `Statement`
--   The new constructs are the `Print`, `Throw` and the `Try` statements.
----------------------------------------------------------------------------------------------

data Statement 
  = Assign   Variable   Expression
  | If       Expression Statement Statement
  | While    Expression Statement
  | Sequence Statement  Statement
  | Skip
  | Print    String     Expression
  | Throw    Expression
  | Try      Statement  Variable Statement
  deriving (Show)

block :: [Statement] -> Statement 
block = foldr Sequence Skip

----------------------------------------------------------------------------------------------
-- | `WState` is the "State" maintained by the interpreter's State-Transformer Monad
--   if ws :: WState, you can "access" the `Store` and `Log` as (wStore ws) and (wLog ws)
----------------------------------------------------------------------------------------------

data WState = WS 
  { wStore :: Store -- ^ store mapping Variables to Values 
  , wLog   :: Log   -- ^ list of strings printed during execution  
  } 

-- | A `Log` is the list of messages printed out during execution
type Log      = [String]

------------------------------------------------------------------------------------
-- | These are some helper functions to make it easy to "write" WHILE++ programs as 
--   Haskell: read the docs, and just print out the `test1`,`test2`, `test3` etc. 
--   in GHCi to understand what is going on.
------------------------------------------------------------------------------------

(<-:) :: Variable -> Expression -> Statement
(<-:) x e = Assign x e
(<=:)    = Op Le
(<:)     = Op Lt

-- | This will let us write "X" and automatically convert it to (Var "X") 
instance IsString Expression where
  fromString = Var 

-- | This will let us write (1 + 2) and automatically convert it to (Op Plus (Val (IntVal 1)) (Val (IntVal 2)))
instance Num Expression where
  fromInteger i = Val (IntVal (fromIntegral i))
  (+)           = Op Plus
  (-)           = Op Minus 
  (*)           = Op Times 
  abs           = undefined
  signum        = undefined

-- | This will let us write [s1,s2,s3,...] and automatically convert it to s1 `Sequence` s2 `Sequence` s3 ... 
instance IsList Statement where
  type Item Statement = Statement 
  fromList = block
  toList s = [s]

-- | `initStore` is the empty state (all variables undefined), log is empty
initStore :: Store  
initStore = Map.empty

-- | Example 1 ---------------------------------------------------------------------
test1 :: Statement
test1 = 
  [ "X" <-: 0,
    "Y" <-: 1, 
    Print "hello world: " "X",
    If ("X" <: "Y") (Throw ("X" + "Y")) Skip,
    "Z" <-: 3, 
    Print "goodbye world: " "Z"
  ]

out1 :: (Store, Maybe Value, String)
out1 = ( Map.fromList [("X",IntVal 0),("Y",IntVal 1)]   
       , Just (IntVal 1)                                
       , "hello world: 0\n"                      
       )

-- | Example 2 ---------------------------------------------------------------------
test2 :: Statement
test2  = 
  [ "X" <-: 0,
    "Y" <-: 1,
    Try (If ("X" <: "Y")
           [ "A" <-: 100,
             Throw ("X" + "Y"),
             "B" <-: 200 
           ]
          Skip
        )
    {- catch -} "E"
        ("Z" <-: ("E" + "A"))
  ]

out2 :: (Store, Maybe Value, String)
out2 = ( Map.fromList [("A",IntVal 100),("E",IntVal 1),("X",IntVal 0)     
                      ,("Y",IntVal 1),("Z",IntVal 101)]
       , Nothing                                                      
       , ""                                                           
       )


-- | Example 3 ---------------------------------------------------------------------
test3 :: Statement
test3 = 
  [ "N" <-: 10,
    "I" <-: 0,
    "A" <-: 0,
    "B" <-: 1,
    While ("I" <: 15) [
      Print "A: " "A",
      "I" <-: ("I" + 1),
      "T" <-: "A",
      "A" <-: "B",
      "B" <-: ("T" + "B"),
      Skip
    ]
  ]

out3 :: (Store, Maybe Value, String)
out3 = ( fromList [("A", IntVal 610), ("B", IntVal 987), ("I", IntVal 15), ("N", IntVal 10), ("T", IntVal 377) ] 
       , Nothing
       , unlines ["A: 0","A: 1","A: 1","A: 2","A: 3","A: 5","A: 8","A: 13","A: 21","A: 34","A: 55","A: 89","A: 144","A: 233","A: 377"]
       )

