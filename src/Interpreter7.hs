-- I want these language extensions for my syntactic sugaring tricks at the end

{-# Language MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
--added flexible contexts to use parser
{-# Language FlexibleContexts #-}

module Interpreter7 where

-- I want my own definition of lookup and I want to write my own function named "print".

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

-- I want to get at the standard "print" function using the name System.print

import qualified System.IO as System

-- I plan to use these monads to construct the parts of my interpreter

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Text.Parsec as P

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

data Val = I Int | B Bool
           deriving (Eq)

--Make Val an instance of show for console output
instance Show Val where
    show (I i) = show i
    show (B b) = show b

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr | Not Expr
     | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
     | Var Name
   deriving (Eq)

--Make Expr an instance of show for console output
instance Show Expr where
    show (Const val) = show val
    show (Add e1 e2) = show e1 ++" .+ " ++show e2
    show (Sub e1 e2) = show e1 ++" .- " ++show e2
    show (Mul e1 e2) = show e1 ++" .* " ++show e2
    show (Div e1 e2) = show e1 ++" ./ " ++show e2
    show (Eq e1 e2)  = show e1 ++" .== " ++show e2
    show (Gt e1 e2)  = show e1 ++" .> "  ++show e2
    show (Lt e1 e2)  = show e1 ++" .< "  ++show e2
    show (Var str) = str

type Exprlist = [Expr]

type Name = String
type Env = Map.Map Name Val

lookup :: String -> Env -> Eval Val
lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing -> throwError ("Unknown variable "++k)


{-- Monadic style expression evaluator,
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Env (ExceptT String Identity) a
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

{--
 -- Exercise: This evaluator could be a little neater
 -- Integer typed expressions
--}

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> throwError "type error in arithmetic expression"

-- Boolean expressions

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> throwError "type error in boolean expression"

-- Operations over integers which produce Booleans

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> throwError "type error in arithmetic expression"

-- Evaluate an expression

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = evali (+) e0 e1
eval (Sub e0 e1) = evali (-) e0 e1
eval (Mul e0 e1) = evali (*) e0 e1
eval (Div e0 e1) = evali div e0 e1

eval (And e0 e1) = evalb (&&) e0 e1
eval (Or e0 e1) =  evalb (||) e0 e1

eval (Not e0  ) = evalb (const not) e0 (Const (B True))

eval (Eq e0 e1) = evalib (==) e0 e1
eval (Gt e0 e1) = evalib (>) e0 e1
eval (Lt e0 e1) = evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}
{-------------------------------------------------------------------}


data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Condbreak Expr
               | Pass
      deriving (Eq, Show)

-- The 'Pass' statement is useful when making Statement an instance of
-- Monoid later on, we never actually expect to see it in a real program.

type Run a = StateT (Env, Exprlist) (ExceptT String IO) a
runRun p =  runExceptT ( runStateT p Map.empty)

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\(table,exprlist) -> ((), (Map.insert s i table, exprlist)))

step :: Statement -> Run ()

step (Assign s v) = do (env, _) <- get
                       case runEval env (eval v) of
                         Right val -> set (s,val)
                         Left err  -> throwError err

step (Seq s0 s1) = do
                      printHandler s0 >> userprompt >> step s0 >> checkbreakpoints >> printHandler s1  >> step s1 >> checkbreakpoints

step (Print e) = do (env, _) <- get
                    case  runEval env (eval e) of
                      Right val -> printout val
                      Left err  -> throwError err

step (If cond s0 s1) = do (env, _) <- get
                          case runEval env (eval cond) of
                            Right (B val) -> do
                              if val then do step s0 else do step s1
                            Left err -> throwError err

step (While cond s) = do (env, _) <- get
                         case runEval env (eval cond) of
                           Right (B val) -> do
                             if val then do step s >> step (While cond s) else return ()
                           Left err -> throwError err

step (Try s0 s1) = do catchError (step s0) (\e -> step s1)

step (Condbreak e) = do (env, list) <- get
                        put (env, list ++ [e])



-- We never actually expect to encounter one of these, the programs should run fine if we left this equation out:

step Pass = return ()


{--

The transformer libraries define an overloaded "liftIO" operation that passes the required operation along the stack of monads to the next "liftIO" in line until the actual IO monad is reached. In this case it's equivalent to :

 lift . lift . System.IO.print

because we have to pass through StateT and ExceptT to reach the IO monad.

--}
printout :: Val -> Run ()
printout = liftIO . System.print

printoutString :: String -> Run()
printoutString = liftIO . putStr

waitstep :: Run String
waitstep = liftIO getLine

userprompt :: Run()
userprompt = do printoutString "\n-------------------------------------------------------------------\n"
                printoutString "\nProgram execution halted, please input one of the following options: \n'next' - To execute the next statement \n'list' - To show the current list of variables \n'print <var>': To print the value of a variable of your choice\n"
                printoutString "\n-------------------------------------------------------------------\n"
                interact2

interact2 :: Run ()
interact2 = do printoutString ">"
               input <- waitstep
               case input of
                 "next"           -> return()
                 "list"           -> do (env, _) <- get
                                        liftIO (System.print $ Map.toList env)
                                        interact2
                 _                -> case take 5 input of
                                      "print"   -> showvar (drop 6 input) >> interact2
                                      _         -> interact2

showvar :: String -> Run()
showvar str  = do
                  (env, _) <- get
                  case Map.lookup str env of
                    Just x   -> printout x
                    Nothing  -> printoutString("Variable not found: " ++str ++"\n")

checkbreakpoints :: Run()
checkbreakpoints = do (env, list) <- get
                      case list of
                        [] -> return()
                        _  -> if Right (B False) `elem` evaluateList env list then printoutString("A conditional breakpoint has returned False\n") >> interact2 
                              else return()

evaluateList :: Env -> [Expr] -> [Either String Val]
evaluateList env = map (runEval env . eval)

printHandler :: Statement -> Run ()

printHandler (Assign x y)     = printoutString(x ++" .= " ++show y ++"  <-")

printHandler (Print x)        = printoutString("print $ var " ++ show x ++"  <-")

printHandler (If cond _ _)    = printoutString("iif " ++show cond ++"  <-")

printHandler (While cond _)   = printoutString("while (" ++show cond ++")  <-")

printHandler (Try s0 s1)      = printoutString"try "

printHandler (Condbreak e)    = printoutString("condbreak ("++show e ++")  <-")

printHandler (Seq _ _)        = return()
              
                 
{-------------------------------------------------------------------}
{- Pour some sugar over this -}
{-------------------------------------------------------------------}

{- This next section deals exclusively with defining convenience functions -}
{- which we will use when writing the actual programs in the DSL. -}

-- A couple of convenience functions so that we don't have to litter the program
-- with ``obvious'' constructors

int = Const . I
bool = Const . B
var = Var

-- The idea is that we can put a simple value on the RHS and have Haskell select the correct
-- instance by inspecting the type.

class SmartAssignment a where
  assign :: String -> a -> Statement

instance SmartAssignment Int where
  assign v i = Assign v (Const (I i))

instance SmartAssignment Bool where
  assign v b = Assign v (Const (B b))

instance SmartAssignment Expr where
  assign v e = Assign v e

-- going further with this (and only providing the classes and instances we actually usein the example program, but there could be others)

class PrettyExpr a b where
  (.*) :: a -> b -> Expr
  (.-) :: a -> b -> Expr
  (.+) :: a -> b -> Expr
  (./) :: a -> b -> Expr  
  (.>) :: a -> b -> Expr
  (.<) :: a -> b -> Expr
  (.==):: a -> b -> Expr
instance PrettyExpr String String where
  x .* y = (Var x) `Mul` (Var y)
  x .- y = (Var x) `Sub` (Var y)
  x .+ y = (Var x) `Add` (Var y)
  x ./ y = (Var x) `Div` (Var y)
  x .> y = (Var x) `Gt`  (Var y)
  x .< y = (Var x) `Lt`  (Var y)
  x .== y =(Var x) `Eq`  (Var y)

instance PrettyExpr String Int where
  x .* y = (Var x) `Mul` (Const (I y))
  x .- y = (Var x) `Sub` (Const (I y))
  x .+ y = (Var x) `Add` (Const (I y))
  x ./ y = (Var x) `Div` (Const (I y))
  x .> y = (Var x) `Gt`  (Const (I y))
  x .< y = (Var x) `Lt`  (Const (I y))
  x .== y =(Var x) `Eq`  (Const (I y))



{--
Making use of this we can write a program in a slightly nicer style:

I feel we're hitting the point of diminishing returns here, but I
fancy one last example of using a monad. Something to remove the need
to explicitely write "Seq" inbetween each pair of statements. Recall
that I said that >>= could be though of as a progammable semicolon?
--}


type Program = Writer Statement ()


{--
The writer monad has an operation, "tell" which appends a piece of
output to an accumulated value. For this to work the type we are
accumulating (Statement, in this case) must be have both an appending
(plus-like) operation and a base (zero-like) operation. In algebra
something with that structure is called a Monoid:
--}


-- Semigroup: how do we join two values together?
instance Semigroup Statement where
  a <> b = a `Seq` b

-- monoid: a semigroup with an identity
instance Monoid Statement where
  mempty = Pass
--  mappend a b = (<>)


{--

The idea is that the bind of the Writer monad will be used to apply
Seq (our semicolon-like operation) between each "statement". The zero
statement is needed for theoretical completeness, but it will only
appear in a result if we were to write something like this:

junk :: Program
junk = return ()

For this reason we never expect to see it in a real "compiled"
statement, so there's no case for it in the exec function.

Converting a Program to a Statement just means running the writer monad:
--}

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

{--
Executing a "program" means compiling it and then running the
resulting Statement with an empty variable map.
--}

run :: Program -> IO ()
run program = do result <- runExceptT $ (runStateT $  step $ snd $ runIdentity $ (runWriterT program)) (Map.empty, [])
                 case result of
                      Right ( (), (env,_)) -> do
                                              System.print (env Map.! "arg")
                                              return ()
                      Left exn -> System.print ("Uncaught exception: "++exn)

{--
And finally some convenience functions for our syntactic sugar:
--}

infixl 1 .=
(.=) :: String -> Expr -> Program
var .= val = tell $ assign var val


{-- if is a keyword in Haskell so I can't hide it. I renamed it so: --}

iif :: Expr -> Program -> Program -> Program
iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

while :: Expr -> Program -> Program
while cond body = tell $ While cond (compile body)

{-- This is why I wanted to hide the system function "print": --}

print :: Expr -> Program
print e = tell $ Print e

try :: Program -> Program -> Program
try block recover = tell $ Try (compile block) (compile recover)

condbreak :: Expr -> Program
condbreak ex = tell $ Condbreak ex


{--static analysis--}

parse rule text = P.parse rule "(source)" text

myParser :: P.Parsec String () (String)
myParser = do
    P.skipMany P.letter P.<|>  P.spaces
    digits <- P.string "Assign"
    return (digits)

uninitvarCheck :: Statement -> [Expr]
uninitvarCheck prog = []

unusedvarCheck :: Statement -> [Expr]
unusedvarCheck prog = []
{--
Phew.

After all that our embedded imperative language is ready to go. Here's the factorial function in all it's glory:
--}

prog10 :: Program
prog10 = do
           condbreak ("scratch" .> (5::Int))
           "arg"     .= int 10
           "scratch" .= var "arg"
           "total"   .= int 1
           while ( "scratch" .> (1::Int) )(
            do "total"   .=  "total" .* "scratch"
               "scratch" .= "scratch" .- (1::Int)
               print $ var "scratch"
            )
           print $ var "total"

prog11 :: Program
prog11 = do
           "temp"   .= int 0
           "temp2"  .= int 11
           print $ var "temp"
           iif ( (var "temp") `Gt` (int 1) )
             do
              print $ var "temp2"

             do
              print $ var "temp"
           "temp3"  .= int 100
           print $ var "temp3"

prog12 :: Program
prog12 = do
           "test"  .= int 0
           "test2" .= int 1
           "test3" .= int 2
