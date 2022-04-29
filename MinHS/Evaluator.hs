{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- Reference: when writting this, I used some google search, which for me to study the Haskell, 
--basically understand what assignment ask for and see some instance, and do the copycat:
--http://www.cse.chalmers.se/edu/year/2017/course/TDA555/FAQ.html
--http://learn.hfm.io/fundamentals.html
--https://www.cs.uu.nl/docs/vakken/mcpd/2021/website/slides/AbstractMachines.pdf
--https://www.haskell.org/tutorial/functions.html
--https://stackoverflow.com/questions/33983151/implementing-abstract-stack-machine-in-haskell

-- Yuxuan Hou, stu no. 4962273
-- Solved task 1-4, trying to task5 but got a little bit mass with environment. Met problem during this assignment including:
-- Haskell's code position is a serious problem, task 2 cost me one day until my friend gave that tips to me 
-- when case includ "if null x then....", it cannot pass, I assume it is because the case of determining whether the list is empty in the list as my comments
-- some cases's solution were the copycat by test hints mentioned in doc; some from the evaluation process showed in lecture notes from previous lecture (not abstract machine)
-- recfun part shall devide in two steps, here I use if to deal it, which might cause less accurate.
module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Debug.Trace
import Text.Show.Functions ()
import System.Console.GetOpt (ArgOrder(ReturnInOrder))
import GHC.RTS.Flags (getGCFlags)

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- | Var String
           -- Add other variants as needed
           | Closur VEnv Exp --Bind
           | PartiOp Exp Value--Partial Primops
           | LetBind VEnv Bind --task 5
           deriving (Show, Read)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)

data Frame = PrimOpE Op Exp -- +-*/'s exp
           | PrimOpV Op Value -- +-*/'s value
           | ConsOpE Id Exp --cons' operation when its in expression
           | ConsOpV Id Value --cons' operation when its become value
           | NegOp Op --negative
           | Htn Op -- head/tail/null 's operation
           | IfCon Exp Exp --if condition
           | FunAppE Exp --function application's expression
           | FunAppV Value --funcation application's value
           | EEnv VEnv --environment
           | BindOp Id Type [Id] Exp --bind
           | MultLet [Bind] --multi let
           | Clos Exp VEnv --closur
           deriving (Show)

type Stack = [Frame]    --stack

--type BindEnv = E.Env Bind  --E-machine bind to enviroment

data MachineState = EvaluateState Stack Exp VEnv --Currently evaluating an expression
                   |ReturnState Stack Value VEnv -- or returning a value after evaluating
                   deriving (Show)
                   
-- do not change this definition
evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE e
evaluate _ = error "Input program did not have exactly one binding"

-- do not change this definition
evalE :: Exp -> Value
evalE expr = loop (msInitialState expr)
  where 
    loop ms = -- (trace "debug message") $  -- uncomment this line and pretty print the machine state/parts of it to
                                            -- observe the machine states
             if (msInFinalState newMsState)
                then msGetValue newMsState
                else loop newMsState
              where
                 newMsState = msStep ms

msInitialState :: Exp -> MachineState
msInitialState exp = EvaluateState [] exp E.empty -- empty stack

-- checks whether machine is in final state
msInFinalState :: MachineState -> Bool
msInFinalState ms = case ms of
                          ReturnState [] (I n) venv -> True -- stack == empty, and the expression is a value
                          ReturnState [] (B True) venv -> True -- use venv to match the "data MachineState"'s definition
                          ReturnState [] (B False) venv -> True
                          ReturnState [] Nil venv -> True
                          ReturnState [] (Cons x v) venv -> True
                          _ -> False

-- returns the final value if machine is in final state. If the machine is not in final state, throw an error
msGetValue :: MachineState -> Value
msGetValue ms = case ms of  
                      ReturnState stack (I n) venv -> (I n)
                      ReturnState stack (B True) venv -> (B True)
                      ReturnState stack (B False) venv -> (B False)
                      ReturnState stack (Nil) venv -> Nil
                      ReturnState stack (Cons x v) venv -> Cons x v

msStep :: MachineState -> MachineState
msStep ms = case ms of 
  
--1: define expression
    EvaluateState stack (Num n) venv -> ReturnState stack (I n) venv
    EvaluateState stack (Con "True") venv -> ReturnState stack (B True) venv
    EvaluateState stack (Con "False") venv-> ReturnState stack (B False) venv
    EvaluateState stack (Con "Nil") venv -> ReturnState stack (Nil) venv
    EvaluateState stack (App (App (Con "Cons")x)xs) venv -> EvaluateState ((ConsOpE "Cons" xs):stack) x venv
    ReturnState ((ConsOpE "Cons" xs):stack) (I x) venv -> EvaluateState ((ConsOpV "Cons" (I x)):stack) xs venv
    ReturnState ((ConsOpV "Cons" (I y)):stack) Nil venv -> ReturnState stack (Cons y Nil) venv
    ReturnState ((ConsOpV "Cons" Nil):stack) (I x) venv -> ReturnState stack (Cons x Nil) venv
    ReturnState ((ConsOpV "Cons" (I y)):stack) (Cons x v) venv -> ReturnState stack (Cons y (Cons x v)) venv

    -- push :: a -> Stack a -> Stack a 
    -- push x (stack xs)= stack (x:xs)

    -- pop :: Stack a -> (Maybe a, Stack a)
    -- pop (Stack []) = (Nothing, Stack [])
    -- pop (Stack (x:xs)) = (Just x, Stack xs)

    -- followed by lecture notes' step and example, first pop and push each expression and eval it, put it's result back

    --task 2:partial application --cannot work if put them below
-- [Bind "main" (TypeCon Int) [] <first target exoression>
        --expression: (Let 
          --[Bind "inc" (Arrow (TypeCon Int) (TypeCon Int)) [] <second target expression>
          --expression : ->(Recfun 
            --(Bind "inc" (Arrow (TypeCon Int) (TypeCon Int)) [] <third target expression>
              -- expression: (App (Prim Add) (Num 1))))] (App (Var "inc") (Num 2)))] -- apply inc's in prim
    
    EvaluateState stack (Recfun (Bind x typ [] (App (Prim op)e))) venv -> ReturnState stack (Closur (venv) (Recfun (Bind x typ [""] tte))) venv where
                                                                                                                        tte = (App (App (Prim op)e) e2) where e2 = (Var "")
    EvaluateState stack (Recfun (Bind x typ [] (App (Prim Neg)e))) venv -> ReturnState stack (Closur (venv) (Recfun (Bind x typ [""] tte))) venv where
                                                                                                                        tte = (App (Prim Neg)e) 
    EvaluateState stack (Recfun (Bind x typ [] (App (Prim Head)e))) venv -> ReturnState stack (Closur (venv) (Recfun (Bind x typ [""] tte))) venv where
                                                                                                                        tte = (App (Prim Head)e) 
    EvaluateState stack (Recfun (Bind x typ [] (App (Prim Tail)e))) venv -> ReturnState stack (Closur (venv) (Recfun (Bind x typ [""] tte))) venv where
                                                                                                                        tte = (App (Prim Tail)e) 
    EvaluateState stack (Recfun (Bind x typ [] (App (Prim Null)e))) venv -> ReturnState stack (Closur (venv) (Recfun (Bind x typ [""] tte))) venv where
                                                                                                                        tte = (App (Prim Null)e) 
    EvaluateState stack (Recfun (Bind a typ [] (App (Con "Cons")x))) venv -> ReturnState stack (Closur (venv) (Recfun (Bind a typ [""] tte))) venv where
                                                                                                                        tte = (App (App (Con "Cons")x)xs) where xs = (Var "")


    -- task1 
    EvaluateState stack (App (App (Prim op) e1) e2) venv -> EvaluateState ((PrimOpE op e2):stack) e1 venv
    ReturnState ((PrimOpE op e2): stack) (I n1) venv -> EvaluateState ((PrimOpV op (I n1)) :stack) e2 venv
    ReturnState ((PrimOpV op (I n1)): stack) (I n2) venv -> case op of
      Add -> ReturnState stack (I (n1 + n2)) venv
      Sub -> ReturnState stack (I (n1 - n2)) venv
      Mul -> ReturnState stack (I (n1 * n2)) venv
      Quot-> case n2 of
        _ -> ReturnState stack (I (quot n1 n2)) venv 
        0 -> error "error: do not allowed devide in zero"
      Rem -> case n2 of
        _ -> ReturnState stack (I (mod n1 n2)) venv 
        0 -> error "error: do not allowed devide in zero"
      -- neg cannot use this 
      Gt  -> ReturnState stack (B(n1 >  n2)) venv
      Ge  -> ReturnState stack (B(n1 >= n2)) venv
      Lt  -> ReturnState stack (B(n1 <  n2)) venv
      Le  -> ReturnState stack (B(n1 <= n2)) venv
      Eq  -> ReturnState stack (B(n1 == n2)) venv
      Ne  -> ReturnState stack (B(n1 /= n2)) venv

    -- Neg
    EvaluateState stack (App (Prim Neg) e) venv -> EvaluateState ((NegOp Neg ):stack) e venv
    ReturnState ((NegOp Neg): stack) (I n) venv -> ReturnState stack (I (n * (-1))) venv

    -- Head, Tail, Null (Htn)
    -- Head
    EvaluateState stack (App (Prim Head) e) venv -> EvaluateState ((Htn Head):stack) e venv
    EvaluateState ((Htn Head):stack) e venv -> case e of
      Con "Nil" -> error "Not in List"
    ReturnState ((Htn Head):stack) (Cons n _) venv -> ReturnState stack (I n) venv  --problem here, I want to present that 
                                                                                    --if e == nil, then, else, 
                                                                                    --but idk how to use correst sentance in Haskell
    -- Tail
    EvaluateState stack (App (Prim Tail) e) venv -> EvaluateState ((Htn Tail):stack) e venv
    EvaluateState ((Htn Tail):stack) e venv -> case e of
      Con "Nil" -> error "Not in List"
    ReturnState ((Htn Tail):stack) (Cons _ n) venv -> ReturnState stack n venv

    -- Null
    -- EvaluateState stack (App (Prim Null) e) venv -> case e of
    --   Con "Nil" -> ReturnState stack (B True) venv
    --   _ -> ReturnState stack (B False) venv
    EvaluateState stack (App (Prim Null) e) venv -> EvaluateState ((Htn Null):stack) e venv
    ReturnState ((Htn Null):stack) Nil venv -> ReturnState stack (B True) venv
    ReturnState ((Htn Null):stack) _ venv -> ReturnState stack (B False) venv
    
    --if-then-else
    EvaluateState stack (If e1 e2 e3) venv -> EvaluateState ((IfCon e2 e3):stack) e1 venv
    ReturnState ((IfCon e2 e3):stack) (B True) venv -> EvaluateState stack e2 venv
    ReturnState ((IfCon e2 e3):stack) (B False) venv -> EvaluateState stack e3 venv
      --case e1 of 
    --   Con str -> case str of
    --     "True" -> EvaluateState stack e2 venv 
    --     "False"-> EvaluateState stack e3 venv 

    --variable
    --lookup :: Env e -> String -> Maybe e
    --lookup (Env env) var = M.lookup var env
    EvaluateState stack (Var x) venv -> case (E.lookup venv x) of
      Nothing -> error "no such variable in under environment"
      Just va -> ReturnState stack va venv
      
    -- let: 
    -- e2 is bind to x, which x is under one envrionment's evaluate e1's result, return e1's result, put them into e2's evaluatestate, and e is expression for whole e2's operation
    -- data Bind = Bind Id Type [Id] Exp; Type here is Environment e1's Result (eer)
    --add :: Env e -> (String, e) -> Env e
    --add (Env env) (key,elt) = Env (M.insert key elt env)

    EvaluateState stack (Let [Bind x xtyp blist e1] e2) venv -> EvaluateState ((BindOp x xtyp blist e2):stack) e1 venv
    ReturnState ((BindOp x xtyp blist e2):stack) v venv -> EvaluateState stack e2 (E.add venv (x, v))

    --task 4 multi let
      --let Bind (let Bind e1; let Bind e2 )
      -- let e = prim (e1 e2)
        -- let e1
        -- let e2
    --[Bind "main" (TypeCon Int) [] (Let [Bind "a" (TypeCon Int) [] (Num 3),Bind "b" (TypeCon Int) [] (Num 2)] (App (App (Prim Add) (Var "a")) (Var "b")))]
    --[Bind "main" (TypeCon Int) [] (Let [Bind "a" (TypeCon Int) [] (Num 3)] (Let [Bind "b" (TypeCon Int) [] (Num 2)] (App (App (Prim Add) (Var "a")) (Var "b"))))]
    -- let [Bind] e where e = (let [Bind])
    
    -- expecially put environment first and then deal with simple let
    -- btw, some of his code gave me great hints, also cleared my logic; finished this with learning from him while understand lecture notes from semantics.                             
    ReturnState ((BindOp x xtyp blist e2):(BindOp x' xtyp' blist' e):(MultLet binds):stack) v venv -> EvaluateState stack (Let ((Bind x' xtyp' blist' e2):binds) e2) (E.add venv (x, v))
    EvaluateState stack (Let ((Bind x xtyp blist e1):(Bind x' xtyp' blist' e):binds) e2) venv -> EvaluateState ((BindOp x' xtyp' blist' e):(MultLet binds):stack) (Let [Bind x xtyp blist e1] e2) venv
    
    -- task 3 n-ary & recfun
    -- task 3 n-ary
    --[Bind "main" (TypeCon Bool) []                                                       | [Bind "main" (TypeCon Bool) [] 
    -- (Let [Bind "eq" (Arrow (TypeCon Int) (Arrow (TypeCon Int) (TypeCon Bool))) []       | (Let [Bind "eq" (Arrow (TypeCon Int) (Arrow (TypeCon Int) (TypeCon Bool))) [] 
      -- <closure> (Recfun                                                                 | (Recfun 
      
      --  (Bind "eq" (Arrow (TypeCon Int) (Arrow (TypeCon Int) (TypeCon Bool))) ["x","y"]  | (Bind "eq" (Arrow (TypeCon Int) (Arrow (TypeCon Int) (TypeCon Bool))) ["x"] 
           --(App (App (Prim Eq) (Var "x")) (Var "y"))))]                                  | (Recfun 
              -- (App (App (Var "eq") (Num 3)) (Num 4)))]                                  | (Bind "eq2" (Arrow (TypeCon Int) (TypeCon Bool)) ["y"] 
                                                                                        -- | (App (App (Prim Eq) (Var "x")) (Var "y"))))))] 
                                                                                        -- | (App (App (Var "eq") (Num 3)) (Num 4)))]

    EvaluateState stack (Recfun (Bind recfunid rtyp rlist funexpr)) venv -> if (length rlist) > 1
      then ReturnState stack (Closur (venv) (Recfun (Bind recfunid rtyp [head rlist] (Recfun (Bind (recfunid ++ "2") rtyp (tail rlist) funexpr))))) venv 
      else ReturnState stack (Closur (venv) (Recfun (Bind (recfunid) rtyp rlist funexpr))) venv             

    -- recfun (do not need to use it as above's "if" can deal with this when in "else" situation)
    -- EvaluateState stack (Recfun (Bind recfunid rtyp rlist funexpr)) venv -> ReturnState stack (Closur (venv) (Recfun (Bind (recfunid) rtyp rlist funexpr))) venv 

    -- task 5:
    EvaluateState stack (Let [(Bind recfunid rtyp rlist funexpr)] e) venv-> EvaluateState stack e (E.add venv (recfunid, val)) where 
                                                                                              val = LetBind venv (Bind recfunid rtyp rlist funexpr)

    -- func apply 
    --addAll :: Env e -> [(String,e)] -> Env e
    --addAll (Env env) pairs = Env $ foldr (\(k,e) g -> M.insert k e g) env pairs
    EvaluateState stack (App e1 e2) venv -> EvaluateState ((FunAppE e1):stack) e2 venv 
    ReturnState ((FunAppE e1):stack) v venv -> EvaluateState ((FunAppV v):stack) e1 venv
    ReturnState ((FunAppV v):stack) (Closur (nvenv) (Recfun (Bind (recfunid) rtyp [alist] funexpr))) venv -> EvaluateState  envstack funexpr addallenv where
                                                                                                    envstack = ((EEnv venv):stack)
                                                                                                    addallenv = (E.addAll (nvenv) ([(addallString), (addalle)])) where
                                                                                                      addallString = (alist, v)
                                                                                                      addalle = (recfunid, (Closur (nvenv) (Recfun (Bind (recfunid) rtyp [alist] funexpr))))
    ReturnState ((EEnv venv):stack) funexprV newnvenv -> ReturnState stack funexprV venv     












