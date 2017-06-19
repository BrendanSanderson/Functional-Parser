module Eval (
  EvalResult,
  eval
) where

import Parser
import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

-- Error message or result number.
type EvalResult = Either String Number

type Bindings = [(Name, Number)]


-- Return wrapped list of bare results if all inputs are Right.
-- Otherwise, returns the first Left error message.
allRight :: [EvalResult] -> Either String [Number]
allRight = foldr (liftA2 (:)) (Right [])


-- Returns either an error string or a resulting integer.
eval :: MathExp -> EvalResult
eval mathExp = evalMathExp [] mathExp
--   | length names == length mathExps = performEval
--   | otherwise                       = Left errorMsg

-- Bindings are the variables in scope.
--
-- Returns either an error string or a resulting number.
evalMathExp :: Bindings -> MathExp -> EvalResult
evalMathExp bindings exp =
  let
    recurse        = evalMathExp bindings
    unOp op e      = op <$> recurse e
    binOp op e1 e2 = liftA2 op (recurse e1) (recurse e2)
  in
  case exp of

    IfExp boolExp comp compMathExps thenExp elseExp ->
      let
        evalIf bool comp compExps = case bool of
          Not -> not $ evalComp comp compExps
          Yes -> evalComp comp compExps
        evalComp c (e1,e2) = case c of
          Greater -> evalMathExp bindings e1 > evalMathExp bindings e2
          Lesser -> evalMathExp bindings e1 < evalMathExp bindings e2
          Equal -> evalMathExp bindings e1 == evalMathExp bindings e2
          GreaterEqual -> evalMathExp bindings e1 > evalMathExp bindings e2
          LesserEqual -> evalMathExp bindings e1 > evalMathExp bindings e2
          NotEqual -> evalMathExp bindings e1 > evalMathExp bindings e2
      in
      if evalIf boolExp comp compMathExps
        then
          evalMathExp bindings thenExp
        else
          evalMathExp bindings elseExp
    Let names exps mathExp ->
      if length names == length exps
        then
          let
            performEval = do
              newBindings <- bindingsOrError
              evalMathExp (newBindings ++ bindings) mathExp
            bindingsOrError =
              zip names <$> (allRight . map (evalMathExp []) $ exps)
            in
            performEval
        else
          let
            errorMsg =
              "must assign " ++ show (length names) ++
              " names but given " ++ show (length exps) ++
              " expressions"
          in
          Left errorMsg
    Number n -> Right n
    Var name ->
      case lookup name bindings of
        Just n  -> Right n
        Nothing -> Left $ "could not find variable \"" ++ name ++ "\""
    Neg   e     -> unOp negate e
    Plus  e1 e2 -> binOp (+)      e1 e2
    Minus e1 e2 -> binOp subtract e2 e1
    Mult  e1 e2 -> binOp (*)      e1 e2
    Div   e1 e2 ->
      if recurse e2 == Right 0
      then Left "division by zero"
      else binOp quot e1 e2
    Pow   e1 e2 ->
      case recurse e2 of
        Right pow -> if pow < 0
                     then Left "negative exponent"
                     else binOp (^) e1 e2
        Left err  -> Left err
