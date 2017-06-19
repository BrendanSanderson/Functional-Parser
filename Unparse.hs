-- Unparse expressions for debug display.
--
-- You should not need to modify this for the regular lab.
--
module Unparse (
  unparse
) where

import Data.List
import Parser

unparse :: MathExp -> String
unparse main = unparseMathExp main
  where
    commaList [str] =  str
    commaList strs  = "(" ++ (intercalate ", " strs) ++ ")"

unparseMathExp :: MathExp -> String
unparseMathExp (Number n)     = show n
unparseMathExp (Var    name)  = name
unparseMathExp (Neg    e1)    = "-" ++ unparseMathExp e1
unparseMathExp (Plus   e1 e2) = "(" ++ unparseMathExp e1 ++ " + " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Minus  e1 e2) = "(" ++ unparseMathExp e1 ++ " - " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Mult   e1 e2) = "(" ++ unparseMathExp e1 ++ " * " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Div    e1 e2) = "(" ++ unparseMathExp e1 ++ " / " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Pow    e1 e2) = "(" ++ unparseMathExp e1 ++  "^"  ++ unparseMathExp e2 ++ ")"
unparseMathExp (Let names assigns mathExp) = let
                                              commaList [str] =  str
                                              commaList strs  = "(" ++ (intercalate ", " strs) ++ ")"
                                             in
                                             "let " ++ commaList names ++ " = " ++
                                             (commaList . map unparseMathExp $ assigns) ++ " in " ++ unparseMathExp mathExp
unparseMathExp (IfExp boolExp comp (e1, e2) thenExp elseExp) = let
                                                                  maybeNot = case boolExp of
                                                                    Not -> "not "
                                                                    Yes -> ""
                                                                  compStr = case comp of
                                                                    Greater -> " > "
                                                                    Lesser -> " < "
                                                                    Equal -> " == "
                                                                    GreaterEqual -> " >= "
                                                                    LesserEqual -> " <= "
                                                                    NotEqual -> "/= "
                                                                in
                                                                "if " ++ maybeNot ++ unparseMathExp e1 ++ compStr ++ unparseMathExp e2 ++
                                                                " then " ++ unparseMathExp thenExp ++ " else " ++ unparseMathExp elseExp
