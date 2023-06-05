module Expr where

import Control.Applicative ((<|>))

type Var = String

data Expr = Var Var
          | Const Bool
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
    deriving (Eq, Show)

-- Returns the first free variable in the clause.
firstVar :: Expr -> Maybe Var
firstVar (Var v) = Just v
firstVar (Const _) = Nothing
firstVar (Not e) = firstVar e
firstVar (And e1 e2) = firstVar e1 <|> firstVar e2
firstVar (Or e1 e2) = firstVar e1 <|> firstVar e2

-- Substitutes a boolean for the variable in the clause.
substVar :: Var -> Bool -> Expr -> Expr
substVar v b (Var u) = if u == v then Const b else Var u
substVar v b (Const c) = Const c
substVar v b (Not e) = Not (substVar v b e)
substVar v b (And e1 e2) = And (substVar v b e1) (substVar v b e2)
substVar v b (Or e1 e2) = Or (substVar v b e1) (substVar v b e2)

-- Simplifies the clause.
simplify :: Expr -> Expr
simplify (Var v) = Var v
simplify (Const b) = Const b
simplify (Not e) = case simplify e of
    Const b -> Const (not b)
    e' -> Not e'
simplify (And e1 e2) = case (simplify e1, simplify e2) of
    (Const False, _) -> Const False
    (_, Const False) -> Const False
    (Const True, Const True) -> Const True
    (Const True, e'') -> e''
    (e', Const True) -> e'
    (e', e'') -> And e' e''
simplify (Or e1 e2) = case (simplify e1, simplify e2) of
    (Const True, _) -> Const True
    (_, Const True) -> Const True
    (Const False, Const False) -> Const False
    (Const False, e'') -> e''
    (e', Const False) -> e'
    (e', e'') -> Or e' e''

-- Gets rid of unnecessary negations.
unfoldNot :: Expr -> Expr
unfoldNot (Not (Const b)) = Const (not b)
unfoldNot (Not (Not e)) = unfoldNot e
unfoldNot (Not (And e1 e2)) = Or (unfoldNot $ Not e1) (unfoldNot $ Not e2)
unfoldNot (Not (Or e1 e2)) = And (unfoldNot $ Not e1) (unfoldNot $ Not e2)
unfoldNot (Not e) = Not (unfoldNot e)
unfoldNot (And e1 e2) = And (unfoldNot e1) (unfoldNot e2)
unfoldNot (Or e1 e2) = Or (unfoldNot e1) (unfoldNot e2)
unfoldNot e = e

-- Applies De Morgan's law to the clause:
-- X or (Y and Z) == (X or Y) and (X or Z)
distribute :: Expr -> Expr
distribute (Or e1 (And e2 e3)) = And (Or e1 e2) (Or e1 e3)
distribute (Or (And e1 e2) e3) = And (Or e1 e3) (Or e2 e3)
distribute (Not e) = Not (distribute e)
distribute (And e1 e2) = And (distribute e1) (distribute e2)
distribute (Or e1 e2) = Or (distribute e1) (distribute e2)
distribute e = e

-- Converts a clause to the CNF form.
convertToCnf :: Expr -> Expr
convertToCnf e = let e' = distribute (unfoldNot e) in
    if e == e' then e else convertToCnf e'
