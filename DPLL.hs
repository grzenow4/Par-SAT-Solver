module DPLL where

import Expr

import Control.Parallel.Strategies
import Data.Maybe
import qualified Data.Set as Set

data Polarity = Positive | Negative | Mixed

-- Gets all the literals of the clause and collects them to the Set.
getLiterals :: Expr -> Set.Set Var
getLiterals (Var v) = Set.singleton v
getLiterals (Const _) = Set.empty
getLiterals (Not e) = getLiterals e
getLiterals (And e1 e2) = Set.union (getLiterals e1) (getLiterals e2)
getLiterals (Or e1 e2) = Set.union (getLiterals e1) (getLiterals e2)

-- Returns the variable's polarity. If the varaible is always in positive
-- form returns Positive. If it is always negated, then returns Negative.
-- Otherwise, returns Mixed polarity or Nothing, if variable is not
-- present in the clause.
literalPolarity :: Expr -> Var -> Maybe Polarity
literalPolarity e v = case e of
    Var u -> if v == u then Just Positive else Nothing
    Not (Var u) -> if v == u then Just Negative else Nothing
    And e1 e2 -> helper e1 e2
    Or e1 e2 -> helper e1 e2
    _ -> Nothing
    where
        helper :: Expr -> Expr -> Maybe Polarity
        helper e1 e2 = let
            p1 = literalPolarity e1 v
            p2 = literalPolarity e2 v
            in case (p1, p2) of
                (Nothing, Nothing) -> Nothing
                (Just Positive, Just Positive) -> Just Positive
                (Just Positive, Nothing) -> Just Positive
                (Nothing, Just Positive) -> Just Positive
                (Just Negative, Just Negative) -> Just Negative
                (Just Negative, Nothing) -> Just Negative
                (Nothing, Just Negative) -> Just Negative
                (_, _) -> Just Mixed

-- The first part of the DPLL algorithm. It computes all the variables' polarities
-- and then, if any of them is Positive or Negative, substitutes True or False,
-- respectively.
literalElimination :: Expr -> Expr
literalElimination e = assignAll e
    where
        literals = Set.toList (getLiterals e)
        polarities = parMap rseq (literalPolarity e) literals

        extractPolarized :: Var -> Maybe Polarity -> Maybe (Var, Bool)
        extractPolarized v (Just Positive) = Just (v, True)
        extractPolarized v (Just Negative) = Just (v, False)
        extractPolarized _ _ = Nothing

        assignments :: [(Var, Bool)]
        assignments = catMaybes $ parMap rseq (uncurry extractPolarized) (zip literals polarities)

        assign :: [Expr -> Expr]
        assign = parMap rseq (uncurry substVar) assignments

        assignAll :: Expr -> Expr
        assignAll = foldl (.) id assign

-- Unit clause is a clause that contains only variable, negated or not.
unitClause :: Expr -> Maybe (Var, Bool)
unitClause (Var v) = Just (v, True)
unitClause (Not (Var v)) = Just (v, False)
unitClause _ = Nothing

-- Split the clause by conjuctions and returns a list of parts.
clauses :: Expr -> [Expr]
clauses (And e1 e2) = clauses e1 ++ clauses e2
clauses e = [e]

-- Returns all the unit clauses.
allUnitClauses :: Expr -> [(Var, Bool)]
allUnitClauses e = mapMaybe unitClause (clauses e)

-- The second part of the DPLL algorithm. It finds all the unit clauses
-- and substitutes the proper boolean value for them.
unitPropagation :: Expr -> Expr
unitPropagation e = assignAll e
    where
        assignments :: [(Var, Bool)]
        assignments = allUnitClauses e

        assign :: [Expr -> Expr]
        assign = parMap rseq (uncurry substVar) assignments

        assignAll :: Expr -> Expr
        assignAll = foldl (.) id assign

-- The DPLL algorithm. It uses literal elimination and unit
-- propagation to solve the SAT problem.
satisfiable :: Expr -> Bool
satisfiable e = case firstVar e' of
    Just v -> let
        substTrue = simplify (substVar v True e)
        substFalse = simplify (substVar v False e)
        in satisfiable substTrue || satisfiable substFalse
    Nothing -> unwrap (simplify e')
    where
        e' = literalElimination $ unitPropagation e

        unwrap :: Expr -> Bool
        unwrap (Const b) = b
        unwrap _ = error "unwrap failed"
