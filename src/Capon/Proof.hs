module Capon.Proof (
    Proof,
    Proove (..),
    ProovingError (..),
    completed,
    assumptions,
    consequence,
    proof,
    intro,
    applyAssm,
    qed,
) where

import Control.Monad.Except
import qualified Data.Map as Map
import Data.Text

import qualified Capon.Context as Context
import Capon.Syntax.Ast (Expr)
import Capon.Typechecker (TypingError, checkAgainst)
import Capon.Types

type Goal = (Env, Term)
data ProofTree
    = Done Term
    | Goal Goal
    | Appl ProofTree ProofTree
    | Abst Text Term ProofTree

data Context
    = Root
    | CApL Context ProofTree
    | CApR ProofTree Context
    | CAbs Text Term Context

data ProofStatus = Complete Term | Incomplete Goal Context
newtype Proof = P (Term, ProofStatus)

data ProovingError
    = NoMoreGoals
    | GoalLeft
    | ExpectedProduct
    | ExpectedPiOrGoal Term
    | AssumptionNotFound Text
    | WrongProof (TypingError Term)
    | ExpectedProp (TypingError Expr)

type Proove a = Except ProovingError a

type Proover a = Proof -> Proove a
type WrappedProover = Goal -> Context -> Proove ProofStatus
wrap :: WrappedProover -> Proof -> Proove Proof
wrap f (P (_, Complete _)) = throwError NoMoreGoals
wrap f (P (m, Incomplete g ctx)) = f g ctx >>= (\s -> return $ P (m, s))

proof :: Env -> Expr -> Proove Proof
proof env e = case checkAgainst e (Sort Prop) of
    Left err -> throwError $ ExpectedProp err
    Right t -> pure $ P (t, Incomplete (env, t) Root)

assumptions :: Proof -> Env
assumptions (P (_, Complete _)) = error "proof completed"
assumptions (P (_, Incomplete (env, _) _)) = env
consequence :: Proof -> Term
consequence (P (_, Complete _)) = error "proof completed"
consequence (P (_, Incomplete (_, g) _)) = g

completed :: Proof -> Bool
completed (P (_, Complete _)) = True
completed (P (_, Incomplete _ _)) = False

qed :: Proof -> Proove Term
qed (P (g, Complete t)) = case checkAgainst t g of
    Left err -> throwError $ WrongProof err
    Right te -> return te
qed (P (_, Incomplete _ _)) = throwError GoalLeft

goUp :: Context -> ProofTree -> ProofStatus
goUp ctx pf = case pf of
    Done te -> Complete te
    Goal g -> Incomplete g ctx
    Abst v t pf' -> goUp (CAbs v t ctx) pf'
    Appl l r -> case goUp (CApL ctx r) l of
        Complete t -> goUp (CApR l ctx) r
        inc -> inc

intro :: Text -> Proover Proof
intro name = wrap f
  where
    f :: WrappedProover
    f (env, g) ctx = case normalize g of
        ForAll (FD v tp bd) ->
            return $ Incomplete (env', ass) (CAbs name tp ctx)
          where
            ass = substitute v (Var $ var name) (normalize bd)
            env' = Context.insertAbstract name tp env
        _ -> throwError ExpectedProduct

unfoldApp :: Term -> Goal -> Context -> Proove Context
unfoldApp arg (env, t) ctx = case arg of
    t' | eval env t == eval env t' -> return ctx -- arg może być typem zależnym
    ForAll (FD v tp bd) -> do
        newCtx <- unfoldApp bd (env, t) ctx
        return $ CApL newCtx $ Goal (env, tp) -- co z v? bd może je zawierać!
    _ -> throwError $ ExpectedPiOrGoal arg

fill :: Term -> Context -> ProofStatus
fill t ctx = case ctx of
    Root -> Complete t
    CAbs v tp ctx' -> fill (Lambda $ LD v tp t) ctx'
    CApL ctx' pf -> case goUp ctx' pf of
        Complete t' -> fill (App t t') ctx'
        Incomplete g ctx'' -> Incomplete g $ CApR (Done t) ctx''
    CApR pf ctx' -> case goUp ctx' pf of
        Complete t' -> fill (App t' t) ctx'
        Incomplete g ctx'' -> Incomplete g $ CApL ctx'' $ Done t

applyAssm :: Text -> Proover Proof
applyAssm name = wrap f
  where
    f g@(env, t) ctx = case Context.lookupType name env of
        Nothing -> throwError $ AssumptionNotFound name
        Just t' -> do
            newCtx <- unfoldApp t' g ctx
            return $ fill (Var $ var name) newCtx