module Capon.Proof (
  Proof,
  Result (..),
  ProovingError (..),
  completed,
  assumptions,
  consequence,
  proof,
  intro,
  apply,
  qed,
) where

import Data.Text (Text)

import Capon.Syntax.Ast (Expr)
import Capon.Typechecker
import Capon.Types
import Data.List (delete)

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

data ProofState = Complete Term | Incomplete Goal Context
newtype Proof = P (Term, ProofState)

data ProovingError
  = NoMoreGoals
  | GoalLeft
  | ExpectedProduct
  | NotUnifiable Term Term
  | WrongProof (TypingError Term)
  | ExpectedProp (TypingError Expr)
  | TermError (TypingError Term) Env

type Result a = Either ProovingError a

type Proove a = Proof -> Result a

proof :: Env -> Expr -> Result Proof
proof env e = case checkType env e (Sort Prop) of
  Left err -> Left $ ExpectedProp err
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

qed :: Proove Term
qed (P (g, Complete t)) =
  case checkType emptyEnv t g of
    Left err -> Left $ WrongProof err
    Right te -> return te
qed (P (_, Incomplete _ _)) = Left GoalLeft

goUp :: Context -> ProofTree -> ProofState
goUp ctx pf = case pf of
  Done te -> Complete te
  Goal g -> Incomplete g ctx
  Abst v t pf' -> goUp (CAbs v t ctx) pf'
  Appl l r -> case goUp (CApL ctx r) l of
    Complete t -> goUp (CApR l ctx) r
    inc -> inc

withGoal :: (Goal -> Context -> Result ProofState) -> Proove Proof
withGoal f (P (_, Complete _)) = Left NoMoreGoals
withGoal f (P (m, Incomplete g ctx)) = f g ctx >>= (\s -> return $ P (m, s))

intro :: Text -> Proove Proof
intro name = withGoal doIntro
 where
  doIntro (env, g) ctx = case whnf g of
    ForAll (FD v tp bd) ->
      return $ Incomplete (env', ass) (CAbs name tp ctx)
     where
      ass = substitute v (Var $ var name) bd
      env' = insertAbstract name tp env
    _ -> Left ExpectedProduct

unfoldApp :: [(Text, Term)] -> Term -> Goal -> Context -> Result (Goal, Context)
unfoldApp defs arg g@(env, t) ctx = case arg of
  t' | eval env t == eval env t' -> return (g, ctx)
  ForAll (FD x tp bd) -> do
    (rpf, newDefs, arg') <-
      case lookup x defs of
        Nothing ->
          if var x `freeIn` bd
            then Left $ NotUnifiable arg t
            else return (Goal (env, tp), defs, bd)
        Just def -> case checkType env def tp of
          Left err -> Left $ TermError err env
          Right def -> return (Done def, defs', bd')
           where
            defs' = delete (x, def) defs
            bd' = substitute x def bd

    (g', newCtx) <- unfoldApp newDefs arg' g ctx
    return (g', CApL newCtx rpf)
  _ -> Left $ NotUnifiable arg t

fill :: Term -> Context -> ProofState
fill t ctx = case ctx of
  Root -> Complete t
  CAbs v tp ctx' -> fill (Lambda $ LD v tp t) ctx'
  CApL ctx' pf -> case goUp ctx' pf of
    Complete t' -> fill (App t t') ctx'
    Incomplete g ctx'' -> Incomplete g $ CApR (Done t) ctx''
  CApR pf ctx' -> case goUp ctx' pf of
    Complete t' -> fill (App t' t) ctx'
    Incomplete g ctx'' -> Incomplete g $ CApL ctx'' $ Done t

apply :: Term -> [(Text, Term)] -> Proove Proof
apply t defs = withGoal doApply
 where
  doApply g@(env, consq) ctx = case inferType env t of
    Left err -> Left $ TermError err env
    Right (t', tp) -> do
      (_, newCtx) <- unfoldApp defs tp g ctx
      return $ fill t' newCtx