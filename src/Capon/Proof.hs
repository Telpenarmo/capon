module Capon.Proof (
    Proof,
    Result (..),
    ProovingError (..),
    completed,
    assumptions,
    consequence,
    proof,
    intro,
    applyAssm,
    qed,
) where

import Data.Text (Text)

import qualified Capon.Context as Context
import Capon.Syntax.Ast (Expr)
import Capon.Typechecker (TypingError, checkAgainst, typecheck, typecheckWith)
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

data ProofState = Complete Term | Incomplete Goal Context
newtype Proof = P (Term, ProofState)

data ProovingError
    = NoMoreGoals
    | GoalLeft
    | ExpectedProduct
    | ExpectedPiOrGoal Term
    | AssumptionNotFound Text
    | WrongProof (TypingError Term)
    | ExpectedProp (TypingError Expr)

type Result a = Either ProovingError a

type Proove a = Proof -> Result a

proof :: Env -> Expr -> Result Proof
proof env e = case checkAgainst e (Sort Prop) of
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
qed (P (g, Complete t)) = case checkAgainst t g of
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
    doIntro (env, g) ctx = case normalize g of
        ForAll (FD v tp bd) ->
            return $ Incomplete (env', ass) (CAbs name tp ctx)
          where
            ass = substitute v (Var $ var name) (normalize bd)
            env' = Context.insertAbstract name tp env
        _ -> Left ExpectedProduct

unfoldApp :: Term -> Goal -> Context -> Result Context
unfoldApp arg (env, t) ctx = case arg of
    t' | eval env t == eval env t' -> return ctx -- arg może być typem zależnym
    ForAll (FD v tp bd) -> do
        newCtx <- unfoldApp bd (env, t) ctx
        return $ CApL newCtx $ Goal (env, tp) -- co z v? bd może je zawierać!
    _ -> Left $ ExpectedPiOrGoal t

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

applyAssm :: Text -> Proove Proof
applyAssm name = withGoal doApply
  where
    doApply g@(env, t) ctx = case Context.lookupType name env of
        Nothing -> Left $ AssumptionNotFound name
        Just t' -> do
            newCtx <- unfoldApp t' g ctx
            return $ fill v newCtx
      where
        v = Var $ var name