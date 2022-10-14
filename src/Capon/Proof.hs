{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Capon.Proof (
  Proof,
  ProofState (..),
  ProovingResult (..),
  UnknownProof (..),
  ProovingError (..),
  assumptions,
  consequence,
  proof,
  intro,
  apply,
  qed,
) where

import Data.Text (Text)

import Capon.Pretty (Pretty, fillSep, pretty)
import Capon.Syntax.Ast (Expr)
import Capon.Typechecker
import Capon.Types
import Data.Bifunctor (bimap)
import Data.List (delete)

type Goal = (Env, Term)

data ProofTree where
  Done :: Term -> ProofTree
  Goal :: Goal -> ProofTree
  Appl :: ProofTree -> ProofTree -> ProofTree
  Abst :: Text -> Term -> ProofTree -> ProofTree

data Context
  = Root
  | CApL Context ProofTree
  | CApR ProofTree Context
  | CAbs Text Term Context

data ProofState = Complete | Incomplete
data Proof (state :: ProofState) where
  HasGoal :: Goal -> Context -> Proof 'Incomplete
  NoGoal :: Term -> Proof 'Complete

data ProovingError
  = NoMoreGoals
  | GoalLeft
  | ExpectedProduct
  | NotUnifiable Term Term
  | WrongProof (TypingError Term)
  | ExpectedProp (TypingError Expr)
  | TermError (TypingError Term) Env

type ProovingResult a = Either ProovingError a
type UnknownProof = Either (Proof 'Complete) (Proof 'Incomplete)

mapResult :: (a -> b) -> (c -> d) -> Either a c -> Either b d
mapResult = bimap

withGoal :: Goal -> Context -> Either a (Proof 'Incomplete)
withGoal g ctx = Right $ HasGoal g ctx

finished :: Term -> Either (Proof 'Complete) a
finished t = Left $ NoGoal t

proof :: Env -> Expr -> ProovingResult (Proof 'Incomplete)
proof env e = mapResult onErr onOk $ checkType env e (Sort Prop)
 where
  onErr = ExpectedProp
  onOk t = HasGoal (env, t) Root

assumptions :: Proof 'Incomplete -> Env
assumptions (HasGoal (env, _) _) = env
consequence :: Proof 'Incomplete -> Term
consequence (HasGoal (_, g) _) = g

qed :: Proof 'Complete -> Term
qed (NoGoal t) = t

goUp :: Context -> ProofTree -> UnknownProof
goUp ctx = \case
  Done t -> finished t
  Goal g -> withGoal g ctx
  Abst v t pf' -> goUp (CAbs v t ctx) pf'
  Appl l r -> case goUp (CApL ctx r) l of
    Left (NoGoal t) -> goUp (CApR l ctx) r
    inc -> inc

intro :: Text -> Proof 'Incomplete -> ProovingResult (Proof 'Incomplete)
intro name (HasGoal (env, g) ctx) = case whnf g of
  ForAll (FD v tp bd) ->
    withGoal (env', ass) $ CAbs name tp ctx
   where
    ass = substitute v (Var $ var name) bd
    env' = insertAbstract name tp env
  _notPi -> Left ExpectedProduct

unfoldApp :: [(Text, Term)] -> Term -> Goal -> Context -> ProovingResult (Proof 'Incomplete)
unfoldApp defs arg g@(env, t) ctx = case arg of
  t' | eval env t == eval env t' -> withGoal g ctx
  ForAll (FD x tp bd) -> do
    (rpf, newDefs, arg') <-
      case lookup x defs of
        Nothing ->
          if var x `freeIn` bd
            then Left $ NotUnifiable arg t
            else return (Goal (env, tp), defs, bd)
        Just def -> mapResult onErr onOk $ checkType env def tp
         where
          onErr = flip TermError env
          onOk def = (Done def, defs', bd')
          defs' = delete (x, def) defs
          bd' = substitute x def bd

    res <- unfoldApp newDefs arg' g ctx
    let HasGoal g' newCtx = res
    withGoal g' $ CApL newCtx rpf
  _notPi -> Left $ NotUnifiable arg t

fill :: Term -> Context -> UnknownProof
fill t ctx = case ctx of
  Root -> finished t
  CAbs v tp ctx' -> fill (Lambda $ LD v tp t) ctx'
  CApL ctx' pf -> case goUp ctx' pf of
    Left (NoGoal t') -> fill (App t t') ctx'
    Right (HasGoal g ctx'') -> withGoal g $ CApR (Done t) ctx''
  CApR pf ctx' -> case goUp ctx' pf of
    Left (NoGoal t') -> fill (App t' t) ctx'
    Right (HasGoal g ctx'') -> withGoal g $ CApL ctx'' $ Done t

apply :: Term -> [(Text, Term)] -> Proof 'Incomplete -> ProovingResult UnknownProof
apply t defs (HasGoal g@(env, consq) ctx) = case inferType env t of
  Left err -> Left $ TermError err env
  Right (t', tp) -> do
    res <- unfoldApp defs tp g ctx
    let HasGoal _ newCtx = res
    return $ fill t' newCtx

instance Pretty ProovingError where
  pretty = \case
    NoMoreGoals -> "Proof is not yet complete."
    GoalLeft -> "Proof is already complete."
    ExpectedProduct -> "No product even after reduction."
    NotUnifiable from to ->
      fillSep ["Unable to unify", pretty from, "to", pretty to] <> "."
    WrongProof err -> fillSep ["The proof is wrong:", pretty err]
    ExpectedProp err ->
      fillSep
        [ "Typechecker failed with following error:"
        , pretty err
        ]
    TermError err env ->
      fillSep
        [ "Typechecker failed with following error:"
        , pretty err
        ]