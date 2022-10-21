{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Capon.Engine where

import Capon.Proof (Proof, ProofState (..), ProovingError, UnknownProof (..), apply, assumptions, intro, proof, qed)
import Capon.Syntax.Stmt (EngineError (..), Statement (..))
import Capon.Typechecker
import Capon.Types (Env, Term, union)

import Capon.Syntax.Ast (Expr)
import Control.Arrow (right)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State (MonadState)
import Data.Bifunctor (bimap)
import Data.Foldable (foldlM)

type IState = (Env, Maybe (Proof 'Incomplete))

evalStatement :: IState -> Statement -> Either EngineError IState
evalStatement (env, pf) = \case
    InitProof ethm -> case pf of
        Nothing -> do
            thm <- getType env ethm
            case proof env thm of
                Left pe -> throwError $ ProovingErr pe
                Right pr -> return (env, Just pr)
        Just pr -> throwError ActiveProof
    Intro assm -> case pf of
        Just pf -> case intro assm pf of
            Left err -> throwError $ ProovingErr err
            Right pf -> return (env, Just pf)
        Nothing -> throwError NoProof
    Apply e defs -> case pf of
        Nothing -> throwError NoProof
        Just pf -> do
            arg <- getType env' e
            defs <- mapM mapBinding defs
            case apply arg defs pf of
                Left err -> throwError $ ProovingErr err
                Right ukn_pf -> case ukn_pf of
                    Right inc_pf -> return (env, Just inc_pf)
                    Left cpt_pf -> verifyProof env cpt_pf
          where
            mapBinding (v, e) = (v,) `right` getType env' e
            env' = assumptions pf `union` env
    Rewrite assm defs -> error "not implemented"
    Unfold name -> error "not implemented"
    Show name -> error "not implemented"
    Define name expr -> error "not implemented"
    Abandon -> case pf of
        Nothing -> throwError NoProof
        Just _ -> return (env, Nothing)
    Qed -> error "not supported"

getType :: Env -> Expr -> Either EngineError Term
getType env' = bimap TypingError term . inferType env'

verifyProof :: Env -> Proof 'Complete -> Either EngineError IState
verifyProof env = verifyTerm . qed
  where
    verifyTerm t = return (env, Nothing) -- TODO: check if proof is OK

eval :: IState -> [Statement] -> Either EngineError IState
eval = foldlM evalStatement