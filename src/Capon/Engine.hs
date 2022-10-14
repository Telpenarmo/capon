{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Capon.Engine where

import Capon.Proof (Proof, ProovingError, apply, assumptions, intro, proof, qed, ProofState (..), UnknownProof (..))
import Capon.Syntax.Stmt (EngineError (..), Statement (..))
import Capon.Typechecker
import Capon.Types (Env, union)

import Control.Arrow (right)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State (MonadState)
import Data.Bifunctor (bimap)
import Data.Foldable (foldlM)

type IState = (Env, Maybe (Proof 'Incomplete))

evalStatement :: IState -> Statement -> Either EngineError IState
evalStatement (env, pf) = \case
    InitProof thm -> case pf of
        Nothing -> case proof env thm of
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
            t <- getType e
            defs <- mapM mapBinding defs
            case apply t defs pf of
                Left err -> throwError $ ProovingErr err
                Right pf -> case pf of
                  Right pf -> return (env, Just pf)
                  Left cpf -> return (env, Nothing)
            where
                mapBinding (v, e) = (v,) `right` getType e
                getType = bimap TypingError fst . inferType env'
                env' = assumptions pf `union` env
    Rewrite assm defs -> error "not implemented"
    Unfold name -> error "not implemented"
    Show name -> error "not implemented"
    Define name expr -> error "not implemented"
    Abandon -> case pf of
        Nothing -> throwError NoProof
        Just _ -> return (env, Nothing)
    Qed -> error "not supported"
eval :: IState -> [Statement] -> Either EngineError IState
eval = foldlM evalStatement