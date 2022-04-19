module Capon.Engine where

import Capon.Proof (Proof, ProovingError, applyAssm, intro, proof, qed)
import Capon.Syntax.Stmt ( EngineError(..), Statement(..) )
import Capon.Types (Env)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State (MonadState)
import Data.Foldable (Foldable (foldl'), foldlM)

type IState = (Env, Maybe Proof)

evalStatement :: IState -> Statement -> Either EngineError IState
evalStatement (env, pf) stmt = do
    case stmt of
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
        Apply assm defs -> case pf of
            Nothing -> throwError NoProof
            Just pf -> case applyAssm assm pf of
                Left err -> throwError $ ProovingErr err
                Right pf -> return (env, Just pf)
        Rewrite assm defs -> error "not implemented"
        Unfold def -> error "not implemented"
        Show txt -> error "not implemented"
        Define txt x0 -> error "not implemented"
        Abandon -> case pf of
            Nothing -> throwError NoProof
            Just _ -> return (env, Nothing)
        Qed -> case pf of
            Nothing -> throwError NoProof
            Just pf -> case qed pf of
                Left err -> throwError $ ProovingErr err
                Right te -> return (env, Nothing)

eval :: IState -> [Statement] -> Either EngineError IState
eval = foldlM evalStatement