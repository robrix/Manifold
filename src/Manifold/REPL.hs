{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Manifold.REPL where

import Control.Applicative (Alternative(..))
import Control.Effect
import Data.Functor (($>))
import Data.Semilattice.Lower
import Data.Semiring
import qualified GHC.Generics as G
import Manifold.Abstract.Address.Precise as Precise (AllocatorC, EnvC, Precise, runAllocator, runEnv)
import Manifold.Abstract.Env (Environment, EnvError)
import Manifold.Abstract.Evaluator (Evaluator(..))
import Manifold.Abstract.Store (Store, StoreError, runStore)
import Manifold.Constraint
import Manifold.Context
import Manifold.Eval (EvalC, eval, runEval)
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Parser as Parser
import Manifold.Pretty (Pretty, prettyShow)
import Manifold.Prompt
import Manifold.Proof
import Manifold.Proof.Checking
import Manifold.Purpose
import Manifold.Substitution
import Manifold.Term
import Manifold.Type
import Manifold.Unification
import Manifold.Value as Value
import System.Console.Haskeline (InputT)
import Text.Trifecta as Trifecta

repl :: (Member Prompt sig, Member (REPL usage) sig, Monoid usage, Pretty usage, Carrier sig m) => Proof usage m ()
repl = prompt >>= maybe repl handleInput

handleInput :: (Member Prompt sig, Member (REPL usage) sig, Monoid usage, Pretty usage, Carrier sig m) => String -> Proof usage m ()
handleInput str = case Parser.parseString command str of
  Left err -> output err *> repl
  Right action -> action

command :: (Member Prompt sig, Member (REPL usage) sig, Monoid usage, Pretty usage, Carrier sig m) => Parser.Parser (Proof usage m ())
command = whole (meta <|> eval <$> term) <?> "command"
  where meta = colon
          *> ((long "help" <|> short 'h' <|> short '?' <?> "help") $> (sendREPL (Help (gen ())) *> repl)
          <|> (long "quit" <|> short 'q' <?> "quit") $> pure ()
          <|> (typeOf <$> ((long "type" <|> short 't') *> term) <?> "type of")
          <?> "command; use :? for help")
        eval term = sendREPL (Eval term gen) >>= output . either prettyShow (either prettyShow prettyShow) >> repl
        typeOf term = sendREPL (TypeOf term gen) >>= output . either prettyShow prettyShow >> repl

        short = symbol . (:[])
        long = symbol


sendREPL :: (Member (REPL usage) sig, Carrier sig m) => REPL usage (Proof usage m) (Proof usage m result) -> Proof usage m result
sendREPL = send

data REPL usage m k
  = Help k
  | TypeOf (Term Name) (Either (ProofError (Annotated usage)) (Type (Annotated usage)) -> k)
  | Eval
    (Term Name)
    (Either
      (ProofError (Annotated usage))
      (Either
        (SomeError
          (     EnvError Precise
          G.:+: StoreError Precise (Value Precise (ValueC (Eff VoidC)))
          G.:+: ValueError Precise (ValueC (Eff VoidC))))
        (Value Precise (ValueC (Eff VoidC))))
    -> k)
  deriving (Functor)

instance HFunctor (REPL usage) where
  hfmap _ (Help      k) = Help k
  hfmap _ (TypeOf tm k) = TypeOf tm k
  hfmap _ (Eval   tm k) = Eval tm k

instance Effect (REPL usage) where
  handle state handler (Help     k) = Help     (handler . (<$ state) $ k)
  handle state handler (TypeOf t k) = TypeOf t (handler . (<$ state) . k)
  handle state handler (Eval   t k) = Eval   t (handler . (<$ state) . k)

newtype ValueC m a
  = ValueC (Evaluator Precise (Value Precise (ValueC m)) (Precise.EnvC
           (Evaluator Precise (Value Precise (ValueC m)) (ReaderC (Environment Precise)
           (Evaluator Precise (Value Precise (ValueC m)) (Precise.AllocatorC
           (Evaluator Precise (Value Precise (ValueC m)) (StateC (Store Precise (Value Precise (ValueC m)))
           (Evaluator Precise (Value Precise (ValueC m)) (FreshC
           (Evaluator Precise (Value Precise (ValueC m)) (ResumableC (EnvError Precise)
           (Evaluator Precise (Value Precise (ValueC m)) (ResumableC (StoreError Precise (Value Precise (ValueC m)))
           (Evaluator Precise (Value Precise (ValueC m)) (ResumableC (ValueError Precise (ValueC m))
            m))))))))))))))) a)

type Prelude var = Context (Constraint var (Type var))

runREPL :: (Eq usage, Member Prompt sig, Monoid usage, Unital usage, Carrier sig m, Effect sig) => Prelude (Annotated usage) -> Proof usage (REPLC usage (Proof usage m)) a -> Proof usage m a
runREPL prelude = flip runREPLC prelude . interpret . runProof

newtype REPLC usage m a = REPLC { runREPLC :: Prelude (Annotated usage) -> m a }

instance (Eq usage, Member Prompt sig, Carrier sig m, Monoid usage, Unital usage, Effect sig) => Carrier (REPL usage :+: sig) (REPLC usage (Proof usage m)) where
  gen a = REPLC (\ _ -> pure a)
  alg = algR \/ algOther
    where algR (Help k) = REPLC (\ prelude -> output (unlines
            [ ":help, :h, :?     - print this help text"
            , ":quit, :q         - exit the REPL"
            , ":type, :t <expr>  - print the type of <expr>"
            , "<expr>            - typecheck & evaluate <expr>"
            ]) >> runREPLC k prelude)
          algR (TypeOf term k) = REPLC (\ prelude -> runCheck' Intensional (local (const prelude) (infer term)) >>= flip runREPLC prelude . k)
          algR (Eval term k) = REPLC (\ prelude -> do
            res <- runCheck' Intensional (local (const prelude) (infer term))
            case res of
              Left err -> runREPLC (k (Left err)) prelude
              _        -> runREPLC (k (Right (run (runEval' (eval term))))) prelude)
          algOther op = REPLC (\ prelude -> alg (handlePure (flip runREPLC prelude) op))

runCheck' :: ( Effectful sig m'
             , Eq usage
             , Monoid usage
             , Unital usage
             )
          => Purpose
          -> Proof usage (CheckC usage
            (Proof usage (UnifyC usage
            (Proof usage (ReaderC (Context (Constraint (Annotated usage) (Type (Annotated usage))))
            (Proof usage (ReaderC usage
            (Proof usage (FreshC
            (Proof usage (StateC (Substitution (Type (Annotated usage)))
            (Proof usage (ErrorC (ProofError (Annotated usage))
            m'))))))))))))) (Type (Annotated usage))
          -> m'
            (Either
              (ProofError (Annotated usage))
              (Type (Annotated usage)))
runCheck' purpose = runError . runProof . runSubstitution . runFresh . runProof . runSigma purpose . runContext . runUnify . runCheck

runEval' :: Effectful sig m
         => Evaluator Precise (Value Precise (ValueC m)) (EvalC
           (Evaluator Precise (Value Precise (ValueC m)) (Value.DataC
           (Evaluator Precise (Value Precise (ValueC m)) (Value.FunctionC
           (Evaluator Precise (Value Precise (ValueC m)) (Precise.EnvC
           (Evaluator Precise (Value Precise (ValueC m)) (ReaderC (Environment Precise)
           (Evaluator Precise (Value Precise (ValueC m)) (Precise.AllocatorC
           (Evaluator Precise (Value Precise (ValueC m)) (StateC (Store Precise (Value Precise (ValueC m)))
           (Evaluator Precise (Value Precise (ValueC m)) (FreshC
           (Evaluator Precise (Value Precise (ValueC m)) (ResumableC (EnvError Precise)
           (Evaluator Precise (Value Precise (ValueC m)) (ResumableC (StoreError Precise (Value Precise (ValueC m)))
           (Evaluator Precise (Value Precise (ValueC m)) (ResumableC (ValueError Precise (ValueC m))
            m))))))))))))))))))))) a
         -> m
           (Either
             (SomeError
               (     EnvError Precise
               G.:+: StoreError Precise (Value Precise (ValueC m))
               G.:+: ValueError Precise (ValueC m)))
             a)
runEval' = fmap (merge . merge) . runResumable . runEvaluator . runResumable . runEvaluator . runResumable . runEvaluator . runFresh . runEvaluator . fmap snd . runStore . Precise.runAllocator . runReader lowerBound . runEvaluator . Precise.runEnv . Value.runFunction . Value.runData . runEval
  where merge :: Either (SomeError sum) (Either (SomeError exc) a) -> Either (SomeError (exc G.:+: sum)) a
        merge (Left (SomeError exc)) = Left (SomeError (G.R1 exc))
        merge (Right (Left (SomeError exc))) = Left (SomeError (G.L1 exc))
        merge (Right (Right a)) = Right a

runIO :: (Eq usage, Monoid usage, Unital usage) => Prelude (Annotated usage) -> Proof usage (REPLC usage (Proof usage (PromptC (Eff (LiftC (InputT IO)))))) a -> IO a
runIO prelude = runPrompt "λ: " . runProof . runREPL prelude
