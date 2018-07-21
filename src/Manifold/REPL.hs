{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, LambdaCase, TypeOperators #-}
module Manifold.REPL where

import Control.Applicative (Alternative(..))
import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Resumable
import Data.Functor (($>))
import Data.Semiring
import GHC.Generics ((:+:)(..))
import Manifold.Abstract.Address (Precise)
import Manifold.Abstract.Env (Env, EnvError, runEnv)
import Manifold.Abstract.Evaluator (Evaluator(..))
import Manifold.Abstract.Store (Allocator, Store, StoreError, runAllocatorPrecise, runStore)
import qualified Manifold.Abstract.Value as Abstract
import Manifold.Constraint
import Manifold.Context
import Manifold.Eval (Eval, eval, runEval)
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Pretty (Pretty, prettyShow)
import Manifold.Prompt
import Manifold.Proof
import Manifold.Proof.Checking
import Manifold.Purpose
import Manifold.Parser as Parser
import Manifold.Substitution
import Manifold.Term
import Manifold.Type
import Manifold.Unification
import Manifold.Value as Value
import Text.Trifecta as Trifecta

repl :: (Member Prompt effects, Member (REPL usage) effects, Monoid usage, Pretty usage) => Proof usage effects ()
repl = prompt >>= maybe repl handleInput

handleInput :: (Member Prompt effects, Member (REPL usage) effects, Monoid usage, Pretty usage) => String -> Proof usage effects ()
handleInput str = case Parser.parseString command str of
  Left err -> output err *> repl
  Right action -> action

command :: (Member Prompt effects, Member (REPL usage) effects, Monoid usage, Pretty usage) => Parser.Parser (Proof usage effects ())
command = whole (meta <|> eval <$> term) <?> "command"
  where meta = colon
          *> ((long "help" <|> short 'h' <|> short '?' <?> "help") $> (sendREPL Help *> repl)
          <|> (long "quit" <|> short 'q' <?> "quit") $> pure ()
          <|> (typeOf <$> ((long "type" <|> short 't') *> term) <?> "type of")
          <?> "command; use :? for help")
        eval term = sendREPL (Eval term) >>= output . either prettyShow (either prettyShow prettyShow) >> repl
        typeOf term = sendREPL (TypeOf term) >>= output . either prettyShow prettyShow >> repl

        short = symbol . (:[])
        long = symbol


sendREPL :: Member (REPL usage) effects => REPL usage (Eff effects) result -> Proof usage effects result
sendREPL = send

data REPL usage (m :: * -> *) result where
  Help :: REPL usage m ()
  TypeOf :: Term Name -> REPL usage m (Either (Error (Annotated usage)) (Type (Annotated usage)))
  Eval :: Term Name -> REPL usage m (Either
    (Error (Annotated usage))
    (Either
      (SomeExc
        (   EnvError Precise
        :+: StoreError Precise (Value Precise (ValueEff Precise '[]))
        :+: ValueError Precise (ValueEff Precise '[])))
      (Value Precise (ValueEff Precise '[]))))

instance PureEffect (REPL usage)
instance Effect (REPL usage) where
  handleState c dist (Request Help k) = Request Help (dist . (<$ c) . k)
  handleState c dist (Request (TypeOf t) k) = Request (TypeOf t) (dist . (<$ c) . k)
  handleState c dist (Request (Eval t) k) = Request (Eval t) (dist . (<$ c) . k)

newtype ValueEff address effects a
  = ValueEff (Eff (  Reader (Env address)
                  ': Allocator address (Value address (ValueEff address effects))
                  ': State (Store address (Value address (ValueEff address effects)))
                  ': Fresh
                  ': Resumable (EnvError address)
                  ': Resumable (StoreError address (Value address (ValueEff address effects)))
                  ': Resumable (ValueError address (ValueEff address effects))
                  ': effects) a)

type Prelude var = Context (Constraint var (Type var))

runREPL :: (Effects effects, Eq usage, Member Prompt effects, Monoid usage, Unital usage) => Prelude (Annotated usage) -> Proof usage (REPL usage ': effects) a -> Proof usage effects a
runREPL prelude = interpret (\case
  Help -> output (unlines
    [ ":help, :h, :?     - print this help text"
    , ":quit, :q         - exit the REPL"
    , ":type, :t <expr>  - print the type of <expr>"
    , "<expr>            - typecheck & evaluate <expr>"
    ])
  TypeOf term -> runCheck' Intensional (local (const prelude) (infer term))
  Eval term -> do
    res <- runCheck' Intensional (local (const prelude) (infer term))
    case res of
      Left err -> pure (Left err)
      _        -> pure (Right (run (runEvaluator (runEval' (eval term))))))


runCheck' :: ( Effects effects
             , Eq usage
             , Monoid usage
             , Unital usage
             )
          => Purpose
          -> Proof usage
            (  Check usage
            ': Unify usage
            ': Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))
            ': Reader usage
            ': Fresh
            ': State (Substitution (Type (Annotated usage)))
            ': Exc (Error (Annotated usage))
            ': effects) (Type (Annotated usage))
          -> Proof usage effects
            (Either
              (Error (Annotated usage))
              (Type (Annotated usage)))
runCheck' purpose = runError . runSubstitution . runFresh 0 . runSigma purpose . runContext . runUnify . runCheck

runEval' :: Effects effects
         => Evaluator Precise (Value Precise (ValueEff Precise effects))
           (  Eval (Value Precise (ValueEff Precise effects))
           ': Abstract.Data (Value Precise (ValueEff Precise effects))
           ': Abstract.Function (Value Precise (ValueEff Precise effects))
           ': Reader (Env Precise)
           ': Allocator Precise (Value Precise (ValueEff Precise effects))
           ': State (Store Precise (Value Precise (ValueEff Precise effects)))
           ': Fresh
           ': Resumable (EnvError Precise)
           ': Resumable (StoreError Precise (Value Precise (ValueEff Precise effects)))
           ': Resumable (ValueError Precise (ValueEff Precise effects))
           ': effects) a
         -> Evaluator Precise (Value Precise (ValueEff Precise effects)) effects
           (Either
             (SomeExc
               (   EnvError Precise
               :+: StoreError Precise (Value Precise (ValueEff Precise effects))
               :+: ValueError Precise (ValueEff Precise effects)))
             a)
runEval' = fmap (merge . merge) . runResumable . runResumable . runResumable . runFresh 0 . fmap snd . runStore . runAllocatorPrecise . runEnv . Value.runFunction . Value.runData . runEval
  where merge :: Either (SomeExc sum) (Either (SomeExc exc) a) -> Either (SomeExc (exc :+: sum)) a
        merge (Left (SomeExc exc)) = Left (SomeExc (R1 exc))
        merge (Right (Left (SomeExc exc))) = Left (SomeExc (L1 exc))
        merge (Right (Right a)) = Right a

runIO :: (Eq usage, Monoid usage, Unital usage) => Prelude (Annotated usage) -> Proof usage '[REPL usage, Prompt] a -> IO a
runIO prelude = runPrompt "λ: " . runREPL prelude
