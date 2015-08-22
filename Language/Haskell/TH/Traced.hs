{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings                                     #-}
module Language.Haskell.TH.Traced (QTrace, QState, tracing, traced, unsafeLiftQ) where
import           Control.Monad.State.Strict
import           Data.Dynamic
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax hiding (lift)
import           System.IO.Unsafe

th_trace_dic :: IORef (M.Map TypeRep Dynamic)
th_trace_dic = unsafePerformIO $ newIORef M.empty
{-# NOINLINE th_trace_dic #-}

data QState = QState { depFiles   :: [FilePath]
                     , topDecls   :: [Dec]
                     , finalizers :: [Q ()]
                     }

-- | Lifted @'Q'@ monad with logs of @'addDependentFile'@, @'addTopDecls'@
--   and @'addModFinalizer'@. @'QTrace'@ also snatches @'getQ'@ and @'putQ'@,
--   because these doesn't work till GHC 7.10.2.
newtype QTrace a = QTrace { runQTrace :: StateT QState Q a
                          } deriving (Monad, Functor, Applicative)

qtLift :: (a -> Q b) -> a -> QTrace b
qtLift = fmap (QTrace . lift)

qtLift2 :: (a -> b -> Q c) -> a -> b -> QTrace c
qtLift2 = fmap (fmap ((QTrace . lift)))

instance Quasi QTrace where
  qNewName = qtLift qNewName
  qReport  = qtLift2 qReport
  qLookupName = qtLift2 qLookupName
  qRecover (QTrace (StateT f)) (QTrace (StateT g)) =
    QTrace $ StateT $ \s -> do
      f s `qRecover` g s
  qReify = qtLift qReify
  qPutQ t = QTrace $ lift $ qRunIO $
            modifyIORef' th_trace_dic (M.insert (typeOf t) (toDyn t))
  qGetQ = QTrace $ do
    dic <- lift $ qRunIO $ readIORef th_trace_dic
    let x = fromDynamic =<< M.lookup (typeOf $ fromJust x) dic
    return x
  qReifyInstances = qtLift2 qReifyInstances
  qReifyRoles = qtLift qReifyRoles
  qReifyAnnotations = qtLift qReifyAnnotations
  qReifyModule = qtLift qReifyModule
  qLocation = QTrace $ lift qLocation
  qRunIO = qtLift qRunIO
  qAddDependentFile fp = QTrace $ do
    modify (\s -> s {depFiles = depFiles s ++ [fp]})
    lift $ qAddDependentFile fp
  qAddTopDecls decs = QTrace $ do
    modify (\s -> s {topDecls = topDecls s ++ decs})
    lift $ qAddTopDecls decs
  qAddModFinalizer f = QTrace $ do
    modify (\s -> s {finalizers = finalizers s ++ [f]})
    lift $ qAddModFinalizer f

-- | Lift @'Q'@ computation to @'QTrace'@, with logging and snatching.
traced :: Q a -> QTrace a
traced (Q a) = a

-- | Running @'Q'@ computation with state logging and hooking @'getQ'@ and @'putQ'@.
tracing :: Q a -> Q (a, QState)
tracing (Q act) = runStateT (runQTrace act) (QState [] [] [])

-- | Lift @'Q'@ computation to @'QTrace'@, WITHOUT logging and snatching.
unsafeLiftQ :: Q a -> QTrace a
unsafeLiftQ act = QTrace $ lift act
