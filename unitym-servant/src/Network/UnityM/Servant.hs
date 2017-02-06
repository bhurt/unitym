{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module          : Network.UnityM.Servant
Description     : The Unity Monad implementation for Servant
Copyright       : (c) Brian Hurt, 2017
License         : BSD3
Maintainer      : bhurt42@gmail.com
Stability       : experimental
Portability     : Haskell

This is the implementation of the Unity monad for Servant.  I assume that
the code is using enter to add a ReaderT or StateT on top of the Handler
monad.

Note that if we try to supply a implementation of UnityM for MTL's Reader
type class, this conflicts with the Trans implementation of UnityM in the
main library.  This whole implementation is tap dancing on the edge of
needing OverlappingInstances, which is bad. For more complex instances,
hand rolled implementations of UnityM may be required.

-}
module Network.UnityM.Servant where

    import Control.Monad.Except
    import Network.UnityM
    import qualified Control.Monad.Trans.Reader as TransR
    import qualified Control.Monad.Trans.State as TransS
    import qualified Data.Text as Text
    import Servant

    instance {-# OVERLAPPING #-}
        UnityM c (TransR.ReaderT c Handler) where
            getContext = TransR.ask
            permissionDenied msg = lift $ throwError (err403
                                                    { errReasonPhrase =
                                                        Text.unpack msg })
    instance {-# OVERLAPPING #-}
        UnityM c (TransS.StateT c Handler) where
            getContext = TransS.get
            permissionDenied msg = lift $ throwError (err403
                                                    { errReasonPhrase =
                                                        Text.unpack msg })

