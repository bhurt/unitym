{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Module          : Network.UnityM.Yesod
Description     : The Unity Monad implementation for Yesod
Copyright       : (c) Brian Hurt, 2017
License         : BSD3
Maintainer      : bhurt42@gmail.com
Stability       : experimental
Portability     : Haskell

This is the implementation of the Unity monad for Yesod.

-}
module Network.UnityM.Yesod where

    import Network.UnityM
    import qualified Yesod

    instance {-# OVERLAPPING #-}
        (Yesod.MonadHandler m, c ~ Yesod.HandlerSite m) => UnityM c m where
            getContext = Yesod.getYesod
            permissionDenied = Yesod.permissionDenied

