{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

{- |
Module          : Network.UnityM
Description     : The Unity Monad- share code between Yesod and Servant, etc.
Copyright       : (c) Brian Hurt, 2017
License         : BSD3
Maintainer      : bhurt42@gmail.com
Stability       : experimental
Portability     : Haskell

One of the nice things about WAI is that it allows us to combine multiple
different server frameworks in a single web server- allowing us to use
(for example) Yesod to develop the server-side rendered HTML part of the
website, and Servant to develop the REST API part.  But this gives rise
to a desire to share code between the different frameworks.  This is
where UnityM comes in.

UnityM is a type class for monad transformer stacks for web servers.
The assumption, in addition to being in some monad, is that we are
responding to some HTTP request, and can thus assume three things:

1. That we have read-only access to some global environment data,
generally a structure of some sort.

2. That the containing framework will catch all exceptions and
convert them into 500 (internal server error) status codes.  And that
thus the low level code does not have to capture and handle exceptions,
and that a workable way to return 500 would be to just throw an exception.

3. That we have the ability to "short-circuit" the evaluation on
errors, and return 403 (permission denied) status codes from
the request.

The type class is a multi-parameter type class, with the first parameter
being the type of the environment variable that can be gotten.  This allows
code to constrain this type with further type class constaints, and thus
work with multiple different environments, for example:

>   class HasFoo a where
>       getFoo :: a -> Foo
>
>   bar :: (Monad m, HasFoo c, UnityM c m) => Something -> m Whatever
>   bar x = do
>       myFoo <- getFoo <$> getContext
>       ...

Note that this type class assumes the code is "low-level", away from the
web request- buisness logic, database access, etc.   And thus a rich
API for different status responses is not needed.  Only two error results
are supported- the generic 500 Internal Server error, and the 403 Permission
Denied error.  The second is only supported so that access grants can
be generalized.  Error checking code that needs different status results
should be written in a framework-specific way.

The different implementations of the unity monad are in separate projects,
so you don't get unnecessary dependencies.

-}
module Network.UnityM where

    import Control.Monad.Trans.Class
    import Data.Text (Text)

    {- | The Unity Monad type class.

        For code that executes in multiple different web contexts.
    -}
    class UnityM c m | m -> c where
        {- | Get the context, the global read-only data object -}
        getContext :: m c
        {- | Short circuit the request with a 403 (Forbidden) status. -}
        permissionDenied :: Text -> m a

    {- | Support monad transformers on top of a Unity monad. -}
    instance {-# OVERLAPPABLE #-} (Monad m, UnityM c m, MonadTrans t) =>
        UnityM c (t m)
        where
            getContext = lift getContext
            permissionDenied = lift . permissionDenied

