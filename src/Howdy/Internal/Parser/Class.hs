{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Howdy.Internal.Parser.Class where

import           Control.Monad.Except        (MonadError (throwError),
                                              liftEither)
import           Control.Monad.State         (MonadState (get, put), StateT,
                                              gets)
import           Data.Text                   (Text)
import           Howdy.Internal.Error        (HowdyException (ParseError))
import           Howdy.Internal.Parser.Cons  (rest)
import           Howdy.Internal.Parser.Types (Parser (runParser))

class Monad m => MonadParse m where
    parse :: Parser a -> m a


instance (Monad m, MonadError HowdyException m) => MonadParse (StateT Text m) where
    parse p = do
        t <- get
        (v, rest) <- liftEither $ handleE $ runParser p t
        put rest
        pure v
        where handleE Nothing  = Left ParseError
              handleE (Just v) = Right v
