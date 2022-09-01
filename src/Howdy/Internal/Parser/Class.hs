{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Howdy.Internal.Parser.Class where

import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Except        (MonadError (throwError),
                                              liftEither)
import           Control.Monad.State         (MonadState (get, put), StateT,
                                              gets)
import           Data.Text                   (Text)
import           Howdy.Error                 (HowdyException (..), report)
import           Howdy.Internal.Error        (KnownError)
import           Howdy.Internal.Parser.Cons  (rest)
import           Howdy.Internal.Parser.Types (Parser (runParser))


parse :: MonadThrow m => Parser a -> Text -> m (a, Text)
parse p t = report ParseError $ runParser p t

parseWithError :: MonadThrow m => HowdyException -> Parser a -> Text -> m (a, Text)
parseWithError e p = report e . runParser p

parseOpt ::Monad m => Parser a -> Text -> m (Maybe (a, Text))
parseOpt p = pure . runParser p
