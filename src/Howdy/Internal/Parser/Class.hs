{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Howdy.Internal.Parser.Class where

import           Control.Monad.Except        (MonadError (throwError),
                                              liftEither)
import           Control.Monad.State         (MonadState (get, put), StateT,
                                              gets)
import           Data.Text                   (Text)
import           Howdy.Internal.Parser.Cons  (rest)
import           Howdy.Internal.Parser.Types (Parser (runParser))
import Howdy.Error (report, HowdyException(..))
import Howdy.Internal.Error (KnownError)


parse :: MonadError HowdyException m => Parser a -> Text -> m (a, Text)
parse p t = report ParseError $ runParser p t

parseWithError :: MonadError HowdyException m => HowdyException -> Parser a -> Text -> m (a, Text)
parseWithError e p = report e . runParser p

parseOpt ::Monad m => Parser a -> Text -> m (Maybe (a, Text))
parseOpt p = pure . runParser p