{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Howdy.Internal.Parser.Class where

import           Control.Monad.State         (MonadState (get, put), StateT)
import           Data.Text                   (Text)
import           Howdy.Internal.Parser.Types (Parser (runParser))

class Monad m => MonadParse m where
    parse :: Parser a -> m (Maybe a)


instance Monad m => MonadParse (StateT Text m) where
    parse p = do
        t <- get
        case runParser p t of Nothing       -> pure Nothing
                              Just (v,rest) -> do
                                  put rest
                                  pure $ Just v
