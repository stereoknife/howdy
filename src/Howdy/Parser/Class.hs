{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Howdy.Parser.Class where

import           Control.Monad.State (MonadState (get, put))
import           Data.Text           (Text)
import           Howdy.Parser.Types  (Parser (runParser))

class MonadParse m where
    parse :: Parser a -> m (Maybe a)

instance MonadState Text m => MonadParse m where
    parse p = do
        t <- get
        case runParser p t of Nothing       -> pure Nothing
                              Just (v,rest) -> do
                                  put rest
                                  pure $ Just v
