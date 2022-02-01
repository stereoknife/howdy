{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Howdy.Effects.Parse where

import           Control.Monad.Except        (MonadError (throwError),
                                              liftEither)
import           Data.Text                   (Text)
import           Howdy.Internal.Error        (HowdyException (ParseError))
import           Howdy.Internal.Parser.Cons  (rest)
import           Howdy.Internal.Parser.Types (Parser (runParser))
import           Howdy.Context (Set (set), Get (get))

class (MonadError HowdyException m, Set Text m) => Parse m where
    parse :: Parser a -> m a
    parse p = do
        t <- get
        (v, rest) <- liftEither $ handleE $ runParser p t
        set rest
        pure v
        where handleE Nothing  = Left ParseError
              handleE (Just v) = Right v