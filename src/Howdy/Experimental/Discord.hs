module Howdy.Experimental.Discord where
import Data.Default
import Discord

data HowdyParallel r a = HowdyParallel
    { discordCtx :: DiscordHandler a
    , howdyCtx   :: r
    }

instance Functor (HowdyParallel r) where
    fmap f m = m{ discordCtx = fmap f m.discordCtx }

instance Default r => Applicative (HowdyParallel r) where
    f <*> m = m{ discordCtx = f.discordCtx <*> m.discordCtx }
    pure a = HowdyParallel (pure a) def
