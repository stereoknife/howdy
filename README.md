# Howdy

A simple Discord bot framework built on top of [discord-haskell](https://hackage.haskell.org/package/discord-haskell).

## Documentation

Nothing's documented yet :)

## How to use

It's not on hackage yet :) but you can use it as a submodule:

1. Add submodule to your project's root

    $ git submodule add https://github.com/discord-haskell/discord-haskell

2. Add cabal.project ro your project's root

    packages:
	    howdy/howdy.cabal
	    my_bot.cabal

3. Add to your build-depends

    cabal-version:      3.0
	name:               my_bot
	version:            0.1.0.0
	
	executable bot
		main-is:          Main.hs
	
		build-depends:
			  howdy
			, base
