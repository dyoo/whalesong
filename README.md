Whalesong
=========

IMPORTANT: Use -j 1 to build Whalesong (this turns off parallel builds)

This fork of Whalesong differs from dyoo/whalesong in the following ways:

  * Builds on latest release of Racket
    (fixes the x undefined problem)
  * Adds for
    (require whalesong/lang/for)
  * Adds match
    (require whalesong/lang/match)
  * Adds on-release
    (as a complement to on-key)
    Contributed by Darren Cruse
  * Adds parameters
    (require whalesong/lang/parameters)
  * Extended whalesong/image and whalesong/images
    (more functions, bug fixes, now matches WeScheme)
    Contributed by Emmanuel Schanzer
  * Adds play-sound
    (assumes a browser with html5 audio support)

Note: The implementation of parameters works fine,
      as long as you don't mix parameterize with non-local-exits
      and reentries (i.e. call/cc and friends)
      
/soegaard
