Whalesong
=========

Important
---------

Whalesong needs Racket 6.2.
As is Whalesong doesn't work on version 6.3 or greater.
See https://github.com/soegaard/whalesong/issues/48

Installation
------------

raco pkg install -j 1 --force --deps search-auto --scope installation whalesong

Important: Use -j 1 to build Whalesong (this turns off parallel builds)
           This also means, that you can't install Whalesong from the DrRacket package manager.

This fork of Whalesong differs from dyoo/whalesong in the following ways:

  * Builds on version 6.2 of Racket
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
    Contributed by Emmanuel Schanzer and Darren Cruse
  * Bug fixes by Vishesh Yadav
  * The flag --as-standalone-xhtml is now --as-standalone-html
    and produces standalone html rather than xhtml.

Note: The implementation of parameters works fine,
      as long as you don't mix parameterize with non-local-exits
      and reentries (i.e. call/cc and friends)
      
/soegaard
