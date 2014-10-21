Whalesong
=========

This fork of Whalesong differs from dyoo/whalesong in the following ways:

  * Builds on latest release of Racket
    (fixes the x undefined problem)
  * Adds for
    (require whalesong/lang/for)
  * Adds match
    (require whalesong/lang/match)
  * Adds parameters
    (require whalesong/lang/parameters)
    
Note: The implementation of parameters works fine,
      as long as you don't mix parameterize with non-local-exits
      and reentries (i.e. call/cc and friends)
      
/soegaard
