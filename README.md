happy-alex-example
==================

Alex + Happy + Lazy ByteString

Quick start: `stack build && stack exec -- happy-alex-example`

This is based mostly on my own work plus some bits from happy-plus-alex [1].

Some things to consider (when comparing with the more nicely written happy-plus-alex):

 * I'd like to leave as much as possible outside of the .x and .y files.
 * It is possible to pass the token position to the parsed
   result (see the construction of Let).
 * Type-safe error wrapper.
 * Uses lazy ByteString instead of String
 * ghc-mod emacs syntax checking works only if you generate some nasty symlinks:
   (see TIPS)

[1] https://github.com/dagit/happy-plus-alex
