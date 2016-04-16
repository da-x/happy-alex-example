happy-alex-example
==================

Alex + Happy + Lazy ByteString

This is based mostly on my own work plus some bits from
appy-plus-alex [1].

Some things to consider (when comparing with the more
nicely written happy-plus-alex):

 * I'd like to leave as much as possible outside of the .x and
   .y files.
 * ghc-mod emacs syntax checking works only if you generate some nasty symlinks:
   (see TIPS)
 * It is possible to pass the token position to the parsed
   result (see the construction of Let).
 * Uses lazy ByteString instead of String
 * Main program testing needs some improving.
 * Yeah, file layout - I know, but this was quick.

[1] https://github.com/dagit/happy-plus-alex
