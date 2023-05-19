# Data.List.Combinator
Copyright © 2012 Bart Massey

This program is licensed under the "BSD License".  Please
see the file COPYING in the source distribution of this
software for license terms.

This is a mostly-combinator replacement for Data.List, using
only a couple of recursive functions.  At this point this is
a work-in-progress, with the entire library translated, but
with a few weird inefficiencies and holes.

Please see the comments in the source for details.

There are a few preliminary Criterion benchmarks built as
the "lcbench" executable. You may run them to see how
abysmal performance is.

Note that Criterion 2.14.2 does not seem to build with GHC 9
currently. GHC 8 seems to work.
