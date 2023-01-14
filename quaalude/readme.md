`Essentials` is a small `Prelude` alternative. It was born out of the experience
of maintaining a number of libraries that don't use a prelude at all, preferring
instead to use tightly limited explicit imports. When we do this, we find that
although the standard prelude contains many things that most modules can live
without, there is a handful of items that most code truly needs.
