/// Additional operations on the tuple types.
module Tuple =

  /// Transforms tuple with seperate functions to new form.
  let map f g (x, y) = f x, g y

  /// Collapses the tuple on itself with a given function to a new form.
  let collapse f (x, y) = f x y

/// Additional operations on triple types.
module Triple =

  /// Transforms triple with seperate functions to new form.
  let map f g h (x, y, z) = f x, g y, h z

  /// Collapses the tuple on itself with a given function to a new form.
  let collapse f (x, y, z) = f x y z
