/- Definition of the `Monoid` type class -/

import Cats.Kernel.Semigroup

open Cats.Kernel

namespace Cats.Kernel

/- A monoid is a semigroup with a Unit element -/
class Monoid (α : Type u) extends Semigroup α where
  empty : α

instance : Monoid (List α) where
  empty := []

instance : Monoid String where
  empty := ""

instance : Monoid (Array α) where
  empty := #[]

end Cats.Kernel
