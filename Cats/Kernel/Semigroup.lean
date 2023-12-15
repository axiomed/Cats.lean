/- Definition of the `Semigroup` type class -/

namespace Cats.Kernel

class Semigroup (α : Type u) where
  /-- An associative operation. -/
  concat : α → α → α

instance : Semigroup (List α) where
  concat := List.append

instance : Semigroup String where
  concat := String.append

instance : Semigroup (Array α) where
  concat := Array.append

instance : Semigroup Unit where
  concat _ _ := ()

instance [Semigroup α] [Semigroup β] : Semigroup (α × β) where
  concat t₁ t₂ := ⟨Semigroup.concat t₁.1 t₂.1, Semigroup.concat t₁.2 t₂.2⟩

end Cats.Kernel
