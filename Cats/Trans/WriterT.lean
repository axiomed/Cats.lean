/- Implementation of the classic `WriterT` monad transformer from Haskell. -/

import Cats.Kernel

open Cats.Kernel

/--
  The `WriterT` monad transformer.

  [`WriterT`]: https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Writer-CPS.html#t:WriterT
-/
def WriterT (ρ: Type u) (m: Type u -> Type v) (α: Type u) : Type (max u v) :=
  ρ → m (ρ × α)

instance [Inhabited (ρ × α)] [Monad m] : Inhabited (WriterT ρ m α) where
  default := fun _ => return default

@[always_inline]
def WriterT.run (x: WriterT ρ m α) (r: ρ) : m (ρ × α) :=
  x r

@[always_inline]
def WriterT.exec [Functor m] (x: WriterT ρ m α) (r: ρ) : m ρ :=
  Prod.fst <$> x r

namespace WriterT

@[always_inline]
instance [Monad m] : MonadLift m (WriterT ρ m) where
  monadLift x := fun p => (p, ·) <$> x

/-- The `pure` operation for the `WriterT` monad -/
@[always_inline, inline]
def pure [inst: Monad m] (a: α) : WriterT ρ m α :=
  fun r => inst.pure (r, a)

/-- The `bind` operation for the `WriterT` monad -/
@[always_inline, inline]
def bind [inst: Monad m] (x: WriterT ρ m α) (f: α → WriterT ρ m β) : WriterT ρ m β :=
  fun r => do
    let (r', a) ← x r
    f a r'

instance [Monad m] : Functor (WriterT ρ m) where
  map f x := λr => Prod.map id f <$> x r

instance [inst: Monad m] : Applicative (WriterT ρ m) where
  pure := pure
  seq f x := λr => do
    let (r', f) ← f r
    let (r'', a) ← x () r'
    inst.pure (r'', f a)

instance [inst: Monad m] : Monad (WriterT ρ m) where
  bind := bind

def WriterT.writer [Monoid w] [inst: Monad m] (elem: a × w) : WriterT w m a :=
  fun ρ => do
    let wt := Semigroup.concat elem.snd ρ
    inst.pure (wt, elem.fst)

def WriterT.tell [Monoid α] [Monad m] (a : α) : WriterT α m Unit :=
  WriterT.writer ((), a)

end WriterT
