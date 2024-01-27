/- Implementation of the classic `StateT` monad transformer based on `mtl`-/

import Cats.Kernel
open Cats.Kernel

namespace Cats.Trans

/--
  The `StateT` state transformer.

  [`StateT`]: https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-State-Lazy.html#t:StateT

  - σ: The state type.
  - m: The inner monad.
  - α: The result type.

--/
def StateT (σ : Type u) (m : Type u → Type v) (α : Type u) : Type (max u v) :=
  σ → m (α × σ)

instance [Inhabited (α × σ)] [Monad m] : Inhabited (StateT σ m α) where
  default _ := default

/-- Runs a `StateT` and returns the inner monad with a tuple with the inner state and the return value -/
@[always_inline]
def StateT.run (trans: StateT σ m α) (initialState: σ) : m (α × σ) :=
  trans initialState

/-- Runs a `StateT` and returns the final value -/
@[always_inline]
def StateT.exec [Monad m] (trans: StateT σ m α) (initialState: σ) : m σ :=
  trans initialState <&> Prod.snd

/-- Runs a `StateT` and returns the final state -/
@[always_inline]
def StateT.eval  [Monad m] (trans: StateT σ m α) (initialState: σ) : m α :=
  trans initialState <&> Prod.fst

@[always_inline]
instance [Monad m] : MonadLift m (StateT σ m) where
  monadLift innerMonad state := (·, state) <$> innerMonad

/-- The `pure` operation for the `StateT` monad -/
@[always_inline]
def pure [inst: Monad m] (a: α) : StateT σ m α :=
  fun state => inst.pure (a, state)

/-- The `bind` operation for the `StateT` monad -/
@[always_inline]
def bind [inst: Monad m] (x: StateT σ m α) (f: α → StateT σ m β) : StateT σ m β :=
  fun state => do
    let (value, newState) ← x state
    f value newState

instance [Monad m] : Functor (StateT σ m) where
  map f x := fun state => Prod.map f id <$> x state

instance [inst: Monad m] : Applicative (StateT σ m) where
  pure := pure
  seq f x := fun state => do
    let (f, newState) ← f state
    let (x, newState) ← x () newState
    inst.pure (f x, newState)

instance [inst: Monad m] : Monad (StateT σ m) where
  bind := bind

/- API -/

/-- Get the current state -/
def StateT.get [inst: Monad m] : StateT σ m σ :=
  fun state => inst.pure (state, state)

/-- Set the state to the provided value -/
def StateT.put [inst: Monad m] (state: σ) : StateT σ m PUnit :=
  fun _ => inst.pure (PUnit.unit, state)

/-- Modify the state using the provided function -/
def StateT.modify [inst: Monad m] (f: σ → σ) : StateT σ m PUnit :=
  fun state => inst.pure (PUnit.unit, f state)

end Cats.Trans
