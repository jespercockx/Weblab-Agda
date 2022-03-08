open import library

halve : Nat → Nat
halve zero          = zero
halve (suc zero)    = zero
halve (suc (suc n)) = suc (halve n)
