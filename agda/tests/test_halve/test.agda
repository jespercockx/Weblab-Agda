
open import Agda.Builtin.Equality

test-halve0 : halve 0 ≡ 0
test-halve0 = refl

test-halve1 : halve 1 ≡ 0
test-halve1 = refl

test-halve8 : halve 8 ≡ 4
test-halve8 = refl

test-halve13 : halve 13 ≡ 6
test-halve13 = refl
