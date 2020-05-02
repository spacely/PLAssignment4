load harness

@test "hard-1" {
  check 'if true ∧ -3 < 4 then x := -1 else y := 2' '⇒ x := -1, {}
⇒ skip, {x → -1}'
}

@test "hard-2" {
  check 'if ( 1 - 1 ) < 0 then z8 := 09 else z3 := 90' '⇒ z3 := 90, {}
⇒ skip, {z3 → 90}'
}

@test "hard-3" {
  check 'z := ( x8 + 1 ) * -4' '⇒ skip, {z → -4}'
}

@test "hard-4" {
  check 'x := y - -2' '⇒ skip, {x → 2}'
}

@test "hard-5" {
  check 'while 0 = z * -4 do z := -1' '⇒ z := -1; while (0=(z*-4)) do { z := -1 }, {}
⇒ skip; while (0=(z*-4)) do { z := -1 }, {z → -1}
⇒ while (0=(z*-4)) do { z := -1 }, {z → -1}
⇒ skip, {z → -1}'
}

@test "hard-6" {
  check 'if 3 < -3 then g := 3 + -2 else h := 09 + 90' '⇒ h := (9+90), {}
⇒ skip, {h → 99}'
}

@test "hard-7" {
  check 'if ¬ true then x := 1 else Y := 1' '⇒ Y := 1, {}
⇒ skip, {Y → 1}'
}

@test "hard-8" {
  check 'if ( true ) then x := 1 else zir9 := 2' '⇒ x := 1, {}
⇒ skip, {x → 1}'
}

@test "hard-9" {
  check 'if -1 < -2 then g40 := 40 else g41 := 14' '⇒ g41 := 14, {}
⇒ skip, {g41 → 14}'
}

@test "hard-10" {
  check 'if ( true ∧ true ) then p := t else p := t + 1' '⇒ p := t, {}
⇒ skip, {p → 0}'
}

@test "hard-11" {
  check 'if ( true ∨ -1 < 0 ) then k := ( 49 ) * 3 + k else k := 2 * 2 * 2 + 3' '⇒ k := ((49*3)+k), {}
⇒ skip, {k → 147}'
}

@test "hard-12" {
  check 'if ( y < z ) then g := 3 else gh := 2' '⇒ gh := 2, {}
⇒ skip, {gh → 2}'
}

@test "hard-13" {
  check 'if ( ¬ true ) then skip else x := z * -1' '⇒ x := (z*-1), {}
⇒ skip, {x → 0}'
}

@test "hard-14" {
  check 'while x + 1 = 3 + 4 ∧ z - -1 = -2 * z do z := -2' '⇒ skip, {}'
}

@test "hard-15" {
  check 'x := 3 * f0 ; z := 2 * x' '⇒ skip; z := (2*x), {x → 0}
⇒ z := (2*x), {x → 0}
⇒ skip, {x → 0, z → 0}'
}

@test "hard-16" {
  check 'x := C - 0 ; y := g' '⇒ skip; y := g, {x → 0}
⇒ y := g, {x → 0}
⇒ skip, {x → 0, y → 0}'
}

@test "hard-17" {
  check 'if ( ¬ true ) then y := z + 3 else wz := -1 + x' '⇒ wz := (-1+x), {}
⇒ skip, {wz → -1}'
}

@test "hard-18" {
  check 'while false do c := x * z' '⇒ skip, {}'
}

@test "hard-19" {
  check 'while z * z < L - y do skip' '⇒ skip, {}'
}


@test "hard-20" {
  check 'if ( le * z < x - p ∧ 3 - 2 < 4 * x ) then y := z else y := z - x' '⇒ y := (z-x), {}
⇒ skip, {y → 0}'
}
