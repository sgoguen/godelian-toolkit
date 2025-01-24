//  0 := λf.λx.x
let zero f x = x
//  1 := λf.λx.f x
let one f x = f x
// SUCC := λn.λf.λx.f (n f x)
let succ n f x = f (n f x)

let two x = succ one x
let three x = succ two x
//  PLUS := λm.λn.λf.λx.m f (n f x)
let plus m n f x = m f (n f x)

// MULT := λm.λn.λf.m (n f)
let mult m n f = m (n f)

// POW := λb.λe.e b
let pow b e = e b

//  PRED := λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
let pred n f x = n (fun g h -> h (g f)) (fun u -> x) (fun u -> u)

//  SUB := λm.λn.n PRED m,
let sub m n = n pred m

// TRUE := λx.λy.x
let TRUE x y = x
// FALSE := λx.λy.y
let FALSE x y = y

//  AND := λp.λq.p q p
// let AND p q = p q p