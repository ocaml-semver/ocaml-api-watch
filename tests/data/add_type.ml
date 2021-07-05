type t = { a : int; b : string }

let f ~integer ~str t = { a = t.a + integer; b = t.b ^ str }

type u = int
