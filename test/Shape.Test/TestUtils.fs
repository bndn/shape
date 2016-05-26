module Shape.Test.Utils

open Shape

let Epsilon = Shape.Epsilon
let Delta = Epsilon * 10.

let mat = Material.make (Color.make 1. 0. 0.) 1.
let texture = Texture.make (fun _ _ -> mat)
let origo = Point.make 0. 0. 0.
let shift = Point.make 0. 1. 0.
