module Shape.Plane.Test

open Xunit
open FsUnit.Xunit

open Shape
open Test.Utils

let up = Vector.make 0. 1. 0.
let plane = Plane.make origo up texture

[<Fact>]
let ``make creates a Plane Shape`` () =
    plane |> should be instanceOfType<Shape>

[<Fact>]
let ``make with |up| = 0 fails`` () =
    let up = Vector.make 0. 0. 0.
    (fun() -> Plane.make origo up texture |> ignore)
    |> shouldFail

[<Fact>]
let ``bounds of a plane should be None`` () =
    let bounds = Shape.bounds plane

    bounds.IsNone |> should be True

[<Fact>]
let ``plane intersect returns a hit for a orthogonal ray`` () =
    let r = Ray.make (Point.make 0. 5. 0.) (Vector.make 0. -1. 0.)

    let hits = Shape.hit r plane
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 1

[<Fact>]
let ``plane intersect returns nothing for a parallel ray`` () =
    let r = Ray.make (Point.make 0. 0. 0.) (Vector.make 0. 0. 1.)

    let hits = Shape.hit r plane
    List.length hits |> should equal 0
