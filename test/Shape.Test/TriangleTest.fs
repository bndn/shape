module Shape.Triangle.Test

open Xunit
open FsUnit.Xunit

open Shape
open Test.Utils

let b = Point.make 5. 0. 0.
let c = Point.make 2.5 5. 1.
let triangle = Triangle.make origo b c texture

[<Fact>]
let ``make creates a Triangle Shape`` () =
    triangle |> should be instanceOfType<Shape>

[<Fact>]
let ``make with a = b || b = c || a = c fails`` () =
    let a = Point.make 5. 3. 2. in let b = a in let a = b

    (fun() -> Triangle.make a b c texture |> ignore)
    |> shouldFail

[<Fact>]
let ``bounds of a triangle depend on the a, b and c positions`` () =
    let lo = 0. - Epsilon
    let ewh, ed = (5. + Epsilon * 2.), (1. + Epsilon * 2.)

    match Shape.bounds triangle with
    | Some (P0, w, h, d) ->
        let x, y, z = Point.getCoord P0
        x  |> should (equalWithin Epsilon) lo
        y  |> should (equalWithin Epsilon) lo
        z  |> should (equalWithin Epsilon) lo
        w  |> should (equalWithin Epsilon) ewh
        h  |> should (equalWithin Epsilon) ewh
        d  |> should (equalWithin Epsilon) ed
    | None -> None.IsNone |> should be False

[<Fact>]
let ``triangle intersect returns a hit for a dead-on ray`` () =
    let r = Ray.make (Point.make 2.5 2.5 5.) (Vector.make 0. 0. -1.)

    let hits = Shape.hit r triangle
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 1

[<Fact>]
let ``triangle intersect returns nothing for a near-miss ray`` () =
    let r = Ray.make (Point.make 2.5 -0.0001 5.) (Vector.make 0. 0. -1.)

    let hit = Shape.hit r triangle
    List.length hit |> should equal 0
