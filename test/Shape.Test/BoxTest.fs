module Shape.Box.Test

open Xunit
open FsUnit.Xunit

open Shape
open Test.Utils

let hi = Point.make 5. 10. 4.
let t = texture
let box = Box.make origo hi t t t t t t

[<Fact>]
let ``make creates a Box Shape`` () =
    box |> should be instanceOfType<Shape>

[<Fact>]
let ``make with lo = hi fails`` () =
    (fun() -> Box.make origo origo t t t t t t |> ignore)
    |> shouldFail

[<Fact>]
let ``bounds of a box depend on the lo and hi points`` () =
    match Shape.bounds box with
    | Some (P0, w, h, d) ->
        let x, y, z = Point.getCoord P0
        x  |> should (equalWithin Epsilon) -Epsilon
        y  |> should (equalWithin Epsilon) -Epsilon
        z  |> should (equalWithin Epsilon) -Epsilon
        w  |> should (equalWithin Epsilon) (5. + Epsilon * 2.)
        h  |> should (equalWithin Epsilon) (10. + Epsilon * 2.)
        d  |> should (equalWithin Epsilon) (4. + Epsilon * 2.)
    | None -> None.IsNone |> should be False

[<Fact>]
let ``box intersect returns hits for a dead-on ray`` () =
    let r = Ray.make (Point.make 2.5 5. 8.) (Vector.make 0. 0. -1.)

    let hits = Shape.hit r box
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 2

[<Fact>]
let ``box intersect returns nothing for a near-miss ray`` () =
    let r = Ray.make (Point.make -0.001 0. 5.) (Vector.make 0. 0. -1.)

    let hit = Shape.hit r box
    List.length hit |> should equal 0
