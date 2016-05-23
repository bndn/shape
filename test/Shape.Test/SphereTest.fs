module Shape.Sphere.Test

open Xunit
open FsUnit.Xunit

open Shape
open Test.Utils

let radius = 1.
let sphereOrigo = Sphere.make origo radius texture
let sphereShift = Sphere.make shift radius texture

[<Fact>]
let ``make creates a Sphere Shape`` () =
    sphereOrigo |> should be instanceOfType<Shape>

[<Fact>]
let ``make with radius <= 0 fails`` () =
    (fun() -> Sphere.make origo 0. texture |> ignore)
    |> shouldFail

    (fun() -> Sphere.make origo -5. texture |> ignore)
    |> shouldFail

[<Fact>]
let ``bounds of a sphere depend on the radius and position`` () =
    let whd = radius * 2. + Epsilon * 2.

    match Shape.bounds sphereShift with
    | Some (P0, w, h, d) ->
        let x, y, z = Point.getCoord P0
        x  |> should (equalWithin Epsilon) (-1. - Epsilon)
        y  |> should (equalWithin Epsilon) (0. - Epsilon)
        z  |> should (equalWithin Epsilon) (-1. - Epsilon)
        w  |> should (equalWithin Epsilon) whd
        h  |> should (equalWithin Epsilon) whd
        d  |> should (equalWithin Epsilon) whd
    | None -> None.IsNone |> should be False

[<Fact>]
let ``sphere intersect returns hits for a dead-on ray`` () =
    let r = Ray.make (Point.make 0. 0. 5.) (Vector.make 0. 0. -1.)

    let hits = Shape.hit r sphereOrigo
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 2

[<Fact>]
let ``sphere intersect returns a hit for a strafing ray`` () =
    let r = Ray.make (Point.make 0. 1. 5.) (Vector.make 0. 0. -1.)

    let hits = Shape.hit r sphereOrigo
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 1

[<Fact>]
let ``sphere intersect returns nothing for a near-miss ray`` () =
    let r = Ray.make (Point.make 0. 1.0001 5.) (Vector.make 0. 0. -1.)

    let hit = Shape.hit r sphereOrigo
    List.length hit |> should equal 0
