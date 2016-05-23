module Shape.Rectangle.Test

open Xunit
open FsUnit.Xunit

open Shape
open Test.Utils

let w  = 4.
let h  = 3.
let rectangle = Rectangle.make origo w h texture

[<Fact>]
let ``make creates a Rectangle Shape`` () =
    rectangle |> should be instanceOfType<Shape>

[<Fact>]
let ``make with w <= 0 || h <= 0 fails`` () =
    (fun() -> Rectangle.make origo 0. 3. texture |> ignore) |> shouldFail
    (fun() -> Rectangle.make origo 3. -3. texture |> ignore) |> shouldFail
    (fun() -> Rectangle.make origo -1. 3. texture |> ignore) |> shouldFail
    (fun() -> Rectangle.make origo 3. 0. texture |> ignore) |> shouldFail

[<Fact>]
let ``bounds of a rectangle depend on the position of p0, width and height`` () =
    match Shape.bounds rectangle with
    | Some (P0, w, h, d) ->
        let x, y, z = Point.getCoord P0
        x  |> should (equalWithin Epsilon) (0. - Epsilon)
        y  |> should (equalWithin Epsilon) (0. - Epsilon)
        z  |> should (equalWithin Epsilon) (0. - Epsilon)
        w  |> should (equalWithin Epsilon) (4. + 2. * Epsilon)
        h  |> should (equalWithin Epsilon) (3. + 2. * Epsilon)
        d  |> should (equalWithin Epsilon) (2. * Epsilon)
    | None -> None.IsNone |> should be False

[<Fact>]
let ``rectangle intersect returns a hit for a dead-on ray`` () =
    let r = Ray.make (Point.make 2. 1.5 5.) (Vector.make 0. 0. -1.)

    let hits = Shape.hit r rectangle
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 1

[<Fact>]
let ``rectangle intersect returns nothing for a near-miss ray`` () =
    let r = Ray.make (Point.make -0.001 0. 5.) (Vector.make 0. 0. -1.)

    let hit = Shape.hit r rectangle
    List.length hit |> should equal 0
