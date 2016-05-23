module Shape.Disc.Test

open Xunit
open FsUnit.Xunit

open Shape
open Test.Utils

let radius = 3.
let disc = Disc.make origo radius texture

[<Fact>]
let ``make creates a Disc Shape`` () =
    disc |> should be instanceOfType<Shape>

[<Fact>]
let ``make with radius <= 0 fails`` () =
    (fun() -> Disc.make origo 0. texture |> ignore)
    |> shouldFail
    (fun() -> Disc.make origo -1. texture |> ignore)
    |> shouldFail

[<Fact>]
let ``bounds of a disc depend on the center and radius`` () =
    match Shape.bounds disc with
    | Some (P0, w, h, d) ->
        let x, y, z = Point.getCoord P0
        x  |> should (equalWithin Epsilon) (-3. - Epsilon)
        y  |> should (equalWithin Epsilon) (-3. - Epsilon)
        z  |> should (equalWithin Epsilon) -Epsilon
        w  |> should (equalWithin Epsilon) (6. + Epsilon * 2.)
        h  |> should (equalWithin Epsilon) (6. + Epsilon * 2.)
        d  |> should (equalWithin Epsilon) (Epsilon * 2.)
    | None -> None.IsNone |> should be False

[<Fact>]
let ``disc intersect returns a hit for a orthogonal ray`` () =
    let r = Ray.make (Point.make 0. 0. 5.) (Vector.make 0. 0. -1.)

    let hits = Shape.hit r disc
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 1

[<Fact>]
let ``disc intersect returns nothing for a parallel ray`` () =
    let r = Ray.make (Point.make 5. 0. 0.) (Vector.make -1. 0. 0.)

    let hits = Shape.hit r disc
    List.length hits |> should equal 0

[<Fact>]
let ``disc intersect returns nothing for a near-miss ray`` () =
    let r = Ray.make (Point.make 0. 3.0001 5.) (Vector.make 0. 0. -1.)

    let hit = Shape.hit r disc
    List.length hit |> should equal 0
