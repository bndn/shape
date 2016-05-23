module Shape.Cylinder.Test

open Xunit
open FsUnit.Xunit

open Shape
open Test.Utils

let radius = 3.
let height = 10.

let hollowCylinder = Cylinder.makeHollow origo radius height texture
let solidCylinder = Cylinder.makeSolid origo radius height texture texture texture

[<Fact>]
let ``make{Hollow|Solid} creates a Cylinder Shape`` () =
    hollowCylinder |> should be instanceOfType<Shape>
    solidCylinder  |> should be instanceOfType<Shape>

[<Fact>]
let ``make with r <= 0 or h <= 0 fails`` () =
    let mkCyl r h =
        (fun() -> Cylinder.makeSolid origo r h texture texture texture |> ignore)
        |> shouldFail
        (fun() -> Cylinder.makeHollow origo r h texture |> ignore)
        |> shouldFail
        ()

    mkCyl 0. 2.
    mkCyl -2. 2.
    mkCyl 2. 0.
    mkCyl 2. -2.

[<Fact>]
let ``bounds of a cylinder depend on the center, radius and height`` () =
    match Shape.bounds hollowCylinder with
    | Some (P0, w, h, d) ->
        let x, y, z = Point.getCoord P0
        x  |> should (equalWithin Epsilon) (-3. - Epsilon)
        y  |> should (equalWithin Epsilon) (-Epsilon)
        z  |> should (equalWithin Epsilon) (-3. - Epsilon)
        w  |> should (equalWithin Epsilon) (6. + Epsilon * 2.)
        h  |> should (equalWithin Epsilon) (10. + Epsilon * 2.)
        d  |> should (equalWithin Epsilon) (6. + Epsilon * 2.)
    | None -> None.IsNone |> should be False

    match Shape.bounds solidCylinder with
    | Some (P0, w, h, d) ->
        let x, y, z = Point.getCoord P0
        x  |> should (equalWithin Epsilon) (-3. - Epsilon)
        y  |> should (equalWithin Epsilon) (-Epsilon)
        z  |> should (equalWithin Epsilon) (-3. - Epsilon)
        w  |> should (equalWithin Epsilon) (6. + Epsilon * 2.)
        h  |> should (equalWithin Epsilon) (10. + Epsilon * 2.)
        d  |> should (equalWithin Epsilon) (6. + Epsilon * 2.)
    | None -> None.IsNone |> should be False

[<Fact>]
let ``cylinder intersect returns hits for a dead-on ray`` () =
    let r = Ray.make (Point.make 0. 0. 10.) (Vector.make 0. 0. -1.)

    let hits = Shape.hit r hollowCylinder
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 2

    let hits = Shape.hit r solidCylinder
    hits |> should be instanceOfType<Hit list>
    List.length hits |> should equal 2

[<Fact>]
let ``hollow cylinder intersect returns nothing when ray travels through center`` () =
    let r = Ray.make (Point.make 0. -3. 0.) (Vector.make 0. 1. 0.)

    let hits = Shape.hit r hollowCylinder
    List.length hits |> should equal 0

[<Fact>]
let ``intersect returns a hit on a strafing hit`` () =
    let r = Ray.make (Point.make -3. 1. 10.) (Vector.make 0. 0. -1.)

    let hits = Shape.hit r hollowCylinder
    List.length hits |> should equal 1

[<Fact>]
let ``intersect returns a hit through the bottom and out of a side on a hollow cylinder`` () =
    let r = Ray.make (Point.make 0. -3. 0.) (Vector.make 0. 3. 1.)

    let hits = Shape.hit r hollowCylinder
    List.length hits |> should equal 1

[<Fact>]
let ``solid cylinder intersect returns hits for a ray through top and bottom`` () =
    let r = Ray.make (Point.make 0. -3. 0.) (Vector.make 0. 1. 0.)

    let hits = Shape.hit r solidCylinder
    List.length hits |> should equal 2

[<Fact>]
let ``cylinder intersect returns nothing for a near-miss ray`` () =
    let r = Ray.make (Point.make 0. 10.001 5.) (Vector.make 0. 0. -1.)

    let hit = Shape.hit r hollowCylinder
    List.length hit |> should equal 0
