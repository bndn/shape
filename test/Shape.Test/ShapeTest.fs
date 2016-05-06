﻿module ShapeTest

open Xunit
open FsUnit.Xunit

open Color
open Material
open Ray
open Vector
open Texture
open Point
open Shape

let mat = Material.make (Color.make 1. 0. 0.) 1.
let texture = Texture.make (fun _ _ -> mat)

[<Fact>]
let ``mkSphere should create a sphere with the specified arguments`` () =
    let sphereOrigin = Point.make 1. 1. 1.
    let s = Shape.mkSphere sphereOrigin 2. texture
    s |> should be instanceOfType<Shape>

[<Fact>]
let ``mkSphere with radius 0, should fail with NonPositiveShapeSizeException`` () =
    let sphereOrigin = Point.make 1. 1. 1.
    (fun() -> Shape.mkSphere sphereOrigin 0. texture |> ignore)
    |> should throw typeof<NonPositiveShapeSizeException>

[<Fact>]
let ``mkPlane should create a plane with the specified arguments`` () =
    let planeOrigin = Point.make 1. 1. 1.
    let v = Vector.make 1. 2. 3.
    let p = Shape.mkPlane planeOrigin v texture
    p |> should be instanceOfType<Shape>

[<Fact>]
let ``mkPlane with 0-vector should fail`` () =
    let planeOrigin = Point.make 1. 1. 1.
    let v = Vector.make 0. 0. 0.
    (fun() -> Shape.mkPlane planeOrigin v texture |> ignore)
    |> shouldFail

[<Fact>]
let ``mkTriangle should create a triangle with the specified arguments`` () =
    let Ap = Point.make 1. 2. 3.
    let Bp = Point.make 0. 4. -1.
    let Cp = Point.make -1. 3. 7.
    let t = Shape.mkTriangle Ap Bp Cp mat
    t |> should be instanceOfType<Shape>

[<Fact>]
let ``mkTriangle should fail with specified arguments`` () =
    let Ap = Point.make 1. 2. 3.
    let Bp = Point.make 0. 4. -1.
    let sphereOrigin = Point.make 1. 1. 1.
    (fun() -> Shape.mkTriangle Ap Bp Bp mat |> ignore)
    |> shouldFail

[<Fact>]
let ``hitFunction should return hitpoints for some ray and a plane`` () =
    let planeOrigin = Point.make 1. 1. 1.
    let v1 = Vector.make 1. 2. 4.
    let p = Shape.mkPlane planeOrigin v1 texture

    let rayOrigin1 = Point.make 0. 0. 0.
    let ray1 = Ray.make rayOrigin1 v1

    let v2 = Vector.make 2. 2. 4.
    let rayOrigin2 = Point.make 2. 2. 2.
    let ray2 = Ray.make rayOrigin2 v2

    let h1 = Shape.hitFunction ray1 p
    h1 |> should be instanceOfType<Hitpoint list>
    List.length h1 |> should equal 1

    let h2 = Shape.hitFunction ray2 p
    h2 |> should be instanceOfType<Hitpoint list>
    List.length h2 |> should equal 0

[<Fact>]
let ``hitFunction should return hitpoints for some ray and a sphere`` () =
    let sphereOrigin = Point.make 0. 0. 0.
    let s = Shape.mkSphere sphereOrigin 3. texture

    let rayOrigin1 = Point.make -4. -4. -4.
    let v1 = Vector.make 1. 1. 1.
    let ray1 = Ray.make rayOrigin1 v1

    let rayOrigin2 = Point.make -3. -3. 0.
    let v2 = Vector.make 1. 0. 0.
    let ray2 = Ray.make rayOrigin2 v2

    let v3 = Vector.make 1. 0. 0.
    let ray3 = Ray.make rayOrigin1 v3

    let h1 = Shape.hitFunction ray1 s
    h1 |> should be instanceOfType<Hitpoint list>
    List.length h1 |> should equal 2

    let h2 = Shape.hitFunction ray2 s
    h2 |> should be instanceOfType<Hitpoint list>
    List.length h2 |> should equal 1

    let h3 = Shape.hitFunction ray3 s
    h3 |> should be instanceOfType<Hitpoint list>
    List.length h3 |> should equal 0

[<Fact>]
let ``hitFunction should return hitpoints for some ray and a triangle`` () =
    let Ap = Point.make 1. 2. 3.
    let Bp = Point.make 0. 4. -1.
    let Cp = Point.make -1. 3. 7.
    let t = Shape.mkTriangle Ap Bp Cp mat

    let rayOrigin1 = Point.make 0. 2. 3.
    let v1 = Vector.make 1. 1. 1.
    let ray1 = Ray.make rayOrigin1 v1

    let v2 = Vector.make 0. 0. 1.
    let ray2 = Ray.make rayOrigin1 v2

    let h1 = Shape.hitFunction ray1 t
    h1 |> should be instanceOfType<Hitpoint list>
    List.length h1 |> should equal 1

    let h2 = Shape.hitFunction ray2 t
    h2 |> should be instanceOfType<Hitpoint list>
    List.length h2 |> should equal 0

[<Fact>]
let ``getHitpoint should return the contents of a hitpoint ``() =
    let sphereOrigin = Point.make 0. 0. 0.
    let s = Shape.mkSphere sphereOrigin 3. texture

    let rayOrigin = Point.make -3. -3. 0.
    let rayVector = Vector.make 1. 0. 0.
    let ray = Ray.make rayOrigin rayVector

    let h = Shape.hitFunction ray s
    let hit = Shape.getHitpoint (List.head h)
    match hit with
    | (d, n ,m) ->
                    (d - 3.) |> should be (lessThan 0.01)
                    n |> should equal (Vector.make 0. -1. 0.)
                    m |> should equal mat

[<Fact>]
let ``getHitDistance should return the float distance of a hitpoint ``() =
    let sphereOrigin = Point.make 0. 0. 0.
    let s = Shape.mkSphere sphereOrigin 3. texture

    let rayOrigin = Point.make -3. -3. 0.
    let rayVector = Vector.make 1. 0. 0.
    let ray = Ray.make rayOrigin rayVector

    let h = Shape.hitFunction ray s
    let hitDistance = Shape.getHitDistance (List.head h)
    (hitDistance - 3.) |> should be (lessThan 0.01)

[<Fact>]
let ``getHitNormal should return the normal vector of a hitpoint ``() =
    let sphereOrigin = Point.make 0. 0. 0.
    let s = Shape.mkSphere sphereOrigin 3. texture

    let rayOrigin = Point.make -3. -3. 0.
    let rayVector = Vector.make 1. 0. 0.
    let ray = Ray.make rayOrigin rayVector

    let h = Shape.hitFunction ray s
    let hitNormal = Shape.getHitNormal (List.head h)
    hitNormal |> should be instanceOfType<Vector>
    hitNormal |> should equal (Vector.make 0. -1. 0.)

[<Fact>]
let ``getHitMaterial should return the Material of a hitpoint ``() =
    let sphereOrigin = Point.make 0. 0. 0.
    let s = Shape.mkSphere sphereOrigin 3. texture

    let rayOrigin = Point.make -3. -3. 0.
    let rayVector = Vector.make 1. 0. 0.
    let ray = Ray.make rayOrigin rayVector

    let h = Shape.hitFunction ray s
    let hitMaterial = Shape.getHitMaterial (List.head h)
    hitMaterial |> should be instanceOfType<Material>
    hitMaterial |> should equal mat