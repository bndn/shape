module Shape.Composite.Test

open Xunit
open FsUnit.Xunit

open Color
open Material
open Ray
open Vector
open Texture
open Point
open Shape
open Shape.Sphere
open Shape.Composite

let Epsilon = Shape.Epsilon
let c = Color.make 0. 0. 0.
let mat = Material.make c 0.5
let texture = Texture.make (fun _ _ -> mat)
let origo = Point.make 0. 0. 0.
let origoShift = Point.make 0. 1. 0.
let sphereOrigo = Shape.Sphere.make origo 1. texture
let sphereShift = Shape.Sphere.make origoShift 1. texture

let distCheck hitList distFloat index1 index2 =
    let distBetweenHits = abs ((Shape.hitDistance (List.item index1 hitList)) - (Shape.hitDistance (List.item index2 hitList)))
    let lessThanMax = distFloat + Epsilon > distBetweenHits
    let greaterThanMin = distBetweenHits > distFloat - Epsilon
    (lessThanMax && greaterThanMin)

[<Fact>]
let ``mkUnion should create Union of two spheres`` () =
    let union = Composite.make sphereOrigo sphereShift Composite.Union
    union |> should be instanceOfType<Shape>

[<Fact>]
let ``mkSubtraction should create Subtraction of two spheres`` () =
    let subtraction = Composite.make sphereOrigo sphereShift Composite.Subtraction
    subtraction |> should be instanceOfType<Shape>

[<Fact>]
let ``mkIntersection should create Intersection of two spheres`` () =
    let intersection = Composite.make sphereOrigo sphereShift Composite.Intersection
    intersection |> should be instanceOfType<Shape>

[<Fact>]
let ``hitFunction should return 2 hitpoints 3 units apart for deadcenter ray and a union of two adjacent spheres`` () =
    let union = Composite.make sphereOrigo sphereShift Composite.Union
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray union

    List.length hitList |> should equal 2
    (distCheck hitList 3.0 1 0) |> should equal true

[<Fact>]
let ``hitFunction should return 4 hitpoints for offcenter ray and a union of two adjacent spheres`` () =
    let union = Composite.make sphereOrigo sphereShift Composite.Union
    let rayOrigin = Point.make 0.9 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray union

    List.length hitList |> should equal 4

    (distCheck hitList 1.0 2 0) |> should equal true

    (distCheck hitList 1.0 3 1) |> should equal true

[<Fact>]
let ``hitFunction should return 2 glancing hitpoints for offcenter ray and a union of two adjacent spheres`` () =
    let union = Composite.make sphereOrigo sphereShift Composite.Union
    let rayOrigin = Point.make 1.0 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray union

    List.length hitList |> should equal 2
   
[<Fact>]
let ``hitFunction should return 2 hitpoints 1 unit apart for deadcenter ray in subtraction of two adjacent spheres`` () =
    let subtraction = Composite.make sphereOrigo sphereShift Composite.Subtraction
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray subtraction

    List.length hitList |> should equal 2

    (distCheck hitList 1.0 0 1) |> should equal true

[<Fact>]
let ``hitFunction should return 1 hitpoint for glancing ray in subtraction of two adjacent spheres`` () =
    let subtraction = Composite.make sphereOrigo sphereShift Composite.Subtraction
    let rayOrigin = Point.make 1. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray subtraction

    List.length hitList |> should equal 1

[<Fact>]
let ``hitFunction should return 3 hitpoints 1 unit apart for ray centered in sphere1, glancing sphere2 in subtraction of two adjacent spheres`` () =
    let subtraction = Composite.make sphereOrigo sphereShift Composite.Subtraction
    let rayOrigin = Point.make -2. 0. 0.
    let rayVector = Vector.make 2. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray subtraction

    List.length hitList |> should equal 3

    (distCheck hitList 1.0 0 1) |> should equal true
    (distCheck hitList 1.0 1 2) |> should equal true

[<Fact>]
let ``hitFunction should return 0 hitpoints for ray centered in sphere2, glancing sphere1 in subtraction of two adjacent spheres`` () =
    let subtraction = Composite.make sphereOrigo sphereShift Composite.Subtraction
    let rayOrigin = Point.make -2. 1. 0.
    let rayVector = Vector.make 2. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray subtraction

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction through the subtraction of two spheres in subtraction returns 0 hits from the subtractee sphere`` () =
    let subtraction = Composite.make sphereOrigo sphereShift Composite.Subtraction
    let rayOrigin = Point.make 0. 1.25 -3.
    let rayVector = Vector.make 0. 0. 1.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray subtraction

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction through the subtraction of two spheres in subtraction returns 2 hits from the subtracter sphere`` () =
    let subtraction = Composite.make sphereOrigo sphereShift Composite.Subtraction
    let rayOrigin = Point.make 0. -0.5 3.
    let rayVector = Vector.make 0. 0. -1.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray subtraction

    List.length hitList |> should equal 2

[<Fact>]
let ``hitFunction should return 2 hitpoints 1 unit apart for deadcenter ray and an intersection of two adjacent spheres`` () =
    let intersection = Composite.make sphereOrigo sphereShift Composite.Intersection
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray intersection

    List.length hitList |> should equal 2

    (distCheck hitList 1.0 0 1) |> should equal true

[<Fact>]
let ``hitFunction should return 0 hitpoints for off-center ray and an intersection of two adjacent spheres`` () =
    let intersection = Composite.make sphereOrigo sphereShift Composite.Intersection
    let rayOrigin = Point.make 0.9 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray intersection

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction should return 1 hitpoints for ray glancing shape2 in shape1 in intersection of two adjacent spheres`` () =
    let intersection = Composite.make sphereOrigo sphereShift Composite.Intersection
    let rayOrigin = Point.make 0. 1. -2.
    let rayVector = Vector.make 0. 0. 2.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hit ray intersection

    List.length hitList |> should equal 1


