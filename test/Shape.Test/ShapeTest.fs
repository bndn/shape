module ShapeTest

open Xunit
open FsUnit.Xunit

open Color
open Material
open Ray
open Vector
open Texture
open Point
open Shape

[<Literal>]
let Epsilon = 1.0e-6

let mat = Material.make (Color.make 1. 0. 0.) 1.
let texture = Texture.make (fun _ _ -> mat)
let origo = Point.make 0. 0. 0.
let origoShiftOne = Point.make 0. 1. 0.
let sphereOrigo = Shape.mkSphere origo 1. texture
let planeOrigo = Shape.mkPlane origo (Vector.make 0. 1. 0.) texture
let hollowCylinderOrigo = Shape.mkHollowCylinder origo 1. 2. texture
let planeOrigoShiftOne = Shape.mkPlane origoShiftOne (Vector.make 0. 1. 0.) texture
let spherePlaneUnion = Shape.mkUnion sphereOrigo planeOrigo
let sphereShiftOne = Shape.mkSphere origoShiftOne 1. texture

let distCheck hitList distFloat index1 index2 =
    let distBetweenHits = abs ((Shape.getHitDistance (List.item index1 hitList)) - (Shape.getHitDistance (List.item index2 hitList)))
    let lessThanMax = distFloat + Epsilon > distBetweenHits
    let greaterThanMin = distBetweenHits > distFloat - Epsilon
    (lessThanMax && greaterThanMin)

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

[<Fact>]
let ``hitFunction should return a hitpoint for a ray and a disc``() =
    let center = Point.make 0. 0. 0.
    let radius = 1.
    let disc = Shape.mkDisc center radius texture

    let rO = Point.make 1. 0. -3.
    let rV = Vector.make 0. 0. 1.
    let ray = Ray.make rO rV

    let result = Shape.hitFunction ray disc
    result |> should be instanceOfType<Hitpoint list>
    List.length result |> should equal 1

[<Fact>]
let ``hitFunction should return nothing if we do not hit in the disc with our ray``() =
    let center = Point.make 0. 0. 0.
    let radius = 1.
    let disc = Shape.mkDisc center radius texture

    let rO = Point.make -2. -1. 0.
    let rV = Vector.make 0. 1. 0.
    let ray = Ray.make rO rV

    let result = Shape.hitFunction ray disc
    result |> should be instanceOfType<Hitpoint list>
    List.length result |> should equal 0

[<Fact>]
let ``mkUnion should create Union of two spheres`` () =
    let union = Shape.mkUnion sphereOrigo sphereShiftOne
    union |> should be instanceOfType<Shape>

[<Fact>]
let ``mkSubtraction should create Subtraction of two spheres`` () =
    let subtraction = Shape.mkSubtraction sphereOrigo sphereShiftOne
    subtraction |> should be instanceOfType<Shape>

[<Fact>]
let ``mkIntersection should create Intersection of two spheres`` () =
    let intersection = Shape.mkIntersection sphereOrigo sphereShiftOne
    intersection |> should be instanceOfType<Shape>

[<Fact>]
let ``hitFunction should return 2 hitpoints 3 units apart for deadcenter ray and a union of two adjacent spheres`` () =
    let union = Shape.mkUnion sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray union

    List.length hitList |> should equal 2
    (distCheck hitList 3.0 1 0) |> should equal true

[<Fact>]
let ``hitFunction should return 4 hitpoints for offcenter ray and a union of two adjacent spheres`` () =
    let union = Shape.mkUnion sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 0.9 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray union

    List.length hitList |> should equal 4

    (distCheck hitList 1.0 2 0) |> should equal true

    (distCheck hitList 1.0 3 1) |> should equal true

[<Fact>]
let ``hitFunction should return 2 glancing hitpoints for offcenter ray and a union of two adjacent spheres`` () =
    let union = Shape.mkUnion sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 1.0 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray union

    List.length hitList |> should equal 2

    (distCheck hitList 1.0 0 1) |> should equal true

[<Fact>]
let ``hitFunction should return 2 hitpoints for ray which hits shape1 in a union of two adjacent spheres`` () =
    let union = Shape.mkUnion sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 2. -0.5 0.
    let rayVector = Vector.make -1. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray union

    List.length hitList |> should equal 2

[<Fact>]
let ``hitFunction returns 2 hitpoints for ray which hits shape2`` () =
    let rayOrigin = Point.make 2. 1.5 0.
    let rayVector = Vector.make -1. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray sphereShiftOne

    List.length hitList |> should equal 2

[<Fact>]
let ``hitFunction should return 2 hitpoints for ray which hits shape2 in a union of two adjacent spheres`` () =
    let union = Shape.mkUnion sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 2. 1.5 0.
    let rayVector = Vector.make -1. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray union

    List.length hitList |> should equal 2

[<Fact>]
let ``hitFunction should return 2 hitpoints for ray which hits deadcenter in a union of sphere and plane`` () =
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray spherePlaneUnion

    List.length hitList |> should equal 2

    (distCheck hitList 2.0 1 0) |> should equal true

[<Fact>]
let ``hitFunction should return 3 hitpoints for ray which hits sphere off-center in a union of sphere and plane`` () =
    let union = Shape.mkUnion sphereOrigo planeOrigoShiftOne
    let rayOrigin = Point.make 0.9 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray union

    List.length hitList |> should equal 3

[<Fact>]
let ``hitFunction should return 2 hitpoints for ray which hits sphere glancingly in a union of sphere and plane`` () =
    let union = Shape.mkUnion sphereOrigo planeOrigoShiftOne
    let rayOrigin = Point.make 1.0 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray union

    List.length hitList |> should equal 2

    (distCheck hitList 1.0 0 1) |> should equal true

[<Fact>]
let ``getBounds returns the bounds of a shape as a quadruplet`` () =
    let bounds = Shape.getBounds sphereShiftOne

    let Delta = 1.0e-5

    match bounds with
    | Some((p, width, height, depth)) ->
        Point.getX p |> should (equalWithin Delta) -1.
        Point.getY p |> should (equalWithin Delta) 0.
        Point.getZ p |> should (equalWithin Delta) -1.
        width        |> should (equalWithin Delta) 2.
        height       |> should (equalWithin Delta) 2.
        depth        |> should (equalWithin Delta) 2.
    | None -> bounds.IsNone |> should be False // fail!

[<Fact>]
let ``getBounds returns None when the shape is a plane`` () =
    let bounds = Shape.getBounds planeOrigo

    bounds.IsNone |> should be True
    bounds.IsSome |> should be False
    bounds.IsNone |> should be True // is obvious at this point

[<Fact>]
let ``hitFunction should return 2 hitpoints for ray which hits HollowCylinder`` () =
    let rayOrigin = Point.make -3. 1. 0.
    let rayVector = Vector.make 1. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray hollowCylinderOrigo

    List.length hitList |> should equal 2

[<Fact>]
let ``hitFunction should return 0 hitpoints for ray which goes up the middle of HollowCylinder`` () =
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray hollowCylinderOrigo

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction should return 1 hitpoints for ray which glancingly hits HollowCylinder`` () =
    let rayOrigin = Point.make 1. 1. -2.
    let rayVector = Vector.make 0. 0. 1.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray hollowCylinderOrigo

    List.length hitList |> should equal 1

[<Fact>]
let ``hitFunction should return 1 hitpoints for ray which goes through and out the top of HollowCylinder`` () =
    let rayOrigin = Point.make -1.5 0. 0.
    let rayVector = Vector.make 1. 2. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray hollowCylinderOrigo

    List.length hitList |> should equal 1

[<Fact>]
let ``hitFunction should return 2 hitpoints 1 unit apart for deadcenter ray and an intersection of two adjacent spheres`` () =
    let intersection = Shape.mkIntersection sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray intersection

    List.length hitList |> should equal 2

    (distCheck hitList 1.0 0 1) |> should equal true

[<Fact>]
let ``hitFunction should return 0 hitpoints for off-center ray and an intersection of two adjacent spheres`` () =
    let intersection = Shape.mkIntersection sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 0.9 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray intersection

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction should return 1 hitpoints for ray glancing shape2 in shape1 in intersection of two adjacent spheres`` () =
    let intersection = Shape.mkIntersection sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 0. 1. -2.
    let rayVector = Vector.make 0. 0. 2.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray intersection

    List.length hitList |> should equal 1

[<Fact>]
let ``hitFunction should return 1 hitpoints for ray which hits sphere deadcenter in an intersection of sphere and plane`` () =
    let intersection = Shape.mkIntersection sphereOrigo planeOrigo
    let rayOrigin = Point.make 0.0 -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray intersection

    List.length hitList |> should equal 1

[<Fact>]
let ``hitFunction should return 0 hitpoints for ray which goes over HollowCylinder`` () =
    let rayOrigin = Point.make -2. 3. 0.
    let rayVector = Vector.make 1. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray hollowCylinderOrigo

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction should return 2 hitpoints 1 unit apart for deadcenter ray in subtraction of two adjacent spheres`` () =
    let subtraction = Shape.mkSubtraction sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray subtraction

    List.length hitList |> should equal 2

    (distCheck hitList 1.0 0 1) |> should equal true

[<Fact>]
let ``hitFunction should return 1 hitpoint for glancing ray in subtraction of two adjacent spheres`` () =
    let subtraction = Shape.mkSubtraction sphereOrigo sphereShiftOne
    let rayOrigin = Point.make 1. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray subtraction

    List.length hitList |> should equal 1

[<Fact>]
let ``hitFunction should return 3 hitpoints 1 unit apart for ray centered in sphere1, glancing sphere2 in subtraction of two adjacent spheres`` () =
    let subtraction = Shape.mkSubtraction sphereOrigo sphereShiftOne
    let rayOrigin = Point.make -2. 0. 0.
    let rayVector = Vector.make 2. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray subtraction

    List.length hitList |> should equal 3

    (distCheck hitList 1.0 0 1) |> should equal true
    (distCheck hitList 1.0 1 2) |> should equal true

[<Fact>]
let ``hitFunction should return 0 hitpoints for ray centered in sphere2, glancing sphere1 in subtraction of two adjacent spheres`` () =
    let subtraction = Shape.mkSubtraction sphereOrigo sphereShiftOne
    let rayOrigin = Point.make -2. 1. 0.
    let rayVector = Vector.make 2. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray subtraction

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction should return 0 hitpoints for ray which goes under HollowCylinder`` () =
    let rayOrigin = Point.make -2. -0.5 0.
    let rayVector = Vector.make 1. 0. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray hollowCylinderOrigo

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction should return 0 hitpoints for ray which hits plane outside of sphere in an intersection of sphere and plane`` () =
    let intersection = Shape.mkIntersection sphereOrigo planeOrigo
    let rayOrigin = Point.make 1.0 -3. 1.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray intersection

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction should return 3 hitpoints 1 units apart for ray deadcenter in subtraction of a sphere and plane`` () =
    let subtraction = Shape.mkSubtraction sphereOrigo planeOrigo
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray subtraction

    List.length hitList |> should equal 3

    (distCheck hitList 1.0 0 1) |> should equal true
    (distCheck hitList 1.0 1 2) |> should equal true

[<Fact>]
let ``hitFunction should return 0 hitpoints for ray deadcenter in subtraction of a plane and sphere`` () =
    let subtraction = Shape.mkSubtraction planeOrigo sphereOrigo
    let rayOrigin = Point.make 0. -3. 0.
    let rayVector = Vector.make 0. 1. 0.
    let ray = Ray.make rayOrigin rayVector
    let hitList = Shape.hitFunction ray subtraction

    List.length hitList |> should equal 0

[<Fact>]
let ``hitFunction through the top of a box returns hitpoints for the top and bottom`` () =
    let low = Point.make 0. 0. 0.
    let high = Point.make 1. 1. 1.
    let t = texture
    let box = Shape.mkBox low high t t t t t t

    let rayOrigin = Point.make 0.5 1.5 0.5
    let rayVector = Vector.make 0. -1. 0.
    let ray = Ray.make rayOrigin rayVector

    let (top, bottom) = match Shape.hitFunction ray box with
                        | top :: bottom :: [] -> top, bottom
                        | _ -> failwith "Failed to hit box twice"

    let topNormal = Shape.getHitNormal top
    let bottomNormal = Shape.getHitNormal bottom

    topNormal |> should equal <| Vector.make 0. 1. 0.
    bottomNormal |> should equal <| Vector.make 0. -1. 0.

[<Fact>]
let ``hitFunction through the sides of a box returns hitpoints for both the sides`` () =
    let low = Point.make 0. 0. 0.
    let high = Point.make 1. 1. 1.
    let t = texture
    let box = Shape.mkBox low high t t t t t t

    let rayOrigin = Point.make 1.5 0.5 0.5
    // check when the ray is not axis aligned
    let rayVector = Vector.make -1.001 0. 0.
    let ray = Ray.make rayOrigin rayVector

    let (right, left) = match Shape.hitFunction ray box with
                       | right :: top :: [] -> right, top
                       | _ -> failwith "Failed to hit box twice"

    let rightNormal = Shape.getHitNormal right
    let leftNormal = Shape.getHitNormal left

    rightNormal |> should equal <| Vector.make 1. 0. 0.
    leftNormal |> should equal <| Vector.make -1. 0. 0.
