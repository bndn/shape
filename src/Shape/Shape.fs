// Copyright (C) 2015 The Authors.
module Shape

open System // for Math

open Material
open Point
open Ray
open Texture
open Vector

[<Literal>]
let EPSILON = 1.0e-6

type Composition =
    | Union
    | Subtraction
    | Intersection

type Bounds = Point * float * float * float

type Shape =
    | Plane of Point * Vector * Texture
    | Disc of Point * float * Texture
    | Sphere of Point * float * Texture
    | HollowCylinder of Point * float * float * Texture
    | Triangle of Point * Point * Point * Texture
    | Composite of Shape * Shape * Composition
    | Box of Point * Point * Texture * Texture * Texture * Texture * Texture * Texture

type Hitpoint =
    | Hit of float * Vector * Material

exception NonPositiveShapeSizeException

/// <summary>
/// Get the distance, the normal and the material of the hit.
/// </summary>
/// <param name=hp>The hitpoint to get the values of.</param>
/// <returns>
/// A triplet of the distance, the normal and the material
/// of the hit.
/// </returns>
let getHitpoint (Hit(d, n, m)) = (d, n, m)

/// <summary>
/// Get the distance from the ray's origin, to the hitpoint on the shape.
/// </summary>
/// <param name=hp>The hitpoint on the shape to get the distance of.</param>
/// <returns>The distance to the hitpoint.</returns>
let getHitDistance (Hit(d, _, _)) = d

/// <summary>
/// Get the normal of the shape at the hitpoint.
/// </summary>
/// <param name=hp>The hitpoint on the shape to get the normal of.</param>
/// <returns>The normal of the shape at the hitpoint.</returns>
let getHitNormal (Hit(_, n, _)) = n

/// <summary>
/// Get the material of the shape at the hitpoint.
/// </summary>
/// <param name=hp>The hitpoint on the shape to get the material of.</param>
/// <returns>The material on the shape at the hitpoint.</returns>
let getHitMaterial (Hit(_, _, m)) = m

/// <summary>
/// Combine the bounds of two shapes into a single boundary.
/// </summary>
/// <param name=b1>The first boundary in the combination.</param>
/// <param name=b2>The second boundary in the combination.<param>
/// <returns>The combined bounds of b1 and b2.</returns>
let combineBounds = function
    | (None, None)       -> None
    | (Some bound, None) -> Some bound
    | (None, Some bound) -> Some bound
    | (Some (b1, b1w, b1h, b1d), Some (b2, b2w, b2h, b2d)) ->
        let e = EPSILON // local alias
        // get coordinates of bounding boxes' starting points
        let (b1x, b1y, b1z), (b2x, b2y, b2z) =
            Point.getCoord b1, Point.getCoord b2
        // get coordinates of higher bounds of the two bounding boxes
        let (b1x2, b1y2, b1z2), (b2x2, b2y2, b2z2) =
            Point.getCoord (Point.move b1 (Vector.make b1w b1h b1d)),
                Point.getCoord (Point.move b2 (Vector.make b2w b2h b2d))

        let (lx, ly, lz) = min b1x b2x, min b1y b2y, min b1z b2z
        let (hx, hy, hz) = max b1x2 b2x2, max b1y2 b2y2, max b1z2 b2z2
        let boundsP0 = Point.make (lx - e) (ly - e) (lz - e)
        let bounds = (boundsP0, abs (hx - lx) + e, abs (hy - ly) + e, abs (hz - lz) + e)
        Some bounds

/// <summary>
/// Get the bounds of a shape.
/// </summary>
/// <param name=s>The shape to get the bounds of.</param>
/// <returns>The bounds of the shape.</returns>
let getBounds shape =
    let e = EPSILON // local alias
    let rec getBounds' shape cont =
        match shape with
        | Plane(_, _, _)                   -> None
        | Box(low, high, _, _, _, _, _, _) ->
            let lx, ly, lz = Point.getCoord low
            let hx, hy, hz = Point.getCoord high
            let boundsP0 = Point.make (lx - e) (ly - e) (lz - e)
            let bounds = (boundsP0,
                          abs (hx - lx) + e,
                          abs (hy - ly) + e,
                          abs (hz - lz) + e)
            cont (Some(bounds))
        | Disc(c, r, _) ->
            let cx, cy, cz = Point.getCoord c
            let boundsP0 = Point.make (cx - r - e) (cy - r - e) (cz - e)
            let bounds = (boundsP0, r * 2. + e, r * 2. + e, e * 2.)
            cont (Some(bounds))
        | HollowCylinder(c,r,h,_) ->
            let cx, cy, cz = Point.getCoord c
            let boundsP0 = Point.make (cx - r - e) (cy - r - e) (cz - r - e)
            let bounds = (boundsP0, r * 2. + e, h + e, r * 2. + e)
            cont (Some(bounds))
        | Sphere(c, r, _)      ->
            let cx, cy, cz = Point.getCoord c
            let boundsP0 = Point.make (cx - r - e) (cy - r - e) (cz - r - e)
            let bounds = (boundsP0, r * 2. + e, r * 2. + e, r * 2. + e)
            cont (Some(bounds))
        | Triangle(a, b, c, _) ->
            let (ax, ay, az), (bx, by, bz), (cx, cy, cz) =
                Point.getCoord a, Point.getCoord b, Point.getCoord c

            let lx, ly, lz = min ax (min bx cx), min ay (min by cy), min az (min bz cz)
            let hx, hy, hz = max ax (max bx cx), max ay (max by cy), max az (max bz cz)
            let boundsP0 = Point.make (lx - e) (ly - e) (lz - e)
            let bounds = (boundsP0,
                          abs (hx - lx) + e, // width
                          abs (hy - ly) + e, // height
                          abs (hz - lz) + e) // depth
            cont (Some(bounds))
        | Composite(s1, s2, _) ->
            let bounds = getBounds' s1 (fun s1b ->
                            getBounds' s2 (fun s2b ->
                                cont (combineBounds (s1b, s2b))))
            bounds
    getBounds' shape id

/// <summary>
/// Make a plane with a point of origin (affects the texture mapping),
/// an upvector and a texture.
/// </summary>
/// <param name=p0>
/// The point of origin (affects the texture mapping of the plane).
/// </param>
/// <param name=up>The upvector of the plane.</param>
/// <param name=t>The texture of the plane.</param>
/// <returns>
/// A plane object facing in the direction of the upvector, with a
/// point of origin (for texture mapping) and a texture.
/// </returns>
let mkPlane p0 up texture =
    if Vector.magnitude up = 0.
    then failwith "Attempting to create a plane with a 0-vector as the normal"
    Plane(p0, up, texture)

/// <summary>
/// Make a disc with a center point, an upvector, a radius and a texture.
/// </summary>
/// <param name=p0>The center point of the disc.</param>
/// <param name=radius>The radius of the disc.</param>
/// <param name=t>The texture of the disc.</param>
/// <returns>
/// A disc object facing in the direction of the upvector, with a center
/// point, a radius and a texture.
/// </returns>
let mkDisc p0 radius t =
    if radius <= 0.
    then raise NonPositiveShapeSizeException
    Disc(p0, radius, t)

/// <summary>
/// Make a sphere with a point of origin, a radius and a texture.
/// </summary>
/// <param name=center>The center point of the sphere.</param>
/// <param name=radius>The radius of the sphere.</param>
/// <param name=t>The texture of the sphere.</param>
/// <returns>
/// A sphere object, with a point of origin, a radius and a texture.
/// </returns>
let mkSphere center radius texture =
    if radius <= 0. then raise NonPositiveShapeSizeException
    Sphere(center, radius, texture)

/// <summary>
/// Make a hollow cylinder with a center point of origin, a radius,
/// a height and a texture.
/// </summary>
/// <param name=center>
/// The center point at the bottom.
/// of the cylinder.
/// </param>
/// <param name=radius>The radius of the cylinder.</param>
/// <param name=height>
/// The height of the cylinder.
/// </param>
/// <param name=t>The texture of the cylinder.</param>
/// <returns>
/// A hollow cylinder object, with a center point of origin, a radius,
/// a height and a texture.
/// </returns>
let mkHollowCylinder center radius height texture =
    if radius <= 0. || height <= 0. then raise NonPositiveShapeSizeException
    HollowCylinder(center, radius, height, texture)

/// <summary>
/// Make a triangle with points, `a`, `b` and `c`.
/// </summary>
/// <param name=a>Point `a` in the triangle.</param>
/// <param name=b>Point `b` in the triangle.</param>
/// <param name=c>Point `c` in the triangle.</param>
/// <param name=m>The material of the triangle.</param>
/// <returns>
/// A triangle object, with points `a`, `b` and `c`, and a material.
/// </returns>
let mkTriangle a b c material =
    if a = b || a = c || b = c then raise NonPositiveShapeSizeException
    Triangle(a, b, c, Texture.make (fun x y -> material))

/// <summary>
/// Make a box with a low and high point and 6 textures, one for each side.
/// </summary>
/// <param name=low>The low point in the box.</param>
/// <param name=high>The high point in the box.</param>
/// <param name=front>The texture on the front side.</param>
/// <param name=back>The texture on the back side.</param>
/// <param name=top>The texture on the top side.</param>
/// <param name=bottom>The texture on the bottom side.</param>
/// <param name=left>The texture on the left side.</param>
/// <param name=right>The texture on the right side.</param>
/// <returns>
/// A box object, with a high and low point, and a texture for each side.
/// </returns>
let mkBox low high front back top bottom left right =
    if low = high
    then failwith "Can not create a box with low and high in the same point."
    Box(low, high, front, back, top, bottom, left, right)

/// <summary>
/// Make a union between `shape1` and `shape2`.
/// </summary>
/// <param name=shape1>`shape1` is the first shape in the union.</param>
/// <param name=shape2>`shape2` is the second shape in the union.</param>
/// <returns>
/// The composite union of the two shapes, acting as a single solid shape.
/// </returns>
let mkUnion shape1 shape2 = Composite(shape1, shape2, Union)

/// <summary>
/// Subtracts `shape2` from `shape2`.
/// </summary>
/// <param name=shape1>`shape1` is the shape to be subtracted from.</param>
/// <param name=shape2>`shape2` is the shape used for subtraction.</param>
/// <returns>
/// The composite subtraction of the two shapes, acting as a single solid shape.
/// </returns>
let mkSubtraction shape1 shape2 = Composite(shape1, shape2, Subtraction)

/// <summary>
/// Make an intersection between `shape1` and `shape2`.
/// </summary>
/// <param name=shape1>`shape1` is the first shape in the intersection.</param>
/// <param name=shape2>`shape1` is the second shape in the intersection.</param>
/// <returns>
/// The composite intersection of the two shapes, acting as the overlapping areas of the two shapes.
/// </returns>
let mkIntersection shape1 shape2 = Composite(shape1, shape2, Intersection)

/// Check if a ray hits a boundingbox.
/// </summary>
/// <param name=bbox>The boundingbox to check ray intersection with.</param>
/// <param name=rayO>
/// The origin of the ray to check boundingbox intersection with.
/// </param>
/// <param name=rayD>
/// The direction of the ray to check boundingbox intersection with.
/// </param>
/// <returns>
/// True, in the case that the ray hits the boundingbox (in the
/// positive direction), else false.
/// </returns>
let hitsBounds (p0, width, height, depth) rayO rayD =
    let lx, ly, lz = Point.getCoord p0
    let hx, hy, hz = Point.make (lx + width) (ly + height) (lz + depth)
                     |> Point.getCoord

    let ox, oy, oz = Point.getCoord rayO
    let dx, dy, dz = Vector.getCoord rayD
    let idx, idy, idz = 1. / dx, 1. / dy, 1. / dz // inverse ray direction

    let txmin, txmax =
        if idx < 0.
        then ((hx - ox) * idx), ((lx - ox) * idx)
        else ((lx - ox) * idx), ((hx - ox) * idx)
    let tymin, tymax =
        if idy < 0.
        then ((hy - oy) * idy), ((ly - oy) * idy)
        else ((ly - oy) * idy), ((hy - oy) * idy)

    if txmin > tymax || tymin > txmax then false else

    let tmin = if tymin > txmin then tymin else txmin
    let tmax = if tymax < txmax then tymax else txmax

    let tzmin, tzmax =
        if idz < 0.
        then ((hz - oz) * idz), ((lz - oz) * idz)
        else ((lz - oz) * idz), ((hz - oz) * idz)

    if tmin > tzmax || tzmin > tmax then false else

    let tmin = if tzmin > tmin then tzmin else tmin
    let tmax = if tzmax < tmax then tzmax else tmax

    tmin > 0. || tmax > 0.

/// <summary>
/// Calculates the hit distances between a ray and a shape created from
/// a polynomial expression, using values a, b and c from the expression.
/// </summary>
/// <param name="a">Value of `a` in a polynomial expression.</param>
/// <param name="b">Value of `b` in a polynomial expression.</param>
/// <param name="c">Value of `c` in a polynomial expression.</param>
/// <returns>
/// The list of 0 to 2 hit distances from the ray origin to the hitpoints
/// on the shape. Only returns hitpoints that intersect in the positive
/// direction of the ray.
/// </returns>
let distance a b c =
    let D = b**2. - 4.*a*c // discriminant
    match D with
    | D when D < -EPSILON -> []
    | D when D < EPSILON  -> let d = -(b / (2.*a))
                             if d > 0. then [d] else []
    | _ -> let dneg = (-b - sqrt(D)) / (2.*a)
           let dpos = (-b + sqrt(D)) / (2.*a)
           match (dneg > 0., dpos > 0.) with
           | (true, true)   -> [dneg;dpos]
           | (true, false)  -> [dneg]
           | (false, true)  -> [dpos]
           | (false, false) -> []

/// <summary>
/// Function for sorting a hitlist into a list of triples, sorted by distance to origin of ray.
/// </summary>
/// <param name="shape1"> `shape1` is the first shape used in the composite hit.</param>
/// <param name="shape2"> `shape2` is the second shape used in the composite hit.</param>
/// <param name="hits1"> `hits1` is a list of the hitpoints on shape1.</param>
/// <param name="hits2"> `hits1` is a list of the hitpoints on shape2.</param>
/// <returns>
/// A list of triples of the type (id,shape,hit), sorted by the hits distance to the origin of the ray.
/// </returns>
let sortToTuples shape1 shape2 hits1 hits2 =
    let shape1list = List.map (fun x -> (1,shape1,x)) hits1
    let shape2list = List.map (fun x -> (2,shape2,x)) hits2
    let tupleList = shape1list @ shape2list
    List.sortWith (fun (_,_,h1) (_,_,h2) ->
        let d1 = getHitDistance h1
        let d2 = getHitDistance h2
        if d1 > d2 then 1 elif d1 < d2 then -1 else 0) tupleList

/// <summary>
/// Function for testing if a hits normal vector is orthogonal on a ray.
/// </summary>
/// <param name="ray">`ray` is the ray responsible for the hit.</param>
/// <param name="hitNormalVector">`hitNormalVector` is the normal Vector of the hit.</param>
/// <returns>
/// A boolean value which is true if the normal vector is orthogonal, or false if it isn't.
/// </returns>
let isOrthogonal ray hitNormalVector =
    let rayVector = Ray.getVector ray
    let dp = Vector.dotProduct rayVector hitNormalVector
    dp < EPSILON && dp > -EPSILON

/// <summary>
/// A function for determining the "exit hit" in a union composite.
/// An exit hit is defined as the hit where the ray "leaves" the solid body of a shape.
/// </summary>
/// <param name="hitTupleList"> A list containing triples of the type (int,shape,hit) </param>
/// <returns>
/// A tuple containing a trimmed hitTupleList and the "exit hit".
/// </returns>
let rec findExitHit hitTupleList =
    match hitTupleList with
    | (id1,_,h) :: (id2,_,_) :: (id3,_,_) :: hitTupleList when id2 = id3 -> (hitTupleList,h)
    | (id1,_,_) :: (id2,s2,h2) :: hitTupleList when id1 <> id2 -> findExitHit ((id2,s2,h2) :: hitTupleList)
    | (id,s,h) :: hitTupleList -> (hitTupleList,h)
    | [] -> failwith "No hits in tuple list."

/// <summary>
/// Creates a hitpoint on a sphere, given a distance, a ray direction
/// a ray origin, the radius of the sphere and the texture.
/// </summary>
/// <param name=d>The distance between rayO and the sphere's surface.</param>
/// <param name=rayV>The direction of the ray (vector).</param>
/// <param name=rayO>The origin of the ray.</param>
/// <param name=r>The radius of the sphere.</param>
/// <param name=t>The texture for the sphere.</param>
/// <returns>
/// A hitpoint for the sphere, with a distance, the inverse vector
/// (going outwards from the sphere surface) and the material for the
/// hitpoint.
/// </returns>
let sphereDeterminer d center rayV rayO t =
    // hitpoint on sphere
    let hitPoint = Point.move rayO (Vector.multScalar rayV d)

    // normalised vector from hitpoint towards center
    let n = Vector.normalise <| Point.distance center hitPoint
    let (nx, ny, nz) = Vector.getCoord n

    let theta = acos ny      // angle in y-space
    let phi' = atan2 nx nz
    let phi = if phi' < 0.   // angle in x- and z-space
              then phi' + 2. * Math.PI
              else phi'

    let u = phi / (2. * Math.PI)   // u coordinate in texture space
    let v = 1. - (theta / Math.PI) // v coordinate in texture space

    let material = Texture.getMaterial u v t // material to return

    Hit(d, n, material)

/// <summary>
/// Determines whether a hitpoint is within the disc area and returns the Hit if it is,
/// or nothing if it does not.
/// </summary>
/// <param name=d>The distance from the ray origin point to the hit point</params>
/// <param name=hit>The hitpoint on the disc plane in question.</params>
/// <param name=normal>The normal of the disc.</params>
/// <param name=r>The radius of the disc.</params>
/// <param name=p0>The center point of the disc.</params>
/// <param name=t>The texture of the disc.</params>
/// <returns>
/// A hitpoint list, which either contains a single hitpoint or nothing.
/// </returns>
let discDeterminer d hit normal r p0 t  =
    let xCoord = Point.getX hit - Point.getX p0
    let yCoord = Point.getY hit - Point.getY p0

    if ((xCoord**2.) + (yCoord**2.)) <= r**2.
    then
        let u = (xCoord + r) / (2. * r)
        let v = (yCoord + r) / (2. * r)
        let material = Texture.getMaterial u v t
        [Hit(d, normal, material)]
    else
        List.empty

let rayPlaneHit rayO rayD p0 normal =
    let rdn = rayD * normal // ray and normal dotproduct
    // If the ray and normals' dotproducts are too close, we do not
    // want to render the hit. We render both sides of the plane.
    if rdn < EPSILON && rdn > -EPSILON then None else

    let t = ((Point.distance rayO p0) * normal) / rdn
    // The hit is behind the ray origin
    if t < 0. then None else

    let hitpoint = Point.move rayO (Vector.multScalar rayD t)
    Some((t, hitpoint))

/// <summary>
/// Creates a hitpoint on a cylinder, given a distance, a ray direction and
/// a ray origin.
/// </summary>
/// <param name=d>The distance between rayO and the cylinders's surface.</param>
/// <param name=center>The center point of the cylinder.</param>
/// <param name=r>The radius of the cylinder.</param>
/// <param name=h>The height of the cylinder.</param>
/// <param name=rayV>The direction of the ray (vector).</param>
/// <param name=rayO>The origin of the ray.</param>
/// <param name=t>The texture for the cylinder.</param>
/// <returns>
/// A hitpoint for the cylinder, with a distance, the inverse vector
/// (going outwards from the cylinder surface) and the material for the
/// hitpoint.
/// </returns>
let cylinderDeterminer d center r h rayV rayO texture=
    let hitPoint = Point.move rayO (Vector.multScalar rayV d)

    let y = Point.getY hitPoint
    let circleCenter = Point.move center (Vector.make 0. y 0.)
    let normal = Vector.make ((Point.getX hitPoint) / r) 0. ((Point.getZ hitPoint) / r)

    let phi' = atan2 (Vector.getX normal) (Vector.getZ normal)
    let phi =  if phi' < 0.
                then phi' + (2. * Math.PI)
                else phi'

    let u = phi / (2. * Math.PI)
    let v = ((Point.getY hitPoint) / h) + 0.5

    let material = Texture.getMaterial u v texture

    Hit(d, normal, material)

/// <summary>
/// A function for checking whether a specific hitpoint was on a non-solid shape.
/// </summary>
/// <remark> This is a helper function for the shapeNonSolid function, it's calculations only necessary if a union Composite is hit.
/// <param name="point"> The point in which the hit was recorded. </param>
/// <param name="shape"> The shape hit by the ray. </param>
/// <param name="c"> The continuation. The id function at initial function call. </param>
/// <returns>
/// A boolean if the shape was non-solid, or a non-solid shape in another composite.
/// </returns>
let rec hitInNonSolid point shape c =
    match shape with
    | Plane(origin,normal,_) ->
        //Check if hitpoint is in shape.
        let pointVec = Point.distance point origin
        let dotP = (Vector.dotProduct pointVec normal)
        if dotP < EPSILON && dotP > -EPSILON
        then true else false
    | Triangle(a,b,c,_) ->
        //Calculated using Barycentric coordinates, see http://math.stackexchange.com/questions/4322/check-whether-a-point-is-within-a-3d-triangle
        let area = Vector.magnitude (Vector.crossProduct (Point.distance a b) (Point.distance a c))
        let alpha = (Vector.magnitude (Vector.crossProduct (Point.distance point b) (Point.distance point c)))/area
        if alpha < -EPSILON || alpha > (1.+EPSILON) then false
        else
            let beta = (Vector.magnitude (Vector.crossProduct (Point.distance point c) (Point.distance point a)))/area
            if beta < -EPSILON || beta > (1.+EPSILON) then false
            else
                let gamma = 1. - alpha - beta
                if gamma < -EPSILON || gamma > (1.+EPSILON) then false
                else true
    | Composite (shape1,shape2,_) ->
        hitInNonSolid point shape1 (fun s1 ->
            hitInNonSolid point shape2 (fun s2 ->
                c (s1 || s2)))
    | _ -> false

/// <summary>
/// A function for checking whether a specific hitpoint was on a non-solid shape.
/// </summary>
/// <param name="ray"> The ray responsible for the hit. </param>
/// <param name="hit"> The specific hit. </param>
/// <param name="shape"> The shape hit by the ray. </param>
/// <returns>
/// A boolean if the shape was one non-solid, or a non-solid shape in another composite.
/// </returns>
let rec shapeNonSolid ray hit shape c =
    match shape with
    | Plane(_,_,_) -> true
    | Triangle(_,_,_,_) -> true
    // If one of the shapes in an Intersection is 1 dimensional, both are.
    | Composite(shape1,shape2,Intersection) ->
        shapeNonSolid ray hit shape1 (fun s1 ->
            shapeNonSolid ray hit shape2 (fun s2 ->
                c (s1 || s2)))
    | Composite(shape1,shape2,Union) ->
        let rayV  = Vector.multScalar (Ray.getVector ray) (getHitDistance hit)
        let point = Point.move (Ray.getOrigin ray) rayV
        hitInNonSolid point shape id
    // The difference between two shapes will always have the same solidity as shape 1
    | Composite(shape1,shape2,Subtraction) ->
        shapeNonSolid ray hit shape1 c
    | _ -> false

/// <summary>
/// Hitfunction specific to the Union composite.
/// </summary>
/// <param name="ray"> The ray to check for hits. </param>
/// <param name="hitTupleList"> A list containing triples of the type (id,shape,hit) </param>
/// <param name="hitList"> An empty list, acting as accumulator, to which the hits of the union are added. </param>
/// <returns>
/// A list of hitpoints.
/// </returns>
let rec unionHitFunction ray hitTupleList hitList =
    match hitTupleList with
    | (_,s,h) :: hitTupleList when isOrthogonal ray (getHitNormal h) || shapeNonSolid ray h s id ->
        unionHitFunction ray hitTupleList (h :: hitList)
    | (id1,s1,h1) :: (id2,s2,h2) :: hitTupleList when id1 = id2 ->
        unionHitFunction ray hitTupleList (h1 :: h2 :: hitList)
    | (id1,s1,h1) :: (id2,s2,h2) :: hitTupleList when id1 <> id2 ->
        let exitTuple = findExitHit hitTupleList
        unionHitFunction ray (fst exitTuple) (h1 :: (snd exitTuple) :: hitList)
    | (id,s,h) :: hitTupleList ->
        unionHitFunction ray hitTupleList (h :: hitList)
    | [] -> hitList

/// <summary>
/// Hitfunction specific to the Intersection composite.
/// </summary>
/// <param name="ray"> The ray to check for hits. </param>
/// <param name="hitTupleList"> A list containing triples of the type (id,shape,hit) </param>
/// <param name="hitList"> An empty list, acting as accumulator, to which the hits of the union are added. </param>
/// <param name="inside"> Parameter determining whether the recursion is currently inside another shape or not. </param>
/// <returns>
/// A list of hitpoints.
/// </returns>
let rec intersectionHitFunction ray hitTupleList hitList inside =
    match hitTupleList with
    | (_,s,h) :: hitTupleList when isOrthogonal ray (getHitNormal h) || shapeNonSolid ray h s id ->
        if inside then intersectionHitFunction ray hitTupleList (h :: hitList) true
        else intersectionHitFunction ray hitTupleList hitList false
    | (id1,s1,h1) :: (id2,s2,h2) :: hitTupleList when id1 = id2 ->
        if inside then intersectionHitFunction ray hitTupleList (h1 :: h2 :: hitList) true
        else intersectionHitFunction ray hitTupleList hitList false
    | (id1,s1,h1) :: (id2,s2,h2) :: hitTupleList when id1 <> id2 ->
        if inside then intersectionHitFunction ray hitTupleList (h1 :: hitList) (shapeNonSolid ray h2 s2 id)
        else intersectionHitFunction ray hitTupleList (h2 :: hitList) (not (shapeNonSolid ray h2 s2 id))
    | (id,s,h) :: hitTupleList ->
        hitList
    | [] ->
        hitList

/// <summary>
/// Hitfunction specific to the Subtraction composite.
/// </summary>
/// <param name="ray"> The ray to check for hits.</param>
/// <param name="hitTupleList"> A list containing triples of the type (id,shape,hit).</param>
/// <param name="hitList"> An empty list, acting as accumulator, to which the hits of the union are added.</param>
/// <returns>
/// A list of hitpoints.
/// </returns>
let rec subtractionHitFunction ray hitTupleList hitList inSubtraction =
    match hitTupleList with
    | (id1,s,h) :: hitTupleList when isOrthogonal ray (getHitNormal h) || shapeNonSolid ray h s id ->
        if not inSubtraction && id1 = 1
        then subtractionHitFunction ray hitTupleList (h :: hitList) inSubtraction
        else subtractionHitFunction ray hitTupleList hitList inSubtraction
    | (id1,s1,h1) :: (id2,s2,h2) :: hitTupleList when id1 = id2 ->
        if not inSubtraction
        then subtractionHitFunction ray hitTupleList (h1 :: h2 :: hitList) false
        else subtractionHitFunction ray hitTupleList hitList inSubtraction
    | (id1,s1,h1) :: (id2,s2,h2) :: hitTupleList when id1 <> id2 ->
        match inSubtraction with
        | true when id1 = 1 -> subtractionHitFunction ray hitTupleList hitList false
        | true when id1 = 2 -> subtractionHitFunction ray hitTupleList (h2 :: hitList) false
        | false when id1 = 1 -> subtractionHitFunction ray hitTupleList (h2 :: h1 :: hitList) (not (isOrthogonal ray (getHitNormal h2) || shapeNonSolid ray h2 s2 id))
        | false when id1 = 2 -> subtractionHitFunction ray hitTupleList hitList true
    | (id,s,h) :: hitTupleList ->
        if inSubtraction
        then hitList
        else (h :: hitList)
    | [] -> hitList

/// <summary>
/// Double Equal. Checks if two doubles are "equal" to
/// eachother with a precision of EPSILON.
/// </summary>
/// <param name=a>The first double to compare.</param>
/// <param name=b>The second double to compare.</param>
/// <returns>
/// If the two doubles are (very close to being) equal to eachother.
/// </returns>
let deq a b =
    abs (a - b) < EPSILON

/// <summary>
/// Takes the low and high point, a hitpoint, a hitnormal at the point and the texture from a box to return the texture material at the specific point.
/// </summary>
/// <param name=low>The low point of the box.</params>
/// <param name=high>The high point of the box.</params>
/// <param name=hit>The hitpoint, from which the texture should be retrieved.</params>
/// <param name=hn>The hitnormal at the hitpoint.</params>
/// <param name=t>The texture on the relevant side of the hitpoint.</params>
/// <returns>
/// The texture material at the hitpoint.
/// </returns>
let boxTexturer low high hit hn t =
    let (lx, ly, lz) = Point.getCoord low
    let (hx, hy, hz) = Point.getCoord high
    match Vector.getCoord hn with
    | (x, _, _) when x = -1. ->
                                let boxHeight = abs(ly - hy)
                                let boxDepth = abs(lz - hz)
                                let u = 1.0 - (((Point.getZ hit) - lz) / boxDepth)
                                let v = (((Point.getY hit) - ly) / boxHeight)
                                Texture.getMaterial u v t
    | (_, y, _) when y = -1. ->
                                let boxWidth = abs(lx - hx)
                                let boxDepth = abs(lz - hz)
                                let u = 1.0 - (((Point.getX hit) - lx) / boxWidth)
                                let v = (((Point.getZ hit) - lz) / boxDepth)
                                Texture.getMaterial u v t
    | (_, _, z) when z = -1. ->
                                let boxWidth = abs(lx - hx)
                                let boxHeight = abs(ly - hy)
                                let u = (((Point.getX hit) - lx) / boxWidth)
                                let v = (((Point.getY hit) - ly) / boxHeight)
                                Texture.getMaterial u v t
    | (x, _, _) when x = 1. ->
                                let boxHeight = abs(ly - hy)
                                let boxDepth = abs(lz - hz)
                                let u = ((Point.getZ hit) - lz) / boxDepth
                                let v = ((Point.getY hit) - ly) / boxHeight
                                Texture.getMaterial u v t
    | (_, y, _) when y = 1. ->
                                let boxWidth = abs(lx - hx)
                                let boxDepth = abs(lz - hz)
                                let u = ((Point.getX hit) - lx) / boxWidth
                                let v = ((Point.getZ hit) - lz) / boxDepth
                                Texture.getMaterial u v t
    | (_, _, z) when z = 1. ->
                                let boxWidth = abs(lx - hx)
                                let boxHeight = abs(ly - hy)
                                let u = 1.0 - (((Point.getX hit) - lx) / boxWidth)
                                let v = ((Point.getY hit) - ly) / boxHeight
                                Texture.getMaterial u v t


/// <summary>
/// Shoot a ray, and check if it hits the specificed shape.
/// Returns a hitpoint for each point on the shape that
/// was hit, as a list.
/// </summary>
/// <param name="ray">The ray.</param>
/// <param name="shape">The shape.</param>
/// <returns>The list of hitpoints with the ray, on the shape.</returns>
let rec hitFunction ray shape =
    let rayVector = Ray.getVector ray
    let rayOrigin = Ray.getOrigin ray
    let bounds = getBounds shape
    let withinBounds = match bounds with
                       | None     -> true
                       | Some(bb) -> hitsBounds bb rayOrigin rayVector

    if not withinBounds then List.empty else
    match shape with
    | Plane(p0, normal, texture) ->
        // get hit point and its coordinate on the infinite plane
        // TODO: we do not take the y-axis into account (maybe fix later)
        let rph = rayPlaneHit rayOrigin rayVector p0 normal
        if rph.IsNone then List.empty else
        let t, hitpoint = rph.Value

        let (hpx, _, hpz) = Point.getCoord hitpoint

        let u = abs(hpx) % 1.0
        let v = abs(hpz) % 1.0

        let u = if hpx < 0. then 1. - u else u
        let v = if hpz < 0. then 1. - v else v

        // if we hit the backside, return the inverse normal
        let normal =
            if Vector.dotProduct rayVector normal > 0.
            then -normal else normal

        // gets material for the hit point
        let material = Texture.getMaterial u v texture
        [Hit(t, normal, material)]
    | Disc(p0, radius, texture) ->
        let up = Vector.make 0. 0. 1.
        let rph = rayPlaneHit rayOrigin rayVector p0 up
        if rph.IsNone then List.empty else
        let t, hitpoint = rph.Value

        // if we hit the backside, return the inverse normal
        let up =
            if Vector.dotProduct rayVector up > 0.
            then -up else up

        discDeterminer t hitpoint up radius p0 texture
    | Sphere(center, radius, texture) ->
        let (dx, dy, dz) = Vector.getCoord rayVector
        let (ox, oy, oz) = Point.getCoord rayOrigin
        let (x, y, z) = Point.getCoord center
        let a = dx**2. + dy**2. + dz**2.
        let b = 2. * ((ox - x) * dx + (oy - y) * dy + (oz - z) * dz)
        let c = (ox - x)**2. + (oy - y)**2. + (oz - z)**2. - radius**2.
        let distances = distance a b c
        match distances with
        | []         -> List.empty
        | [hp]       -> [sphereDeterminer hp center rayVector rayOrigin texture]
        | [hp1; hp2] -> [sphereDeterminer hp1 center rayVector rayOrigin texture;
                         sphereDeterminer hp2 center rayVector rayOrigin texture]
        | _          -> failwith "Error: Hitting a sphere more than two times!"
    | HollowCylinder(center, radius, height, texture) ->
        // unit cylinder along the y-axis: x^2+z^2 = 1
        let (dx, dy, dz) = Vector.getCoord rayVector
        let (ox, oy, oz) = Point.getCoord rayOrigin
        let (x, y, z) = Point.getCoord center

        let a = dx**2. + dz**2.
        let b = 2. * ((ox - x) * dx + (oz - z) * dz)
        let c = (ox - x)**2. + (oz - z)**2. - radius**2.
        let distances = distance a b c
        let constrainedDistances = List.filter (fun dist ->
            let hitPoint = Point.move rayOrigin (Vector.multScalar rayVector dist)
            let y = Point.getY hitPoint
            y < (height + EPSILON) && y > -EPSILON) distances

        match constrainedDistances with
        | []         -> List.empty
        | [d]        -> [cylinderDeterminer d center radius height rayVector rayOrigin texture]
        | [d1; d2]   -> [cylinderDeterminer d1 center radius height rayVector rayOrigin texture;
                         cylinderDeterminer d2 center radius height rayVector rayOrigin texture]
        | _          -> failwith "Error: Hitting a sphere more than two times!"

    | Triangle(a, b, c, t) ->
        let material =  Texture.getMaterial 0. 0. t

        // Möller-Trumbore intersection algorithm
        let e1 = Point.distance a b
        let e2 = Point.distance a c
        let P = Vector.crossProduct rayVector e2
        let det = Vector.dotProduct e1 P

        if det > -EPSILON && det < EPSILON then List.empty else
        let invDet = 1. / det

        let T = Point.distance a rayOrigin

        let u = (Vector.dotProduct T P) * invDet
        if u < 0. || u > 1. then List.empty else

        let Q = Vector.crossProduct T e1

        let v = (Vector.dotProduct rayVector Q) * invDet
        if v < 0. || (u + v) > 1. then List.empty else

        let uvcp = Vector.crossProduct e1 e2
        let n = Vector.multScalar uvcp (1. / (Vector.magnitude (uvcp)))

        let t = (Vector.dotProduct e2 Q) * invDet
        if t > EPSILON
        then [Hit(t, n, material)]
        else List.empty
    | Composite(shape1, shape2, composition) ->
        let hitTupleList = sortToTuples shape1 shape2 (hitFunction ray shape1) (hitFunction ray shape2)
        match composition with
        | Union ->
            unionHitFunction ray hitTupleList []
        | Subtraction ->
            subtractionHitFunction ray hitTupleList [] false
        | Intersection ->
            intersectionHitFunction ray hitTupleList [] false
    | Box(low, high, front, back, top, bottom, left, right) ->
        let (lx, ly, lz) = Point.getCoord low
        let (hx, hy, hz) = Point.getCoord high

        let (ox, oy, oz) = Point.getCoord rayOrigin
        let (dx, dy, dz) = Vector.getCoord rayVector
        let (idx, idy, idz) = 1. / dx, 1. / dy, 1. / dz // inverse ray direction

        let (txmin, txmax) =
            if idx < 0.
            then ((hx - ox) * idx), ((lx - ox) * idx)
            else ((lx - ox) * idx), ((hx - ox) * idx)
        let (tymin, tymax) =
            if idy < 0.
            then ((hy - oy) * idy), ((ly - oy) * idy)
            else ((ly - oy) * idy), ((hy - oy) * idy)

        if txmin > tymax || tymin > txmax then List.empty else

        let tmin = if tymin > txmin then tymin else txmin
        let tmax = if tymax < txmax then tymax else txmax

        let (tzmin, tzmax) =
            if idz < 0.
            then ((hz - oz) * idz), ((lz - oz) * idz)
            else ((lz - oz) * idz), ((hz - oz) * idz)

        if tmin > tzmax || tzmin > tmax then List.empty else

        let tmin = if tzmin > tmin then tzmin else tmin
        let tminHit = Point.move rayOrigin (Vector.multScalar rayVector tmin)
        let tmax = if tzmax < tmax then tzmax else tmax
        let tmaxHit = Point.move rayOrigin (Vector.multScalar rayVector tmax)

        let sideHit hitpoint =
            match Point.getCoord hitpoint with
            | (x, _, _) when deq x lx -> (Vector.make -1. 0. 0., left)
            | (_, y, _) when deq y ly -> (Vector.make 0. -1. 0., bottom)
            | (_, _, z) when deq z lz -> (Vector.make 0. 0. -1., front)
            | (x, _, _) when deq x hx -> (Vector.make 1. 0. 0., right)
            | (_, y, _) when deq y hy -> (Vector.make 0. 1. 0., top)
            | (_, _, z) when deq z hz -> (Vector.make 0. 0. 1., back)
            | _ -> failwith "Paradox: Hit a box side, without hitting it"

        let (tminHitNormal, tminTexture) = sideHit tminHit
        let (tmaxHitNormal, tmaxTexture) = sideHit tmaxHit

        let tminHitMaterial = boxTexturer low high tminHit tminHitNormal tminTexture
        let tmaxHitMaterial = boxTexturer low high tmaxHit tmaxHitNormal tmaxTexture

        match (tmin > 0., tmax > 0.) with
        | (true, true)   -> [Hit(tmin, tminHitNormal, tminHitMaterial);
                             Hit(tmax, tmaxHitNormal, tmaxHitMaterial)]
        | (true, false)  -> [Hit(tmin, tminHitNormal, tminHitMaterial)]
        | (false, true)  -> [Hit(tmax, tmaxHitNormal, tmaxHitMaterial)]
        | (false, false) -> []
    | _ ->
        failwith "No hit function for this shape"
