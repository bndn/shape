/// Copyright (C) 2015 The Authors.
module Shape

open System // for Math

open Material
open Point
open Ray
open Texture
open Vector

[<Literal>]
let EPSILON = 1.0e-6

type Shape =
    | Plane of Point * Vector * Texture
    | Sphere of Point * float * Texture
    | HollowCylinder of Point * float * float * Texture
    | Triangle of Point * Point * Point * Texture

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
/// The center point at the bottom (or top, if the height is negative)
/// of the cylinder.
/// </param>
/// <param name=radius>The radius of the cylinder.</param>
/// <param name=height>
/// The height of the cylinder. Can be negative, which will make
/// the cylinder grow in the negative direction of the y-axis.
/// </param>
/// <param name=t>The texture of the cylinder.</param>
/// <returns>
/// A hollow cylinder object, with a center point of origin, a radius,
/// a height and a texture.
/// </returns>
let mkHollowCylinder center radius height texture =
    if radius <= 0. || height = 0. then raise NonPositiveShapeSizeException
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
/// Shoot a ray, and check if it hits the specificed shape.
/// Returns a hitpoint for each point on the shape that
/// was hit, as a list.
/// </summary>
/// <param name=r>The ray.</param>
/// <param name=s>The shape.</param>
/// <returns>The list of hitpoints with the ray, on the shape.</returns>
let hitFunction ray shape =
    let rayVector = Ray.getVector ray
    let rayOrigin = Ray.getOrigin ray
    match shape with
    | Plane(p0, normal, texture) ->
        let rdn = rayVector * normal // ray and normal dotproduct
        // If the ray and normals' dotproducts are too close, we do not
        // want to render the hit. We render both sides of the plane.
        if rdn < EPSILON && rdn > -EPSILON then List.empty else

        // hit distance traveled
        let t = ((Point.distance rayOrigin p0) * normal) / rdn
        // The hit is behind the camera
        if t < 0. then List.empty else

        // get hit point and its coordinate on the infinite plane
        // TODO: we do not take the y-axis into account (maybe fix later)
        let hitpoint = Point.move rayOrigin (Vector.multScalar rayVector t)
        let (hpx, _, hpz) = Point.getCoord hitpoint

        let u = abs(hpx % 1.0)
        let v = abs(hpz % 1.0)

        // gets material for the hit point
        let material = Texture.getMaterial u v texture
        [Hit(t, normal, material)]
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
    | _ ->
        failwith "No hit function for this shape"
