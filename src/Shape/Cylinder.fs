// Copyright (C) 2015 The Authors.
module Shape.Cylinder

open Point
open System // for Math
open Texture
open Vector
open Utils

[<NoComparison>]
type Cylinder =
    | HollowCylinder of Point * float * float * Texture
    | SolidCylinder of Point * float * float * Texture * Texture * Texture

/// <summary>
/// Get the fields of a cylinder (center, radius and height).
/// </summary>
/// <param name=cylinder>Cylinder to get fields of.</param>
/// <returns>The fields of cylinder as a triplet.</returns>
let cylinderFields = function
    | HollowCylinder(c, r, h, t)
    | SolidCylinder(c, r, h, t, _, _) -> c, r, h, t

/// <summary>
/// Creates a hitpoint on a cylinder, given a distance, a ray direction and
/// a ray origin.
/// </summary>
/// <param name=d>The distance between rayO and the cylinders's surface.</param>
/// <param name=center>The center point of the cylinder.</param>
/// <param name=r>The radius of the cylinder.</param>
/// <param name=h>The height of the cylinder.</param>
/// <param name=rayD>The direction of the ray (vector).</param>
/// <param name=rayO>The origin of the ray.</param>
/// <param name=t>The texture for the cylinder.</param>
/// <returns>
/// A hitpoint for the cylinder, with a distance, the inverse vector
/// (going outwards from the cylinder surface) and the material
/// for the hitpoint.
/// </returns>
let cylinderDeterminer d center h rayD rayO t =
    let hitPoint = Point.move rayO (Vector.multScalar rayD d)

    let cx, cy, cz = Point.getCoord center
    let hy = Point.getY hitPoint
    let normal = Point.direction (Point.make cx hy cz) hitPoint

    // if we hit the backside, return the inverse normal
    let normal = if Vector.dotProduct rayD normal > 0.
                 then -normal else normal

    let phi' = atan2 (Vector.getX normal) (Vector.getZ normal)
    let phi = if phi' < 0.
              then phi' + (2. * Math.PI)
              else phi'

    let u = phi / (2. * Math.PI)
    let v = (hy - cy) / h

    Hit(d, normal, Texture.getMaterial u v t)

/// <summary>
/// Compute an intersection between a ray and a hollow cylinder.
/// </summary>
/// <param name=cylinder>Cylinder to check for intersections.</param>
/// <param name=r>Ray to check for intersections.</param>
/// <returns>
/// The list of 0 to 2 hit distances from the ray origin to the hitpoints
/// on the shape. Only returns hitpoints that intersect in the positive
/// distance of the ray.
/// </returns>
let hollowCylIntersect cylinder r =
    // unit cylinder along the y-axis: x^2+z^2 = 1
    let center, radius, h, t = cylinderFields cylinder
    let rayO, rayD = Ray.getOrigin r, Ray.getVector r
    let dx, _, dz = Vector.getCoord rayD
    let ox, _, oz = Point.getCoord rayO
    let x, y, z = Point.getCoord center

    let a = dx**2. + dz**2.
    let b = 2. * ((ox - x) * dx + (oz - z) * dz)
    let c = (ox - x)**2. + (oz - z)**2. - radius**2.
    let distances = distanceQuadratic a b c
    let constrainedDistances =
        List.filter (fun dist ->
            let hitPoint = Point.move rayO (Vector.multScalar rayD dist)
            let hy = Point.getY hitPoint
            (y <= hy && hy <= h + y)
        ) distances

    match constrainedDistances with
    | []       -> List.empty
    | [d]      -> [cylinderDeterminer d center h rayD rayO t]
    | [d1; d2] -> [cylinderDeterminer d1 center h rayD rayO t;
                   cylinderDeterminer d2 center h rayD rayO t]
    | _        -> failwith "Hitting a cylinder more than twice"

/// <summary>
/// Compute an intersection between a ray and a solid cylinder.
/// </summary>
/// <param name=cylinder>Cylinder to check for intersections.</param>
/// <param name=r>Ray to check for intersections.</param>
/// <returns>
/// The list of 0 to 2 hit distances from the ray origin to the hitpoints
/// on the shape. Only returns hitpoints that intersect in the positive
/// distance of the ray.
/// </returns>
let solidCylIntersect (c, radius, h, t, top, bot) r =
    let rayO, rayD = Ray.getOrigin r, Ray.getVector r
    let cylinderHits = hollowCylIntersect (HollowCylinder(c, radius, h, t)) r

    let up = Vector.make 0. 1. 0.
    let topCenter = Point.move c (Vector.make 0. h 0.)

    let rphTop = rayPlaneHit rayO rayD topCenter up
    let rphBot = rayPlaneHit rayO rayD c up

    let xzDiscDeterminer d hit normal r p0 dt =
        let xCoord = Point.getX hit - Point.getX p0
        let zCoord = Point.getZ hit - Point.getZ p0

        if xCoord**2. + zCoord**2. <= r**2. then
            let u = (xCoord + r) / (2. * r)
            let v = (zCoord + r) / (2. * r)
            // if we hit the backside, return the inverse normal
            let normal = if Vector.dotProduct rayD normal > 0.
                         then -normal else normal
            Some (Hit(d, normal, Texture.getMaterial u v dt))
        else None

    let discHits =
        if rphTop.IsSome then
            let t, hitPoint = rphTop.Value
            let discHit = xzDiscDeterminer t hitPoint up radius topCenter top
            if discHit.IsSome then [discHit.Value] else []
        else []
    let discHits =
        if rphBot.IsSome then
            let t, hitPoint = rphBot.Value
            let discHit = xzDiscDeterminer t hitPoint up radius c bot
            if discHit.IsSome then discHit.Value :: discHits else discHits
        else discHits

    discHits @ cylinderHits

/// <summary>
/// Compute an intersection between a ray and a cylinder.
/// </summary>
/// <param name=cylinder>Cylinder to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// The list of 0 to 2 hit distances from the ray origin to the hitpoints
/// on the shape. Only returns hitpoints that intersect in the positive
/// distance of the ray.
/// </returns>
let intersect cylinder r =
    match cylinder with
    | HollowCylinder(_, _, _, _) -> hollowCylIntersect cylinder r
    | SolidCylinder(c, radius, h, t, top, bot) ->
        solidCylIntersect (c, radius, h, t, top, bot) r

/// <summary>
/// Get the bounds of a cylinder.
/// </summary>
/// <param name=cylinder>Cylinder to get bounds of.</param>
/// <returns>The bounds of a cylinder.</returns>
let bounds cylinder =
    let c, r, h, _ = cylinderFields cylinder

    let e = Epsilon // alias
    let ee = e * 2.
    let cx, cy, cz = Point.getCoord c
    let boundsP0 = Point.make (cx - r - e) (cy - e) (cz - r - e)

    Some (
        boundsP0,
        r * 2. + ee,
        h + ee,
        r * 2. + ee
    )

/// <summary>
/// Create a hollow cylinder shape.
/// </summary>
/// <param name=c>Center of the cylinder.</param>
/// <param name=w>Width of the cylinder.</param>
/// <param name=h>Height of the cylinder.</param>
/// <param name=t>Texture of the cylinder.</param>
/// <returns>A hollow cylinder shape.</returns>
let makeHollow c r h t =
    if r <= 0. || h <= 0.
    then failwith "Nonpositive Cylinder size"
    let cylinder = HollowCylinder(c, r, h, t)
    Shape.make (intersect cylinder) false (bounds cylinder)

/// <summary>
/// Create a solid cylinder shape.
/// </summary>
/// <param name=c>Center of the cylinder.</param>
/// <param name=w>Width of the cylinder.</param>
/// <param name=h>Height of the cylinder.</param>
/// <param name=t>Texture of the cylinder.</param>
/// <param name=top>Texture of the top disc.</param>
/// <param name=bot>Texture of the bottom disc.</param>
/// <returns>A solid cylinder shape.</returns>
let makeSolid c r h t top bot =
    if r <= 0. || h <= 0.
    then failwith "Nonpositive Cylinder size"
    let cylinder = SolidCylinder(c, r, h, t, top, bot)
    Shape.make (intersect cylinder) true (bounds cylinder)
