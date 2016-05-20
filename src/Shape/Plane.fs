// Copyright (C) 2015 The Authors.
module Shape.Plane

open Point
open Texture
open Vector
open Utils

[<NoComparison>]
type Plane = Plane of Point * Vector * Texture

/// <summary>
/// Compute an intersection between a ray and an infinite plane.
/// </summary>
/// <param name=plane>Plane to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// A single intersection when the ray hits, or none when the ray misses.
/// </returns>
let intersect (Plane(p0, up, tex)) r =
    let rayO, rayD = Ray.getOrigin r, Ray.getVector r
    // get hit point and its coordinate on the infinite plane
    let rph = rayPlaneHit rayO rayD p0 up
    if rph.IsNone then List.empty else
    let t, hitpoint = rph.Value

    let hpx, hpy, _ = Point.getCoord hitpoint

    let u = abs(hpx) % 1.0
    let v = abs(hpy) % 1.0

    let u = if hpx < 0. then 1. - u else u
    let v = if hpy < 0. then 1. - v else v

    // if we hit the backside, return the inverse normal
    let normal = if Vector.dotProduct rayD up > 0.
                 then -up else up

    let material = Texture.getMaterial u v tex

    [Hit(t, normal, material)]

/// <summary>
/// Create an infinite plane shape.
/// </summary>
/// <param name=p0>
/// Point of origin (affects the texture mapping of the plane).
/// </param>
/// <param name=up>Upvector of the plane.</param>
/// <param name=t>Texture of the plane.</param>
/// <returns>
/// A infinite plane shape, facing in the direction of the upvector,
/// with a point of origin (for texture mapping).
/// </returns>
let make p0 up t =
    if Vector.magnitude up = 0.
    then failwith "Creating a plane with a 0-vector as a normal"
    let plane = Plane(p0, up, t)
    Shape.make (intersect plane) false None
