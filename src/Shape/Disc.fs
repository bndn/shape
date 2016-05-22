// Copyright (C) 2015 The Authors.
module Shape.Disc

open Point
open Texture
open Utils

[<NoComparison>]
type Disc = Disc of Point * float * Texture

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
let discDeterminer d hit normal r c t =
    let x = Point.getX hit - Point.getX c
    let y = Point.getY hit - Point.getY c

    if x**2. + y**2. <= r**2. then
        let u = (x + r) / (2. * r)
        let v = (y + r) / (2. * r)
        let material = Texture.getMaterial u v t
        Some (Hit(d, normal, material))
    else None

/// <summary>
/// Compute an intersection between a ray and a disc.
/// </summary>
/// <param name=disc>Disc to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// A single intersection when the ray hits, or none when the ray misses.
/// </returns>
let intersect (Disc(c, radius, tex)) r =
    let rayO, rayD = Ray.getOrigin r, Ray.getVector r
    let up = Vector.make 0. 0. 1.
    let rph = rayPlaneHit rayO rayD c up
    if rph.IsNone then List.empty else
    let t, hitpoint = rph.Value

    // if we hit the backside, return the inverse normal
    let normal = if Vector.dotProduct rayD up > 0.
                 then -up else up

    match discDeterminer t hitpoint normal radius c tex with
    | Some hit -> [hit]
    | None     -> []

/// <summary>
/// Get the bounds of a disc.
/// </summary>
/// <param name=disc>Disc to get bounds of.</param>
/// <returns>The bounds of a disc.</returns>
let bounds (Disc(c, r, _)) =
    let e = Epsilon // alias
    let ee = e * 2.
    let cx, cy, cz = Point.getCoord c
    let boundsP0 = Point.make (cx - r - e) (cy - r - e) (cz - e)

    Some (
        boundsP0,
        r * 2. + ee,
        r * 2. + ee,
        ee
    )

/// <summary>
/// Create a disc shape.
/// </summary>
/// <param name=c>Center of the disc.</param>
/// <param name=r>Radius of the disc.</param>
/// <param name=t>Texture of the disc.</param>
/// <returns>A disc shape.</returns>
let make c r t =
    if r <= 0.
    then failwith "Nonpositive Disc size"
    let disc = Disc(c, r, t)
    Shape.make (intersect disc) false (bounds disc)
