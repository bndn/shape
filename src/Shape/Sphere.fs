// Copyright (C) 2015 The Authors.
module Shape.Sphere

open Point
open System // for Math.PI
open Texture
open Utils

[<NoComparison>]
type Sphere = Sphere of Point * float * Texture

/// <summary>
/// Creates a hitpoint on a sphere, given a distance, a ray direction
/// a ray origin, the radius of the sphere and the texture.
/// </summary>
/// <param name=d>The distance between rayO and the sphere's surface.</param>
/// <param name=rayD>The direction of the ray.</param>
/// <param name=rayO>The origin of the ray.</param>
/// <param name=r>The radius of the sphere.</param>
/// <param name=t>The texture for the sphere.</param>
/// <returns>
/// A hitpoint for the sphere, with a distance, the inverse vector
/// (going outwards from the sphere surface) and the material for the
/// hitpoint.
/// </returns>
let sphereDeterminer d center rayD rayO t =
   // hitpoint on sphere
   let hitPoint = Point.move rayO (Vector.multScalar rayD d)

   // normalised vector from center towards hitpoint
   let n = Vector.normalise <| Point.distance center hitPoint
   // if we hit the inside going out, return the inverse normal
   let n = if Vector.dotProduct rayD n > 0.
           then -n else n
   let nx, ny, nz = Vector.getCoord n

   let theta = acos ny      // angle in y-space
   let phi' = atan2 nx nz
   let phi = if phi' < 0.   // angle in x- and z-space
             then phi' + 2. * Math.PI
             else phi'

   let u = phi / (2. * Math.PI)
   let v = 1. - (theta / Math.PI)

   let material = Texture.getMaterial u v t

   Hit(d, n, material)

/// <summary>
/// Compute an intersection between a ray and a sphere.
/// </summary>
/// <param name=sphere>Sphere to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// A list of intersections of the ray and sphere. Empty if the ray
/// does not hit the sphere.
/// </returns>
let intersect (Sphere(center, radius, t)) r =
    let rayO, rayD = Ray.getOrigin r, Ray.getVector r
    let dx, dy, dz = Vector.getCoord rayD
    let ox, oy, oz = Point.getCoord rayO
    let x, y, z = Point.getCoord center

    let a = dx**2. + dy**2. + dz**2.
    let b = 2. * ((ox - x) * dx + (oy - y) * dy + (oz - z) * dz)
    let c = (ox - x)**2. + (oy - y)**2. + (oz - z)**2. - radius**2.

    let distances = distanceQuadratic a b c
    match distances with
    | []         -> List.empty
    | [hp]       -> [sphereDeterminer hp center rayD rayO t]
    | [hp1; hp2] -> [sphereDeterminer hp1 center rayD rayO t;
                     sphereDeterminer hp2 center rayD rayO t]
    | _          -> failwith "Error: Hitting a sphere more than two times!"

/// <summary>
/// Get the bounds of a sphere.
/// </summary>
/// <param name=s>Sphere to get bounds of.</param>
/// <returns>The bounds of a sphere.</returns>
let bounds (Sphere(c, r, _)) =
    let e = Epsilon // alias
    let ee = e * 2.
    let cx, cy, cz = Point.getCoord c
    let boundsP0 = Point.make (cx - r - e) (cy - r - e) (cz - r - e)

    Some (
        boundsP0,
        r * 2. + ee,
        r * 2. + ee,
        r * 2. + ee
    )

/// <summary>
/// Create a sphere shape.
/// </summary>
/// <param name=c>Center of the sphere.</param>
/// <param name=r>Radius of the sphere.</param>
/// <param name=t>Texture of the sphere.</param>
/// <returns>A sphere shape.</returns>
let make c r t =
    if r <= 0. then failwith "Nonpositive Sphere size"
    let sphere = Sphere(c, r, t)
    Shape.make (intersect sphere) true (bounds sphere)
