// Copyright (C) 2015 The Authors.
module Shape.Triangle

open Point
open Texture

[<NoComparison>]
type Triangle = Triangle of Point * Point * Point * Texture

/// <summary>
/// Compute an intersection between a ray and a triangle.
/// </summary>
/// <param name=triangle>Triangle to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// A single intersection when the ray hits, or none when the ray misses.
/// </returns>
let intersect (Triangle(a, b, c, tex)) r =
    let rayO, rayD = Ray.getOrigin r, Ray.getVector r
    // Möller-Trumbore intersection algorithm
    let e1 = Point.distance a b
    let e2 = Point.distance a c
    let P = Vector.crossProduct rayD e2
    let det = Vector.dotProduct e1 P

    if det > -Epsilon && det < Epsilon then List.empty else
    let invDet = 1. / det

    let T = Point.distance a rayO

    let u = (Vector.dotProduct T P) * invDet
    if u < 0. || u > 1. then List.empty else

    let Q = Vector.crossProduct T e1

    let v = (Vector.dotProduct rayD Q) * invDet
    if v < 0. || (u + v) > 1. then List.empty else

    let uvcp = Vector.crossProduct e1 e2
    let n = Vector.multScalar uvcp (1. / (Vector.magnitude (uvcp)))

    let t = (Vector.dotProduct e2 Q) * invDet

    if t > Epsilon
    then [Hit(t, n, Texture.getMaterial 0. 0. tex)]
    else List.empty

/// <summary>
/// Get the bounds of a triangle.
/// </summary>
/// <param name=t>Triangle to get bounds of.</param>
/// <returns>The bounds of a triangle.</returns>
let bounds (Triangle(a, b, c, _)) =
    let e = Epsilon // alias
    let ee = e * 2.
    let (ax, ay, az), (bx, by, bz), (cx, cy, cz) =
        Point.getCoord a, Point.getCoord b, Point.getCoord c

    let lx, ly, lz = min ax (min bx cx), min ay (min by cy), min az (min bz cz)
    let hx, hy, hz = max ax (max bx cx), max ay (max by cy), max az (max bz cz)
    let boundsP0 = Point.make (lx - e) (ly - e) (lz - e)

    Some (
        boundsP0,
        abs (hx - lx) + ee, // width
        abs (hy - ly) + ee, // height
        abs (hz - lz) + ee  // depth
    )

/// <summary>
/// Create a triangle shape.
/// </summary>
/// <param name=a>Point a in the triangle.</param>
/// <param name=b>Point b in the triangle.</param>
/// <param name=c>Point c in the triangle.</param>
/// <param name=t>Texture of the triangle.</param>
/// <returns>A triangle shape.</returns>
let make a b c t =
    if a = b || a = c || b = c then failwith "Nonpositive Triangle size"
    let triangle = Triangle(a, b, c, t)
    Shape.make (intersect triangle) false (bounds triangle)
