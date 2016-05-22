// Copyright (C) 2015 The Authors.
module Shape.Rectangle

open Point
open Texture
open Vector
open Utils

[<NoComparison>]
type Rectangle = Rectangle of Point * float * float * Texture

/// <summary>
/// Compute an intersection between a ray and a rectangle.
/// </summary>
/// <param name=rect>Rectangle to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// A single intersection when the ray hits, or none when the ray misses.
/// </returns>
let intersect (Rectangle(p0, w, h, tex)) r =
    let rayO, rayD = Ray.getOrigin r, Ray.getVector r
    let up = Vector.make 0. 0. 1.
    let rph = rayPlaneHit rayO rayD p0 up
    if rph.IsNone then List.empty else
    let t, hitpoint = rph.Value

    let pX = Point.getX p0
    let pY = Point.getY p0

    let hpX = Point.getX hitpoint
    let hpY = Point.getY hitpoint

    let withinX = pX <= hpX && hpX <= (pX + w)
    let withinY = pY <= hpY && hpY <= (pY + h)

    match withinX && withinY with
    | true  -> let u = (hpX - pX) / w
               let v = (hpY - pY) / h
               // if we hit the backside, return the inverse normal
               let normal = if Vector.dotProduct rayD up > 0.
                            then -up else up
               let material = Texture.getMaterial u v tex
               [Hit(t, normal, material)]
    | false -> List.empty

/// <summary>
/// Get the bounds of a rectangle.
/// </summary>
/// <param name=r>Rectangle to get bounds of.</param>
/// <returns>The bounds of a rectangle.</returns>
let bounds (Rectangle(p0, w, h, _)) =
    let e = Epsilon // alias
    let ee = e * 2.
    let px, py, pz = Point.getCoord p0
    let boundsP0 = Point.make (px - e) (py - e) (pz - e)

    Some (
        boundsP0,
        w + ee,
        h + ee,
        ee
    )

/// <summary>
/// Create a rectangle shape.
/// </summary>
/// <param name=p0>Bottom left point.</param>
/// <param name=w>Width of the rectangle.</param>
/// <param name=h>Height of the rectangle.</param>
/// <param name=t>Texture of the rectangle.</param>
/// <returns>A rectangle shape in the x-z axis.</returns>
let make p0 w h t =
    if w <= 0. || h <= 0.
    then failwith "Nonpositive Rectangle size"
    let rectangle = Rectangle(p0, w, h, t)
    Shape.make (intersect rectangle) false (bounds rectangle)
