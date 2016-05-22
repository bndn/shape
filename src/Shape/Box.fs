// Copyright (C) 2015 The Authors.
module Shape.Box

open Point
open Texture
open Vector
open Utils

[<NoComparison>]
type Box =
    Box of Point * Point * Texture * Texture * Texture * Texture * Texture * Texture

/// <summary>
/// Double Equal. Checks if two doubles are "equal" to
/// eachother with a precision of EPSILON.
/// </summary>
/// <param name=a>The first double to compare.</param>
/// <param name=b>The second double to compare.</param>
/// <returns>
/// If the two doubles are (very close to being) equal to eachother.
/// </returns>
let deq a b = abs (a - b) < Epsilon

/// <summary>
/// Takes the low and high point, a hitpoint, a hitnormal at the point and
/// the texture from a box to return the UV coordinates at the specific point.
/// </summary>
/// <param name=low>The low point of the box.</params>
/// <param name=high>The high point of the box.</params>
/// <param name=hit>The hitpoint, from which the texture should be retrieved.</params>
/// <param name=hn>The hitnormal at the hitpoint.</params>
/// <param name=t>The texture on the relevant side of the hitpoint.</params>
/// <returns>
/// The UV coordinates at the hitpoint.
/// </returns>
let boxDeterminer lo hi hit hn t =
    let lx, ly, lz = Point.getCoord lo
    let hx, hy, hz = Point.getCoord hi
    match Vector.getCoord hn with
    | (x, _, _) when deq x -1. ->
        let boxHeight = abs(ly - hy)
        let boxDepth = abs(lz - hz)
        let u = 1.0 - abs (((Point.getZ hit) - lz) / boxDepth)
        let v = abs (((Point.getY hit) - ly) / boxHeight)
        Texture.getMaterial u v t
    | (_, y, _) when deq y -1. ->
        let boxWidth = abs(lx - hx)
        let boxDepth = abs(lz - hz)
        let u = 1.0 - abs (((Point.getX hit) - lx) / boxWidth)
        let v = abs (((Point.getZ hit) - lz) / boxDepth)
        Texture.getMaterial u v t
    | (_, _, z) when deq z -1. ->
        let boxWidth = abs(lx - hx)
        let boxHeight = abs(ly - hy)
        let u = (((Point.getX hit) - lx) / boxWidth)
        let v = abs (((Point.getY hit) - ly) / boxHeight)
        Texture.getMaterial u v t
    | (x, _, _) when deq x 1. ->
        let boxHeight = abs(ly - hy)
        let boxDepth = abs(lz - hz)
        let u = ((Point.getZ hit) - lz) / boxDepth
        let v = abs ((Point.getY hit) - ly) / boxHeight
        Texture.getMaterial u v t
    | (_, y, _) when deq y 1. ->
        let boxWidth = abs(lx - hx)
        let boxDepth = abs(lz - hz)
        let u = ((Point.getX hit) - lx) / boxWidth
        let v = abs (((Point.getZ hit) - lz) / boxDepth)
        Texture.getMaterial u v t
    | (_, _, z) when deq z 1. ->
        let boxWidth = abs(lx - hx)
        let boxHeight = abs(ly - hy)
        let u = 1.0 - abs (((Point.getX hit) - lx) / boxWidth)
        let v = abs (((Point.getY hit) - ly) / boxHeight)
        Texture.getMaterial u v t
    | _ -> failwith "Paradox: Hit a box side, without hitting it"

/// <summary>
/// Compute an intersection between a ray and a box.
/// </summary>
/// <param name=box>Box to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// The list of 0 to 2 hit distances from the ray origin to the hitpoints
/// on the shape. Only returns hitpoints that intersect in the positive
/// distance of the ray.
/// </returns>
let intersect (Box(lo, hi, fr, ba, t, b, l, r)) ray =
    let rayO, rayD = Ray.getOrigin ray, Ray.getVector ray
    let lx, ly, lz = Point.getCoord lo
    let hx, hy, hz = Point.getCoord hi

    let ox, oy, oz = Point.getCoord rayO
    let dx, dy, dz = Vector.getCoord rayD
    let idx, idy, idz = 1. / dx, 1. / dy, 1. / dz // inverse ray direction

    let txmin, txmax = if idx < 0.
                       then ((hx - ox) * idx), ((lx - ox) * idx)
                       else ((lx - ox) * idx), ((hx - ox) * idx)
    let tymin, tymax = if idy < 0.
                       then ((hy - oy) * idy), ((ly - oy) * idy)
                       else ((ly - oy) * idy), ((hy - oy) * idy)

    if txmin > tymax || tymin > txmax then List.empty else
    let tmin, tmax = max tymin txmin, min tymax txmax

    let tzmin, tzmax = if idz < 0.
                       then ((hz - oz) * idz), ((lz - oz) * idz)
                       else ((lz - oz) * idz), ((hz - oz) * idz)
    if tmin > tzmax || tzmin > tmax then List.empty else

    let tmin, tmax = max tzmin tmin, min tzmax tmax
    let tminHit = Point.move rayO (Vector.multScalar rayD tmin)
    let tmaxHit = Point.move rayO (Vector.multScalar rayD tmax)

    let sideHit hitpoint =
        match Point.getCoord hitpoint with
        | (x, _, _) when deq x lx -> (Vector.make -1. 0. 0., l)
        | (_, y, _) when deq y ly -> (Vector.make 0. -1. 0., b)
        | (_, _, z) when deq z lz -> (Vector.make 0. 0. -1., ba)
        | (x, _, _) when deq x hx -> (Vector.make 1. 0. 0., r)
        | (_, y, _) when deq y hy -> (Vector.make 0. 1. 0., t)
        | (_, _, z) when deq z hz -> (Vector.make 0. 0. 1., fr)
        | _ -> failwith "Paradox: Hit a box side, without hitting it"

    let tminHitNormal, tminTexture = sideHit tminHit
    let tmaxHitNormal, tmaxTexture = sideHit tmaxHit

    // if we hit the backside, return the inverse normal
    let tminHitNormal = if Vector.dotProduct rayD tminHitNormal > 0.
                        then -tminHitNormal else tminHitNormal
    let tmaxHitNormal = if Vector.dotProduct rayD tmaxHitNormal > 0.
                        then -tmaxHitNormal else tmaxHitNormal


    let tminHitMat = boxDeterminer lo hi tminHit tminHitNormal tminTexture
    let tmaxHitMat = boxDeterminer lo hi tmaxHit tmaxHitNormal tmaxTexture

    match (tmin > 0., tmax > 0.) with
    | (true, true)   -> [Hit(tmin, tminHitNormal, tminHitMat);
                         Hit(tmax, tmaxHitNormal, tmaxHitMat)]
    | (true, false)  -> [Hit(tmin, tminHitNormal, tminHitMat)]
    | (false, true)  -> [Hit(tmax, tmaxHitNormal, tmaxHitMat)]
    | (false, false) -> []

/// <summary>
/// Get the bounds of a box.
/// </summary>
/// <param name=box>Box to get bounds of.</param>
/// <returns>The bounds of a box.</returns>
let bounds (Box(lo, hi, _, _, _, _, _, _)) =
    let e = Epsilon // alias
    let ee = e * 2.
    let lx, ly, lz = Point.getCoord lo
    let hx, hy, hz = Point.getCoord hi
    let boundsP0 = Point.make (lx - e) (ly - e) (lz - e)

    Some (
        boundsP0,
        abs (hx - lx) + ee,
        abs (hy - ly) + ee,
        abs (hz - lz) + ee
    )

/// <summary>
/// Create an axis aligned box shape.
/// </summary>
/// <param name=lo>Low point of the box.</param>
/// <param name=hi>High point of the box.</param>
/// <param name=t>Texture of the box.</param>
/// <returns>A box shape.</returns>
let make lo hi fr ba t b l r =
    if lo = hi
    then failwith "Nonpositive Box size"
    let box = Box(lo, hi, fr, ba, t, b, l, r)
    Shape.make (intersect box) true (bounds box)
