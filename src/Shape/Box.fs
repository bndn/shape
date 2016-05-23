// Copyright (C) 2015 The Authors.
module Shape.Box

open System // to check if doubles are NaN

open Point
open Texture
open Vector
open Utils

type Side =
    | Front | Back
    | Top   | Bottom
    | Left  | Right

[<NoComparison>]
type Box =
    Box of Point * Point * Texture * Texture * Texture * Texture * Texture * Texture

/// <summary>
/// Double Equal. Checks if two doubles are "equal" to each other
/// within a precision of Epsilon.
/// </summary>
/// <param name=a>The first double to compare.</param>
/// <param name=b>The second double to compare.</param>
/// <returns>
/// If the two doubles are (very close to being) equal to each other.
/// </returns>
let deq a b = abs (a - b) < Epsilon

/// <summary>
/// Takes the low and high point, a hitpoint, a hitnormal at the point and
/// the texture from a box to return the material at the specific point.
/// </summary>
/// <param name=lo>Low point of the box.</params>
/// <param name=hi>High point of the box.</params>
/// <param name=hit>Hitpoint to get the material from.</params>
/// <param name=s>Side that the ray hit.</param>
/// <param name=t>The texture on the relevant side of the hitpoint.</params>
/// <returns>Material at the hitpoint.</returns>
let boxDeterminer lo hi hit s t =
    let lx, ly, lz = Point.getCoord lo
    let hx, hy, hz = Point.getCoord hi
    let hpx, hpy, hpz = Point.getCoord hit
    let boxHeight = abs (hy - ly)
    let boxWidth = abs (hx - lx)
    let boxDepth = abs (hz - lz)

    let u, v = match s with
               | Front  -> let u = (abs (hpx - lx)) / boxWidth
                           let v = (abs (hpy - ly)) / boxHeight in (u, v)
               | Top    -> let u = (abs (hpx - lx)) / boxWidth
                           let v = (abs (hpz - lz)) / boxDepth  in (u, v)
               | Left   -> let u = (abs (hpy - ly)) / boxHeight
                           let v = (abs (hpz - lz)) / boxDepth  in (u, v)
               | Right  -> let u = (abs (hpy - ly)) / boxHeight
                           let v = (abs (hpz - lz)) / boxDepth  in (u, v)
               | Back   -> let u = (abs (hpx - lx)) / boxWidth
                           let v = (abs (hpy - ly)) / boxHeight in (u, v)
               | Bottom -> let u = (abs (hpx - lx)) / boxWidth
                           let v = (abs (hpz - lz)) / boxDepth  in (u, v)

    Texture.getMaterial u v t

/// <summary>
/// Get the hit normal from a ray to a side of the box.
/// </summary>
/// <param name=side>Side which was hit.</param>
/// <param name=rayD>Ray direction.</param>
/// <returns>Hit normal from the ray-box intersection.</returns>
let hitNormal side rayD =
    let normal = match side with
                 | Front  -> Vector.make 0. 0. 1.
                 | Top    -> Vector.make 0. 1. 0.
                 | Left   -> Vector.make -1. 0. 0.
                 | Right  -> Vector.make 1. 0. 0.
                 | Back   -> Vector.make 0. 0. -1.
                 | Bottom -> Vector.make 0. -1. 0.
    if Vector.dotProduct rayD normal > 0.
    then -normal
    else normal

/// <summary>
/// Compute an intersection between a ray and a box.
/// </summary>
/// <param name=box>Box to check intersection with.</param>
/// <param name=ray>Ray to check intersection with.</param>
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

    // when we're really close to hitting the shape, we might get
    // some falsy hits, avoid this by checking if tmin or tmax are NaN.
    // the IsNaN operation is acceptably fast, see https://goo.gl/D7aPnJ
    if Double.IsNaN(tmin) || Double.IsNaN(tmax) then List.empty else
    let tminHit = Point.move rayO (Vector.multScalar rayD tmin)
    let tmaxHit = Point.move rayO (Vector.multScalar rayD tmax)

    let sideHit hitpoint =
        match Point.getCoord hitpoint with
        // ordered based on the statistical likelyhood of getting selected,
        // having the most probable at the top will in most cases yield
        // faster runtimes, as the unlikely cases are tested less often, see:
        // https://msdn.microsoft.com/en-us/library/dd547125(v=vs.110).aspx
        | (_, _, z) when deq z hz -> (Front,  fr)
        | (_, y, _) when deq y hy -> (Top,    t)
        | (x, _, _) when deq x lx -> (Left,   l)
        | (x, _, _) when deq x hx -> (Right,  r)
        | (_, _, z) when deq z lz -> (Back,   ba)
        | (_, y, _) when deq y ly -> (Bottom, b)
        | _ -> failwith "Paradox: Hit a box side, without hitting it"

    let tminSide, tminTexture = sideHit tminHit
    let tmaxSide, tmaxTexture = sideHit tmaxHit

    let tminHitMat = boxDeterminer lo hi tminHit tminSide tminTexture
    let tmaxHitMat = boxDeterminer lo hi tmaxHit tmaxSide tmaxTexture

    match (tmin > 0., tmax > 0.) with
    | (true, true)   -> [Hit(tmin, hitNormal tminSide rayD, tminHitMat);
                         Hit(tmax, hitNormal tmaxSide rayD, tmaxHitMat)]
    | (true, false)  -> [Hit(tmin, hitNormal tminSide rayD, tminHitMat)]
    | (false, true)  -> [Hit(tmax, hitNormal tmaxSide rayD, tmaxHitMat)]
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
/// <param name=fr>Front side texture of the box.</param>
/// <param name=ba>Back side texture of the box.</param>
/// <param name=t>Top side texture of the box.</param>
/// <param name=b>Bottom side texture of the box.</param>
/// <param name=l>Left side texture of the box.</param>
/// <param name=r>Right side texture of the box.</param>
/// <returns>A box shape.</returns>
let make lo hi fr ba t b l r =
    if lo = hi
    then failwith "Nonpositive Box size"
    let box = Box(lo, hi, fr, ba, t, b, l, r)
    Shape.make (intersect box) true (bounds box)
