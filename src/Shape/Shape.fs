// Copyright (C) 2015 The Authors.
[<AutoOpen>]
module Shape.Shape

open Material
open Point
open Ray
open Transform
open Vector
open Utils

[<Literal>]
let Epsilon = 1.0e-6

type Distance = float
type Normal = Vector

[<NoComparison>]
type Hit = Hit of Distance * Normal * Material

type Bounds = Point * float * float * float

[<NoComparison>]
[<NoEquality>]
type Shape =
    Shape of (Ray -> Hit list) * bool * Bounds option * Transformation option

/// <summary>
/// Compute a hit on a shape.
/// </summary>
/// <param name=r>Ray of the hit.</param>
/// <param name=s>Shape to check for hits.</param>
/// <returns>A list of intersection hits between the ray and shape.</returns>
let hit r (Shape(intersect, _, _, invTrans)) =
    let r = if invTrans.IsSome then r * invTrans.Value else r

    // Fire in the hole!
    let intersections = intersect r

    let hitPrep (Hit(d, n, m)) =
        let n = if invTrans.IsSome
                then Vector.normalise (n * (transpose invTrans.Value))
                else n
        Hit(d, n, m)

    List.map (fun h -> hitPrep h) intersections

/// <summary>
/// Get the distance of a hit.
/// </summary>
/// <param name=h>Hit to get the distance of.</param>
/// <returns>The distance of a hit.</returns>
let hitDistance (Hit(d, _, _)) = d

/// <summary>
/// Get the normal of a hit.
/// </summary>
/// <param name=h>Hit to get the normal of.</param>
/// <returns>The normal of a hit.</returns>
let hitNormal (Hit(_, n, _)) = n

/// <summary>
/// Get the material of a hit.
/// </summary>
/// <param name=h>Hit to get the material of.</param>
/// <returns>The material of a hit.</returns>
let hitMaterial (Hit(_, _, m)) = m

/// <summary>
/// Check if the shape is solid or not.
/// </summary>
/// <param name=s>Shape to check solidity(?) on.</param>
/// <returns>Whether the shape is solid or not.</returns>
let solid (Shape(_, isSolid, _, _)) = isSolid

/// <summary>
/// Create a generic shape.
/// </summary>
/// <param name=h>Hitfunction of the shape.</param>
/// <param name=b>Bounds option of the shape.</param>
/// <param name=isSolid>If the shape is solid or not.</param>
/// <returns>A generic shape.</returns>
let make h isSolid b = Shape(h, isSolid, b, None)

/// <summary>
/// Get the bounds of a shape.
/// </summary>
/// <param name=s>Shape to get the bounds of.</param>
/// <returns>The bounds of a shape.</returns>
let bounds (Shape(_, _, b, invTrans)) =
    match b, invTrans with
    | None, _    -> None
    | _, None    -> b
    | _, Some it -> let trans = inverse it
                    Some (transformBounds b.Value trans)

/// <summary>
/// Apply a transformation on a shape.
/// </summary>
/// <param name=s>Shape to transform.</param>
/// <param name=trans>New transformation to apply to the shape.</param>
/// <returns>A transformed shape.</returns>
let transform (Shape(h, s, b, invTrans)) trans =
    let prevInvTrans = match invTrans with
                       | Some invTrans -> invTrans
                       | None          -> empty
    let newInvTrans = inverse trans
    Shape(h, s, b, Some (prevInvTrans * newInvTrans))
