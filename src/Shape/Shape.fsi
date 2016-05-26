/// Copyright (C) 2015 The Authors.
[<AutoOpen>]
module Shape.Shape

open Material
open Point
open Ray
open Texture
open Transform
open Vector

type Distance = float
type Normal = Vector

[<NoComparison>]
type Hit = Hit of Distance * Normal * Material

type Bounds = Point * float * float * float

type Shape

/// <summary>
/// Compute a hit on a shape.
/// </summary>
/// <param name=r>Ray of the hit.</param>
/// <param name=s>Shape to check for hits.</param>
/// <returns>A list of intersection hits between the ray and shape.</returns>
val hit : r:Ray -> s:Shape -> Hit list

/// <summary>
/// Get the distance of a hit.
/// </summary>
/// <param name=h>Hit to get the distance of.</param>
/// <returns>The distance of a hit.</returns>
val hitDistance : h:Hit -> Distance

/// <summary>
/// Get the normal of a hit.
/// </summary>
/// <param name=h>Hit to get the normal of.</param>
/// <returns>The normal of a hit.</returns>
val hitNormal : h:Hit -> Normal

/// <summary>
/// Get the material of a hit.
/// </summary>
/// <param name=h>Hit to get the material of.</param>
/// <returns>The material of a hit.</returns>
val hitMaterial : h:Hit -> Material

/// <summary>
/// Check if the shape is solid or not.
/// </summary>
/// <param name=s>Shape to check solidity(?) on.</param>
/// <returns>Whether the shape is solid or not.</returns>
val solid : s:Shape -> bool

/// <summary>
/// Create a generic shape.
/// </summary>
/// <param name=h>Hitfunction of the shape.</param>
/// <param name=t>Texture of the shape.</param>
/// <returns>A generic shape.</returns>
val make : h:(Ray -> Hit list) -> isSolid:bool -> b:Bounds option -> Shape

/// <summary>
/// Creates a base shape.
/// </summary>
/// <param name=h>Hitfunction of the shape.</param>
/// <param name=b>Bounds option of the shape.</param>
/// <param name=isSolid>If the shape is solid or not.</param>
/// <returns>A base shape.</returns>
val makeBaseShape : h:(Ray -> Texture -> Hit list) -> isSolid:bool -> b:Bounds option -> Shape

/// <summary>
/// Takes a BaseShape and re-texturizes it.
/// </summary>
/// <param name=b>The BaseShape input.</param>
/// <param name=t>The texture to apply.</param>
/// <returns>A texturized shape.</returns>
val texturize : b:Shape -> t:Texture -> Shape

/// <summary>
/// Get the bounds of a shape.
/// </summary>
/// <param name=s>Shape to get the bounds of.</param>
/// <returns>The bounds of a shape.</returns>
val bounds : s:Shape -> Bounds option

/// <summary>
/// Apply a transformation on a shape.
/// </summary>
/// <param name=s>Shape to transform.</param>
/// <param name=newTrans>New transformation to apply to the shape.</param>
/// <returns>A transformed shape.</returns>
val transform : s:Shape -> trans:Transformation -> Shape

[<Literal>]
// HACK: for some reason, compiling while having Epsilon at the
// top of the file will fail. Drop it here!
val Epsilon : float = 1.0e-6
