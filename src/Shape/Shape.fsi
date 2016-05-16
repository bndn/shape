/// Copyright (C) 2015 The Authors.
module Shape

open Material
open Point
open Ray
open Texture
open Vector

type Shape

type Bounds = Point * float * float * float

type Hitpoint

exception NonPositiveShapeSizeException

/// <summary>
/// Get the distance, the normal and the material of the hit.
/// </summary>
/// <param name=hp>The hitpoint to get the values of.</param>
/// <returns>
/// A triplet of the distance, the normal and the material
/// of the hit.
/// </returns>
val getHitpoint : hp:Hitpoint -> float * Vector * Material

/// <summary>
/// Get the distance from the ray's origin, to the hitpoint on the shape.
/// </summary>
/// <param name=hp>The hitpoint on the shape to get the distance of.</param>
/// <returns>The distance to the hitpoint.</returns>
val getHitDistance : hp:Hitpoint -> float

/// <summary>
/// Get the normal of the shape at the hitpoint.
/// </summary>
/// <param name=hp>The hitpoint on the shape to get the normal of.</param>
/// <returns>The normal of the shape at the hitpoint.</returns>
val getHitNormal : hp:Hitpoint -> Vector

/// <summary>
/// Get the material of the shape at the hitpoint.
/// </summary>
/// <param name=hp>The hitpoint on the shape to get the material of.</param>
/// <returns>The material on the shape at the hitpoint.</returns>
val getHitMaterial : hp:Hitpoint -> Material

/// <summary>
/// Get the bounds of a shape, consisting of a point (P0), the width
/// of the bounds, the height of the bounds and the depth of the bounds.
/// </summary>
/// <param name=s>The shape to get the bounds of.</param>
/// <returns>The bounds of a shape.</returns>
val getBounds : s:Shape -> Bounds option

/// <summary>
/// Make a plane with a point of origin (affects the texture mapping),
/// an upvector and a texture.
/// </summary>
/// <param name=p0>
/// The point of origin (affects the texture mapping of the plane).
/// </param>
/// <param name=up>The upvector of the plane.</param>
/// <param name=t>The texture of the plane.</param>
/// <returns>
/// A plane object facing in the direction of the upvector, with a
/// point of origin (for texture mapping) and a texture.
/// </returns>
val mkPlane : p0:Point -> up:Vector -> t:Texture -> Shape

/// <summary>
/// Make a disc with a center point, an upvector, a radius and a texture.
/// </summary>
/// <param name=p0>The center point of the disc.</param>
/// <param name=up>The upvector of the disc.</param>
/// <param name=radius>The radius of the disc.</param>
/// <param name=t>The texture of the disc.</param>
/// <returns>
/// A disc object facing in the direction of the upvector, with a center
/// point, a radius and a texture.
/// </returns>
val mkDisc : p0:Point -> radius:float -> t:Texture -> Shape

/// <summary>
/// Make a sphere with a point of origin, a radius and a texture.
/// </summary>
/// <param name=center>The center point of the sphere.</param>
/// <param name=radius>The radius of the sphere.</param>
/// <param name=t>The texture of the sphere.</param>
/// <returns>
/// A sphere object, with a point of origin, a radius and a texture.
/// </returns>
val mkSphere : center:Point -> radius:float -> t:Texture -> Shape

/// <summary>
/// Make a hollow cylinder with a center point of origin, a radius,
/// a height and a texture.
/// </summary>
/// <param name=center>
/// The center point at the bottom (or top, if the height is negative)
/// of the cylinder.
/// </param>
/// <param name=radius>The radius of the cylinder.</param>
/// <param name=height>
/// The height of the cylinder. Can be negative, which will make
/// the cylinder grow in the negative direction of the y-axis.
/// </param>
/// <param name=t>The texture of the cylinder.</param>
/// <returns>
/// A hollow cylinder object, with a center point of origin, a radius,
/// a height and a texture.
/// </returns>
val mkHollowCylinder : center:Point -> radius:float -> height:float ->
        t:Texture -> Shape

/// <summary>
/// Make a triangle with points, `a`, `b` and `c`.
/// </summary>
/// <param name=a>Point `a` in the triangle.</param>
/// <param name=b>Point `b` in the triangle.</param>
/// <param name=c>Point `c` in the triangle.</param>
/// <param name=m>The material of the triangle.</param>
/// <returns>
/// A triangle object, with points `a`, `b` and `c`, and a material.
/// </returns>
val mkTriangle : a:Point -> b:Point -> c:Point -> m:Material -> Shape

val mkUnion : Shape -> Shape -> Shape

val mkIntersection : Shape -> Shape -> Shape

val mkSubtraction : Shape -> Shape -> Shape

/// <summary>
/// Shoot a ray, and check if it hits the specificed shape.
/// Returns a hitpoint for each point on the shape that
/// was hit, as a list.
/// </summary>
/// <param name=r>The ray.</param>
/// <param name=s>The shape.</param>
/// <returns>The list of hitpoints with the ray, on the shape.</returns>
val hitFunction : r:Ray -> s:Shape -> Hitpoint list
