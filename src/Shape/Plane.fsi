// Copyright (C) 2015 The Authors.
module Shape.Plane

open Point
open Texture
open Vector

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
val make : p0:Point -> up:Vector -> t:Texture -> Shape
