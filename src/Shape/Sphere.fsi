// Copyright (C) 2015 The Authors.
module Shape.Sphere

open Point
open Texture

/// <summary>
/// Create a sphere shape.
/// </summary>
/// <param name=c>Center of the sphere.</param>
/// <param name=r>Radius of the sphere.</param>
/// <param name=t>Texture of the sphere.</param>
/// <returns>A sphere shape.</returns>
val make : c:Point -> r:float -> t:Texture -> Shape
