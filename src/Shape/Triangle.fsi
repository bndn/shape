// Copyright (C) 2015 The Authors.
module Shape.Triangle

open Point
open Texture

/// <summary>
/// Create a triangle shape.
/// </summary>
/// <param name=a>Point a in the triangle.</param>
/// <param name=b>Point b in the triangle.</param>
/// <param name=c>Point c in the triangle.</param>
/// <param name=t>Texture of the triangle.</param>
/// <returns>A triangle shape.</returns>
val make : a:Point -> b:Point -> c:Point -> t:Texture -> Shape
