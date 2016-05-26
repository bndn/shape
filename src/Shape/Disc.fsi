// Copyright (C) 2015 The Authors.
module Shape.Disc

open Point
open Texture

/// <summary>
/// Create a disc shape.
/// </summary>
/// <param name=c>Center of the disc.</param>
/// <param name=r>Radius of the disc.</param>
/// <param name=t>Texture of the disc.</param>
/// <returns>A disc shape.</returns>
val make : c:Point -> r:float -> t:Texture -> Shape
