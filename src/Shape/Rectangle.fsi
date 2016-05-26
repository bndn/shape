// Copyright (C) 2015 The Authors.
module Shape.Rectangle

open Point
open Texture

/// <summary>
/// Create a rectangle shape.
/// </summary>
/// <param name=p0>Bottom left point.</param>
/// <param name=w>Width of the rectangle.</param>
/// <param name=h>Height of the rectangle.</param>
/// <param name=t>Texture of the rectangle.</param>
/// <returns>A rectangle shape in the x-z axis.</returns>
val make : p0:Point -> w:float -> h:float -> t:Texture -> Shape
