// Copyright (C) 2015 The Authors.
module Shape.Cylinder

open Point
open Texture

/// <summary>
/// Create a hollow cylinder shape.
/// </summary>
/// <param name=c>Center of the cylinder.</param>
/// <param name=r>Radius of the cylinder.</param>
/// <param name=h>Height of the cylinder.</param>
/// <param name=t>Texture of the cylinder.</param>
/// <returns>A hollow cylinder shape.</returns>
val makeHollow : c:Point -> r:float -> h:float -> t:Texture -> Shape

/// <summary>
/// Create a solid cylinder shape.
/// </summary>
/// <param name=c>Center of the cylinder.</param>
/// <param name=r>Radius of the cylinder.</param>
/// <param name=h>Height of the cylinder.</param>
/// <param name=t>Texture of the cylinder.</param>
/// <param name=top>Texture of the top disc.</param>
/// <param name=bot>Texture of the bottom disc.</param>
/// <returns>A solid cylinder shape.</returns>
val makeSolid : c:Point -> r:float -> h:float -> t:Texture -> top:Texture -> bot:Texture -> Shape
