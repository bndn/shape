// Copyright (C) 2015 The Authors.
module Shape.Box

open Point
open Texture

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
val make : lo:Point -> hi:Point -> fr:Texture -> ba:Texture -> t:Texture -> b:Texture -> l:Texture -> r:Texture -> Shape
