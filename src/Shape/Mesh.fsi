// Copyright (C) 2016 The Authors.
module Shape.Mesh

/// <summary>
/// Create a triangle mesh from a PLY-formatted string.
/// </summary>
/// <param name=s>The input string.</param>
/// <param name=n>Define if the normals are flat- or smooth shaded.</param>
/// <returns>The created shape.</returns>
val make : s:char seq -> n:bool -> Shape
