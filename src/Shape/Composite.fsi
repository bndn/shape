// Copyright (C) 2015 The Authors.
module Shape.Composite

type Composition =
    | Group
    | Union
    | Subtraction
    | Intersection

/// <summary>
/// Create a CSG composite shape.
/// </summary>
/// <param name=s1>First shape of the composite.</param>
/// <param name=s2>Second shape of the composite.</param>
/// <param name=c>Composition type.</param>
/// <returns>A CSG composite shape.</returns>
val make : s1:Shape -> s2:Shape -> c:Composition -> Shape
