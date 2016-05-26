// Copyright (C) 2016 The Authors.
module Shape.Mesh

open Meshly
open Texture

/// <summary>
/// Compute an intersection between a ray and a mesh.
/// </summary>
/// <param name=m>Mesh to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <param name=t>Texture to return material of.</param>
/// <returns>
/// A single intersection when the ray hits, or none when the ray misses.
/// </returns>
let intersect m r t =
    match Meshly.getIntersection r m with
    | Some i ->
        let d = Meshly.getDistance i
        let n = Meshly.getNormal i
        let u, v = Meshly.getCoordinates i
        let m = Texture.getMaterial u v t

        [Hit(d, n, m)]
    | None -> []

/// <summary>
/// Get the bounds of a mesh.
/// </summary>
/// <param name=m>Mesh to get bounds of.</param>
/// <returns>The bounds of a mesh.</returns>
let bounds m = Meshly.getBounds m |> Some

/// <summary>
/// Create a triangle mesh from a PLY-formatted string.
/// </summary>
/// <param name=s>The input string.</param>
/// <param name=n>Define if the normals are flat- or smooth shaded.</param>
/// <returns>The created shape.</returns>
let make s n =
    let n = if n then Meshly.Smooth else Meshly.Flat
    let m = Meshly.make s n

    Shape.makeBaseShape (intersect m) true (bounds m)
