// Copyright (C) 2016 The Authors.
module Shape.Mesh

open Meshly
open Texture

let intersect m r t =
    match Meshly.getIntersection r m with
    | Some i ->
        let d = Meshly.getDistance i
        let n = Meshly.getNormal i
        let u, v = Meshly.getCoordinates i
        let m = Texture.getMaterial u v t

        [Hit(d, n, m)]
    | None -> []

let bounds m = Meshly.getBounds m |> Some

let make s n =
    let n = if n then Meshly.Smooth else Meshly.Flat
    let m = Meshly.make s n

    Shape.makeBaseShape (intersect m) true (bounds m)
