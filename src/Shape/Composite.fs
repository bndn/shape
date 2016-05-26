// Copyright (C) 2015 The Authors.
module Shape.Composite

open Point
open Vector

type Composition =
    | Group
    | Union
    | Subtraction
    | Intersection

// Constructive Solid Geometry
[<NoComparison>]
type Composite = Composite of Shape * Shape * Composition

type ID = One | Two

/// <summary>
/// Function for sorting a hitlist into a list of triples, sorted by distance to origin of ray.
/// </summary>
/// <param name="shape1"> `shape1` is the first shape used in the composite hit.</param>
/// <param name="shape2"> `shape2` is the second shape used in the composite hit.</param>
/// <param name="hits1"> `hits1` is a list of the hitpoints on shape1.</param>
/// <param name="hits2"> `hits1` is a list of the hitpoints on shape2.</param>
/// <returns>
/// A list of triples of the type (id,shape,hit), sorted by the hits distance to the origin of the ray.
/// </returns>
let sortToTuples hits1 hits2 =
    let shape1list = List.map (fun x -> (One,x)) hits1
    let shape2list = List.map (fun x -> (Two,x)) hits2
    let tupleList = shape1list @ shape2list
    tupleList |> List.sortWith (fun (_,h1) (_,h2) ->
        let d1 = Shape.hitDistance h1
        let d2 = Shape.hitDistance h2
        if d1 > d2 then 1 elif d1 < d2 then -1 else 0)

/// <summary>
/// Function for testing if a hits normal vector is orthogonal on a ray.
/// </summary>
/// <param name="ray">`ray` is the ray responsible for the hit.</param>
/// <param name="hitNormalVector">`hitNormalVector` is the normal Vector of the hit.</param>
/// <returns>
/// A boolean value which is true if the normal vector is orthogonal, or false if it isn't.
/// </returns>
let isOrthogonal ray hit =
    let hitNormalVector = Shape.hitNormal hit
    let rayVector = Ray.getVector ray
    let dp = Vector.dotProduct rayVector hitNormalVector
    dp < Epsilon && dp > -Epsilon

/// <summary>
/// Hitfunction specific to the Union composite.
/// </summary>
/// <param name="ray"> The ray to check for hits. </param>
/// <param name="hitTupleList"> A list containing triples of the type (id,shape,hit) </param>
/// <param name="hitList"> An empty list, acting as accumulator, to which the hits of the union are added. </param>
/// <param name="inside1"> Boolean value indicating whether the current hit is inside shape1 or not.</param>
/// <param name="inside2"> Boolean value indicating whether the current hit is inside shape2 or not.</param>
/// <returns>
/// A list of hitpoints.
/// </returns>
let rec unionHitFunction ray hitTupleList hitList inside1 inside2 =
    match hitTupleList with
    | (id, h) :: hitTupleList ->
        let hitOrthogonal = isOrthogonal ray h
        match id with
        | One ->
            match inside1 with
            | true when inside2 -> unionHitFunction ray hitTupleList hitList hitOrthogonal true
            | true -> unionHitFunction ray hitTupleList (h :: hitList) hitOrthogonal false
            | false when inside2 -> unionHitFunction ray hitTupleList hitList (not hitOrthogonal) true
            | false -> unionHitFunction ray hitTupleList (h :: hitList) (not hitOrthogonal) false
        | Two ->
            match inside1 with
            | true when inside2 -> unionHitFunction ray hitTupleList hitList true hitOrthogonal
            | true -> unionHitFunction ray hitTupleList hitList true (not hitOrthogonal)
            | false when inside2 -> unionHitFunction ray hitTupleList (h :: hitList) false hitOrthogonal
            | false -> unionHitFunction ray hitTupleList (h :: hitList) false (not hitOrthogonal)
    | [] -> hitList

/// <summary>
/// Hitfunction specific to the Subtraction composite.
/// </summary>
/// <param name="ray"> The ray to check for hits.</param>
/// <param name="hitTupleList"> A list containing triples of the type (id,shape,hit).</param>
/// <param name="hitList"> An empty list, acting as accumulator, to which the hits of the union are added.</param>
/// <param name="inside1"> Boolean value indicating whether the current hit is inside shape1 or not.</param>
/// <param name="inside2"> Boolean value indicating whether the current hit is inside shape2 or not.</param>
/// <returns>
/// A list of hitpoints.
/// </returns>
let rec subtractionHitFunction ray hitTupleList hitList inside1 inside2 =
    match hitTupleList with
    | (id, h) :: hitTupleList ->
        let hitOrthogonal = isOrthogonal ray h
        match id with
            | One ->
                match inside1 with
                | true when inside2 -> subtractionHitFunction ray hitTupleList hitList hitOrthogonal true
                | true -> subtractionHitFunction ray hitTupleList (h :: hitList) hitOrthogonal false
                | false when inside2 -> subtractionHitFunction ray hitTupleList hitList (not hitOrthogonal) true
                | false -> subtractionHitFunction ray hitTupleList (h :: hitList) (not hitOrthogonal) false
            | Two ->
                match inside1 with
                | true when inside2 -> subtractionHitFunction ray hitTupleList (h :: hitList) true hitOrthogonal
                | true -> subtractionHitFunction ray hitTupleList (h :: hitList) true (not hitOrthogonal)
                | false when inside2 -> subtractionHitFunction ray hitTupleList hitList false hitOrthogonal
                | false -> subtractionHitFunction ray hitTupleList hitList false (not hitOrthogonal)
    | [] -> hitList

/// <summary>
/// Hitfunction specific to the Intersection composite.
/// </summary>
/// <param name="ray"> The ray to check for hits. </param>
/// <param name="hitTupleList"> A list containing triples of the type (id,shape,hit) </param>
/// <param name="hitList"> An empty list, acting as accumulator, to which the hits of the union are added. </param>
/// <param name="inside1"> Boolean value indicating whether the current hit is inside shape1 or not.</param>
/// <param name="inside2"> Boolean value indicating whether the current hit is inside shape2 or not.</param>
/// <returns>
/// A list of hitpoints.
/// </returns>
let rec intersectionHitFunction ray hitTupleList hitList inside1 inside2 =
    match hitTupleList with
    | (id, h) :: hitTupleList ->
        let hitOrthogonal = isOrthogonal ray h
        match id with
            | One ->
                match inside1 with
                | true when inside2 -> intersectionHitFunction ray hitTupleList (h :: hitList) hitOrthogonal true
                | true -> intersectionHitFunction ray hitTupleList hitList hitOrthogonal false
                | false when inside2 -> intersectionHitFunction ray hitTupleList (h :: hitList) (not hitOrthogonal) true
                | false -> intersectionHitFunction ray hitTupleList hitList (not hitOrthogonal) false
            | Two ->
                match inside1 with
                | true when inside2 -> intersectionHitFunction ray hitTupleList (h :: hitList) true hitOrthogonal
                | true -> intersectionHitFunction ray hitTupleList (h :: hitList) true (not hitOrthogonal)
                | false when inside2 -> intersectionHitFunction ray hitTupleList hitList false hitOrthogonal
                | false -> intersectionHitFunction ray hitTupleList hitList false (not hitOrthogonal)
    | [] -> hitList

/// <summary>
/// Compute an intersection between a ray and a CSG composite shape.
/// </summary>
/// <param name=composite>Composite to check intersection with.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// A list of hits on the shape, if any.
/// </returns>
let intersect (Composite(s1, s2, c)) ray =
    let hitsS1 = Shape.hit ray s1
    let hitsS2 = Shape.hit ray s2
    match c with
    | Group        -> hitsS1 @ hitsS2
    | c ->
        let tupleList = sortToTuples hitsS1 hitsS2
        match c with
        | Union        -> unionHitFunction ray tupleList [] false false
        | Subtraction  -> subtractionHitFunction ray tupleList [] false false
        | _            -> intersectionHitFunction ray tupleList [] false false

/// <summary>
/// Combine the bounds of two shapes into a single boundary.
/// </summary>
/// <param name=b1>The first boundary in the combination.</param>
/// <param name=b2>The second boundary in the combination.<param>
/// <returns>The combined bounds of b1 and b2.</returns>
let combineBounds b1 b2 =
    match (b1, b2) with
    | (None, None)       -> None
    | (Some bound, None) -> Some bound
    | (None, Some bound) -> Some bound
    | (Some (b1, b1w, b1h, b1d), Some (b2, b2w, b2h, b2d)) ->
        let e = Epsilon // local alias
        let ee = e * 2.
        // get coordinates of bounding boxes' starting points
        let (b1x, b1y, b1z), (b2x, b2y, b2z) =
            Point.getCoord b1, Point.getCoord b2
        // get coordinates of higher bounds of the two bounding boxes
        let (b1x2, b1y2, b1z2), (b2x2, b2y2, b2z2) =
            Point.getCoord (Point.move b1 (Vector.make b1w b1h b1d)),
            Point.getCoord (Point.move b2 (Vector.make b2w b2h b2d))

        let lx, ly, lz = min b1x b2x, min b1y b2y, min b1z b2z
        let hx, hy, hz = max b1x2 b2x2, max b1y2 b2y2, max b1z2 b2z2

        let boundsP0 = Point.make (lx - e) (ly - e) (lz - e)

        Some (
            boundsP0,
            abs (hx - lx) + ee,
            abs (hy - ly) + ee,
            abs (hz - lz) + ee
        )

/// <summary>
/// Get the total bounds of a Composite CSG shape.
/// </summary>
/// <param name=composite>Composite to get bounds of.</param>
/// <returns>The bounds of a composite shape.</returns>
let bounds (Composite(s1, s2, _)) =
    combineBounds (Shape.bounds s1) (Shape.bounds s2)

/// <summary>
/// Create a CSG composite shape.
/// </summary>
/// <param name=s1>First shape of the composite.</param>
/// <param name=s2>Second shape of the composite.</param>
/// <param name=c>Composition type.</param>
/// <returns>A CSG composite shape.</returns>
let make s1 s2 c =
    let composite = Composite(s1, s2, c)
    let isSolid = if c = Group then false else true
    Shape.make (intersect composite) isSolid (bounds composite)
