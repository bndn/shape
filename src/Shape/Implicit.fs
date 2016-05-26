// Copyright (C) 2015 The Authors.
module Shape.Implicit

open System // for Math.PI
open System.Numerics // for complex numbers

open Utils
open Texture
open Expression
open Polynomial

[<NoComparison>]
type Implicit = Implicit of string * (Expr * Expr * Expr) * Poly

/// <summary>
/// Creates a hitpoint on a 2nd degree polynomial, given a distance, a ray direction
/// a ray origin and the texture.
/// </summary>
/// <param name=d>The distance between rayO and the polynomial's surface.</param>
/// <param name=rayV>The direction of the ray (vector).</param>
/// <param name=rayO>The origin of the ray.</param>
/// <param name=t>The texture for the polynomial.</param>
/// <returns>
/// A hitpoint for the polynomial, with a distance, the inverse vector
/// (going outwards from the polynomial surface) and the material for the
/// hitpoint.
/// </returns>
let implicitDeterminer d rayV rayO t (x, y, z) =
    let hitPoint = Point.move rayO (Vector.multScalar rayV (d))
    let (hpx, hpy, hpz) = Point.getCoord hitPoint
    let dMap = Map.ofList [("x", hpx); ("y", hpy); ("z", hpz)]

    let n = Vector.make (Polynomial.evaluateExpr dMap x) (Polynomial.evaluateExpr dMap y) (Polynomial.evaluateExpr dMap z) |> Vector.normalise
    let (nx, ny, nz) = Vector.getCoord n

    let theta = acos ny      // angle in y-space
    let phi' = atan2 nx nz
    let phi = if phi' < 0.   // angle in x- and z-space
              then phi' + 2. * Math.PI
              else phi'

    let u = phi / (2. * Math.PI)   // u coordinate in texture space
    let v = 1. - (theta / Math.PI) // v coordinate in texture space

    let material = Texture.getMaterial u v t // material to return

    Hit(d, n, material)

/// <summary>
/// Compute an intersection between a ray and an implicit shape.
/// </summary>
/// <param name=Implicit>The implicit shape.</param>
/// <param name=r>Ray to check intersection with.</param>
/// <returns>
/// A list of intersections of the ray and implicit surface. Empty if the ray
/// does not hit the surface.
/// </returns>
let intersect (Implicit(s, (derX, derY, derZ), poly)) r t =
    let rayOrigin, rayVector = Ray.getOrigin r, Ray.getVector r
    let dx, dy, dz = Vector.getCoord rayVector
    let ox, oy, oz = Point.getCoord rayOrigin

    let map = match poly with | P(m) -> m

    // Find highest degree in polynomial
    let degree = map |> Map.toList |> List.maxBy fst |> fst

    let rayMap = Map.ofList [("ox", ox);("oy", oy);("oz", oz);("dx", dx);("dy", dy);("dz", dz)]

    match degree with
    | 1 ->
        let a = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 1 map then Map.find 1 map else [[]])
        let b = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 0 map then Map.find 0 map else [[]])

        let distance = ((-1.0 * b) / a);

        if distance > -Epsilon then [implicitDeterminer distance rayVector rayOrigin t (derX, derY, derZ)] else List.empty
    | 2 ->
        let a = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 2 map then Map.find 2 map else [[]])
        let b = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 1 map then Map.find 1 map else [[]])
        let c = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 0 map then Map.find 0 map else [[]])
        let distances = distanceQuadratic a b c

        List.fold (fun acc hp -> (implicitDeterminer hp rayVector rayOrigin t (derX, derY, derZ)) :: acc) [] distances
    | 3 ->
        let a = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 3 map then Map.find 3 map else [[]])
        let b = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 2 map then Map.find 2 map else [[]])
        let c = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 1 map then Map.find 1 map else [[]])
        let d = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 0 map then Map.find 0 map else [[]])
        let distances = getPositiveRoot(getRealRoot(distanceCubic a b c d))

        List.fold (fun acc hp -> (implicitDeterminer hp rayVector rayOrigin t (derX, derY, derZ)) :: acc) [] distances
    | 4 ->
        let a = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 4 map then Map.find 4 map else [[]])
        let b = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 3 map then Map.find 3 map else [[]])
        let c = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 2 map then Map.find 2 map else [[]])
        let d = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 1 map then Map.find 1 map else [[]])
        let e = Polynomial.evaluateAtomGroup rayMap (if Map.containsKey 0 map then Map.find 0 map else [[]])
        let distances = getPositiveRoot(distanceQuartic  a b c d e)

        List.fold (fun acc hp -> (implicitDeterminer hp rayVector rayOrigin t (derX, derY, derZ)) :: acc) [] distances
    | n when n > 4 ->
        let p = Map.fold (fun state key value -> (Polynomial.evaluateAtomGroup rayMap value) :: state) [] map

        let distances = (Polynomial.findRoots 10. 0. 0.000001 p)

        List.fold (fun acc hp -> (implicitDeterminer hp rayVector rayOrigin t (derX, derY, derZ)) :: acc) [] distances
    | _ -> failwith "Invalid polynomial"

/// <summary>
/// Make an implicit shape with string expression 's' and no texture.
/// </summary>
/// <param name=s>String expression.</param>
/// <returns>
/// The implicit shape object solved in terms of variable 't'.
/// </returns>
let make s =
    // Check that the expression parses
    let ex' = Expression.parse ("ox + t * dx")
    let ey' = Expression.parse ("oy + t * dy")
    let ez' = Expression.parse ("oz + t * dz")
    let baseExpr = Expression.parse s

    // Parse string to expr and substitute x, y, z
    let expr = List.fold Polynomial.subst baseExpr [("x",ex');("y",ey');("z",ez')]

    // Get derivative of x, y, z respectively
    let derivativeX = Polynomial.derivative "x" baseExpr
    let derivativeY = Polynomial.derivative "y" baseExpr
    let derivativeZ = Polynomial.derivative "z" baseExpr

    let poly = Polynomial.parse "t" expr

    let i = Implicit(s, (derivativeX, derivativeY, derivativeZ), poly)

    Shape.makeBaseShape (intersect i) false (None)

