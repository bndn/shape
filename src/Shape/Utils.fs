// Copyright (C) 2015 The Authors.
module Shape.Utils

open Point
open Transform
open Vector

let Epsilon = 1.0e-6

/// <summary>
/// Transform each point in a list with some transformation.
/// </summary>
/// <param name=pl>The list of points to map-apply transformations onto.</param>
/// <param name=transformation>The transformation to apply to the points.</param>
/// <returns>A list of transformed points.</returns>
let transformPoints pl transformation =
    List.map (fun p -> p * transformation) pl

/// <summary>
/// Generate a new bounds based on the min and max values from
/// an eight-point box (one point for each corner).
/// </summary>
/// <param name=pl>The corner point list of the box.</param>
/// <returns>A bounding box around the box.</returns>
let generateBounds pl =
    let minx = Point.getX <| List.minBy (fun p -> Point.getX p) pl
    let miny = Point.getY <| List.minBy (fun p -> Point.getY p) pl
    let minz = Point.getZ <| List.minBy (fun p -> Point.getZ p) pl
    let maxx = Point.getX <| List.maxBy (fun p -> Point.getX p) pl
    let maxy = Point.getY <| List.maxBy (fun p -> Point.getY p) pl
    let maxz = Point.getZ <| List.maxBy (fun p -> Point.getZ p) pl

    Point.make minx miny minz,
    abs (maxx - minx), abs (maxy - miny), abs (maxz - minz)

/// <summary>
/// Transforms the boundingbox with a transformation.
/// </summary>
/// <param name=bb>The bounds to transform.</param>
/// <param name=trans>The transformation to apply to the bounds.</param>
/// <returns>The transformed boundary.</returns>
let transformBounds (P0, w, h, d) (transformation : Transformation) =
    // get all 8 corners of the initial boundingbox
    // transform each one and find the min and max x, y and z
    // coordinates to create a new bounds around the transformed shape
    let moveX = Vector.make w  0. 0.
    let moveY = Vector.make 0. h  0.
    let moveZ = Vector.make 0. 0. d
    let mp move = Point.move P0 move
    let corners = [P0; mp moveX; mp moveY; mp moveZ;
                   mp (moveX+moveY); mp (moveY+moveZ);
                   mp (moveX+moveZ); mp (moveX+moveY+moveZ)]

    generateBounds <| transformPoints corners transformation

/// <summary>
/// Calculates the hit distances between a ray and a shape created from
/// a polynomial expression, using values a, b and c from the expression.
/// </summary>
/// <param name=a>Value of a in a quadratic expression.</param>
/// <param name=b>Value of b in a quadratic expression.</param>
/// <param name=c>Value of c in a quadratic expression.</param>
/// <returns>
/// The list of 0 to 2 hit distances from the ray origin to the hitpoints
/// on the shape. Only returns hitpoints that intersect in the positive
/// distance of the ray.
/// </returns>
let distanceQuadratic a b c =
    let D = b**2. - 4.*a*c // discriminant
    match D with
    | D when D < -Epsilon -> []
    | D when D < Epsilon  -> let d = -(b / (2.*a))
                             if d > 0. then [d] else []
    | _ -> let dneg = (-b - sqrt(D)) / (2.*a)
           let dpos = (-b + sqrt(D)) / (2.*a)
           match (dneg > 0., dpos > 0.) with
           | (true, true)   -> [dneg;dpos]
           | (true, false)  -> [dneg]
           | (false, true)  -> [dpos]
           | (false, false) -> []

/// <summary>
/// Check if a ray hits an infinite plane.
/// </summary>
/// <param name=rayO>Origin of the ray.</param>
/// <param name=rayD>Direction of the ray.</param>
/// <param name=p0>Point of origin of the plane.</param>
/// <param name=normal>Normal of the plane.</param>
/// <returns>Some distance, hitpoint tuple if there is a hit, else None.</returns>
let rayPlaneHit rayO rayD p0 normal =
    let rdn = rayD * normal // ray and normal dotproduct
    // If the ray and normals' dotproducts are too close, we do not
    // want to render the hit. We render both sides of the plane.
    if rdn < Epsilon && rdn > -Epsilon then None else

    let t = ((Point.distance rayO p0) * normal) / rdn
    // The hit is behind the ray origin
    if t < 0. then None else

    let hitpoint = Point.move rayO (Vector.multScalar rayD t)
    Some((t, hitpoint))
