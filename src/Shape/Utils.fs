// Copyright (C) 2015 The Authors.
module Shape.Utils

open System // for Math.PI
open System.Numerics // for complex numbers

open Point
open Transform
open Vector

let Epsilon = 1.0e-6

/// <summary>
/// Check if float value is between -EPSILON and EPSILON, we can consider it zero.
/// </summary>
/// <param name=f>The float to check.</param>
/// <returns>True if the given float is close to zero, false otherwise.</returns>
let closeToZero f =
    if (f > -Epsilon && f < Epsilon) then true else false

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
/// Get real roots from a list of roots.
/// </summary>
/// <param name="l">List of roots as tuple pairs of the real and imaginary part.</param>
/// <returns>
/// The list of floats containing only real roots.
/// </returns>
let getRealRoot l =
    let rec grr ll acc =
        match ll with
        | (d, i) :: dl when closeToZero i -> grr dl (d :: acc)
        | (_, _) :: dl -> grr dl acc
        | [] -> acc
    grr l List.empty

/// <summary>
/// Get all positive real roots from a list of real roots.
/// </summary>
/// <param name="l">List of real roots.</param>
/// <returns>
/// The list of positive real roots.
/// </returns>
let getPositiveRoot l =
    let rec gnnr ll acc =
        match ll with
        | d :: dl when d > 0. -> gnnr dl (d :: acc)
        | _ :: dl -> gnnr dl acc
        | [] -> acc
    gnnr l List.empty

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
/// Calculates the real and complex roots between a ray and a shape created from
/// a polynomial expression, using values a, b, c and d from the expression.
/// </summary>
/// <see cref="http://www.1728.org/cubic2.htm">Solving Cubic Equations</see>
/// <param name="a">Value of `a` in a polynomial expression.</param>
/// <param name="b">Value of `b` in a polynomial expression.</param>
/// <param name="c">Value of `c` in a polynomial expression.</param>
/// <param name="d">Value of `d` in a polynomial expression.</param>
/// <returns>
/// The list of 0 to 3 roots with real and imaginary part from the ray origin
/// to the hitpoints on the shape.
/// </returns>
let distanceCubic a b c d =
    let cRoot f = if f >= 0. then f**(1.0 / 3.0) else -(-(f)**(1.0 / 3.0))
    let toRad f = f * (Math.PI / 180.)

    let f = ((3. * c / a) - (b**2. / a**2.)) / 3.
    let g = ((2. * b**3. / a**3.) - (9. * b * c / a**2.) + (27. * d / a)) / 27.
    let h = (g**2. / 4.) + (f**3. / 27.)

    if h < 0. then
        let i = ((g**2. / 4.) - h)**(1./2.)
        let j = cRoot i

        let k = acos (-((toRad g) / (2. * (toRad i))))
        let L = j * -1.
        let M = cos (k / 3.)
        let N = sqrt(3.) * sin (k / 3.)
        let P = (b / (3. * a)) * -1.

        let x1 = 2. * j * cos (k / 3.) - (b / (3. * a))
        let x2 = L * (M + N) + P
        let x3 = L * (M - N) + P
        [(x1, 0.0); (x2, 0.0); (x3, 0.0)]
    elif h > 0. then
        let f = ((3.*c / a) - (b**2. / a**2.)) / 3.
        let g = ((2. * b**3. / a**3.) - (9. * b * c / a**2.) + (27. * d / a)) / 27.
        let h = (g**2. / 4.) + (f**3. / 27.)
        let R = -(g / 2.) + (h)**(1./2.)
        let S = cRoot (R)
        let T = -(g / 2.) - (h)**(1./2.)
        let U = cRoot (T)

        let x1 = (S + U) - (b / (3. * a))
        let xC = -(S + U) / 2. - (b / (3. * a))
        let i = (S - U) * sqrt(3.) / 2.
        // 1 real root and 2 complex roots
        [(x1, 0.0); (xC, i); (xC, -i)]
    elif f = 0. && g = 0. && h = 0. then
        let f = ((3. * c / a) - (b**2. / a**2.)) / 3.
        let g = ((2. * b**3. / a**3.) - (9. * b * c / a**2.) + (27. * d / a)) / 27.
        let h = (g**2. / 4.) + (f**3. / 27.)

        let x1 = cRoot(d / a) * -1.
        [(x1, 0.0)]
    else []

/// <summary>
/// Calculates the roots between a ray and a shape created from
/// a polynomial expression, using values a, b, c, d and e from the expression.
/// </summary>
/// <see cref="http://www.1728.org/quartic2.htm">Solving Quartic Equations</see>
/// <param name="a">Value of `a` in a polynomial expression.</param>
/// <param name="b">Value of `b` in a polynomial expression.</param>
/// <param name="c">Value of `c` in a polynomial expression.</param>
/// <param name="d">Value of `d` in a polynomial expression.</param>
/// <param name="e">Value of `e` in a polynomial expression.</param>
/// <returns>
/// The list of 0 to 4 real roots from the ray origin to the hitpoints
/// on the shape.
/// </returns>
let distanceQuartic (ar:float) (br:float) (cr:float) (dr:float) (er:float) =
    if ar = 0. then getRealRoot (distanceCubic br cr dr er)
    else
        let b = br / ar
        let c = cr / ar
        let d = dr / ar
        let e = er / ar

        let f = c - (3. * b**2. / 8.)
        let g = d + (b**3. / 8.) - (b * c / 2.)
        let h = e - (3. * b**4. / 256.) + (b**2. * c / 16.) - (b * d / 4.)
        let roots = distanceCubic 1. (f / 2.) ((f**2. - 4. * h) / 16.) (-((g)**2. / 64.))

        let rec positiveRealRoot l acc =
            match l with
            | (d, i) :: dl when not (closeToZero d) && d > Epsilon && closeToZero i -> positiveRealRoot dl (d :: acc)
            | (d, i) :: dl -> positiveRealRoot dl acc
            | [] -> acc
        let rec complexRoot l acc =
            match l with
            | (d, i) :: dl when not (closeToZero d) && not (closeToZero i) -> complexRoot dl ((d, i) :: acc)
            | (d, i) :: dl -> complexRoot dl acc
            | [] -> acc

        if (List.length roots > 1) then
            let rRoots = positiveRealRoot roots List.empty
            let cRoots = complexRoot roots List.empty
            if (List.length rRoots > 1) then
                let p = sqrt(rRoots.[0])
                let q = sqrt(rRoots.[1])
                let r = -g / (8. * p * q)
                let s = br / (4. * ar)

                let x1 = p + q + r - s
                let x2 = p - q - r - s
                let x3 = -p + q - r - s
                let x4 = -p - q + r - s
                [x1; x2; x3; x4]
            elif (List.length cRoots > 1) then
                let (y1, i1) = cRoots.[0]
                let (y2, i2) = cRoots.[1]

                let p = sqrt (new Complex(y1, i1))
                let q = sqrt (new Complex(y2, i2))
                let pq = p * q
                let r = -g / (8. * pq.Real)
                let s = br / (4. * ar)

                let rc = new Complex(r, 0.0)
                let sc = new Complex(s, 0.0)

                let x1 = p + q + rc - sc
                let x2 = p - q - rc - sc
                let x3 = -p + q - rc - sc
                let x4 = -p - q + rc - sc
                (if (closeToZero x1.Imaginary) then [x1.Real] else []) @ (if (closeToZero x2.Imaginary) then [x2.Real] else []) @ (if (closeToZero x3.Imaginary) then [x3.Real] else []) @ (if (closeToZero x4.Imaginary) then [x4.Real] else [])
            else []
        else []

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
