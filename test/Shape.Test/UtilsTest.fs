module Shape.Utils.Test

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Solving 3rd degree equations (with complex roots)`` () =
    let r1 = distanceCubic 2. -4. -22. 24.
    let ro1, i1 = r1.[0]
    let ro2, i2 = r1.[1]
    let ro3, i3 = r1.[2]
    (closeToZero(ro1 - 4.0) && closeToZero(i1))       |> should equal true
    (closeToZero(ro2 + 3.0) && closeToZero(i2))       |> should equal true
    (closeToZero(ro3 - 1.0) && closeToZero(i3))       |> should equal true
    List.length r1                                    |> should equal 3

    let r2 = distanceCubic 3. -10. 14. 27.
    let ro4, i4 = r2.[0]
    let ro5, i5 = r2.[1]
    let ro6, i6 = r2.[2]
    (closeToZero(ro4 + 1.0) && closeToZero(i4))       |> should equal true
    (closeToZero(ro5 - 2.166666667) && closeToZero(i5 - 2.074983266)) |> should equal true
    (closeToZero(ro6 - 2.166666667) && closeToZero(i6 + 2.074983266)) |> should equal true
    List.length r2                                    |> should equal 3
//
    let r3 = distanceCubic 1. 6. 12. 8.
    let ro7, i7 = r3.[0]
    (closeToZero(ro7 + 2.0) && closeToZero(i7))       |> should equal true
    List.length r3                                    |> should equal 1

[<Fact>]
let ``Solving 4th degree equations (no complex roots)`` () =
    let r1 = distanceQuartic  3. 6. -123. -126. 1080.
    closeToZero(r1.[0] - 5.0)                         |> should equal true
    closeToZero(r1.[1] + 4.0)                         |> should equal true
    closeToZero(r1.[2] - 3.0)                         |> should equal true
    closeToZero(r1.[3] + 6.0)                         |> should equal true
    List.length r1                                    |> should equal 4

    let r2 = distanceQuartic  -20. 5. 17. -29. 87.
    closeToZero(r2.[0] - 1.48758311)                  |> should equal true
    closeToZero(r2.[1] + 1.682003927)                 |> should equal true
    List.length r2                                    |> should equal 2
