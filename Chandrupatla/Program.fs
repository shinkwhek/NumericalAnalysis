// Learn more about F# at http://fsharp.org

open System
open FSharpPlus
open MathNet.Numerics

type Point =
  { x: float
    y: float }

type ThreePoints =
  { p1: Point
    p2: Point
    p3: Point }

type Parametar =
  { limit : float
    threePoints : ThreePoints
    func : float -> float }

type ChandrupatleMonad =
  | CM of Parametar
  | Solved of float
  member inline x.Get =
    match x with
    | Solved s -> s
    | CM { threePoints={ p3={ x=x } } } -> x
  member inline x.IsFinished =
    match x with
    | Solved _ -> true
    | _ -> false

  static member inline Return (func, limit, threePoints) =
    CM { func=func; limit=limit; threePoints=threePoints }
  static member (>>=) (m, f) =
    match m with
    | CM m -> f m
    | Solved s -> Solved s

module ChandrupatleMonad =
  let initialize func limit x1 x2 =
    let p1, p2 = {x=x1; y=func x1}, {x=x2; y=func x2}
    let x3 = (x1+x2)/2.
    let p3 = {x=x3; y=func x3}
    let threePoints = { p1=p1; p2=p2; p3=p3}
    ChandrupatleMonad.Return (func, limit, threePoints)

  let updateThreePoints newPointX param =
    let func = param.func
    let threePoints = param.threePoints
    let newP = { x=newPointX; y=func newPointX }
    let threePoints = { p1=threePoints.p2
                        p2=threePoints.p3
                        p3=newP }
    CM { param with threePoints = threePoints }

  let limited newPointX param =
    let x3 = param.threePoints.p3.x
    if param.limit > abs (newPointX - x3)
    then Solved x3
    else updateThreePoints newPointX param

  let method param =
    let threePoints = param.threePoints
    let lagrange { p1=p1; p2=p2; p3=p3 } =
      p3.x/( (p3.y/p1.y-1.)*(p3.y/p2.y-1.) ) +
      p2.x/( (p2.y/p3.y-1.)*(p2.y/p1.y-1.) ) +
      p1.x/( (p1.y/p2.y-1.)*(p1.y/p3.y-1.) )
    let newPointX = lagrange threePoints
    param |> limited newPointX

  let calc param =
    let loop f (n: ChandrupatleMonad) =
      let rec iter f step (n: ChandrupatleMonad) =
        if n.IsFinished || step > 10
        then n
        else iter f (step+1) (n >>= f)
      iter f 0 n
    
    loop method param

  let inline result (x:^X) =
    (^X: (member Get: float) x)

[<EntryPoint>]
let main argv =

    let f x = x - Math.Pow(Math.E, -x)
    let initialX1, initialX2 = 2., 0.5
    let limit = 0.0000001

    let result =
      ChandrupatleMonad.initialize f limit initialX1 initialX2
      |> ChandrupatleMonad.calc
      |> ChandrupatleMonad.result

    printfn "solved: %f" result
    0 // return an integer exit code
