// Learn more about F# at http://fsharp.org

open System
open FSharpPlus
open MathNet.Numerics

type Funcs =
  { source : Complex32 -> Complex32
    diff : Complex32 -> Complex32 }

type Parameter =
  { funcs : Funcs
    limit : float32
    guess : Complex32 }

type NewtonMonad =
  | NM of Parameter
  | Solved of guess: Complex32
  member inline x.Get =
    match x with
    | Solved guess
    | NM {guess=guess} -> guess
  member inline x.IsFinished =
    match x with
    | Solved _ -> true
    | _ -> false
  
  static member inline Return (x, limit, guess) =
    NM { funcs=x; limit=limit; guess=guess }
  static member (>>=) (m, f) =
    match m with
    | NM m -> f m
    | Solved s -> Solved s

module NewtonMonad =

  let limited newGuess param =
    if param.limit > abs ( Complex32.magnitude (newGuess - param.guess) )
    then Solved newGuess
    else NM { param with guess=newGuess }

  let row param =
    let source, diff = param.funcs.source, param.funcs.diff
    let guess = param.guess
    let newGuess = guess - (source guess) / (diff guess)
    param |> limited newGuess

  let inline result (x:^X) =
    (^X: (member Get: Complex32) x)

[<EntryPoint>]
let main argv =
  let inline pow (z,n) = Complex32.powf n z

  let f x = pow(x, 4.f) - 0.1f * x - 1.f
  let diffF x = 4.f * pow(x, 3.f) - 0.1f
  let funcs =
    { source=f
      diff=diffF }
  let limit = 0.0000001f
  let initialPoints =
    [ Complex32(0.f, 1.f)
      Complex32(1.f, 0.f)
      Complex32(-1.f, 0.f)
      Complex32(0.f, -1.f) ]

  let calc initPoint =
    let loop f (n: NewtonMonad) =
      let rec iter f step (n: NewtonMonad) =
        if n.IsFinished || step > 10
        then n
        else iter f (step+1) (n >>= f)
      iter f 0 n

    NewtonMonad.Return (funcs, limit, initPoint)
    |> loop NewtonMonad.row
    
  let result = List.map (fun x -> x, x |> calc |> NewtonMonad.result) 
                         initialPoints 

  List.iter (fun (init: Complex32, result: Complex32) ->
              printfn "init: %.3f+%.3fi => result: %f+%fi."
                init.Real init.Imaginary
                result.Real result.Imaginary )
            result
  0 // return an integer exit code
