namespace Microsoft.FSharp.Core

open System

/// Computation expression for the `Option<_>` type.
type OptionBuilder internal () =
    member __.Return (x) = Some x
    member __.ReturnFrom (m: 'T option) = m
    member __.Bind (option, binder) = Option.bind binder option
    member __.Zero () = None
    member __.Combine (option, binder) = Option.bind binder option
    member __.Delay (f: unit -> _) = f
    member __.Run (f) = f ()
    member this.TryWith (option, handler) =
        try this.ReturnFrom (option)
        with ex -> handler ex
    member this.TryFinally (option, compensation) =
        try this.ReturnFrom (option)
        finally compensation ()
    member this.Using (res : #IDisposable, body) =
        this.TryFinally (body res, fun () -> match res with null -> () | disp -> disp.Dispose())
    member this.While (guard, f) =
        if not (guard()) then Some () else 
        do f () |> ignore
        this.While (guard, f)
    member this.For (sequence : #seq<_>, body) =
        this.Using (
            sequence.GetEnumerator (),
            fun enum -> this.While (enum.MoveNext, this.Delay (fun () -> body enum.Current)))
 
/// Automatic exposed values for the `Option<_>` type.
[<AutoOpen>]
module OptionExposure =
    /// Gets the computation expression.
    let option = OptionBuilder ()
