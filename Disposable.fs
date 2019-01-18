﻿namespace System

open System
open System.Collections.Generic

/// <summary>
/// Representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
/// </summary>
type CompositeDisposable (?disposables : IDisposable seq) =
    let xs = 
        disposables
        |> Option.map (fun x -> List<IDisposable> (x)) 
        |> Option.defaultValue (List<IDisposable> ())
    /// <summary>
    /// Adds a <see cref="IDisposable" /> implementation to the composite that gets disposed when this instance gets disposed.
    /// </summary>
    member this.Add (x : IDisposable) = xs.Add x; this
    interface IDisposable with
        member __.Dispose () =
            let exns = List<exn> ()
            for x in xs do
                try x.Dispose ()
                with ex -> exns.Add ex
            if exns.Count <> 0 
            then raise (AggregateException (exns.ToArray ()))
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member Create () = new CompositeDisposable ()
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member Create (xs) = new CompositeDisposable (xs)

/// <summary>
/// Exposed functionality for <see cref="IDisposable"/> implementations.
/// </summary>
module Disposable =
    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    let create f = { new IDisposable with member __.Dispose() = f () }
    
    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    let Create (f : Action<_>) = 
        if f = null then nullArg "f"
        create f.Invoke
    
    /// <summary>
    /// Combines the two given <see cref="IDisposable"/> instances into a single instance that disposes both when disposed.
    /// </summary>
    let compose2 (d1 : IDisposable) (d2 : IDisposable) =
        match d1, d2 with
        | :? CompositeDisposable as d, x -> d.Add x
        | d, (:? CompositeDisposable as x) -> x.Add d
        | d, x -> CompositeDisposable.Create [d; x]
        :> IDisposable
    
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    let compose ds = CompositeDisposable.Create ds
    
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    let Compose ([<ParamArray>] ds) = CompositeDisposable.Create (Seq.ofArray ds)

    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    let undoable doFunc undoFunc =
        doFunc ()
        create undoFunc

    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    let Undoable (doFunc : Action) (undoFunc : Action) =
        doFunc.Invoke ()
        create undoFunc.Invoke

/// <summary>
/// Disposable infix operators
/// </summary>
[<AutoOpen>]
module DisposableOp =
    /// <summary>
    /// Combines the two given <see cref="IDisposable"/> instances into a single instance that disposes both when disposed.
    /// </summary>
    let inline (<+>) d1 d2 = Disposable.compose2 d1 d2

[<Extension>]
type DisposableExtensions =
    /// <summary>
    /// Combines the two given <see cref="IDisposable"/> instances into a single instance that disposes both when disposed.
    /// </summary>
    [<Extension>]
    static member And (d1, d2) = 
        if d1 = null then nullArg "d1"
        if d2 = null then nullArg "d2"
        Disposable.compose2 d1 d2
