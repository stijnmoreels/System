namespace System

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading.Tasks

#nowarn "9001"

type IAsyncDisposable =
    inherit IDisposable
    /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
    abstract member DisposeAsync : unit -> Task

type ILifetimeDisposable =
    inherit IDisposable
    /// Setup any application defined tasks.
    abstract member Setup : unit -> unit

type ILifetimeAsyncDisposable =
    inherit IAsyncDisposable
    inherit ILifetimeDisposable
    /// Setup any application defined tasks.
    abstract member SetupAsync : unit -> Task

type IAsyncDisposableFSharp =
    inherit IDisposable
    /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
    abstract member DisposeAsync : unit -> Async<unit>

type ILifetimeAsyncDisposableFSharp =
    inherit IAsyncDisposableFSharp
    inherit ILifetimeDisposable
    /// Setup any application defined tasks.
    abstract member SetupAsync : unit -> Async<unit>

/// <summary>
/// Representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
/// </summary>
type CompositeDisposable ([<ParamArray>] disposables : IDisposable array) =
    /// <summary>
    /// Adds a <see cref="IDisposable" /> implementation to the composite that gets disposed when this instance gets disposed.
    /// </summary>
    member __.Add (x : IDisposable) = new CompositeDisposable (Array.append [|x|] disposables)
    /// Gets the disposables from the composite type.
    member __.ToArray () = disposables
    static member op_Implicit (composite : CompositeDisposable) = composite.ToArray ()

    interface System.Collections.IEnumerable with
        member __.GetEnumerator () = disposables.GetEnumerator ()

    interface ILifetimeDisposable with
        /// Setup any application defined tasks.
        member __.Setup () =
            disposables
            |> Seq.iter (function
              | :? ILifetimeAsyncDisposable as x -> x.SetupAsync().GetAwaiter().GetResult()
              | :? ILifetimeAsyncDisposableFSharp as x -> x.SetupAsync () |> Async.RunSynchronously
              | :? ILifetimeDisposable as x -> x.Setup ()
              | _ -> ())

    interface ILifetimeAsyncDisposable with
        /// Setup any application defined tasks.
        member __.SetupAsync () = async { 
            let! results =
                disposables 
                |> Seq.map (function
                  | :? ILifetimeAsyncDisposable as x -> async { do! x.SetupAsync () |> Async.AwaitTask }
                  | :? ILifetimeAsyncDisposableFSharp as x -> x.SetupAsync ()
                  | :? ILifetimeDisposable as x -> x.Setup (); async.Return ()
                  | _ -> async.Return ())
                |>Seq.map Async.Catch 
                |> Async.Parallel
            let exns =
                results
                |> Seq.choose (function | Choice2Of2 ex -> Some ex | _ -> None)
            if not (Seq.isEmpty exns)
            then raise (AggregateException (Array.ofSeq exns)) } |> Async.StartAsTask :> Task

    interface ILifetimeAsyncDisposableFSharp with
        /// Setup any application defined tasks.
        member __.SetupAsync () = async { 
            let! results =
                disposables 
                |> Seq.map (function
                  | :? ILifetimeAsyncDisposable as x -> async { do! x.SetupAsync () |> Async.AwaitTask }
                  | :? ILifetimeAsyncDisposableFSharp as x -> x.SetupAsync ()
                  | :? ILifetimeDisposable as x -> x.Setup (); async.Return ()
                  | _ -> async.Return ())
                |>Seq.map Async.Catch 
                |> Async.Parallel
            let exns =
                results
                |> Seq.choose (function | Choice2Of2 ex -> Some ex | _ -> None)
            if not (Seq.isEmpty exns)
            then raise (AggregateException (Array.ofSeq exns)) }


    interface IDisposable with
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        member __.Dispose () =
            let exns = List<exn> ()
            for x in disposables do
              try x.Dispose ()
              with ex -> exns.Add ex
            if exns.Count <> 0 
            then raise (AggregateException (exns.ToArray ()))
    
    interface IAsyncDisposable with
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        member __.DisposeAsync () = async { 
          let! results =
            disposables 
            |> Seq.map (function
              | :? IAsyncDisposable as x -> async { do! x.DisposeAsync () |> Async.AwaitTask }
              | :? IAsyncDisposableFSharp as x -> x.DisposeAsync ()
              | x -> async { x.Dispose (); return () })
            |> Seq.map Async.Catch
            |> Async.Parallel
          let exns =
              results |> Seq.choose (function | Choice2Of2 ex -> Some ex | _ -> None)
          if not (Seq.isEmpty exns)
          then raise (AggregateException (Array.ofSeq exns)) } |> Async.StartAsTask :> Task

    interface IAsyncDisposableFSharp with
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        member __.DisposeAsync () = async { 
          let! results =
            disposables 
            |> Seq.map (function
              | :? IAsyncDisposable as x -> async { do! x.DisposeAsync () |> Async.AwaitTask }
              | :? IAsyncDisposableFSharp as x -> x.DisposeAsync ()
              | x -> async { x.Dispose (); return () })
            |> Seq.map Async.Catch
            |> Async.Parallel
          let exns =
              results |> Seq.choose (function | Choice2Of2 ex -> Some ex | _ -> None)
          if not (Seq.isEmpty exns)
          then raise (AggregateException (Array.ofSeq exns)) }
               
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member Create () = new CompositeDisposable ([||])
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member Create (xs : IEnumerable<IDisposable>) = new CompositeDisposable (Array.ofSeq xs)

/// <summary>
/// Exposed functionality for <see cref="IDisposable"/> implementations.
/// </summary>
module Disposable =
    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    let create f = 
        { new IAsyncDisposable with 
            member __.Dispose() = f ()
            member __.DisposeAsync () = Task.Run(Action f) } :> IDisposable

    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    let createAsync f =
        { new IAsyncDisposableFSharp with
            member __.Dispose () = f () |> Async.RunSynchronously
            member __.DisposeAsync () = f () }

    /// <summary>
    /// Creates a disposable without any functionality
    /// </summary>
    [<CompiledName("Empty")>]
    let empty = create id

    /// <summary>
    /// Combines the two given <see cref="IDisposable"/> instances into a single instance that disposes both when disposed.
    /// </summary>
    [<CompiledName("Combine")>]
    let compose2 (d1 : IDisposable) (d2 : IDisposable) =
        match d1, d2 with
        | :? CompositeDisposable as d, x -> d.Add x
        | d, (:? CompositeDisposable as x) -> x.Add d
        | d, x -> CompositeDisposable.Create [d; x]
        :> IDisposable
    
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    let compose (ds : seq<IDisposable>) = CompositeDisposable.Create ds

    /// <summary>
    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    /// </summary>
    let undoable doFunc undoFunc =
        { new ILifetimeDisposable with
            member __.Setup () = doFunc ()
            member __.Dispose () = undoFunc () }
        
    /// <summary>
    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    /// </summary>
    let undoableAsync doFunc undoFunc =
        { new ILifetimeAsyncDisposableFSharp with
            member __.SetupAsync () = doFunc ()
            member __.Setup () = doFunc() |> Async.RunSynchronously
            member __.DisposeAsync () = undoFunc ()
            member __.Dispose () = undoFunc() |> Async.RunSynchronously }

    /// <summary>
    /// Adds a disposable instance to the composite disposable
    /// </summary>
    [<CompiledName("Add")>]
    let add d (state : CompositeDisposable) =
        state.Add d
    
    /// <summary>
    /// Adds a setup function to the composite disposable.
    /// </summary>
    let setup f state =
        add (undoable f id) state

    /// <summary>
    /// Adds a setup function to the composite disposable.
    /// </summary>
    let setupAsync f state =
        add (undoableAsync f async.Return) state

    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    let tearDown f state =
        add (create f) state

    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    let tearDownAsync f state =
        add (createAsync f) state

    /// Runs the `Setup` of the given lifetime disposable.
    let run (state : ILifetimeDisposable) =
        state.Setup ()
    
    /// Runs the `SetupAsync` of the given lifetime disposable
    let runAsync (state : ILifetimeAsyncDisposableFSharp) =
        state.SetupAsync ()

    /// Runs the '`Dispose` of the given disposable.
    [<CompiledName("Dispose")>]
    let dispose (d : IDisposable) = 
        d.Dispose()

    /// Runs the `DisposeAsync` of the given disposable.
    let disposeAsync (d : IAsyncDisposableFSharp) =
        d.DisposeAsync ()

    /// Disposes all the given disposables safely.
    [<CompiledName("DisposeAll")>]
    let disposeAll (disposables : IDisposable seq) =
        let exns = List<exn> ()
        for x in disposables do
          try x.Dispose ()
          with ex -> exns.Add ex
        if exns.Count <> 0 
        then raise (AggregateException (exns.ToArray ()))

    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    let Create (f : Action) = 
        if f = null then nullArg "f"
        create f.Invoke

    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    let CreateAsync (f : Func<Task>) =
        if f = null then nullArg "f"
        { new IAsyncDisposable with
            member __.Dispose () = f.Invoke().GetAwaiter().GetResult() 
            member __.DisposeAsync () = f.Invoke() }

    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    let Compose ([<ParamArray>] ds) = 
        if ds = null then nullArg "ds"
        CompositeDisposable.Create (Seq.ofArray ds)

    /// <summary>
    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    /// </summary>
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    let Undoable (doFunc : Action) (undoFunc : Action) =
        if doFunc = null then nullArg "doFunc"
        if undoFunc = null then nullArg "undoFunc"
        undoable doFunc.Invoke undoFunc.Invoke

 /// <summary>
    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    /// </summary>
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    let UndoableAsync (doFunc : Func<Task>) (undoFunc : Func<Task>) =
        if doFunc = null then nullArg "doFunc"
        if undoFunc = null then nullArg "undoFunc"
        { new ILifetimeAsyncDisposable with
            member __.SetupAsync () = doFunc.Invoke() 
            member __.Setup () = doFunc.Invoke().GetAwaiter().GetResult()
            member __.DisposeAsync () = undoFunc.Invoke()
            member __.Dispose () = undoFunc.Invoke().GetAwaiter().GetResult() }

type DisposableBuilder () =
    /// Adds a setup function to the composite disposable.
    [<CustomOperation("setup")>]
    member __.Setup (state, f) = Disposable.setup f state
    /// Adds a setup function to the composite disposable.
    [<CustomOperation("setupAsync")>]
    member __.SetupAsync (state, f) = Disposable.setupAsync f state
    /// <summary>
    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    /// </summary>
    [<CustomOperation("undoable")>]
    member __.Undo (state, createF, disposeF) =
        Disposable.add (Disposable.undoable createF disposeF) state
    /// <summary>
    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    /// </summary>
    [<CustomOperation("undoableAsync")>]
    member __.UndoAsync (state, createF, disposeF) =
        Disposable.add (Disposable.undoableAsync createF disposeF) state
    /// Adds a disposable instance to the composite disposable
    [<CustomOperation("add")>]
    member __.Add (state, disposable) = Disposable.add disposable state
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    [<CustomOperation("tearDown")>]
    member __.TearDown (state, f) = Disposable.tearDown f state
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    [<CustomOperation("tearDownAsync")>]
    member __.TearDownAsync (state, f) = Disposable.tearDownAsync f state
    member __.Yield (_) =
        Disposable.compose []

/// <summary>
/// Disposable infix operators
/// </summary>
[<AutoOpen>]
module DisposableOp =
    /// <summary>
    /// Combines the two given <see cref="IDisposable"/> instances into a single instance that disposes both when disposed.
    /// </summary>
    let inline (<+>) d1 d2 = Disposable.compose2 d1 d2

    let disposable = DisposableBuilder ()

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

    /// <summary>
    /// Adds a setup function to the composite disposable.
    /// </summary>
    [<Extension>]
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    static member AddSetup (state : CompositeDisposable) f =
        if f = null then nullArg "f"
        state.Add (Disposable.Undoable f (Action id))

    /// <summary>
    /// Adds a setup function to the composite disposable.
    /// </summary>
    [<Extension>]
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    static member AddSetupAsync (state : CompositeDisposable) (f : Func<Task>) =
        if f = null then nullArg "f"
        state.Add (Disposable.UndoableAsync f (Func<_> (fun () -> Task.CompletedTask)))

    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    [<Extension>]
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    static member AddTearDown (state : CompositeDisposable) (f : Action) =
        if f = null then nullArg "f"
        state.Add (Disposable.Create f)

    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    [<Extension>]
    [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
    static member AddTearDownAsync (state : CompositeDisposable) (f : Func<Task>) =
        if f = null then nullArg "f"
        state.Add (Disposable.CreateAsync f)
