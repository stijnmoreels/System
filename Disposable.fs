namespace System

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading.Tasks

#nowarn "9001"

type ILifetimeDisposable =
    inherit IDisposable
    /// Setup any application defined tasks.
    abstract member Setup : unit -> unit

type ILifetimeAsyncDisposable =
    inherit IAsyncDisposable
    /// Setup any application defined tasks.
    abstract member SetupAsync : unit -> Task

type IAsyncDisposableFSharp =
    /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
    abstract member disposeAsync : unit -> Async<unit>

type ILifetimeAsyncDisposableFSharp =
    inherit IAsyncDisposableFSharp
    /// Setup any application defined tasks.
    abstract member setupAsync : unit -> Async<unit>

[<AutoOpen>]
module AsyncBuilderTypeExtensions =
  type AsyncBuilder with
    member __.Bind (comp : Task<_>, binder) = async {
        let! x = comp |> Async.AwaitTask
        return! binder x }
    
    member __.Bind (comp : Task, binder : unit -> Async<unit>) = async {
        let! _ = comp |> Async.AwaitTask
        do! binder () }
    
    member __.Bind (comp : ValueTask<_>, binder) = async {
      let! x = comp.AsTask() |> Async.AwaitTask
      return! binder x }

    member __.Bind (comp : ValueTask, binder : unit -> Async<unit>) = async {
        let! _ = comp.AsTask() |> Async.AwaitTask
        do! binder () }

type Disposal =
  | Sync of IDisposable
  | Async of IAsyncDisposable
  | FAsync of IAsyncDisposableFSharp with
    static member toObj = function
      | Sync x -> x :> obj
      | Async x -> x :> obj
      | FAsync x -> x :> obj

/// <summary>
/// Representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
/// </summary>
type CompositeDisposable ([<ParamArray>] disposables : Disposal array) =
    let disposables = List<Disposal> (disposables)
    /// <summary>
    /// Adds a <see cref="IDisposable" /> implementation to the composite that gets disposed when this instance gets disposed.
    /// </summary>
    member this.Add (x : IDisposable) = disposables.Add (Sync x); this
    /// <summary>
    /// Adds a <see cref="IDisposable" /> implementation to the composite that gets disposed when this instance gets disposed.
    /// </summary>
    member this.Add (x : IAsyncDisposable) = disposables.Add (Async x); this
    /// <summary>
    /// Adds a <see cref="IDisposable" /> implementation to the composite that gets disposed when this instance gets disposed.
    /// </summary>
    member this.Add (x : #IAsyncDisposableFSharp) = disposables.Add (FAsync x); this
    /// Gets the disposables from the composite type.
    member __.ToArray () = disposables.ToArray ()
    static member op_Implicit (composite : CompositeDisposable) = composite.ToArray ()

    interface System.Collections.IEnumerable with
        member __.GetEnumerator () = (Seq.map Disposal.toObj disposables).GetEnumerator () :> Collections.IEnumerator

    member __.Setup () = 
      disposables
      |> Seq.map Disposal.toObj
      |> Seq.iter (function
        | :? ILifetimeDisposable as x -> x.Setup ()
        | _ -> ())
    member __.Dispose () =
      let exns = List<exn> ()
      for x in Seq.map Disposal.toObj disposables do
        try match x with
            | :? IDisposable as x -> x.Dispose ()
            | _ -> ()
        with ex -> exns.Add ex
      if exns.Count <> 0 
      then raise (AggregateException (exns.ToArray ()))

    member __.setupAsync () = async { 
      let! results =
          disposables 
          |> Seq.map Disposal.toObj
          |> Seq.map (function
            | :? ILifetimeAsyncDisposable as x -> async { do! x.SetupAsync () }
            | :? ILifetimeAsyncDisposableFSharp as x -> x.setupAsync ()
            | :? ILifetimeDisposable as x -> x.Setup (); async.Return ()
            | _ -> async.Return ())
          |>Seq.map Async.Catch 
          |> Async.Sequential
      let exns =
          results
          |> Seq.choose (function | Choice2Of2 ex -> Some ex | _ -> None)
      if not (Seq.isEmpty exns)
      then raise (AggregateException (Array.ofSeq exns)) }
    member __.disposeAsync () = async { 
      let! results =
        disposables 
        |> Seq.map Disposal.toObj
        |> Seq.map (function
          | :? IAsyncDisposable as x -> async { do! x.DisposeAsync () }
          | :? IAsyncDisposableFSharp as x -> x.disposeAsync ()
          | :? IDisposable as x -> async { x.Dispose (); return () }
          | _ -> async.Return ())
        |> Seq.map Async.Catch
        |> Async.Sequential
      let exns =
          results |> Seq.choose (function | Choice2Of2 ex -> Some ex | _ -> None)
      if not (Seq.isEmpty exns)
      then raise (AggregateException (Array.ofSeq exns)) }

    member this.SetupAsync () = this.setupAsync () |> Async.StartAsTask :> Task
    member this.DisposeAsync () = this.disposeAsync () |> Async.StartAsTask |> ValueTask

    interface ILifetimeDisposable with
        /// Setup any application defined tasks.
        member this.Setup () = this.Setup ()
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        member this.Dispose () = this.Dispose ()

    interface ILifetimeAsyncDisposableFSharp with
        /// Setup any application defined tasks.
        member this.setupAsync () = this.setupAsync ()
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        member this.disposeAsync () = this.disposeAsync ()

    interface ILifetimeAsyncDisposable with
        /// Setup any application defined tasks.
        member this.SetupAsync () = this.SetupAsync ()
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        member this.DisposeAsync () = this.DisposeAsync ()

    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member Create () = new CompositeDisposable ([||])
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member Create (xs : IEnumerable<IDisposable>) = new CompositeDisposable (Seq.map Sync xs |>  Array.ofSeq)
    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member Create (xs : IEnumerable<IAsyncDisposable>) = new CompositeDisposable (Seq.map Async xs |>  Array.ofSeq)

/// <summary>
/// Exposed functionality for <see cref="IDisposable"/> implementations.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Disposable =
    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    let create f = 
        { new IDisposable with 
            member __.Dispose() = f () }

    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    let createAsync f =
        { new IAsyncDisposableFSharp with
            member __.disposeAsync () = f () }

    /// <summary>
    /// Creates a disposable without any functionality
    /// </summary>
    let empty = create id

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
            member __.setupAsync () = doFunc ()
            member __.disposeAsync () = undoFunc () }

    /// Adds a disposable instance to the composite disposable
    let add (d : IDisposable) (state : CompositeDisposable) =
        state.Add d

    /// Adds a disposable instance to the composite disposable
    let addAsync (d : IAsyncDisposableFSharp) (state : CompositeDisposable) =
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
        addAsync (undoableAsync f async.Return) state

    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    let tearDown f state =
        add (create f) state

    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    let tearDownAsync f state =
        addAsync (createAsync f) state

    /// Runs the `Setup` of the given lifetime disposable.
    let run (state : ILifetimeDisposable) =
        state.Setup ()
    
    /// Runs the `SetupAsync` of the given lifetime disposable
    let runAsync (state : ILifetimeAsyncDisposableFSharp) =
        state.setupAsync ()

    /// Runs the '`Dispose` of the given disposable.
    let dispose (d : IDisposable) = 
        d.Dispose()

    /// Runs the `DisposeAsync` of the given disposable.
    let disposeAsync (d : IAsyncDisposableFSharp) =
        d.disposeAsync ()

    /// Disposes all the given disposables safely.
    let disposeAll (disposables : IDisposable seq) =
        let exns = List<exn> ()
        for x in disposables do
          try x.Dispose ()
          with ex -> exns.Add ex
        if exns.Count <> 0 
        then raise (AggregateException (exns.ToArray ()))

    /// Disposes all the given disposables safely.
    let disposeAllAsync (disposables : IAsyncDisposableFSharp seq) = async {
      if isNull disposables then nullArg "disposables"
      let exns = List<exn> ()
      for x in disposables do
        try do! x.disposeAsync ()
        with ex -> exns.Add ex
      if exns.Count <> 0
      then raise (AggregateException (exns.ToArray ())) }

[<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
type Disposable private () =
    /// <summary>
    /// Creates a disposable without any functionality
    /// </summary>
    static member Empty = Disposable.empty
    /// Runs the '`Dispose` of the given disposable.
    static member Dispose (disposable : IDisposable) =
      if isNull disposable then nullArg "disposable"
      disposable.Dispose ()
    /// Disposes all the given disposables safely.
    static member DisposeAll (disposables : IDisposable seq) =
      if isNull disposables then nullArg "disposables"
      Disposable.disposeAll disposables
    /// Disposes all the given disposables safely.
    static member DisposeAllAsync (disposables : IAsyncDisposable seq) = 
      async {
        if isNull disposables then nullArg "disposables"
        let exns = List<exn> ()
        for x in disposables do
          try do! x.DisposeAsync ()
          with ex -> exns.Add ex
        if exns.Count <> 0
        then raise (AggregateException (exns.ToArray ())) }
      |> Async.StartAsTask
      |> ValueTask
    /// <summary>
    /// Combines the two given <see cref="IDisposable"/> instances into a single instance that disposes both when disposed.
    /// </summary>
    static member Combine (d1, d2) = 
      if isNull d1 then nullArg "d1"
      if isNull d2 then nullArg "d2"
      Disposable.compose2 d1 d2

    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    static member Create (f : Action) = 
        if f = null then nullArg "f"
        Disposable.create f.Invoke

    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    static member CreateAsync (f : Func<ValueTask>) =
      if f = null then nullArg "f"
      { new IAsyncDisposable with
          member __.DisposeAsync () = f.Invoke() }

    /// <summary>
    /// Creates a <see cref="IDisposable" /> implementation that runs the specified function when disposed.
    /// </summary>
    static member CreateAsync (f : Func<Task>) =
      if f = null then nullArg "f"
      { new IAsyncDisposable with
          member __.DisposeAsync () = ValueTask (f.Invoke()) }

    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member Compose ([<ParamArray>] ds : IDisposable []) = 
      if ds = null then nullArg "ds"
      CompositeDisposable.Create (Seq.ofArray ds)

    /// <summary>
    /// Creates a representation of a composite of <see cref="IDisposable" /> implementations that disposes all in the composite when disposed.
    /// </summary>
    static member ComposeAsync ([<ParamArray>] ds : IAsyncDisposable []) = 
      if ds = null then nullArg "ds"
      CompositeDisposable.Create (Seq.ofArray ds)

    /// <summary>
    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    /// </summary>
    static member Undoable (doFunc : Action) (undoFunc : Action) =
      if isNull doFunc then nullArg "doFunc"
      if isNull undoFunc then nullArg "undoFunc"
      Disposable.undoable doFunc.Invoke undoFunc.Invoke

    /// <summary>
    /// Creates a undoable operation by first running the specified <paramref cref="doFunc"/> 
    /// and running the other specified <paramref cref="undoFunc"/> when the returned disposable gets disposed.
    /// </summary>
    static member UndoableAsync (doFunc : Func<Task>) (undoFunc : Func<ValueTask>) =
      if isNull doFunc then nullArg "doFunc"
      if isNull undoFunc then nullArg "undoFunc"
      { new ILifetimeAsyncDisposable with
          member __.SetupAsync () = doFunc.Invoke() 
          member __.DisposeAsync () = undoFunc.Invoke() }

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
        Disposable.addAsync (Disposable.undoableAsync createF disposeF) state
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
[<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
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
    static member AddSetup (state : CompositeDisposable) f =
        if f = null then nullArg "f"
        state.Add (Disposable.Undoable f (Action id))
    /// <summary>
    /// Adds a setup function to the composite disposable.
    /// </summary>
    [<Extension>]
    static member AddSetupAsync (state : CompositeDisposable) (f : Func<Task>) =
        if f = null then nullArg "f"
        state.Add (Disposable.UndoableAsync f (Func<_> (fun () -> ValueTask ())))
    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    [<Extension>]
    static member AddTearDown (state : CompositeDisposable) (f : Action) =
        if f = null then nullArg "f"
        state.Add (Disposable.Create f)
    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    [<Extension>]
    static member AddTearDownAsync (state : CompositeDisposable, f : Func<ValueTask>) =
        if f = null then nullArg "f"
        state.Add (Disposable.CreateAsync f)
    /// <summary>
    /// Adds a function that runs when the `Dispose` is called on the composite disposable.
    /// </summary>
    [<Extension>]
    static member AddTearDownAsync (state : CompositeDisposable, f : Func<Task>) =
        if f = null then nullArg "f"
        state.Add (Disposable.CreateAsync (Func<_> (f.Invoke >> ValueTask)))
