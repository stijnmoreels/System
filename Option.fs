namespace Microsoft.FSharp.Core

open System
open System.Runtime.CompilerServices

/// Additional operations on the `Option<_>` type.
module Option =

  /// Maps to a fixed `Some` value `x` when the option is `Some x`.
  let mapTo value = function
    | Some _ -> Some value
    | None -> None

  /// Lifts a two argument function to work with option types.
  let lift2 f xOption yOption =
    match xOption, yOption with
    | Some x, Some y -> Some (f x y)
    | _ -> None

  /// Applies a function `f` in `Some f` to a `x` in `Some x` when both are `Some` values.  
  let apply fOption xOption =
    lift2 (fun f x -> f x) fOption xOption

  /// Traverse over a sequence running a given mapping function over the elements, 
  /// collecting the outcomes into an `option`.
  let traverseSeq f xs =
    let consR = Some (fun h t -> seq { yield h; yield! t })
    Seq.foldBack
      (fun x acc -> apply (apply consR (f x)) acc)
      xs
      (Some Seq.empty)
    
  /// Transorms a sequence of `option`s into an `option` of a sequence.
  let sequenceSeq xs = traverseSeq id xs

  /// Traverse over a list running a given mapping function over the elements, 
  /// collecting the outcomes into an `option`.
  let traverse f xs =
    let consR = Some (fun h t -> h :: t)
    List.foldBack
      (fun x acc -> apply (apply consR (f x)) acc)
      xs
      (Some [])

  /// Transforms a list of results into an `option` of a list.
  let sequence xs = traverse id xs

  /// Runs a function with the given `option` is `None`.
  let iterNone ifNone x =
    match x with
    | None -> ifNone ()
    | _ -> ()

  /// Runs a given function, mapping the outcome to a `Some` value 
  /// but handling the possible thrown exception with a given `handler` and return a `None` value.
  let catch f handler x =
    try Some (f x)
    with ex -> handler ex; None

  /// Catches the `Option.map` function by handling the exception and return a `None` value.
  let catchMap f handler x =
    try Option.map f x
    with ex -> handler ex; None

  /// Catches the `Option.bind` function by handling the exception and return a `None` value.
  let catchBind f handler x =
    try Option.bind f x
    with ex -> handler ex; None

  /// Catches the `Option.iter` function by handling the exception.
  let catchIter f handler x =
    try Option.iter f x
    with ex -> handler ex

  /// Switch a function as a function that returns a `Some` result value.
  let switch f = f >> Some

  /// Transforms a result to an `option`, discarding the `Error` value of the result.
  let ofResult result =
    match result with
    | Ok x -> Some x
    | _ -> None

  /// Transforms an `option` to a result, using the given `error` when the option is `None`.
  let toResult error xOption = 
    Option.fold (fun _ x -> Ok x) (Error error) xOption

  /// Transforms an `option` to a result, using the given `ifNone` function when the option is `None`.
  let toResultWith ifNone xOption =
    match xOption with
    | Some x -> Ok x
    | None -> Error (ifNone ())

  /// Option operators on the option type.
  module Operators =
    let (>>=) m f = Option.bind f m
    let (>=>) f g = f >> Option.bind g
    let (<!>) m f = Option.map f m
    let (<*>) = apply

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
module OptionBuilder =

  /// Gets the computation expression.
  let option = OptionBuilder ()

/// Add C# extensions for the option type.
[<Extension>]
type OptionExtensions () =
  /// Transforms the optional value to antother type.
  [<Extension>]
  static member Select (option : Option<'T>, selector : Func<'T, 'TResult>) =
    if isNull selector then nullArg "selector"
    Option.map selector.Invoke option
  /// Transforms the optional value to another optional value.
  [<Extension>]
  static member SelectMany (option : Option<'T>, selector : Func<'T, Option<'TResult>>) =
    if isNull selector then nullArg "selector"
    Option.bind selector.Invoke option
  /// Filters the optional value for a give predicate, and return the `None` value if the predicate doesn't hold.
  [<Extension>]
  static member Where (option : Option<'T>, predicate : Func<'T, bool>) =
    if isNull predicate then nullArg "predicate"
    Option.filter predicate.Invoke option
  /// Aggregates the optional value to another value.
  [<Extension>]
  static member Aggregate (option : Option<'T>, seed : 'TAccumulate, aggregator : Func<'TAccumulate, 'T, 'TAccumulate>) =
    if isNull aggregator then nullArg "aggregator"
    Option.fold (fun acc x -> aggregator.Invoke (acc, x)) seed option
  /// Runs a dead-end function on the optional value. Useful for logging for example.
  [<Extension>]
  static member Do (option : Option<'T>, action : Action<'T>) =
    if isNull action then nullArg "action"
    Option.iter action.Invoke option
  /// Gets the value from the optional value or evaluate one from the given creator function.
  [<Extension>]
  static member GetOrElse (option : Option<'T>, creator : Func<'T>) =
    if isNull creator then nullArg "creator"
    Option.defaultWith creator.Invoke option
  /// Gets the value from the optional value or use a given default value instead.
  [<Extension>]
  static member GetOrElse (option : Option<'T>, defaultValue : 'T) =
    Option.defaultValue defaultValue option
  /// Tries to get the value from the optional value.
  [<Extension>]
  static member TryGetValue (option : Option<'T>, result : outref<'T>) =
    match option with
    | Some x -> result <- x; true
    | _ -> result <- Unchecked.defaultof<'T>; false
