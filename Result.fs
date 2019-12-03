namespace Microsoft.FSharp.Core

open System
open System.Runtime.CompilerServices

/// Additional operations on the `Result<_, _>` type.
module Result =

  /// Creates a successful result.
  let ok = Ok

  /// Creates a failure result.
  let error = Error

  /// Creates a failures results.
  let errors x = Error [x]

  /// Determines whether the result is successful.
  let isOk = function Ok _ -> true | _ -> false
  
  /// Determines whether the result is faulted.
  let isError = function Error _ -> false | _ -> true

  /// Maps to a fixed `Ok` value `x` when the result is `Ok x`.
  let mapTo value = function
    | Ok _ -> Ok value
    | Error x -> Error x

  /// Aggregates the `x` in `Ok x` with a provided seed.
  let fold accummulator seed r =
    match r with
    | Ok x -> accummulator seed x
    | _ -> seed

  /// Determines whether the `Ok x` satisfies a given predicate
  let exists predicate = function
    | Ok x -> predicate x
    | _ -> false

  /// Determines whether the `x` in `Ok x` matches a given value.
  let contains value = function
    | Ok x -> value = x
    | _ -> false

  /// Filters the result based on a given predicate, using an error value if the result doesn't satisy the predicate.
  let filter predicate ifError r =
    match r with
    | Ok x when predicate x -> Ok x
    | Ok _ -> Error ifError
    | x -> x

  /// Filters the result based on a given predicate, using an error value if the result doesn't satisy the predicate.
  let filterWith predicate ifError = function
    | Ok x when predicate x -> Ok x
    | Ok _ -> Error (ifError ())
    | x -> x

  /// Lifts a two argument function to work with result types.
  let lift2 f xResult yResult =
    match xResult, yResult with
    | Ok x, Ok y -> Ok (f x y)
    | Error err1, Error err2 -> Error (err1 @ err2)
    | Error err, _ | _, Error err -> Error err

  /// Lifts a three argument function to work with result types.
  let lift3 f xResult yResult zResult =
    match xResult, yResult, zResult with
    | Ok x, Ok y, Ok z -> Ok (f x y z)
    | Error e1, Error e2, Error e3 -> Error ((e1 @ e2) @ e3)
    | Error err, _, _ | _, Error err, _ | _, _, Error err -> Error err

  /// Lifts a four argument function to work with result types.
  let lift4 f xResult yResult zResult aResult =
    match xResult, yResult, zResult, aResult with
    | Ok x, Ok y, Ok z, Ok a -> Ok (f x y z a)
    | Error e1, Error e2, Error e3, Error e4 -> Error (List.append e1 e2 |> List.append e3 |> List.append e4)
    | Error err, _, _, _ 
    | _, Error err, _, _ 
    | _, _, Error err, _ 
    | _, _, _, Error err -> Error err

  /// Applies a function `f` in `Ok f` to a `x` in `Ok x` when both are `Ok` values.
  let apply fResult xResult =
    lift2 (fun f x -> f x) fResult xResult

  /// Transforms two result types togheter into a new result.
  let map2 f r1 r2 =
    match r1, r2 with
    | Ok x, Ok y -> Ok (f x y)
    | Ok x, Error y -> Error y
    | Error x, Ok y -> Error x
    | Error x, Error y -> Error (x @ y)

  /// Transfroms three result types together into a new result.
  let map3 f r1 r2 r3 =
    match r1, r2, r3 with
    | Ok x, Ok y, Ok z -> Ok (f x y z)
    | Ok _, Ok _, Error err
    | Ok _, Error err, Ok _ 
    | Error err, Ok _, Ok _ -> Error err
    | Ok _, Error err1, Error err2 
    | Error err1, Error err2, Ok _ 
    | Error err1, Ok _, Error err2 -> Error (err1 @ err2)
    | Error err1, Error err2, Error err3 -> Error (err1 @ err2 @ err3)

  /// Transfroms four result types together into a new result.
  let map4 f r1 r2 r3 r4 =
    match r1, r2, r3, r4 with
    | Ok a,       Ok b,       Ok c,       Ok d -> Ok (f a b c d)
    | Ok _,       Ok _,       Ok _,       Error err
    | Ok _,       Ok _,       Error err,  Ok _
    | Ok _,       Error err,  Ok _,       Ok _
    | Error err,  Ok _,       Ok _,       Ok _ -> Error err
    | Ok _,       Ok _,       Error err1, Error err2
    | Ok _,       Error err1, Error err2, Ok _
    | Error err1, Error err2, Ok _,       Ok _
    | Ok _,       Error err1, Ok _,       Error err2
    | Error err1, Ok _,       Ok _,       Error err2
    | Error err1, Ok _,       Error err2, Ok _ -> Error (err1 @ err2)
    | Ok _,       Error err1, Error err2, Error err3
    | Error err1, Error err2, Error err3, Ok _
    | Error err1, Ok _,       Error err2, Error err3
    | Error err1, Error err2, Ok _,       Error err3 -> Error (err1 @ err2 @ err3)
    | Error err1, Error err2, Error err3, Error err4 -> Error (err1 @ err2 @ err3 @ err4)

  /// Combines two results into a result with a tuple of the `Ok` values.
  let zip2 r1 r2 = map2 (fun x y -> x, y) r1 r2

  /// Combines three results into a result with a triple of the `Ok` values.
  let zip3 r1 r2 r3 = map3 (fun x y z -> x, y, z) r1 r2 r3

  /// Combines four results into a result with a quadruple of the `Ok` values.
  let zip4 r1 r2 r3 r4 = map4 (fun x y z a -> x, y, z, a) r1 r2 r3 r4

  /// Traverse over a sequence running a given mapping function over the elements, 
  /// collecting the outcomes into a result.
  let traverseSeq f xs =
    let consR = Ok (fun h t -> seq { yield h; yield! t })
    Seq.foldBack
      (fun x acc -> apply (apply consR (f x)) acc)
      xs
      (Ok Seq.empty)
    
  /// Transorms a sequence of results into a result of a sequence.
  let sequenceSeq xs = traverseSeq id xs

  /// Traverse over a list running a given mapping function over the elements, 
  /// collecting the outcomes into a result.
  let traverse f xs =
    let consR = Ok (fun h t -> h :: t)
    List.foldBack
      (fun x acc -> apply (apply consR (f x)) acc)
      xs
      (Ok [])

  /// Transforms a list of results into a result of a list.
  let sequence xs = traverse id xs

  /// Unwraps result of result into a result.
  let flatten = function
    | Ok (Ok x) -> Ok x
    | Ok (Error err) -> Error err
    | Error err -> Error err

  /// Runs a function without output (`unit`) over the `Ok` value of the result.
  let iter f result = Result.map (fun x -> f x; x) result

  /// Runs a function without output (`unit`) over the `Error` value of the result.
  let iterError f result = Result.mapError (fun x -> f x; x) result

  /// Transforms the `Error` value in the result into a series of errors.
  /// Useful when the signature of the required function requires multiple `Error` values.
  let multipleErrors result = Result.mapError (fun x -> [x]) result

  /// Gets the `x` in the `Ok x` value, but use the given `ifError` function otherwise.
  let getOrElse ifError = function
    | Ok x -> x
    | Error err -> ifError err

  /// Gets the `x` in the `Ok x` value, but use the given `ifError` value otherwise.
  let getOrValue ifError result = getOrElse (fun _ -> ifError) result

  /// Gets the `Ok x` branch or evaluate the `ifError` to create a new result.
  let orElse ifError = function
    | Ok x -> Ok x
    | Error err -> ifError err

  /// Gets the `Ok x` branch or use the `ifError` result value.
  let orElseValue ifError result = orElse (fun _ -> ifError) result

  /// Transforms an `option` to a result, using the given `ifNone` function when the option is `None`.
  let ofOptionWith ifNone = function
    | Some x -> Ok x
    | None -> Error (ifNone ())

  /// Transforms an `option` to a result, using the given `ifNone` value when the option is `None`.
  let ofOption ifNone opt  = ofOptionWith (fun () -> ifNone) opt

  /// Transforms the `Ok x` into a list `[x]`, using an empty list when the result is `Error`.
  let toList result = fold (fun _ x -> [x]) [] result

  /// Transforms the `Ok x` into a array `[|x|]`, using an empty array when the result is `Errro`.
  let toArray result = fold (fun _ x -> [|x|]) [||] result

  /// Transforms the `Ok x` into a sequence `seq { yield x }`, using an empty sequence when the result is `Error`.
  let toSeq result = fold (fun _ x -> seq { yield x }) Seq.empty result
  
  /// Transforms the `Ok x` into an option `Some x`, using `None` when the result is `Error`.
  let toOption result = fold (fun _ x -> Some x) None result

  /// Transforms an object from to a `Ok x` result when the object is not `null`; otherwise `Error error` with the specified error.
  let ofObj error = function
    | null -> Error error
    | x -> Ok x

  /// Transforms a result to an object using the `Ok x` value; otherwise use the default value of the `Ok x` type.
  let toObj (result : Result<'T, 'TError>) = 
    getOrValue Unchecked.defaultof<'T> result

  /// Traverse the list and returns the `x` of the `Ok x` of each element; evaluated with the given function.
  let choose (f : 'a -> Result<'b, 'c list>) (xs : 'a list) =
    [ for x in xs do match f x with Ok x -> yield x | _ -> () ]

  /// Runs a given function, mapping the outcome to a `Ok` value 
  /// but handling the possible thrown exception with a given `handler` into a `Error` value.
  let catch f handler x =
    try Ok (f x)
    with ex -> Error (handler ex)

  /// Catches the `Result.map` function by handling the exception into a `Error` value.
  let catchMap f handler x =
    try Result.map f x
    with ex -> Error (handler ex)

  /// Catches the `Result.bind` function by handling the exception into a `Error` value.
  let catchBind f handler x =
    try Result.bind f x
    with ex -> Error (handler ex)

  /// Catches the `Result.iter` function by handling the exception into a `Error` value.
  let catchIter f handler x =
    try iter f x
    with ex -> Error (handler ex)

  /// Switch a function as a function that returns a `Ok` result value.
  let switch f = f >> Ok

  /// Matches both values in the result to the same new value.
  let either ifOk ifError result =
    match result with
    | Ok x -> ifOk x
    | Error err -> ifError err
  
  /// Transforms both values in the result to a new value.
  let mapBoth ifOk ifError result =
    either (ifOk >> Ok) (ifError >> Error) result

  /// Result operators on the result type.
  module Operators =

      let (>>=) m f = Result.bind f m
      let (>=>) f g = f >> Result.bind g
      let (<!>) m f = Result.map f m
      let (<*>) = apply

/// Result computation expression.
type ResultBuilder () =
    member __.Return (value) = Ok value
    member __.ReturnFrom (result : Result<'TOk, 'TError>) = result
    member __.Bind (result, binder) = Result.bind binder result
    member __.Zero () = Ok ()
    member __.Combine (result, binder) = Result.bind binder result
    member __.Delay (f : unit -> _) = f
    member __.Run (f) = f ()
    member this.TryWith (result, handler) =
      try this.ReturnFrom (result)
      with ex -> handler ex
    member this.TryFinally (result, compensation) =
      try this.ReturnFrom (result)
      finally compensation ()
    member this.Using (res : #IDisposable, body) =
      this.TryFinally (body res, fun () -> match res with null -> () | disp -> disp.Dispose ())
    member this.While (guard, f) =
      if not (guard ()) then Ok () else
      do f () |> ignore
      this.While (guard, f)
    member this.For (sequence : #seq<_>, body) =
      this.Using (
        sequence.GetEnumerator (),
        fun enum -> this.While(enum.MoveNext, this.Delay (fun () -> body enum.Current)))

/// Result auto exposed values.
[<AutoOpen>]
module ResultBuilder =

  /// Result computation expression
  let result = new ResultBuilder ()

/// Add C# extensions for the result type.
[<Extension>]
type ResultExtensions () =
  /// Transforms the successful value of the result to another type.
  [<Extension>]
  static member Select (result : Result<'T, 'TError>, selector : Func<'T, 'TResult>) =
      if isNull selector then nullArg "selector"
      Result.map selector.Invoke result
  /// Transforms the failure value of the result to another type.
  [<Extension>]
  static member Select (result : Result<'T, 'TError>, selector : Func<'TError, 'TResult>) =
      if isNull selector then nullArg "selector"
      Result.mapError selector.Invoke result
  /// Transforms the successful value of the result to another result.
  [<Extension>]
  static member SelectMany (result : Result<'T, 'TError>, selector : Func<'T, Result<'TResult, 'TError>>) =
      if isNull selector then nullArg "selector"
      Result.bind selector.Invoke result
  /// Filter the result for a given predicate, and create a failure result for a given value if the predicate doesn't hold.
  [<Extension>]
  static member Where (result : Result<'T, 'TError>, predicate : Func<'T, bool>, createError : Func<'TError>) =
      if isNull predicate then nullArg "predicate"
      Result.filterWith predicate.Invoke createError.Invoke result
  /// Filter the result for a given predicate, and create a failure result for a given value if the predicate doesn't hold.
  [<Extension>]
  static member Where (result : Result<'T, 'TError>, predicate : Func<'T, bool>, error : 'TError) =
      if isNull predicate then nullArg "predicate"
      Result.filter predicate.Invoke error result
  /// Aggregates the successful result value into another value.
  [<Extension>]
  static member Aggregate (result : Result<'T, 'TError>, seed : 'TAccumulate, aggregator : Func<'TAccumulate, 'T, 'TAccumulate>) =
      if isNull aggregator then nullArg "aggregator"
      Result.fold (fun acc x -> aggregator.Invoke (acc, x)) seed result
  /// Tries to get the successful value out of the result.
  [<Extension>]
  static member TryGetValue (result : Result<'T, 'TError>, value : outref<_>) =
      match result with
      | Ok x -> value <- x; true
      | _ -> value <- Unchecked.defaultof<_>; false
  /// Tries to get the successful value out of the result, or a given alternative.
  [<Extension>]
  static member GetOrElse (result : Result<'T, 'TError>, otherwise : Func<'TError, 'T>) =
      if isNull otherwise then nullArg "otherwise"
      Result.getOrElse otherwise.Invoke result
  /// Tries to get the successful value out of the result, or a given alternative.
  [<Extension>]
  static member GetOrElse (result : Result<'T, 'TError>, otherwise) =
      Result.getOrValue otherwise result
