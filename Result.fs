namespace Microsoft.FSharp.Core

/// Additional operations on the `Result<_, _>` type.
module Result =

  /// Creates a successful result.
  let ok = Ok

  /// Creates a failure result.
  let error = Error

  /// Determines whether the result is successful.
  let isOk = function Ok _ -> true | _ -> false
  
  /// Determines whether the result is faulted.
  let isError = function Error _ -> false | _ -> true

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
  let defaultWith ifError = function
    | Ok x -> x
    | Error err -> ifError err

  /// Gets the `x` in the `Ok x` value, but use the given `ifError` value otherwise.
  let defaultValue ifError = defaultWith (fun _ -> ifError)

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

type ResultBuilder () =
    member __.Bind (result, binder) = Result.bind binder result
    member __.Return (value) = Ok value

[<AutoOpen>]
module ResultExposure =

  let (>>=) m f = Result.bind f m
  let (>=>) f g = f >> Result.bind g
  let (<!>) m f = Result.map f m
  let (<*>) = Result.apply

  /// Result computation expression
  let result = new ResultBuilder ()