namespace System

open System.Linq
open System.Runtime.CompilerServices
open System.Diagnostics

/// Exception thrown when the successful value of a result instance is called while the instance represents a failure outcome.
exception NotSuccessfulException of string
/// Exception thrown when the failure value of a result instance is called while the instance represents a successful outcome.
exception NotFailureException of string

/// Represents an abstracted result model that can either represent a successful or failure outcome.
[<Struct; DebuggerDisplay("{IsSuccess ? \"Success: \" + Value : \"Failure: \" + Error}")>]
type Outcome<'T, 'TError> private (value : 'T option, error : 'TError option) =
  /// Creates a successful result instance.
  new (value : 'T) =
    if obj.ReferenceEquals (value, null) then nullArg "value"
    Outcome<'T, 'TError> (value=Some value, error=None)
  /// Creates a failure result instance.
  new (error : 'TError) =
    if obj.ReferenceEquals (error, null) then nullArg "error"
    Outcome<'T, 'TError> (value=None, error= Some error)
  private new (result : Core.Result<'T, 'TError>) =
    let value, error = 
      match result with
      | Ok x -> Some x, None
      | Error err -> None, Some err
    Outcome<'T, 'TError> (value, error)
  /// Gets a flag indicating this result instance represents a successful outcome.
  member __.IsSuccess = Option.isSome value
  /// Gets a flag indicating this result instance represents a failure outcome.
  member __.IsFailure = Option.isSome error
  /// Gets the success value when this instance represents a successful outcome; throw `NotSuccessfulException` otherwise.
  member __.Value = 
    let msg = sprintf "Cannot get successful value of Outcome<%s, %s> because it represents a failure outcome" typeof<'T>.Name typeof<'TError>.Name
    match value with Some x -> x | None -> raise (NotSuccessfulException msg)
  /// Gets the failure value when this instance represents a failure outcome; throw `NotFailureException` otherwise.
  member __.Error =
    let msg = sprintf "Cannot get failure value of Outcome<%s, %s> because it represents a successful outcome" typeof<'T>.Name typeof<'TError>.Name
    match error with Some x -> x | None -> raise (NotFailureException msg)
  /// Creates a successful result instance.
  static member Success (value) = Outcome<'T, 'TError> (Some value, None)
  /// Creates a failure result instance.
  static member Failure (error) = Outcome<'T, 'TError> (None, Some error)

  static member op_Implicit (result : Core.Result<'T, 'TError>) = Outcome<'T, 'TError> result
  static member op_Implicit (result : Outcome<'T, 'TError>) =
    if result.IsSuccess then Ok result.Value
    else Error result.Error
  static member op_Implicit (value : 'T) = Outcome<'T, 'TError> value
  static member op_Implicit (error : 'TError) = Outcome<'T, 'TError> error

/// Represents a obstracted result model that can either present a successful or failure outcome.
type Outcome private () =
  /// Creates a successful result instance.
  static member Success<'T, 'TError> value = Outcome<'T, 'TError>.Success value
  /// Creates a failure result instance.
  static member Failure<'T, 'TError> error = Outcome<'T, 'TError>.Failure error
  /// Creates a result instance representing a value when the given predicate holds.
  static member Create<'T, 'TError> (value, predicate : Func<'T, bool>, createError : Func<'TError>) =
    if isNull predicate then nullArg "predicate"
    if isNull createError then nullArg "createError"
    if predicate.Invoke value
    then Outcome<'T, 'TError>.Success value
    else Outcome<'T, 'TError>.Failure (createError.Invoke ())
  /// Creates a result instance representing a value when the given predicate holds.
  static member Create<'T, 'TError> (value, predicate : Func<'T, bool>, error : 'TError) =
    Outcome.Create<'T, 'TError> (value, predicate, fun () -> error)
  /// Creates a result instance representing a value when the given predicate holds.
  static member Create<'T, 'TError> (value, predicate : Func<'T, ValueTuple<bool, 'TError>>) =
    if isNull predicate then nullArg "predicate"
    match (predicate.Invoke value).ToTuple() with
    | true, _ -> Outcome<'T, 'TError>.Success value
    | false, error -> Outcome<'T, 'TError>.Failure error
  /// Transforms a F# result to a C# result instance.
  static member OfFSharpResult<'T, 'TError> (result : Core.Result<'T, 'TError>) = Outcome.op_Implicit (result=result)
  /// Transforms a C# result to a F# result instance.
  static member ToFSharpResult<'T, 'TError> (result : Outcome<'T, 'TError>) = Outcome.op_Implicit (result=result)

  /// Simulates a `using` statement where an exception gets caught into a result.
  static member Using<'T, 'TError, 'TResult when 'T :> IDisposable> (createResource : Func<'T>, useResource : Func<'T, 'TResult>, handler : Func<exn, 'TError>) =
    if isNull createResource then nullArg "createResource"
    if isNull useResource then nullArg "useResource" 
    if isNull handler then nullArg "handler"
    try use disposal = createResource.Invoke ()
        let result = useResource.Invoke disposal
        Outcome<'TResult, 'TError>.Success result
    with ex -> Outcome<'TResult, 'TError>.Failure (handler.Invoke ex)
  /// Simulates a `try...catch` statement where an exception gets caught into a result.
  static member Catch<'T, 'TResult, 'TError> (critical : Func<'T, 'TResult>, handler : Func<exn, 'TError>, value : 'T) =
    if isNull critical then nullArg "critical"
    if isNull handler then nullArg "handler"
    try Outcome<'TResult, 'TError>.Success (critical.Invoke value)
    with ex -> Outcome<'TResult, 'TError>.Failure (handler.Invoke ex)

/// Adds extensions on the `Result<>` model for easier control of successful/failure models.
[<Extension>]
type OutcomeExtensions private () =
  /// Transforms the inner value of the result instance to another value.
  [<Extension>]
  static member Select (result : Outcome<'T, 'TError>, selector : Func<'T, 'TResult>) =
    if isNull selector then nullArg "selector"
    if result.IsSuccess 
    then Outcome<'TResult, 'TError>.Success (selector.Invoke result.Value)
    else Outcome<'TResult, 'TError>.Failure result.Error
  /// Transforms the inner error value of the result instance to another value.
  [<Extension>]
  static member Select (result : Outcome<'T, 'TError>, selectorError : Func<'TError, 'TErrorResult>) =
    if isNull selectorError then nullArg "selector"
    if result.IsFailure
    then Outcome<'T, 'TErrorResult>.Failure (selectorError.Invoke result.Error)
    else Outcome<'T, 'TErrorResult>.Success result.Value
  /// Transforms both the value and the error at the same time.
  [<Extension>]
  static member Select (result : Outcome<'T, 'TError>, selector : Func<'T, 'TResult>, selectorError : Func<'TError, 'TErrorResult>) =
    if isNull selector then nullArg "selector"
    if isNull selectorError then nullArg "selectorError"
    if result.IsSuccess
    then Outcome<'TResult, 'TErrorResult>.Success (selector.Invoke result.Value)
    else Outcome<'TResult, 'TErrorResult>.Failure (selectorError.Invoke result.Error)
  /// Transforms the inner value of the outcome instance to another value in a safe catched environment with a specified handler.
  [<Extension>]
  static member Catch (result : Outcome<'T, 'TError>, selector : Func<'T, 'TResult>, handler : Func<exn, 'TError>) =
    if isNull selector then nullArg "selector"
    if isNull handler then nullArg "handler"
    try OutcomeExtensions.Select (result, selector)
    with ex -> Outcome<'TResult, 'TError>.Failure (handler.Invoke ex)
  /// Transforms the inner value of the outcome instance to another value in a safe catched environment with a specified handler and disposes the resource afterwards.
  [<Extension>]
  static member Using<'T, 'TError, 'TResult when 'T :> IDisposable> (result : Outcome<'T, 'TError>, selector : Func<'T, 'TResult>, handler : Func<exn, 'TError>) =
    if isNull selector then nullArg "selector"
    if isNull handler then nullArg "handler"
    if result.IsFailure
    then Outcome<'TResult, 'TError>.Failure result.Error
    else try use resource = result.Value
             Outcome<'TResult, 'TError>.Success (selector.Invoke resource)
         with ex -> Outcome<'TResult, 'TError>.Failure (handler.Invoke ex)
  /// Transforms the inner value of the result instance to another result instance, flatting the result.
  /// See also `Then`.
  [<Extension>]
  static member SelectMany (result : Outcome<'T, 'TError>, selector : Func<'T, Outcome<'TResult, 'TError>>) =
    if isNull selector then nullArg "selector"
    if result.IsSuccess then selector.Invoke result.Value
    else Outcome<'TResult, 'TError>.Failure result.Error
  /// Transforms the inner value of the outcome instance to another outcome instance, flatting the result;
  /// in a safe catched environment with a specified handler.
  [<Extension>]
  static member Catch (result : Outcome<'T, 'TError>, selector : Func<'T, Outcome<'TResult, 'TError>>, handler : Func<exn, 'TError>) =
    if isNull selector then nullArg "selector"
    if isNull handler then nullArg "handler"
    try OutcomeExtensions.SelectMany (result, selector)
    with ex -> Outcome<'TResult, 'TError>.Failure (handler.Invoke ex)
  /// Throws the failure error representing an exception when the outcome instance is indeed a failure.
  [<Extension>]
  static member Throw (result : Outcome<'T, 'TError>, createException : Func<'TError, #exn>) =
    if isNull createException then nullArg "createException"
    if result.IsFailure then raise (createException.Invoke result.Error)
  /// Throws the failure error representing an exception when the outcome instance is indeed a failure.
  [<Extension>]
  static member Throw (result : Outcome<'T, #exn>) =
    if result.IsFailure then raise result.Error
  /// Transforms the inner value of the result instance to another result instance, flatting the result.
  /// See also `Then`.
  [<Extension>]
  static member Then (result : Outcome<'T, 'TError>, selector : Func<'T, Outcome<'TResult, 'TError>>) =
    OutcomeExtensions.SelectMany (result, selector)
  /// Filters an optional instance by the given predicate, returning a failure outcome result when the predicate doesn't hold.
  [<Extension>]
  static member Where (result : Outcome<'T, 'TError>, predicate : Func<'T, bool>, createError : Func<'TError>) =
    if isNull predicate then nullArg "predicate"
    if isNull createError then nullArg "createError"
    if result.IsSuccess && predicate.Invoke result.Value then result
    else Outcome<'T, 'TError>.Failure (createError.Invoke ())
  /// Filters an optional instance by the given predicate, returning a failure outcome result when the predicate doesn't hold.
  [<Extension>]
  static member Where (result : Outcome<'T, 'TError>, predicate : Func<'T, bool>, error : 'TError) =
    OutcomeExtensions.Where (result, predicate, fun () -> error)
  /// Collapse an result instance to another type from the seed via the given aggregator function.
  [<Extension>]
  static member Aggregate (result : Outcome<'T, 'TError>, seed, aggregator : Func<'TResult, 'T, 'TResult>) =
    if isNull aggregator then nullArg "aggregator"
    if result.IsSuccess then aggregator.Invoke (seed, result.Value)
    else seed
  /// Runs an action on the inner value, without any result. Useful for logging.
  [<Extension>]
  static member Do (result : Outcome<'T, 'TError>, action : Action<'T>) =
    if isNull action then nullArg "action"
    if result.IsSuccess then action.Invoke result.Value
    result
  /// Runs an action on the inner error value, without any result. Useful for logging.
  [<Extension>]
  static member Do (result : Outcome<'T, 'TError>, actionError : Action<'TError>) =
    if isNull actionError then nullArg "actionError"
    if result.IsFailure then actionError.Invoke result.Error
    result
  /// Runs an action on the inner error value, without any result in a safe catched environment. Useful for logging.
  [<Extension>]
  static member Do (result : Outcome<'T, 'TError>, action : Action<'T>, handler : Func<exn, 'TError>) =
    if isNull action then nullArg "action"
    if isNull handler then nullArg "handler"
    try OutcomeExtensions.Do (result, action)
    with ex -> Outcome<'T, 'TError>.Failure (handler.Invoke ex)
  /// Combine two result instances with a set of given zipper functions.
  [<Extension>]
  static member Zip 
    ( result1 : Outcome<'T1, 'TError>, 
      result2 : Outcome<'T2, 'TError>,
      zipper : Func<'T1, 'T2, 'TResult>, 
      zipperError : Func<'TError, 'TError, 'TError>) =
    if isNull zipper then nullArg "zipper"
    if isNull zipperError then nullArg "zipperError"
    match result1.IsSuccess, result2.IsSuccess with
    | true, true -> Outcome.Success (zipper.Invoke (result1.Value, result2.Value))
    | false, false -> Outcome.Failure (zipperError.Invoke (result1.Error, result2.Error))
    | false, true -> Outcome.Failure (result1.Error)
    | true, false -> Outcome.Failure (result2.Error)
  /// Combine three result instances with a set of given zipper functions.
  [<Extension>]
  static member Zip 
    ( result1 : Outcome<'T1, 'TError>, 
      result2 : Outcome<'T2, 'TError>,
      result3 : Outcome<'T3, 'TError>,
      zipper : Func<'T1, 'T2, 'T3, 'TResult>, 
      zipperError : Func<'TError, 'TError, 'TError>) =
    if isNull zipper then nullArg "zipper"
    if isNull zipperError then nullArg "zipperError"
    match result1.IsSuccess, result2.IsSuccess, result3.IsSuccess with
    | true, true, true -> Outcome.Success (zipper.Invoke (result1.Value, result2.Value, result3.Value))
    | false, false, false -> Outcome.Failure (zipperError.Invoke (zipperError.Invoke (result1.Error, result2.Error), result3.Error))
    | false, true, true -> Outcome.Failure result1.Error
    | true, false, true -> Outcome.Failure result2.Error
    | true, true, false -> Outcome.Failure result3.Error
    | false, false, true -> Outcome.Failure (zipperError.Invoke (result1.Error, result2.Error))
    | true, false, false -> Outcome.Failure (zipperError.Invoke (result2.Error, result3.Error))
    | false, true, false -> Outcome.Failure (zipperError.Invoke (result1.Error, result3.Error))
  /// Combine four result instances with a set of given zipper functions.
  [<Extension>]
  static member Zip 
    ( result1 : Outcome<'T1, 'TError>, 
      result2 : Outcome<'T2, 'TError>,
      result3 : Outcome<'T3, 'TError>,
      result4 : Outcome<'T4, 'TError>,
      zipper : Func<'T1, 'T2, 'T3, 'T4, 'TResult>, 
      zipperError : Func<'TError, 'TError, 'TError>) =
    if isNull zipper then nullArg "zipper"
    if isNull zipperError then nullArg "zipperError"
    match result1.IsSuccess, result2.IsSuccess, result3.IsSuccess, result4.IsSuccess with
    | true, true, true, true -> Outcome.Success (zipper.Invoke (result1.Value, result2.Value, result3.Value, result4.Value))
    | false, false, false, false -> Outcome.Failure (zipperError.Invoke (zipperError.Invoke (zipperError.Invoke (result1.Error, result2.Error), result3.Error), result4.Error))
    | false, true, true, true -> Outcome.Failure result1.Error
    | true, false, true, true -> Outcome.Failure result2.Error
    | true, true, false, true -> Outcome.Failure result3.Error
    | true, true, true, false -> Outcome.Failure result4.Error
    | false, false, true, true -> Outcome.Failure (zipperError.Invoke (result1.Error, result2.Error))
    | true, false, false, true -> Outcome.Failure (zipperError.Invoke (result2.Error, result2.Error))
    | true, true, false, false -> Outcome.Failure (zipperError.Invoke (result3.Error, result4.Error))
    | false, true, false, true -> Outcome.Failure (zipperError.Invoke (result1.Error, result3.Error))
    | true, false, true, false -> Outcome.Failure (zipperError.Invoke (result2.Error, result4.Error))
    | false, false, false, true -> Outcome.Failure (zipperError.Invoke (zipperError.Invoke (result1.Error, result2.Error), result3.Error))
    | true, false, false, false -> Outcome.Failure (zipperError.Invoke (zipperError.Invoke (result2.Error, result3.Error), result4.Error))
    | false, true, false, false -> Outcome.Failure (zipperError.Invoke (zipperError.Invoke (result1.Error, result3.Error), result4.Error))
    | false, false, true, false -> Outcome.Failure (zipperError.Invoke (zipperError.Invoke (result1.Error, result2.Error), result4.Error))
    | false, true, true, false -> Outcome.Failure (zipperError.Invoke (result1.Error, result4.Error))
  /// Transforms two results together, the abstracted 'plus' (+) operator.
  [<Extension>]
  static member Plus 
    ( result1 : Outcome<'T, 'TError>, 
      result2 : Outcome<'T, 'TError>, 
      ifSuccess : Func<'T, 'T, 'T>,
      ifFailure : Func<'TError, 'TError, 'TError> ) =
    if isNull ifSuccess then nullArg "ifSuccess"
    if isNull ifFailure then nullArg "ifFailure"
    match result1.IsSuccess, result2.IsSuccess with
    | true, true -> Outcome<'T, 'TError>.Success (ifSuccess.Invoke (result1.Value, result2.Value))
    | false, true -> Outcome<'T, 'TError>.Failure result1.Error
    | true, false -> Outcome<'T, 'TError>.Failure result2.Error
    | false, false -> Outcome<'T, 'TError>.Failure (ifFailure.Invoke (result1.Error, result2.Error))

  /// Switch to another result if the current result represents a failure.
  [<Extension>]
  static member OrElse (result : Outcome<'T, 'TError>, otherwise : Func<'TError, Outcome<'T, 'TError>>) =
    if isNull otherwise then nullArg "otherwise"
    if result.IsFailure
    then otherwise.Invoke result.Error
    else Outcome<'T, 'TError>.Success result.Value
  /// Gets the successful value of the outcome instance, or an evaluated default value.
  [<Extension>]
  static member GetOrElse (result : Outcome<'T, 'TError>, otherwise : Func<'T>) =
    if isNull otherwise then nullArg "otherwise"
    if result.IsSuccess then result.Value
    else otherwise.Invoke ()
  /// Gets the successful value of the outcome instance, or a default value.
  [<Extension>]
  static member GetOrElse (result : Outcome<'T, 'TError>, defaultValue : 'T) =
    if result.IsSuccess then result.Value
    else defaultValue
  /// Gets the failure error of the outcome instance, or an evaluated default value.
  [<Extension>]
  static member GetOrElse (result : Outcome<'T, 'TError>, otherwise : Func<'TError>) =
    if isNull otherwise then nullArg "otherwise"
    if result.IsFailure then result.Error
    else otherwise.Invoke ()
  /// Tries to get the failure error of the outcome instance, or a default value.
  [<Extension>]
  static member GetOrElse (result : Outcome<'T, 'TError>, defaultError : 'TError) =
    if result.IsFailure then result.Error
    else defaultError
  /// Tries to get the successful value out of the result.
  [<Extension>]
  static member TryGetValue (result : Outcome<'T, 'TError>, value : outref<'T>) =
    if result.IsSuccess 
    then value <- result.Value; true
    else value <- Unchecked.defaultof<'T>; false
  /// Tries to get the failure error out of the result.
  [<Extension>]
  static member TryGetError (result : Outcome<'T, 'TError>, error : outref<'TError>) =
    if result.IsFailure
    then error <- result.Error; true
    else error <- Unchecked.defaultof<'TError>; false
  /// Maps both the successful and failure value of the outcome instance to another type.
  [<Extension>]
  static member Match (result : Outcome<'T, 'TError>, ifSuccess : Func<'T, 'TResult>, ifFailure : Func<'TError, 'TResult>) =
    if isNull ifSuccess then nullArg "ifSuccess"
    if isNull ifFailure then nullArg "ifFailure"
    if result.IsSuccess then ifSuccess.Invoke result.Value
    else ifFailure.Invoke result.Error
  /// Traverse the sequence, running a mapping function over the elements, collecting the results into an `Outcome` instance.
  [<Extension>]
  static member Traverse (sequence : 'T seq, selector : Func<'T, Outcome<'TResult, 'TError>>) =
    if isNull sequence then nullArg "sequence"
    if isNull selector then nullArg "selector"
    sequence.Aggregate(
      Outcome<'TResult seq, 'TError>.Success Seq.empty,
      fun acc x -> 
        acc.SelectMany (fun (xs : 'TResult seq) -> 
          selector.Invoke(x).Select(xs.Append)))
  /// Transforms a sequence of outcome instances to a single outcome instance with a sequence.
  [<Extension>]
  static member Sequence (sequence : Outcome<'T, 'TError> seq) = 
    if isNull sequence then nullArg "sequence"
    OutcomeExtensions.Traverse (sequence, Func<_, _> id)
  /// Use multiple error types for the outcome instance.
  [<Extension>]
  static member ManyErrors (result : Outcome<'T, 'TError>) =
    if result.IsSuccess 
    then Outcome<'T, 'TError seq>.Success result.Value
    else Outcome<'T, 'TError seq>.Failure (seq { yield result.Error })
  /// Append error to the outcome instance.
  [<Extension>]
  static member AppendError (result : Outcome<'T, 'TError seq>, error : 'TError) =
    if obj.ReferenceEquals (error, null) then nullArg "error"
    if result.IsSuccess then result
    else Outcome<'T, 'TError seq>.Failure (seq { yield! result.Error; yield error })
  /// Append errors to the outcome instance.
  [<Extension>]
  static member AppendErrors (result : Outcome<'T, 'TError seq>, errors : 'TError array) =
    if isNull errors then nullArg "errors"
    if result.IsSuccess then result
    else Outcome<'T, 'TError seq>.Failure (Seq.append result.Error errors)
  /// Format errors in the outcome instance.
  [<Extension>]
  static member FormatErrors (result : Outcome<'T, 'TError seq>, seperator) =
    if isNull seperator then nullArg "seperator"
    if result.IsSuccess then String.Empty
    else String.Join (seperator, result.Error)
  /// Transforms the outcome instance to a F# optional type.
  [<Extension>]
  static member ToFSharpOption (result : Outcome<'T, 'TError>) =
    if result.IsSuccess then Some result.Value else None
  /// Transforms the outcome instance to a F# result type.
  [<Extension>]
  static member ToFSharpResult (result : Outcome<'T, 'TError>) =
    if result.IsSuccess then Ok result.Value
    else Error result.Error
  /// Transforms the outcome instance to a value tuple.
  [<Extension>]
  static member Deconstruct (result : Outcome<'T, 'TError>) =
    let value = result.GetOrElse (Unchecked.defaultof<'T>)
    let error = result.GetOrElse (Unchecked.defaultof<'TError>)
    ValueTuple.Create (value, error)
