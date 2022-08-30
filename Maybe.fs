namespace System

open System.Runtime.CompilerServices
open System
open System.Collections
open System.Collections.Generic
open System.Linq

/// Exception thrown when the value of `Maybe<>` instance is requested while the `Maybe<>` instance doesn't contain any.
exception ValueNotPresentException of string

/// Represents a optional model that can either contain a value or not.
[<Struct>]
type Maybe<'T> private (value : 'T, isPresent : bool) =
  new (value : 'T) =
    if obj.ReferenceEquals(value, null) then nullArg "value"
    new Maybe<'T> (value, isPresent=true)
  /// Gets the value indicating whether this optional instance represents a value or the absence of a value.
  member __.IsPresent = isPresent
  /// Gets the value of this optional instance, but throws a `ValueNotPresentException` when there is not value.
  member __.Value = 
    if isPresent then value 
    else raise (ValueNotPresentException (sprintf "Optional type doesn't contain a value or type %s" typeof<'T>.FullName))

  /// Gets a new optional instance representing the absence of a value.
  static member Nothing = Maybe<'T> (value=Unchecked.defaultof<'T>, isPresent=false)

  static member op_Implicit (this : Maybe<'T>) = if this.IsPresent then Some this.Value else None
  static member op_Implicit (this : Option<'T>) = if this.IsSome then Maybe<'T> this.Value else Maybe<'T>.Nothing
  static member op_Implicit (this : 'T) = Maybe<'T> this

  interface IEnumerable<'T> with
    member this.GetEnumerator () = 
      let xs = if this.IsPresent then [this.Value] else []
      (Seq.ofList xs).GetEnumerator() :> IEnumerator
    member this.GetEnumerator () = 
      let xs = if this.IsPresent then [this.Value] else []
      (Seq.ofList xs).GetEnumerator()

/// Represents a optional model that can either contain a value or not.
type Maybe private () =
  /// Creates an optional instance representing a value.
  static member Just value = Maybe<'T> value
  /// Creates an optional instance representing the absence of a value.
  static member Nothing<'T> () = Maybe<'T>.Nothing
  /// Creates an optional instance representing a value when the given value is not null.
  static member JustOrNothing value = if obj.ReferenceEquals(value, null) then Maybe<'T>.Nothing else Maybe<'T> value
  /// Transforms a F# option to an optional `Maybe<>` instance.
  static member OfOption<'T> (option : Option<'T>) = Maybe<'T>.op_Implicit option
  /// Transforms a optional `Maybe<>` instance to a F# option.
  static member ToOption<'T> (maybe : Maybe<'T>) = Maybe<'T>.op_Implicit maybe

/// Active patterns for the C# optional instance.
[<AutoOpen>]
module MaybePatterns =
  /// Match either a value or the absence of a value.
  let (|Just|Nothing|) (maybe : Maybe<_>) =
    if maybe.IsPresent then Just maybe.Value
    else Nothing
  
/// Adds extensions on the `Maybe<>` model for easier control of optional models.
[<Extension>]
type MaybeExtensions private () =
  /// Transforms the inner value of the optional model to another value.
  [<Extension>]
  static member Select (maybe : Maybe<'T>, selector : Func<_, 'TResult>) =
    if isNull selector then nullArg "selector"
    if maybe.IsPresent then Maybe<'TResult> (selector.Invoke maybe.Value)
    else Maybe<'TResult>.Nothing
  /// Transforms the inner value of the optional model to another optional instance, flattening the result.
  /// See also `Then`.
  [<Extension>]
  static member SelectMany (maybe : Maybe<'T>, selector : Func<_, Maybe<'TResult>>) =
    if isNull selector then nullArg "selector"
    if maybe.IsPresent then selector.Invoke maybe.Value
    else Maybe<'TResult>.Nothing
  /// Transforms the inner value of the optional model to another optional instance, flattening the result.
  /// See also `SelectMany`.
  [<Extension>]
  static member Then (maybe : Maybe<'T>, selector : Func<_, Maybe<'TResult>>) =
    MaybeExtensions.SelectMany (maybe, selector)
  /// Filters an optional instance by the given predicate, returning a `Maybe.Nothing` when the predicate doesn't hold.
  [<Extension>]
  static member Where (maybe : Maybe<'T>, predicate : Func<_, _>) =
    if isNull predicate then nullArg "predicate"
    if maybe.IsPresent && predicate.Invoke maybe.Value then maybe
    else Maybe<'T>.Nothing
  /// Collapse an optional instance to another type from the seed or via the given aggregator function.
  [<Extension>]
  static member Aggregate (maybe : Maybe<'T>, seed, aggregator : Func<'TResult, _, _>) =
    if isNull aggregator then nullArg "aggregator"
    if maybe.IsPresent then aggregator.Invoke (seed, maybe.Value)
    else seed
  /// Runs a action on the inner value, without any result. Useful for logging.
  [<Extension>]
  static member Do (maybe : Maybe<'T>, action : Action<_>) =
    if isNull action then nullArg "action"
    if maybe.IsPresent then action.Invoke maybe.Value
    maybe
  /// Switch to another optional instance when the current instance doesn't has a value.
  [<Extension>]
  static member OrElse (maybe : Maybe<'T>, otherwise : Maybe<'T>) =
    if maybe.IsPresent then maybe else otherwise
  /// Switch to another optional instance when the current instance doesn't has a value.
  [<Extension>]
  static member OrElse (maybe : Maybe<'T>, otherwise : Func<_, Maybe<'T>>) =
    if isNull otherwise then nullArg "otherwise"
    if maybe.IsPresent then maybe else otherwise.Invoke ()
  /// Gets the inner value of the optional instance or the given alternative value.
  [<Extension>]
  static member GetOrValue (maybe : Maybe<'T>, value) = 
    if maybe.IsPresent then maybe.Value else value
  /// Gets the inner value of the optional instance or evaluate an alternative value.
  [<Extension>]
  static member GetOrValue (maybe : Maybe<'T>, getValue : Func<_>) =
    if isNull getValue then nullArg "getValue"
    if maybe.IsPresent then maybe.Value
    else getValue.Invoke ()
  /// Gets the inner value of the optional instance or the default value of the inner value type.
  [<Extension>]
  static member GetOrDefault (maybe : Maybe<'T>) =
    maybe.GetOrValue (Unchecked.defaultof<'T>)
  /// Tries to get the inner value of the optional instance.
  [<Extension>]
  static member TryGetValue (maybe : Maybe<'T>, result : outref<'T>) =
    if maybe.IsPresent then result <- maybe.Value; true
    else result <- Unchecked.defaultof<'T>; false
  /// Transforms a double optional instance with an optional inner value to an single optional instance.
  [<Extension>]
  static member Flatten (maybe : Maybe<Maybe<'T>>) =
    if maybe.IsPresent then maybe.Value
    else Maybe<'T>.Nothing
  /// Combine two optional instances with a given zipper function.
  [<Extension>]
  static member Zip (maybe1 : Maybe<'T1>, maybe2 : Maybe<'T2>, selector : Func<'T1, 'T2, 'TResult>) =
    if isNull selector then nullArg "selector"
    if maybe1.IsPresent && maybe2.IsPresent 
    then Maybe<'TResult> (selector.Invoke (maybe1.Value, maybe2.Value))
    else Maybe<'TResult>.Nothing
  /// Combines three optional instances with a given zipper function.
  [<Extension>]
  static member Zip (maybe1 : Maybe<'T1>, maybe2 : Maybe<'T2>, maybe3 : Maybe<'T3>, selector : Func<'T1, 'T2, 'T3, 'TResult>) =
    if isNull selector then nullArg "selector"
    if maybe1.IsPresent && maybe2.IsPresent && maybe3.IsPresent
    then Maybe<'TResult> (selector.Invoke (maybe1.Value, maybe2.Value, maybe3.Value))
    else Maybe<'TResult>.Nothing
  /// Combines four optional instances with a given zipper function.
  [<Extension>]
  static member Zip (maybe1 : Maybe<'T1>, maybe2 : Maybe<'T2>, maybe3 : Maybe<'T3>, maybe4 : Maybe<'T4>, selector : Func<'T1, 'T2, 'T3, 'T4, 'TResult>) =
    if isNull selector then nullArg "selector"
    if maybe1.IsPresent && maybe2.IsPresent && maybe3.IsPresent && maybe4.IsPresent
    then Maybe<'TResult> (selector.Invoke (maybe1.Value, maybe2.Value, maybe3.Value, maybe4.Value))
    else Maybe<'TResult>.Nothing
  /// Append two optional instances into a single instance with a sequence of both values.
  [<Extension>]
  static member Append (maybe1 : Maybe<'T>, maybe2 : Maybe<'T>) =
    if maybe1.IsPresent && maybe2.IsPresent
    then match box maybe1.Value with
         | :? IEnumerable<'T> as v -> Maybe<IEnumerable<'T>> (seq { yield! v; yield maybe2.Value })
         | _ -> Maybe<IEnumerable<'T>> (seq { yield maybe1.Value; yield maybe2.Value })
    else Maybe<IEnumerable<'T>>.Nothing
  /// Selects all the elements in the specified sequence where the result of the given predicate is an optional instance representing a value.
  [<Extension>]
  static member Choose (sequence : IEnumerable<Maybe<'T>>, predicate : Func<_, Maybe<'TResult>>) =
    if isNull sequence then nullArg "sequence"
    if isNull predicate then nullArg "predicate"
    let chooser (x : Maybe<'T>) =
      if x.IsPresent then predicate.Invoke x.Value
      else Maybe<'TResult>.Nothing
    Seq.choose (chooser >> Maybe.ToOption) sequence
  /// Traverse all the elements in the specified sequence where the result of the given predicate is an optional instance representing a value,
  /// and collects all the results in an optional instance.
  [<Extension>]
  static member Traverse (sequence : IEnumerable<'T>, selector : Func<_, Maybe<'TResult>>) =
    if isNull sequence then nullArg "sequence"
    if isNull selector then nullArg "selector"
    sequence.Aggregate(
      new Maybe<IEnumerable<'TResult>>(Enumerable.Empty<'TResult>()),
      Func<_, _, _> (fun acc x -> 
        acc.SelectMany(Func<_, _> (fun (xs : IEnumerable<'TResult>) -> 
          selector.Invoke(x).Select(fun x -> xs.Append(x))))))
  /// Transforms a sequence of optional instances to an optional instance with a sequence.
  [<Extension>]
  static member Sequence (sequence) = MaybeExtensions.Traverse (sequence, Func<Maybe<'T>, _> id)
  /// Gets the first element in the sequence or an optional value representing the absance of a value.
  [<Extension>]
  static member FirstOrNothing (sequence : IEnumerable<'T>) = Enumerable.FirstOrDefault sequence |> Maybe.JustOrNothing
  /// Gets the single element in the sequence or an optional value representing the absance of a value.
  [<Extension>]
  static member SingleOrNothing (sequence : IEnumerable<'T>) = Enumerable.SingleOrDefault sequence |> Maybe.JustOrNothing
  /// Gets the last element in the sequence or an optional value representing the absance of a value.
  [<Extension>]
  static member LastOrNothing (sequence : IEnumerable<'T>) = Enumerable.LastOrDefault sequence |> Maybe.JustOrNothing
  /// Gets the element at the specified index in the sequence or an optional value representing the absance of a value.
  [<Extension>]
  static member ElementAtOrNothing (sequence : IEnumerable<'T>, index : int) = Enumerable.ElementAtOrDefault (sequence, index) |> Maybe.JustOrNothing
  /// Transforms a optional `Maybe<>` instance to a F# option.
  [<Extension>]
  static member ToOption (maybe : Maybe<'T>) = Maybe<'T>.op_Implicit maybe
