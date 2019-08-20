namespace Microsoft.FSharp.Core

open System
open System.Linq.Expressions
open System.Runtime.CompilerServices
open System.Collections.Generic

type State<'T, 'TState> = State of ('TState -> 'T * 'TState)



module State =

    let get = State (fun s -> (s,s))
    let set s = State (fun _ -> ((),s))

    let eval (State m) s = m s |> fst
    let exec (State m) s = m s |> snd
    
    let bind (k : 'T -> State<'TResult, 'TState>) (m : State<'T, 'TState>) = 
        State <| fun s ->
            let (x, s) = let (State f) = m in f s
            let (State g) = k x in g s

type StateBuilder() =
    member this.Return(a) = State (fun s -> (a,s))
    member this.ReturnFrom(m) = m
    member this.Bind(m, k) = State.bind k m
    member this.Zero() = this.Return ()
    member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
    member this.TryWith(State m, h) =
        State (fun env -> 
            try m env
            with e ->  let (State f) = (h e) in f env)
    member this.TryFinally(State m, compensation) =
        State (fun env -> 
            try m env
            finally compensation())
    member this.Using(res:#IDisposable, body) =
        this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
    member this.Delay(f) = this.Bind(this.Return (), f)
    member this.While(guard, m) =
        if not(guard()) then this.Zero() else
            this.Bind(m, (fun () -> this.While(guard, m)))
    member this.For(sequence:seq<_>, body) =
        this.Using(sequence.GetEnumerator(),
            (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

[<AutoOpen>]
module StateExposure =

    let state = new StateBuilder()

type Lens<'T, 'TProp> =
    { get : 'T -> 'TProp
      set : 'TProp -> 'T -> 'T } with
        static member Create (getFunc : Func<'T, 'TProp>, setFunc : Func<'TProp, 'T, 'T>) =
            if isNull getFunc then nullArg "getFunc"
            if isNull setFunc then nullArg "setFunc"
            { get = getFunc.Invoke; set = fun prop x -> setFunc.Invoke (prop, x) }
        static member Create (getFunc : Func<'T, 'TProp>, setFunc : Action<'TProp, 'T>) =
            if isNull getFunc then nullArg "getFunc"
            if isNull setFunc then nullArg "setFunc"
            { get = getFunc.Invoke; set = fun prop x -> setFunc.Invoke (prop, x); x }
        member lens.Get instance = lens.get instance
        member lens.Set value = Func<_, _> (fun x -> lens.set value x)
        member lens.Set (value, instance) = lens.set value instance
        
        member lens.update f a = lens.set (f (lens.get a)) a
        member lens.Update (updater : Func<_, _>, instance) = 
            if isNull updater then nullArg "updater"
            lens.update updater.Invoke instance
        member lens.Update (updater : Func<_, _>) = 
            if isNull updater then nullArg "updater"
            Func<_, _> (fun instance -> lens.update updater.Invoke instance)
        

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =

  let get (lens : Lens<'T, 'TProp>) (instance : 'T) =
      lens.get instance

  let set (lens : Lens<'T, 'TProp>) value (instance : 'T) =
      lens.set value instance

  let update l f a =
      l.set (f (l.get a)) a

  /// Applies a lens in the 'get' direction within a state monad
  let getState l = 
      State (fun a -> get a l, a)

  /// Applies a lens in the 'set' direction within a state monad
  let setState l v = 
      State (fun a -> (), set v a l)

  /// Update through a lens within a state monad
  let updateState l f =
      State (fun a -> (), update f l a)

  /// Modifies the state in a state monad and returns the original value
  let getAndModifyState l f = 
      state { let! v = State.get
              do! updateState l f
              return v }

  /// Modifies the state in a state monad and returns the modified value
  let modifyAndGetState l f = 
      state { do! updateState l f
              return! State.get }

  let compose l1 l2 =
      { get = l1.get >> l2.get
        set = fun x t -> let r = l2.set x (l1.get t) in l1.set r t }


   /// Lens for a particular key in a map
  let forMap key =
      { get = Map.tryFind key
        set = function
          | Some value -> Map.add key value
          | None -> Map.remove key }

   /// Lens for a particular key in a dictionary
  let forDic key =
      { get = fun (d : IDictionary<_, _>) -> match d.TryGetValue key with | true, v -> v | _ -> null
        set = fun x d -> d.[key] <- x; d }

  /// Creates a lens that maps the given lens in a sequence
  let mapSeq l =
      { get = Seq.map l.get
        set = Seq.map2 l.set }

  /// Creates a lens that maps the given lens in a list
  let mapList l =
      { get = List.map l.get
        set = List.map2 l.set }

  /// Creates a lens that maps the given lens in an array
  let mapArray l = 
      { get = Array.map l.get
        set = Array.map2 l.set }

  /// Applies an isomorphism to the value viewed through a lens
  let xmap f g l =
      { get = l.get >> f
        set = g >> l.set }

  /// Converts a lens that views a list into a lens that views an array
  let inline listToArray l = xmap List.ofArray Array.ofList l

  /// Converts a lens that views a list into a lens that views an array
  let inline arrayToList l = xmap Array.ofList List.ofArray l

  /// Converts a lens that views a list into a lens that views an array
  let inline listToSeq l = xmap Seq.ofList List.ofSeq l

  /// Converts a lens that views a list into a lens that views an array
  let inline seqToList l = xmap List.ofSeq Seq.ofList l

  /// Converts a lens that views a list into a lens that views an array
  let inline arrayToSeq l = xmap Seq.ofArray Array.ofSeq l

  /// Converts a lens that views a list into a lens that views an array
  let inline seqToArray l = xmap Array.ofSeq Seq.ofArray l

  /// Head of a list giving a partial lens
  let head =
      { get = List.tryHead
        set = fun hOpt xs ->
          match hOpt with
          | Some h -> h :: xs
          | None -> List.skip 1 xs }

  let element f =
      { get = List.tryFind f
        set = fun x t -> Option.filter f x |> Option.map (fun x -> x :: t) |> Option.defaultValue t }

  let fst =
      { get = fst
        set = fun x (_, y) -> x, y }

  let snd = 
      { get = snd
        set = fun y (x, _) -> x, y }

  let third =
      { get = fun (_, _, x) -> x
        set = fun z (x, y, _) -> x, y,  z }
       

type Lens () =
  static member Create (getFunc : Func<'T, 'TProp>, setFunc : Func<'TProp, 'T, 'T>) =
      Lens<'T, 'TProp>.Create (getFunc, setFunc)

  static member Get (lens : Lens<'T, 'TProp>) (instance : 'T) =
    lens.get instance
    
  static member Set (lens : Lens<'T, 'TProp>) value (instance : 'T) =
    lens.set value instance
    
  static member Update l f a =
    l.set (f (l.get a)) a

[<AutoOpen>]
module LensExposure =
    let (>->) = Lens.compose
    let (<--) = Lens.set
    let (-->) = Lens.get
    let (<->) = Lens.update

[<Extension>]
type LensExtensions () =
    [<Extension>]
    static member Then (lens : Lens<'T, 'TProp>, next: Lens<'TProp, 'TResult>) =
        Lens.compose lens next

    [<Extension>]
    static member Update (lens : Lens<'T, 'TProp>, selector : Func<'TProp, 'TProp>, instance) =
        if isNull selector then nullArg "selector"
        Lens.update lens selector.Invoke instance

    [<Extension>]
    static member ForMany (lens : Lens<'T, 'TProp>) : Lens<IEnumerable<'T>, IEnumerable<'TProp>> =
        Lens.mapSeq lens

    [<Extension>]
    static member Pipe (first : Func<'T, 'TResult>, second : Func<'TResult, 'TFinal>) =
        if isNull first then nullArg "first"
        if isNull second then nullArg "second"
        Func<_, _> (fun x -> second.Invoke (first.Invoke x))
