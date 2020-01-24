namespace Microsoft.FSharp.Core

open System
open System.Collections.Generic
open System.Linq

/// Operations on the `Map<_, _>` type.
module Map =
  /// Convert a `IDictionary<_, _>` instance to a `Map<_, _>` value.
  let toDict m = (Map.toSeq m).ToDictionary(Func<_, _> fst, Func<_, _> snd) :> IDictionary<_, _>
  
  /// Convert a `Map<_, _>` value to a `IDictionary<_, _>` instance.
  let ofDict (d : #IDictionary<_, _>) = Seq.map (|KeyValue|) d |> Map.ofSeq
  
  /// Adds a value to the map but handles dupplicate values with it.
  let addWith updateValue createValue key value m =
    if Map.containsKey key m
    then Map.map (fun _ ys -> updateValue value ys) m
    else Map.add key (createValue value) m

  /// Converts a grouped `seq` to a `Map` value where the values of duplicated keys are grouped together.
  let ofSeqg xs =
    Seq.foldBack (fun (k, ys) m -> 
      addWith Seq.append id k ys m) xs Map.empty

  /// Converts a `seq` to a `Map` value where the values of duplicated keys are grouped together.
  let ofSequ xs =
    Seq.foldBack (fun (k, xs) m -> 
      addWith 
        (fun x xs -> seq { yield x; yield! xs }) 
        (fun x -> seq { yield x }) k xs m) xs Map.empty

  /// Converts a grouped `list` to a `Map` value where the values of duplicated keys are grouped together.
  let ofListg xs =
    List.foldBack (fun (k, xs) m -> 
      addWith List.append id k xs m) xs Map.empty

  /// Converts a `list` to a `Map` value where the values of duplicated keys are grouped together.
  let ofListu xs =
    List.foldBack (fun (k, xs) m -> 
      addWith 
        (fun x xs -> [ yield! xs; yield x ])
        (fun x -> [x]) k xs m) xs Map.empty

  /// Converts a grouped `array` to a `Map` value where the values of duplicate keys are grouped together.
  let ofArrayg xs =
    Array.foldBack (fun (k, xs) m -> 
      addWith Array.append id k xs m) xs Map.empty

  /// Converts a `array` to a `Map` value where the values of duplicate keys are grouped together.
  let ofArrayu xs =
    Array.foldBack (fun (k, xs) m -> 
      addWith 
        (fun x xs -> [| yield! xs; yield x |])
        (fun x -> [| x |]) k xs m) xs Map.empty

  /// Gets the keys of the map.
  let keys (m : Map<_, _>) = seq { for x in m do yield x.Key }

  /// Gets the values of the map.
  let values (m : Map<_, _>) = seq { for x in m do yield x.Value }

  /// Creates a new map with a key and value.
  let create key xs = Map.empty |> Map.add key xs

  /// Maps the values of the map.
  let mapv f m = Map.map (fun _ x -> f x) m

  /// Combines two maps into a new map, duplicate entries are overriden.
  let append m1 m2 = Seq.append (Map.toSeq m1) (Map.toSeq m2) |> Map.ofSeq

  /// Combines two maps into a new map, updating the values of duplicate entries. 
  let appendu m1 m2 = Seq.append (Map.toSeq m1) (Map.toSeq m2) |> ofSequ

  /// Combines two maps with already grouped entries into a new map, updating the values of duplicate entries.
  let appendg m1 m2 = Seq.append (Map.toSeq m1) (Map.toSeq m2) |> ofSeqg

/// Builder type for computation expressions for the `Map<_, _>` type.
type MapBuilder<'TKey, 'TValue when 'TKey : comparison> (ofSeq : (seq<'TKey * 'TValue> -> Map<'TKey, 'TValue>)) =
  member __.Zero () = Map.empty
  member __.Yield x = x
  member __.YieldFrom xs = Map.toSeq xs
  member __.Combine (x, y) = Seq.append (Seq.singleton x) y
  member __.Combine (x, y) = Seq.append x y
  member __.For (xs, f) = Seq.map f xs
  member __.Delay f = f ()
  member __.Run x = ofSeq x

[<AutoOpen>]
module MapValues =
  /// Builds a map where duplicate entries are overridden.
  let map<'TKey, 'TValue when 'TKey : comparison> = MapBuilder<_, _> (Map.ofSeq)

  /// Builds a map where values of duplicate entries are updated.
  let mapu<'TKey, 'TValue when 'TKey : comparison> = MapBuilder<'TKey, 'TValue seq> (Map.ofSeqg)
