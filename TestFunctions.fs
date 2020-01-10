[<AutoOpen>]
module TestFunctions

  let always x _ = x

  let always2 x _ _ = x

  let always3 x _ _ _ = x

  let twice f x = f x |> ignore; f x

  let times f i x = for n in 0..i do f x |> ignore; f x

  let lifecycles setup teardown fs x =
    setup ()
    let exs = System.Collections.Generic.List<_> ()
    for f in fs do
      try f x |> ignore
      with ex -> exs.Add ex
    teardown ()
    if exs.Count > 0
    then raise (System.AggregateException exs)

  let lifecycle setup teardown f x = lifecycles setup teardown [f] x

  let lifecyclesAsync setupAsync teardownAsync fs x = async {
    do! setupAsync ()
    let exs = System.Collections.Generic.List<_> ()
    for f in fs do
      try do! f x
      with ex -> exs.Add ex
    do! teardownAsync ()
    if exs.Count > 0
    then raise (System.AggregateException exs) }

  let lifecycleAsync setupAsync teardownAsync f x = lifecyclesAsync setupAsync teardownAsync [f] x

