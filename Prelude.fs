namespace System

[<AutoOpen>]
module Prelude =

    /// Clean up resources associated with the input object after the completion of the given function. 
    /// Cleanup occurs even when an exception is raised by the protected code.
    let inline using (x : ^a when ^a : (member Dispose : unit -> unit)) f =
        try f x
        finally (^a : (member Dispose : unit -> unit) x)
