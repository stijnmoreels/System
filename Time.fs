namespace System

module TimeInt =
    let _100ms = 100
    let _500ms = 500
    let _1s = 1000
    let _2s = 2000
    let _3s = 3000
    let _4s = 4000
    let _5s = 5000
    let _10s = 10_000
    let _20s = 20_000
    let _30s = 30_000
    let _1min = 60_000
    let _5min = 300_000
    let _10min = 600_000

module TimeString =
    let _100ms = "00:00:00.1"
    let _500ms = "00:00:00.5"
    let _1s = "00:00:01"
    let _2s = "00:00:02"
    let _3s = "00:00:03"
    let _4s = "00:00:04"
    let _5s = "00:00:05"
    let _10s = "00:00:10"
    let _20s = "00:00:20"
    let _30s = "00:00:30"
    let _1min = "00:01:00"
    let _5min = "00:05:00"
    let _10min = "00:10:00"

[<AutoOpen>]
module TimeSpans = 
    let _100ms = TimeSpan.FromMilliseconds 100.
    let _500ms = TimeSpan.FromMilliseconds 500.
    let _1s = TimeSpan.FromSeconds 1.
    let _2s = TimeSpan.FromSeconds 2.
    let _3s = TimeSpan.FromSeconds 3.
    let _4s = TimeSpan.FromSeconds 4.
    let _5s = TimeSpan.FromSeconds 5.
    let _10s = TimeSpan.FromSeconds 10.
    let _20s = TimeSpan.FromSeconds 20.
    let _30s = TimeSpan.FromSeconds 30.
    let _1min = TimeSpan.FromMinutes 1.
    let _5min = TimeSpan.FromMinutes 5.
    let _10min = TimeSpan.FromMinutes 10.

    type TimeSpan with
        static member _100ms = TimeSpan.FromMilliseconds 100.
        static member _500ms = TimeSpan.FromMilliseconds 500.
        static member _1s = TimeSpan.FromSeconds 1.
        static member _2s = TimeSpan.FromSeconds 2.
        static member _3s = TimeSpan.FromSeconds 3.
        static member _4s = TimeSpan.FromSeconds 4.
        static member _5s = TimeSpan.FromSeconds 5.
        static member _10s = TimeSpan.FromSeconds 10.
        static member _20s = TimeSpan.FromSeconds 20.
        static member _30s = TimeSpan.FromSeconds 30.
        static member _1min = TimeSpan.FromMinutes 1.
        static member _5min = TimeSpan.FromMinutes 5.
        static member _10min = TimeSpan.FromMinutes 10.
