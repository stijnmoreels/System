namespace System.Text.RegularExpressions

open System

[<AutoOpen>]
module Regexp =
  let [<CompiledName("AnyChar")>] anyChar = "."
  let [<CompiledName("Digits")>] digits = "0-9"
  let [<CompiledName("AnyDigit")>] anyDigit = "\d"
  let [<CompiledName("NonDigit")>] nonDigit = "\D"
  let [<CompiledName("Dot")>] dot = "\."
  let [<CompiledName("From_a_To_z")>] az = "a-z"
  let [<CompiledName("From_A_To_Z")>] AZ = "A-Z"
  let [<CompiledName("Underscore")>] underscore = "_"
  let [<CompiledName("Hyphen")>] hyphen = "-"
  let [<CompiledName("AnyWord")>] anyWord = "\w"
  let [<CompiledName("NonWord")>] nonWord = "\W"
  let [<CompiledName("Whitespace")>] whitespace = "\s"
  let [<CompiledName("NonWhitespace")>] nonWhitespace = "\S"
  let [<CompiledName("AtSign")>] at_sig = "@"
  let [<CompiledName("ForwardSlash")>] f_slash = "\/"
  let [<CompiledName("BackwardSlash")>] b_slash = "\\"
  let [<CompiledName("Space")>] space = " "
  
  let [<CompiledName("FromTo")>] from_to beginning ending = sprintf "%s-%s" beginning ending
  let [<CompiledName("Add")>] add r pattern = r + pattern

  let [<CompiledName("Start")>] start x = sprintf "^%s" x
  let [<CompiledName("Stop")>] stop x = sprintf "%s$" x

  let internal joinf x gs = String.Join (x, (gs : string list))
  let internal join gs = joinf String.Empty gs
  let internal ensureGroupThen state x = if (state : string).EndsWith ")" then sprintf "%s%s" state x else sprintf "(%s)%s" state x

  let [<CompiledName("ZeroOrOne")>] zero_or_one x = ensureGroupThen x "?"
  let [<CompiledName("ZeroOrMore")>] zero_or_more x = ensureGroupThen x "*"
  let [<CompiledName("OneOrMore")>] one_or_more x = ensureGroupThen x "+"
  let [<CompiledName("Times")>] times i x = sprintf "%s{%i}" x i
  let [<CompiledName("TimesMin")>] times_min i x = sprintf "%s{%i,}" x i
  let [<CompiledName("TimesMinMax")>] times_min_max min max state = sprintf "%s{%i,%i}" state min max

  let [<CompiledName("Alt")>] alt alts x = sprintf "%s%s" x (joinf "|" alts) 
  let [<CompiledName("OrElse")>] orElse x = sprintf "%s|" x
  let [<CompiledName("IfThenElse")>] ifThenElse expr matchTrue matchFalse x = sprintf "%s(?(%s)%s|%s)" x expr matchTrue matchFalse

  let [<CompiledName("OneOf")>] oneof gs x = sprintf "%s[%s]" x (join gs)
  let [<CompiledName("NotOneOf")>] not_oneof gs x = sprintf "%s[^%s]" x (join gs)

  let [<CompiledName("GroupNamed")>] group_named name g x = sprintf "%s(?<%s>%s)" x name g
  let [<CompiledName("Group")>] group g x = sprintf "%s(%s)" x g
  let [<CompiledName("GroupStart")>] group_start x = sprintf "%s(" x
  let [<CompiledName("GroupStop")>] group_stop x = sprintf "%s)" x
  let [<CompiledName("Removes")>] removes gs x = sprintf "%s(:?%s)" x (join gs)
  let [<CompiledName("Comment")>] comment txt x = sprintf "%s(?#%s)" x txt

[<CompiledName("RegexCE")>]
type RegexBuilder () =
  member __.Yield (_) = String.Empty
  member __.Run (state) = Regex state

  [<CustomOperation("start")>]
  member __.Start (state) = start state
  
  [<CustomOperation("stop")>]
  member __.End (state) = stop state

  [<CustomOperation("zero_or_one")>]
  member __.ZeroOrOne (state) = zero_or_one state

  [<CustomOperation("zero_or_more")>]
  member __.ZeroOrMore (state) = zero_or_more state

  [<CustomOperation("one_or_more")>]
  member __.OneOrMore (state) = one_or_more state

  [<CustomOperation("times")>]
  member __.Times (state, x) = times x state

  [<CustomOperation("times_min")>]
  member __.AtLeastTimes (state, x) = times_min x state

  [<CustomOperation("times_min_max")>]
  member __.MinMaxTimes (state, min, max) = times_min_max min max state

  [<CustomOperation("alt")>]
  member __.Alternation (state, alts) = alt alts state

  [<CustomOperation("orElse")>]
  member __.OrElse (state) = orElse state

  [<CustomOperation("ifThenElse")>]
  member __.IfThenElse (state, expr, matchTrue, matchFalse) = ifThenElse expr matchTrue matchFalse state

  [<CustomOperation("add")>]
  member __.Add (state, p) = add state p

  [<CustomOperation("oneof")>]
  member __.OneOf (state, gs) = oneof gs state
  
  [<CustomOperation("not_oneof")>]
  member __.NotOneOf (state, gs) = not_oneof gs state

  [<CustomOperation("group_named")>]
  member __.GroupNamed (state, name, g : Regex) = group_named name (g.ToString()) state

  [<CustomOperation("group")>]
  member __.Group (state, g : Regex) = group (g.ToString()) state

  [<CustomOperation("group_start")>]
  member __.GroupStart (state) = group_start state

  [<CustomOperation("group_stop")>]
  member __.GroupStop (state) = group_stop state

  [<CustomOperation("removes")>]
  member __.Removes (state, gs) = removes gs state

  [<CustomOperation("comment")>]
  member __.Comment (state, txt) = comment txt state

[<CompiledName("RegexBuilder")>]
type RegexBuilderCSharp () =
  member val private Pattern = String.Empty with get, set
  member this.Build () = Regex this.Pattern
  member this.Build (options) = Regex (this.Pattern, options)
  member this.Build (options, timeout) = Regex (this.Pattern, options, timeout)
  
  member this.Add (pattern) = this.Pattern <- this.Pattern + pattern; this
  member private this.Add (addPattern) = this.Add (addPattern this.Pattern)
  member this.Start () = this.Add start
  member this.Stop () = this.Add stop
  member this.OneOf ([<ParamArray>] sequence : string array) = this.Add (oneof (List.ofArray sequence))
  member this.NotOneOf ([<ParamArray>] sequence : string array) = this.Add (not_oneof (List.ofArray sequence))
  member this.Times (amount) = this.Add (times amount)
  member this.TimesMin (min) = this.Add (times_min min)
  member this.TimesMinMax (min, max) = this.Add (times_min_max min max)
  member this.ZeroOrOne () = this.Add zero_or_one
  member this.ZeroOrMore () = this.Add zero_or_more
  member this.OneOrMore () = this.Add one_or_more
  member this.Group (name, setGroup : Func<RegexBuilderCSharp, RegexBuilderCSharp>) = this.Add (group_named name (setGroup.Invoke this).Pattern)
  member this.Group (setGroup : Func<RegexBuilderCSharp, RegexBuilderCSharp>) = this.Add (group (setGroup.Invoke this).Pattern)
  member this.Comment (txt) = this.Add (comment txt)

[<AutoOpen>]
module RegexBuilders =

  let regex = RegexBuilder ()
