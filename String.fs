namespace System

/// Common extensions on the System.String type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
  /// Determines if a string is not `null`.
  let isNotNull (x : string) = isNull x |> not
  /// Determines if a string is not empty.
  let isNotEmpty (x : string) = String.IsNullOrEmpty x |> not
  /// Determines if a string is not blank.
  let isNotBlank (x : string) = String.IsNullOrWhiteSpace x |> not
