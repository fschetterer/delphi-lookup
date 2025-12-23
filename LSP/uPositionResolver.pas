unit uPositionResolver;

/// <summary>
/// Resolves cursor positions to Pascal identifiers.
/// Used to extract the symbol name at a given (line, column) position.
/// </summary>

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>
  /// Utility class for resolving positions in Pascal source code.
  /// </summary>
  TPositionResolver = class
  public
    /// <summary>
    /// Get the Pascal identifier at the specified cursor position.
    /// Line and Column are 0-indexed (LSP convention).
    /// Returns empty string if no identifier is found.
    /// </summary>
    class function GetIdentifierAtPosition(
      const AContent: string;
      ALine, AColumn: Integer): string;

    /// <summary>
    /// Get a specific line from content (0-indexed).
    /// </summary>
    class function GetLine(const AContent: string; ALine: Integer): string;

    /// <summary>
    /// Check if a character is valid in a Pascal identifier.
    /// </summary>
    class function IsIdentifierChar(C: Char): Boolean; inline;

    /// <summary>
    /// Check if a character can start a Pascal identifier.
    /// </summary>
    class function IsIdentifierStartChar(C: Char): Boolean; inline;

    /// <summary>
    /// Find the bounds of the identifier at the given position in a line.
    /// Returns True if found, with StartPos and EndPos set (1-indexed).
    /// </summary>
    class function FindIdentifierBounds(
      const ALine: string;
      AColumn: Integer;
      out AStartPos, AEndPos: Integer): Boolean;

    /// <summary>
    /// Extract the fully qualified name at position (e.g., TMyClass.MyMethod).
    /// </summary>
    class function GetQualifiedIdentifierAtPosition(
      const AContent: string;
      ALine, AColumn: Integer): string;
  end;

implementation

uses
  System.Character;

{ TPositionResolver }

class function TPositionResolver.IsIdentifierChar(C: Char): Boolean;
begin
  Result := C.IsLetterOrDigit or (C = '_');
end;

class function TPositionResolver.IsIdentifierStartChar(C: Char): Boolean;
begin
  Result := C.IsLetter or (C = '_');
end;

class function TPositionResolver.GetLine(const AContent: string; ALine: Integer): string;
var
  Lines: TStringList;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.Text := AContent;
    if (ALine >= 0) and (ALine < Lines.Count) then
      Result := Lines[ALine];
  finally
    Lines.Free;
  end;
end;

class function TPositionResolver.FindIdentifierBounds(
  const ALine: string;
  AColumn: Integer;
  out AStartPos, AEndPos: Integer): Boolean;
var
  Len: Integer;
  Pos: Integer;  // 1-indexed position in string
begin
  Result := False;
  AStartPos := 0;
  AEndPos := 0;
  Len := Length(ALine);

  if Len = 0 then
    Exit;

  // Convert 0-indexed column to 1-indexed string position
  Pos := AColumn + 1;

  // Clamp to valid range
  if Pos < 1 then
    Pos := 1;
  if Pos > Len then
    Pos := Len;

  // Check if we're on an identifier character
  if not IsIdentifierChar(ALine[Pos]) then
  begin
    // Maybe we're just after an identifier (cursor at end)
    if (Pos > 1) and IsIdentifierChar(ALine[Pos - 1]) then
      Dec(Pos)
    else
      Exit;
  end;

  // Find start of identifier (go backwards)
  AStartPos := Pos;
  while (AStartPos > 1) and IsIdentifierChar(ALine[AStartPos - 1]) do
    Dec(AStartPos);

  // Make sure we start with a valid identifier start char
  if not IsIdentifierStartChar(ALine[AStartPos]) then
    Exit;

  // Find end of identifier (go forwards)
  AEndPos := Pos;
  while (AEndPos < Len) and IsIdentifierChar(ALine[AEndPos + 1]) do
    Inc(AEndPos);

  Result := True;
end;

class function TPositionResolver.GetIdentifierAtPosition(
  const AContent: string;
  ALine, AColumn: Integer): string;
var
  Line: string;
  StartPos, EndPos: Integer;
begin
  Result := '';

  Line := GetLine(AContent, ALine);
  if Line = '' then
    Exit;

  if FindIdentifierBounds(Line, AColumn, StartPos, EndPos) then
    Result := Copy(Line, StartPos, EndPos - StartPos + 1);
end;

class function TPositionResolver.GetQualifiedIdentifierAtPosition(
  const AContent: string;
  ALine, AColumn: Integer): string;
var
  Line: string;
  StartPos, EndPos: Integer;
  TempStart, TempEnd: Integer;
  Len: Integer;
begin
  Result := '';

  Line := GetLine(AContent, ALine);
  if Line = '' then
    Exit;

  if not FindIdentifierBounds(Line, AColumn, StartPos, EndPos) then
    Exit;

  Len := Length(Line);

  // Extend backwards to include qualified parts (Foo.Bar.Baz)
  TempStart := StartPos;
  while TempStart > 2 do
  begin
    // Check for dot before identifier
    if Line[TempStart - 1] = '.' then
    begin
      // Check if there's an identifier before the dot
      if IsIdentifierChar(Line[TempStart - 2]) then
      begin
        // Find the start of that identifier
        TempStart := TempStart - 2;
        while (TempStart > 1) and IsIdentifierChar(Line[TempStart - 1]) do
          Dec(TempStart);
      end
      else
        Break;
    end
    else
      Break;
  end;

  // Extend forwards to include qualified parts
  TempEnd := EndPos;
  while TempEnd < Len - 1 do
  begin
    // Check for dot after identifier
    if Line[TempEnd + 1] = '.' then
    begin
      // Check if there's an identifier after the dot
      if (TempEnd + 2 <= Len) and IsIdentifierStartChar(Line[TempEnd + 2]) then
      begin
        TempEnd := TempEnd + 2;
        while (TempEnd < Len) and IsIdentifierChar(Line[TempEnd + 1]) do
          Inc(TempEnd);
      end
      else
        Break;
    end
    else
      Break;
  end;

  Result := Copy(Line, TempStart, TempEnd - TempStart + 1);
end;

end.
