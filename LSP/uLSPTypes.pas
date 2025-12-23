unit uLSPTypes;

/// <summary>
/// LSP (Language Server Protocol) type definitions.
/// Based on LSP Specification 3.17.
/// </summary>

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>
  /// Position in a text document expressed as zero-based line and character offset.
  /// </summary>
  TLSPPosition = record
    Line: Integer;
    Character: Integer;
    class function Create(ALine, ACharacter: Integer): TLSPPosition; static;
  end;

  /// <summary>
  /// A range in a text document expressed as (zero-based) start and end positions.
  /// </summary>
  TLSPRange = record
    Start: TLSPPosition;
    End_: TLSPPosition;
    class function Create(AStartLine, AStartChar, AEndLine, AEndChar: Integer): TLSPRange; static;
    class function CreateSingleLine(ALine, AStartChar, AEndChar: Integer): TLSPRange; static;
  end;

  /// <summary>
  /// Represents a location inside a resource, such as a line inside a text file.
  /// </summary>
  TLSPLocation = record
    URI: string;
    Range: TLSPRange;
    class function Create(const AURI: string; ARange: TLSPRange): TLSPLocation; static;
  end;

  /// <summary>
  /// A symbol kind for workspace/symbol and textDocument/documentSymbol.
  /// </summary>
  TLSPSymbolKind = (
    skUnknown = 0,
    skFile = 1,
    skModule = 2,
    skNamespace = 3,
    skPackage = 4,
    skClass = 5,
    skMethod = 6,
    skProperty = 7,
    skField = 8,
    skConstructor = 9,
    skEnum = 10,
    skInterface = 11,
    skFunction = 12,
    skVariable = 13,
    skConstant = 14,
    skString = 15,
    skNumber = 16,
    skBoolean = 17,
    skArray = 18,
    skObject = 19,
    skKey = 20,
    skNull = 21,
    skEnumMember = 22,
    skStruct = 23,
    skEvent = 24,
    skOperator = 25,
    skTypeParameter = 26
  );

  /// <summary>
  /// Represents information about programming constructs like variables, classes, interfaces, etc.
  /// </summary>
  TLSPSymbolInformation = record
    Name: string;
    Kind: TLSPSymbolKind;
    Location: TLSPLocation;
    ContainerName: string;
  end;

  /// <summary>
  /// Represents a document symbol (for textDocument/documentSymbol).
  /// Hierarchical representation with children.
  /// </summary>
  TLSPDocumentSymbol = class
  private
    FName: string;
    FDetail: string;
    FKind: TLSPSymbolKind;
    FRange: TLSPRange;
    FSelectionRange: TLSPRange;
    FChildren: TObjectList<TLSPDocumentSymbol>;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property Detail: string read FDetail write FDetail;
    property Kind: TLSPSymbolKind read FKind write FKind;
    property Range: TLSPRange read FRange write FRange;
    property SelectionRange: TLSPRange read FSelectionRange write FSelectionRange;
    property Children: TObjectList<TLSPDocumentSymbol> read FChildren;
  end;

  /// <summary>
  /// The marked string is rendered with markdown in hover results.
  /// </summary>
  TLSPMarkupContent = record
    Kind: string;   // 'plaintext' or 'markdown'
    Value: string;
    class function CreateMarkdown(const AValue: string): TLSPMarkupContent; static;
    class function CreatePlainText(const AValue: string): TLSPMarkupContent; static;
  end;

  /// <summary>
  /// The result of a hover request.
  /// </summary>
  TLSPHover = record
    Contents: TLSPMarkupContent;
    Range: TLSPRange;
    HasRange: Boolean;
  end;

  /// <summary>
  /// Text document identifier (contains just a URI).
  /// </summary>
  TLSPTextDocumentIdentifier = record
    URI: string;
  end;

  /// <summary>
  /// Parameters for textDocument/definition, textDocument/references, etc.
  /// </summary>
  TLSPTextDocumentPositionParams = record
    TextDocument: TLSPTextDocumentIdentifier;
    Position: TLSPPosition;
  end;

  /// <summary>
  /// Server capabilities for initialize response.
  /// </summary>
  TLSPServerCapabilities = record
    DefinitionProvider: Boolean;
    ReferencesProvider: Boolean;
    HoverProvider: Boolean;
    DocumentSymbolProvider: Boolean;
    WorkspaceSymbolProvider: Boolean;
  end;

  /// <summary>
  /// LSP Error Codes
  /// </summary>
  TLSPErrorCode = (
    ecParseError = -32700,
    ecInvalidRequest = -32600,
    ecMethodNotFound = -32601,
    ecInvalidParams = -32602,
    ecInternalError = -32603,
    ecServerNotInitialized = -32002,
    ecUnknownErrorCode = -32001,
    ecRequestCancelled = -32800,
    ecContentModified = -32801
  );

/// <summary>
/// Convert delphi-lookup symbol type to LSP SymbolKind.
/// </summary>
function SymbolTypeToLSPKind(const AType: string): TLSPSymbolKind;

/// <summary>
/// Convert a Windows file path to a file:// URI.
/// </summary>
function FilePathToURI(const APath: string): string;

/// <summary>
/// Convert a file:// URI back to a Windows file path.
/// </summary>
function URIToFilePath(const AURI: string): string;

implementation

uses
  System.NetEncoding;

{ TLSPPosition }

class function TLSPPosition.Create(ALine, ACharacter: Integer): TLSPPosition;
begin
  Result.Line := ALine;
  Result.Character := ACharacter;
end;

{ TLSPRange }

class function TLSPRange.Create(AStartLine, AStartChar, AEndLine, AEndChar: Integer): TLSPRange;
begin
  Result.Start := TLSPPosition.Create(AStartLine, AStartChar);
  Result.End_ := TLSPPosition.Create(AEndLine, AEndChar);
end;

class function TLSPRange.CreateSingleLine(ALine, AStartChar, AEndChar: Integer): TLSPRange;
begin
  Result := Create(ALine, AStartChar, ALine, AEndChar);
end;

{ TLSPLocation }

class function TLSPLocation.Create(const AURI: string; ARange: TLSPRange): TLSPLocation;
begin
  Result.URI := AURI;
  Result.Range := ARange;
end;

{ TLSPDocumentSymbol }

constructor TLSPDocumentSymbol.Create;
begin
  inherited Create;
  FChildren := TObjectList<TLSPDocumentSymbol>.Create(True);
end;

destructor TLSPDocumentSymbol.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

{ TLSPMarkupContent }

class function TLSPMarkupContent.CreateMarkdown(const AValue: string): TLSPMarkupContent;
begin
  Result.Kind := 'markdown';
  Result.Value := AValue;
end;

class function TLSPMarkupContent.CreatePlainText(const AValue: string): TLSPMarkupContent;
begin
  Result.Kind := 'plaintext';
  Result.Value := AValue;
end;

{ Helper Functions }

function SymbolTypeToLSPKind(const AType: string): TLSPSymbolKind;
var
  LowerType: string;
begin
  LowerType := LowerCase(AType);

  if LowerType = 'class' then
    Result := skClass
  else if LowerType = 'interface' then
    Result := skInterface
  else if LowerType = 'record' then
    Result := skStruct
  else if LowerType = 'function' then
    Result := skFunction
  else if LowerType = 'procedure' then
    Result := skMethod
  else if LowerType = 'constructor' then
    Result := skConstructor
  else if LowerType = 'destructor' then
    Result := skMethod
  else if LowerType = 'property' then
    Result := skProperty
  else if LowerType = 'const' then
    Result := skConstant
  else if LowerType = 'var' then
    Result := skVariable
  else if LowerType = 'type' then
    Result := skClass  // Generic type alias
  else if LowerType = 'enum' then
    Result := skEnum
  else if LowerType = 'field' then
    Result := skField
  else if LowerType = 'unit' then
    Result := skModule
  else
    Result := skVariable;  // Default fallback
end;

function FilePathToURI(const APath: string): string;
var
  Encoded: string;
begin
  // Convert Windows path to file:// URI
  // C:\Foo\Bar.pas -> file:///C:/Foo/Bar.pas
  Encoded := StringReplace(APath, '\', '/', [rfReplaceAll]);

  // URL-encode special characters (but not / or :)
  Encoded := TNetEncoding.URL.Encode(Encoded);
  Encoded := StringReplace(Encoded, '%2F', '/', [rfReplaceAll]);
  Encoded := StringReplace(Encoded, '%3A', ':', [rfReplaceAll]);

  Result := 'file:///' + Encoded;
end;

function URIToFilePath(const AURI: string): string;
begin
  Result := AURI;

  // Remove file:/// prefix
  if Result.StartsWith('file:///', True) then
    Result := Copy(Result, 9, MaxInt)
  else if Result.StartsWith('file://', True) then
    Result := Copy(Result, 8, MaxInt);

  // URL-decode
  Result := TNetEncoding.URL.Decode(Result);

  // Convert forward slashes to backslashes for Windows
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
end;

end.
