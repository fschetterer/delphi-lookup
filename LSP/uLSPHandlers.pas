unit uLSPHandlers;

/// <summary>
/// LSP method handlers.
/// Implements the LSP methods using delphi-lookup's TQueryProcessor.
/// </summary>

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  uLSPTypes,
  uLSPProtocol,
  uSearchTypes,
  uQueryProcessor;

type
  /// <summary>
  /// LSP server that handles JSON-RPC requests.
  /// </summary>
  TLSPServer = class
  private
    FQueryProcessor: TQueryProcessor;
    FDatabaseFile: string;
    FInitialized: Boolean;
    FShutdownRequested: Boolean;
    FWorkspaceFolders: TStringList;

    // Message handling
    procedure HandleMessage(const AMessage: TLSPMessage);
    procedure DispatchMethod(const AMessage: TLSPMessage);

    // Lifecycle methods
    function HandleInitialize(const AParams: TJSONValue): TJSONValue;
    procedure HandleInitialized(const AParams: TJSONValue);
    function HandleShutdown: TJSONValue;
    procedure HandleExit;

    // Document methods
    function HandleTextDocumentDefinition(const AParams: TJSONValue): TJSONValue;
    function HandleTextDocumentReferences(const AParams: TJSONValue): TJSONValue;
    function HandleTextDocumentHover(const AParams: TJSONValue): TJSONValue;
    function HandleTextDocumentDocumentSymbol(const AParams: TJSONValue): TJSONValue;

    // Workspace methods
    function HandleWorkspaceSymbol(const AParams: TJSONValue): TJSONValue;

    // Helper methods
    function ReadFileContent(const AFilePath: string): string;
    function CreateLocationJSON(const AFilePath: string; ALine: Integer; ACharacter: Integer = 0): TJSONObject;
    function CreateRangeJSON(AStartLine, AStartChar, AEndLine, AEndChar: Integer): TJSONObject;
    function SearchResultToSymbolInfo(AResult: TSearchResult): TJSONObject;
    function SearchResultToDocumentSymbol(AResult: TSearchResult): TJSONObject;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Set the database file to use. Must be called before Run.
    /// </summary>
    procedure SetDatabaseFile(const APath: string);

    /// <summary>
    /// Main message loop. Reads from stdin, processes, writes to stdout.
    /// </summary>
    procedure Run;
  end;

implementation

uses
  System.Variants,
  uPositionResolver,
  uConfig;

{ TLSPServer }

constructor TLSPServer.Create;
begin
  inherited Create;
  FQueryProcessor := nil;
  FDatabaseFile := '';
  FInitialized := False;
  FShutdownRequested := False;
  FWorkspaceFolders := TStringList.Create;
end;

destructor TLSPServer.Destroy;
begin
  FWorkspaceFolders.Free;
  if Assigned(FQueryProcessor) then
    FQueryProcessor.Free;
  inherited Destroy;
end;

procedure TLSPServer.SetDatabaseFile(const APath: string);
begin
  FDatabaseFile := APath;
end;

procedure TLSPServer.Run;
var
  Message: TLSPMessage;
begin
  // Enable logging if environment variable is set
  if GetEnvironmentVariable('DELPHI_LSP_LOG') <> '' then
    TLSPProtocol.EnableLogging(GetEnvironmentVariable('DELPHI_LSP_LOG'));

  while not FShutdownRequested do
  begin
    if not TLSPProtocol.ReadMessage(Message) then
      Break;  // EOF or error

    try
      HandleMessage(Message);
    finally
      if Assigned(Message.Params) then
        Message.Params.Free;
    end;
  end;
end;

procedure TLSPServer.HandleMessage(const AMessage: TLSPMessage);
begin
  // Check for exit notification
  if AMessage.Method = 'exit' then
  begin
    HandleExit;
    Exit;
  end;

  // Dispatch to appropriate handler
  DispatchMethod(AMessage);
end;

procedure TLSPServer.DispatchMethod(const AMessage: TLSPMessage);
var
  Response: TJSONValue;
begin
  Response := nil;

  try
    // Lifecycle methods
    if AMessage.Method = 'initialize' then
      Response := HandleInitialize(AMessage.Params)
    else if AMessage.Method = 'initialized' then
    begin
      HandleInitialized(AMessage.Params);
      Exit;  // Notification, no response
    end
    else if AMessage.Method = 'shutdown' then
      Response := HandleShutdown

    // Check if initialized for other methods
    else if not FInitialized then
    begin
      if AMessage.HasID then
        TLSPProtocol.WriteError(AMessage.ID, ecServerNotInitialized, 'Server not initialized');
      Exit;
    end

    // Document methods
    else if AMessage.Method = 'textDocument/definition' then
      Response := HandleTextDocumentDefinition(AMessage.Params)
    else if AMessage.Method = 'textDocument/references' then
      Response := HandleTextDocumentReferences(AMessage.Params)
    else if AMessage.Method = 'textDocument/hover' then
      Response := HandleTextDocumentHover(AMessage.Params)
    else if AMessage.Method = 'textDocument/documentSymbol' then
      Response := HandleTextDocumentDocumentSymbol(AMessage.Params)

    // Workspace methods
    else if AMessage.Method = 'workspace/symbol' then
      Response := HandleWorkspaceSymbol(AMessage.Params)

    // Unknown method
    else
    begin
      if AMessage.HasID then
        TLSPProtocol.WriteError(AMessage.ID, ecMethodNotFound, 'Method not found: ' + AMessage.Method);
      Exit;
    end;

    // Send response for requests (not notifications)
    if AMessage.HasID then
      TLSPProtocol.WriteResponse(AMessage.ID, Response)
    else if Assigned(Response) then
      Response.Free;

  except
    on E: Exception do
    begin
      if AMessage.HasID then
        TLSPProtocol.WriteError(AMessage.ID, ecInternalError, E.Message);
      if Assigned(Response) then
        Response.Free;
    end;
  end;
end;

function TLSPServer.HandleInitialize(const AParams: TJSONValue): TJSONValue;
var
  Capabilities: TJSONObject;
  ServerInfo: TJSONObject;
  RootURI: string;
begin
  // Extract workspace root
  if (AParams <> nil) and (AParams is TJSONObject) then
  begin
    if TJSONObject(AParams).TryGetValue<string>('rootUri', RootURI) then
      FWorkspaceFolders.Add(URIToFilePath(RootURI));
  end;

  // Initialize database connection
  if FDatabaseFile = '' then
    FDatabaseFile := TPath.Combine(ExtractFilePath(ParamStr(0)), DEFAULT_DB_FILE);

  if FileExists(FDatabaseFile) then
  begin
    FQueryProcessor := TQueryProcessor.Create;
    FQueryProcessor.Initialize(FDatabaseFile);
  end;

  // Build capabilities response
  Capabilities := TJSONObject.Create;
  Capabilities.AddPair('definitionProvider', TJSONBool.Create(True));
  Capabilities.AddPair('referencesProvider', TJSONBool.Create(True));
  Capabilities.AddPair('hoverProvider', TJSONBool.Create(True));
  Capabilities.AddPair('documentSymbolProvider', TJSONBool.Create(True));
  Capabilities.AddPair('workspaceSymbolProvider', TJSONBool.Create(True));

  ServerInfo := TJSONObject.Create;
  ServerInfo.AddPair('name', 'delphi-lsp-server');
  ServerInfo.AddPair('version', '1.1.0');

  Result := TJSONObject.Create;
  TJSONObject(Result).AddPair('capabilities', Capabilities);
  TJSONObject(Result).AddPair('serverInfo', ServerInfo);
end;

procedure TLSPServer.HandleInitialized(const AParams: TJSONValue);
begin
  FInitialized := True;
end;

function TLSPServer.HandleShutdown: TJSONValue;
begin
  FShutdownRequested := True;
  Result := TJSONNull.Create;
end;

procedure TLSPServer.HandleExit;
begin
  if FShutdownRequested then
    Halt(0)
  else
    Halt(1);
end;

function TLSPServer.ReadFileContent(const AFilePath: string): string;
begin
  if FileExists(AFilePath) then
    Result := TFile.ReadAllText(AFilePath)
  else
    Result := '';
end;

function TLSPServer.CreateLocationJSON(const AFilePath: string; ALine: Integer; ACharacter: Integer): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('uri', FilePathToURI(AFilePath));
  Result.AddPair('range', CreateRangeJSON(ALine, ACharacter, ALine, ACharacter));
end;

function TLSPServer.CreateRangeJSON(AStartLine, AStartChar, AEndLine, AEndChar: Integer): TJSONObject;
var
  Start, End_: TJSONObject;
begin
  Start := TJSONObject.Create;
  Start.AddPair('line', TJSONNumber.Create(AStartLine));
  Start.AddPair('character', TJSONNumber.Create(AStartChar));

  End_ := TJSONObject.Create;
  End_.AddPair('line', TJSONNumber.Create(AEndLine));
  End_.AddPair('character', TJSONNumber.Create(AEndChar));

  Result := TJSONObject.Create;
  Result.AddPair('start', Start);
  Result.AddPair('end', End_);
end;

function TLSPServer.HandleTextDocumentDefinition(const AParams: TJSONValue): TJSONValue;
var
  URI, FilePath, SymbolName, FileContent: string;
  Line, Character: Integer;
  SearchResult: TSearchResult;
begin
  Result := TJSONNull.Create;

  if not Assigned(FQueryProcessor) then
    Exit;

  // Extract parameters
  URI := AParams.GetValue<string>('textDocument.uri');
  Line := AParams.GetValue<Integer>('position.line');
  Character := AParams.GetValue<Integer>('position.character');

  FilePath := URIToFilePath(URI);
  FileContent := ReadFileContent(FilePath);

  if FileContent = '' then
    Exit;

  // Get identifier at cursor position
  SymbolName := TPositionResolver.GetIdentifierAtPosition(FileContent, Line, Character);
  if SymbolName = '' then
    Exit;

  // Search for symbol definition
  SearchResult := FQueryProcessor.FindSymbolDefinition(SymbolName);
  if Assigned(SearchResult) then
  begin
    try
      Result.Free;
      // Convert to 0-indexed for LSP (database is 1-indexed)
      Result := CreateLocationJSON(SearchResult.FilePath, SearchResult.StartLine - 1, 0);
    finally
      SearchResult.Free;
    end;
  end;
end;

function TLSPServer.HandleTextDocumentReferences(const AParams: TJSONValue): TJSONValue;
var
  URI, FilePath, SymbolName, FileContent: string;
  Line, Character: Integer;
  References: TSearchResultList;
  RefArray: TJSONArray;
  I: Integer;
begin
  RefArray := TJSONArray.Create;
  Result := RefArray;

  if not Assigned(FQueryProcessor) then
    Exit;

  // Extract parameters
  URI := AParams.GetValue<string>('textDocument.uri');
  Line := AParams.GetValue<Integer>('position.line');
  Character := AParams.GetValue<Integer>('position.character');

  FilePath := URIToFilePath(URI);
  FileContent := ReadFileContent(FilePath);

  if FileContent = '' then
    Exit;

  // Get identifier at cursor position
  SymbolName := TPositionResolver.GetIdentifierAtPosition(FileContent, Line, Character);
  if SymbolName = '' then
    Exit;

  // Find all references
  References := FQueryProcessor.FindSymbolReferences(SymbolName, 100);
  try
    for I := 0 to References.Count - 1 do
      RefArray.AddElement(CreateLocationJSON(
        References[I].FilePath,
        References[I].StartLine - 1,  // Convert to 0-indexed
        0));
  finally
    References.Free;
  end;
end;

function TLSPServer.HandleTextDocumentHover(const AParams: TJSONValue): TJSONValue;
var
  URI, FilePath, SymbolName, FileContent: string;
  Line, Character: Integer;
  SearchResult: TSearchResult;
  HoverContent: string;
  Contents: TJSONObject;
begin
  Result := TJSONNull.Create;

  if not Assigned(FQueryProcessor) then
    Exit;

  // Extract parameters
  URI := AParams.GetValue<string>('textDocument.uri');
  Line := AParams.GetValue<Integer>('position.line');
  Character := AParams.GetValue<Integer>('position.character');

  FilePath := URIToFilePath(URI);
  FileContent := ReadFileContent(FilePath);

  if FileContent = '' then
    Exit;

  // Get identifier at cursor position
  SymbolName := TPositionResolver.GetIdentifierAtPosition(FileContent, Line, Character);
  if SymbolName = '' then
    Exit;

  // Search for symbol
  SearchResult := FQueryProcessor.FindSymbolDefinition(SymbolName);
  if Assigned(SearchResult) then
  begin
    try
      // Build hover content as markdown
      HoverContent := '```pascal' + #10;
      HoverContent := HoverContent + SearchResult.Content;
      HoverContent := HoverContent + #10 + '```';

      if SearchResult.Comments <> '' then
        HoverContent := HoverContent + #10#10 + SearchResult.Comments;

      HoverContent := HoverContent + #10#10 + '_File: ' + ExtractFileName(SearchResult.FilePath) + '_';

      Contents := TJSONObject.Create;
      Contents.AddPair('kind', 'markdown');
      Contents.AddPair('value', HoverContent);

      Result.Free;
      Result := TJSONObject.Create;
      TJSONObject(Result).AddPair('contents', Contents);

    finally
      SearchResult.Free;
    end;
  end;
end;

function TLSPServer.SearchResultToDocumentSymbol(AResult: TSearchResult): TJSONObject;
var
  Kind: Integer;
  Range: TJSONObject;
begin
  Kind := Integer(SymbolTypeToLSPKind(AResult.SymbolType));

  // Create range (convert from 1-indexed to 0-indexed)
  Range := CreateRangeJSON(
    AResult.StartLine - 1, 0,
    AResult.EndLine - 1, 0);

  Result := TJSONObject.Create;
  Result.AddPair('name', AResult.Name);
  Result.AddPair('kind', TJSONNumber.Create(Kind));
  Result.AddPair('range', Range);
  Result.AddPair('selectionRange', TJSONObject(Range.Clone));

  if AResult.ParentClass <> '' then
    Result.AddPair('detail', AResult.ParentClass + '.' + AResult.Name);
end;

function TLSPServer.SearchResultToSymbolInfo(AResult: TSearchResult): TJSONObject;
var
  Kind: Integer;
  Location: TJSONObject;
begin
  Kind := Integer(SymbolTypeToLSPKind(AResult.SymbolType));

  Location := CreateLocationJSON(
    AResult.FilePath,
    AResult.StartLine - 1,  // Convert to 0-indexed
    0);

  Result := TJSONObject.Create;
  Result.AddPair('name', AResult.Name);
  Result.AddPair('kind', TJSONNumber.Create(Kind));
  Result.AddPair('location', Location);

  if AResult.ParentClass <> '' then
    Result.AddPair('containerName', AResult.ParentClass);
end;

function TLSPServer.HandleTextDocumentDocumentSymbol(const AParams: TJSONValue): TJSONValue;
var
  URI, FilePath: string;
  Symbols: TSearchResultList;
  SymbolArray: TJSONArray;
  I: Integer;
begin
  SymbolArray := TJSONArray.Create;
  Result := SymbolArray;

  if not Assigned(FQueryProcessor) then
    Exit;

  // Extract parameters
  URI := AParams.GetValue<string>('textDocument.uri');
  FilePath := URIToFilePath(URI);

  // Get all symbols in file
  Symbols := FQueryProcessor.GetSymbolsByFile(FilePath);
  try
    for I := 0 to Symbols.Count - 1 do
      SymbolArray.AddElement(SearchResultToDocumentSymbol(Symbols[I]));
  finally
    Symbols.Free;
  end;
end;

function TLSPServer.HandleWorkspaceSymbol(const AParams: TJSONValue): TJSONValue;
var
  Query: string;
  Results: TSearchResultList;
  SymbolArray: TJSONArray;
  I: Integer;
begin
  SymbolArray := TJSONArray.Create;
  Result := SymbolArray;

  if not Assigned(FQueryProcessor) then
    Exit;

  // Extract query
  Query := AParams.GetValue<string>('query', '');
  if Query = '' then
    Exit;

  // Perform hybrid search (reuses existing functionality!)
  Results := FQueryProcessor.PerformHybridSearch(Query, 50, nil, 0.3);
  try
    for I := 0 to Results.Count - 1 do
      SymbolArray.AddElement(SearchResultToSymbolInfo(Results[I]));
  finally
    Results.Free;
  end;
end;

end.
