unit uLSPProtocol;

/// <summary>
/// LSP JSON-RPC 2.0 protocol handler.
/// Handles reading/writing LSP messages via stdio.
/// </summary>

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Variants,
  uLSPTypes;

type
  /// <summary>
  /// Represents a parsed LSP request/notification message.
  /// </summary>
  TLSPMessage = record
    JSONRPC: string;
    ID: Variant;          // Integer, string, or Null for notifications
    Method: string;
    Params: TJSONValue;   // Owned by the message, freed when done
    IsNotification: Boolean;

    function HasID: Boolean;
  end;

  /// <summary>
  /// LSP protocol handler for JSON-RPC 2.0 over stdio.
  /// </summary>
  TLSPProtocol = class
  private
    class var FInputStream: TStream;
    class var FOutputStream: TStream;
    class var FLogFile: TextFile;
    class var FLoggingEnabled: Boolean;

    class procedure Log(const AMessage: string);
    class function ReadHeaders: TStringList;
    class function ReadContent(ALength: Integer): string;
  public
    class procedure Initialize;
    class procedure Finalize;

    /// <summary>
    /// Enable logging to a file for debugging.
    /// </summary>
    class procedure EnableLogging(const ALogFile: string);

    /// <summary>
    /// Read the next LSP message from stdin.
    /// Returns True if a message was read, False on EOF/error.
    /// </summary>
    class function ReadMessage(out AMessage: TLSPMessage): Boolean;

    /// <summary>
    /// Write a successful response.
    /// </summary>
    class procedure WriteResponse(const AID: Variant; AResult: TJSONValue);

    /// <summary>
    /// Write an error response.
    /// </summary>
    class procedure WriteError(const AID: Variant; ACode: TLSPErrorCode; const AMessage: string);

    /// <summary>
    /// Write a notification (no ID, no response expected).
    /// </summary>
    class procedure WriteNotification(const AMethod: string; AParams: TJSONValue);

    /// <summary>
    /// Write a raw JSON-RPC message.
    /// </summary>
    class procedure WriteMessage(AMessage: TJSONObject);
  end;

implementation

uses
  System.IOUtils;

{ TLSPMessage }

function TLSPMessage.HasID: Boolean;
begin
  Result := not VarIsNull(ID) and not VarIsEmpty(ID);
end;

{ TLSPProtocol }

class procedure TLSPProtocol.Initialize;
begin
  // Use standard input/output streams
  // In Delphi console apps, we read from Input and write to Output
  FInputStream := nil;  // We'll use ReadLn-based approach
  FOutputStream := nil;
  FLoggingEnabled := False;
end;

class procedure TLSPProtocol.Finalize;
begin
  if FLoggingEnabled then
    CloseFile(FLogFile);
end;

class procedure TLSPProtocol.EnableLogging(const ALogFile: string);
begin
  AssignFile(FLogFile, ALogFile);
  Rewrite(FLogFile);
  FLoggingEnabled := True;
  Log('LSP Protocol logging started');
end;

class procedure TLSPProtocol.Log(const AMessage: string);
begin
  if FLoggingEnabled then
  begin
    WriteLn(FLogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' ' + AMessage);
    Flush(FLogFile);
  end;
end;

class function TLSPProtocol.ReadHeaders: TStringList;
var
  Line: string;
  C: Char;
  LineBuilder: TStringBuilder;
begin
  Result := TStringList.Create;
  LineBuilder := TStringBuilder.Create;
  try
    // Read headers until we get an empty line (CRLF CRLF)
    while True do
    begin
      LineBuilder.Clear;

      // Read until CR LF
      while True do
      begin
        if EOF(Input) then
        begin
          Result.Free;
          Result := nil;
          Exit;
        end;

        Read(Input, C);

        if C = #13 then  // CR
        begin
          // Expect LF next
          if not EOF(Input) then
          begin
            Read(Input, C);
            if C = #10 then
              Break;  // Got CRLF, end of line
          end;
        end
        else if C = #10 then  // LF without CR (Unix style)
          Break
        else
          LineBuilder.Append(C);
      end;

      Line := LineBuilder.ToString;

      if Line = '' then
        Break;  // Empty line = end of headers

      Result.Add(Line);
    end;

    Log('Headers read: ' + Result.Text);
  finally
    LineBuilder.Free;
  end;
end;

class function TLSPProtocol.ReadContent(ALength: Integer): string;
var
  I: Integer;
  C: Char;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    for I := 1 to ALength do
    begin
      if EOF(Input) then
        Break;
      Read(Input, C);
      Builder.Append(C);
    end;
    Result := Builder.ToString;
    Log('Content read (' + IntToStr(ALength) + ' bytes): ' + Copy(Result, 1, 200) + '...');
  finally
    Builder.Free;
  end;
end;

class function TLSPProtocol.ReadMessage(out AMessage: TLSPMessage): Boolean;
var
  Headers: TStringList;
  Header, Key, Value: string;
  ContentLength: Integer;
  Content: string;
  JSON: TJSONObject;
  ColonPos, I: Integer;
begin
  Result := False;
  AMessage.Params := nil;

  // Read headers
  Headers := ReadHeaders;
  if Headers = nil then
    Exit;

  try
    // Parse Content-Length header
    ContentLength := -1;
    for I := 0 to Headers.Count - 1 do
    begin
      Header := Headers[I];
      ColonPos := Pos(':', Header);
      if ColonPos > 0 then
      begin
        Key := Trim(Copy(Header, 1, ColonPos - 1));
        Value := Trim(Copy(Header, ColonPos + 1, MaxInt));
        if SameText(Key, 'Content-Length') then
          ContentLength := StrToIntDef(Value, -1);
      end;
    end;

    if ContentLength < 0 then
    begin
      Log('ERROR: No Content-Length header');
      Exit;
    end;
  finally
    Headers.Free;
  end;

  // Read content
  Content := ReadContent(ContentLength);
  if Length(Content) < ContentLength then
  begin
    Log('ERROR: Incomplete content');
    Exit;
  end;

  // Parse JSON
  try
    JSON := TJSONObject.ParseJSONValue(Content) as TJSONObject;
    if JSON = nil then
    begin
      Log('ERROR: Invalid JSON');
      Exit;
    end;

    try
      AMessage.JSONRPC := JSON.GetValue<string>('jsonrpc', '2.0');
      AMessage.Method := JSON.GetValue<string>('method', '');

      // Get ID (can be number, string, or null/missing for notifications)
      if JSON.TryGetValue<Integer>('id', I) then
        AMessage.ID := I
      else if JSON.TryGetValue<string>('id', Value) then
        AMessage.ID := Value
      else
        AMessage.ID := Null;

      AMessage.IsNotification := VarIsNull(AMessage.ID);

      // Get params (clone it so we own it)
      if JSON.TryGetValue<TJSONValue>('params', AMessage.Params) then
        AMessage.Params := TJSONValue(AMessage.Params.Clone)
      else
        AMessage.Params := nil;

      Result := True;
      Log('Parsed message: method=' + AMessage.Method + ', id=' + VarToStr(AMessage.ID));
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      Log('ERROR parsing JSON: ' + E.Message);
      Exit;
    end;
  end;
end;

class procedure TLSPProtocol.WriteMessage(AMessage: TJSONObject);
var
  Content: string;
  Header: string;
  Output: string;
begin
  Content := AMessage.ToJSON;
  Header := 'Content-Length: ' + IntToStr(Length(Content)) + #13#10 + #13#10;
  Output := Header + Content;

  Log('Sending: ' + Content);

  // Write to stdout
  Write(Output);
  Flush(System.Output);
end;

class procedure TLSPProtocol.WriteResponse(const AID: Variant; AResult: TJSONValue);
var
  Response: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.AddPair('jsonrpc', '2.0');

    if VarIsNull(AID) then
      Response.AddPair('id', TJSONNull.Create)
    else if VarType(AID) = varInteger then
      Response.AddPair('id', TJSONNumber.Create(Integer(AID)))
    else
      Response.AddPair('id', TJSONString.Create(VarToStr(AID)));

    if AResult <> nil then
      Response.AddPair('result', AResult)
    else
      Response.AddPair('result', TJSONNull.Create);

    WriteMessage(Response);
  finally
    Response.Free;
  end;
end;

class procedure TLSPProtocol.WriteError(const AID: Variant; ACode: TLSPErrorCode; const AMessage: string);
var
  Response, ErrorObj: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.AddPair('jsonrpc', '2.0');

    if VarIsNull(AID) then
      Response.AddPair('id', TJSONNull.Create)
    else if VarType(AID) = varInteger then
      Response.AddPair('id', TJSONNumber.Create(Integer(AID)))
    else
      Response.AddPair('id', TJSONString.Create(VarToStr(AID)));

    ErrorObj := TJSONObject.Create;
    ErrorObj.AddPair('code', TJSONNumber.Create(Integer(ACode)));
    ErrorObj.AddPair('message', AMessage);
    Response.AddPair('error', ErrorObj);

    WriteMessage(Response);
  finally
    Response.Free;
  end;
end;

class procedure TLSPProtocol.WriteNotification(const AMethod: string; AParams: TJSONValue);
var
  Notification: TJSONObject;
begin
  Notification := TJSONObject.Create;
  try
    Notification.AddPair('jsonrpc', '2.0');
    Notification.AddPair('method', AMethod);

    if AParams <> nil then
      Notification.AddPair('params', AParams)
    else
      Notification.AddPair('params', TJSONObject.Create);

    WriteMessage(Notification);
  finally
    Notification.Free;
  end;
end;

initialization
  TLSPProtocol.Initialize;

finalization
  TLSPProtocol.Finalize;

end.
