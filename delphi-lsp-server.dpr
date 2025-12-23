program delphi_lsp_server;

{$APPTYPE CONSOLE}

/// <summary>
/// delphi-lsp-server - LSP server for Delphi/Pascal using delphi-lookup index.
///
/// Provides Language Server Protocol support for:
/// - Go to Definition (textDocument/definition)
/// - Find References (textDocument/references)
/// - Hover Information (textDocument/hover)
/// - Document Symbols (textDocument/documentSymbol)
/// - Workspace Symbol Search (workspace/symbol)
///
/// Usage:
///   delphi-lsp-server.exe [--database <path>] [--log <path>]
///
/// Environment Variables:
///   DELPHI_LSP_DATABASE - Path to delphi_symbols.db
///   DELPHI_LSP_LOG      - Path to log file for debugging
/// </summary>

// This application requires 64-bit compilation for sqlite-vec compatibility
{$IFNDEF WIN64}
  {$MESSAGE FATAL 'delphi-lsp-server requires Win64 compilation.'}
{$ENDIF}

uses
  System.SysUtils,
  System.IOUtils,
  uConfig in 'uConfig.pas',
  uDatabaseConnection in 'uDatabaseConnection.pas',
  uSearchTypes in 'uSearchTypes.pas',
  uQueryProcessor in 'uQueryProcessor.pas',
  uLSPTypes in 'LSP\uLSPTypes.pas',
  uLSPProtocol in 'LSP\uLSPProtocol.pas',
  uPositionResolver in 'LSP\uPositionResolver.pas',
  uLSPHandlers in 'LSP\uLSPHandlers.pas';

var
  Server: TLSPServer;
  DatabaseFile: string;
  I: Integer;

begin
  try
    // Parse command line arguments
    DatabaseFile := '';

    I := 1;
    while I <= ParamCount do
    begin
      if (ParamStr(I) = '--database') or (ParamStr(I) = '-d') then
      begin
        Inc(I);
        if I <= ParamCount then
          DatabaseFile := ParamStr(I);
      end
      else if (ParamStr(I) = '--log') or (ParamStr(I) = '-l') then
      begin
        Inc(I);
        if I <= ParamCount then
          TLSPProtocol.EnableLogging(ParamStr(I));
      end
      else if (ParamStr(I) = '--help') or (ParamStr(I) = '-h') then
      begin
        WriteLn('delphi-lsp-server - LSP server for Delphi/Pascal');
        WriteLn;
        WriteLn('Usage: delphi-lsp-server.exe [options]');
        WriteLn;
        WriteLn('Options:');
        WriteLn('  -d, --database <path>  Path to delphi_symbols.db');
        WriteLn('  -l, --log <path>       Enable logging to file');
        WriteLn('  -h, --help             Show this help');
        WriteLn;
        WriteLn('Environment Variables:');
        WriteLn('  DELPHI_LSP_DATABASE    Path to database file');
        WriteLn('  DELPHI_LSP_LOG         Path to log file');
        WriteLn;
        WriteLn('LSP Capabilities:');
        WriteLn('  - textDocument/definition');
        WriteLn('  - textDocument/references');
        WriteLn('  - textDocument/hover');
        WriteLn('  - textDocument/documentSymbol');
        WriteLn('  - workspace/symbol');
        Halt(0);
      end;
      Inc(I);
    end;

    // Check environment variables
    if DatabaseFile = '' then
      DatabaseFile := GetEnvironmentVariable('DELPHI_LSP_DATABASE');

    // Default to database in executable directory
    if DatabaseFile = '' then
      DatabaseFile := TPath.Combine(ExtractFilePath(ParamStr(0)), DEFAULT_DB_FILE);

    // Check if database exists
    if not FileExists(DatabaseFile) then
    begin
      WriteLn(ErrOutput, 'Error: Database not found: ' + DatabaseFile);
      WriteLn(ErrOutput, 'Please run delphi-indexer.exe first to create the index.');
      Halt(1);
    end;

    // Create and run server
    Server := TLSPServer.Create;
    try
      Server.SetDatabaseFile(DatabaseFile);
      Server.Run;
    finally
      Server.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, 'Fatal error: ' + E.Message);
      Halt(1);
    end;
  end;
end.
