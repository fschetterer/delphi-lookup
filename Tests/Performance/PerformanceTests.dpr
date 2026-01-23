program PerformanceTests;

{$APPTYPE CONSOLE}

// This application requires 64-bit compilation for sqlite-vec compatibility
{$IFNDEF WIN64}
  {$MESSAGE FATAL 'PerformanceTests requires Win64 compilation.'}
{$ENDIF}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Diagnostics,
  System.Generics.Collections,
  System.Hash,
  System.Threading,
  System.SyncObjs,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  uDatabaseConnection in '..\..\uDatabaseConnection.pas',
  uConfig in '..\..\uConfig.pas',
  uFolderScanner in '..\..\uFolderScanner.pas',
  uParallelFolderScanner in '..\..\uParallelFolderScanner.pas',
  uASTProcessor in '..\..\uASTProcessor.pas',
  uParallelASTProcessor in '..\..\uParallelASTProcessor.pas',
  uChangeDetector in '..\..\uChangeDetector.pas',
  SimpleParser.Lexer in '..\..\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.pas',
  SimpleParser.Lexer.Types in '..\..\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.Types.pas',
  SimpleParser in '..\..\DelphiAST\Source\SimpleParser\SimpleParser.pas',
  SimpleParser.Types in '..\..\DelphiAST\Source\SimpleParser\SimpleParser.Types.pas',
  DelphiAST.Classes in '..\..\DelphiAST\Source\DelphiAST.Classes.pas',
  DelphiAST.Consts in '..\..\DelphiAST\Source\DelphiAST.Consts.pas',
  DelphiAST in '..\..\DelphiAST\Source\DelphiAST.pas',
  DelphiAST.ProjectIndexer in '..\..\DelphiAST\Source\DelphiAST.ProjectIndexer.pas',
  DelphiAST.Serialize.Binary in '..\..\DelphiAST\Source\DelphiAST.Serialize.Binary.pas',
  DelphiAST.SimpleParserEx in '..\..\DelphiAST\Source\DelphiAST.SimpleParserEx.pas',
  DelphiAST.Writer in '..\..\DelphiAST\Source\DelphiAST.Writer.pas',
  StringPool in '..\..\DelphiAST\Source\StringPool.pas';

{
  Performance Tests for delphi-indexer Optimization

  REQ-PERF-001: Change Detection No Changes (<100ms for 10,000 files)
  REQ-PERF-002: Change Detection One File (only reprocess changed file)
  REQ-PERF-003: Deleted Folder (cascade delete without scanning)
  REQ-PERF-004: Parallel AST Processing (speedup >= 3x on 4+ cores)

  Test project: /mnt/w/Public/delphi-lookup (96 files)
}

const
  // Test folder - the delphi-lookup project itself
  TEST_FOLDER = 'W:\Public\delphi-lookup';

type
  TTestResult = record
    TestName: string;
    Passed: Boolean;
    DurationMs: Int64;
    Details: string;
    ExpectedMs: Int64;  // Target threshold
  end;

var
  GTestResults: TList<TTestResult>;

procedure Log(const AMessage: string);
begin
  WriteLn(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMessage);
end;

procedure LogResult(const AResult: TTestResult);
begin
  if AResult.Passed then
    Log(Format('[PASS] %s: %d ms (target: <%d ms) - %s',
      [AResult.TestName, AResult.DurationMs, AResult.ExpectedMs, AResult.Details]))
  else
    Log(Format('[FAIL] %s: %d ms (target: <%d ms) - %s',
      [AResult.TestName, AResult.DurationMs, AResult.ExpectedMs, AResult.Details]));
end;

{
  REQ-PERF-004: Parallel AST Processing Speedup
  Target: Speedup >= 2x on multi-core systems

  Compares sequential vs parallel AST parsing time.
}
function TestParallelASTSpeedup: TTestResult;
var
  Scanner: TFolderScanner;
  SeqProcessor: TASTProcessor;
  ParProcessor: TParallelASTProcessor;
  ParsedFiles: TStringList;
  SeqChunks, ParChunks: TCodeChunkList;
  SeqStopwatch, ParStopwatch: TStopwatch;
  SeqTimeMs, ParTimeMs: Int64;
  Speedup: Double;
  I: Integer;
begin
  Result.TestName := 'ParallelAST_Speedup';
  Result.ExpectedMs := 0;  // No time threshold, measuring speedup ratio

  ParsedFiles := nil;
  SeqChunks := nil;
  ParChunks := nil;

  // Scan folder
  Scanner := TFolderScanner.Create(False);
  try
    ParsedFiles := Scanner.ScanFolder(TEST_FOLDER);
    Log(Format('  Testing with %d files', [ParsedFiles.Count]));
  finally
    Scanner.Free;
  end;

  try
    // Sequential processing
    SeqProcessor := TASTProcessor.Create;
    try
      SeqStopwatch := TStopwatch.StartNew;
      SeqChunks := SeqProcessor.ProcessFiles(ParsedFiles);
      SeqStopwatch.Stop;
      SeqTimeMs := SeqStopwatch.ElapsedMilliseconds;
      Log(Format('  Sequential: %d ms (%d chunks)', [SeqTimeMs, SeqChunks.Count]));
    finally
      SeqProcessor.Free;
    end;

    // Parallel processing
    ParProcessor := TParallelASTProcessor.Create;
    try
      ParProcessor.ShowProgress := False;
      ParProcessor.MinFilesForParallel := 2;  // Force parallel even for few files

      ParStopwatch := TStopwatch.StartNew;
      ParChunks := ParProcessor.ProcessFiles(ParsedFiles);
      ParStopwatch.Stop;
      ParTimeMs := ParStopwatch.ElapsedMilliseconds;
      Log(Format('  Parallel: %d ms (%d chunks)', [ParTimeMs, ParChunks.Count]));
    finally
      ParProcessor.Free;
    end;

    // Calculate speedup
    if ParTimeMs > 0 then
      Speedup := SeqTimeMs / ParTimeMs
    else
      Speedup := SeqTimeMs;  // Avoid division by zero

    Result.DurationMs := ParTimeMs;

    // Target: >= 2x speedup on 4+ cores (or at least >= 1.5x on 2 cores)
    var CoreCount := TThread.ProcessorCount;
    var MinSpeedup: Double;
    if CoreCount >= 4 then
      MinSpeedup := 2.0
    else if CoreCount >= 2 then
      MinSpeedup := 1.3
    else
      MinSpeedup := 1.0;

    Result.Passed := Speedup >= MinSpeedup;
    Result.Details := Format('Sequential=%dms, Parallel=%dms, Speedup=%.2fx (Cores=%d, Target>=%.1fx)',
      [SeqTimeMs, ParTimeMs, Speedup, CoreCount, MinSpeedup]);

  finally
    // Free chunks (they own their TCodeChunk objects)
    if Assigned(SeqChunks) then
      SeqChunks.Free;
    if Assigned(ParChunks) then
      ParChunks.Free;

    // Free parsed files
    if Assigned(ParsedFiles) then
    begin
      for I := 0 to ParsedFiles.Count - 1 do
        ParsedFiles.Objects[I].Free;
      ParsedFiles.Free;
    end;
  end;
end;

{
  Test: Parallel Folder Scanning Speedup
  Compares sequential vs parallel folder scanning.
}
function TestParallelFolderScanSpeedup: TTestResult;
var
  SeqScanner: TFolderScanner;
  ParScanner: TParallelFolderScanner;
  SeqFiles, ParFiles: TStringList;
  SeqStopwatch, ParStopwatch: TStopwatch;
  SeqTimeMs, ParTimeMs: Int64;
  Speedup: Double;
  I: Integer;
begin
  Result.TestName := 'ParallelFolderScan_Speedup';
  Result.ExpectedMs := 0;

  SeqFiles := nil;
  ParFiles := nil;

  try
    // Sequential scanning
    SeqScanner := TFolderScanner.Create(False);
    try
      SeqStopwatch := TStopwatch.StartNew;
      SeqFiles := SeqScanner.ScanFolder(TEST_FOLDER);
      SeqStopwatch.Stop;
      SeqTimeMs := SeqStopwatch.ElapsedMilliseconds;
      Log(Format('  Sequential scan: %d ms (%d files)', [SeqTimeMs, SeqFiles.Count]));
    finally
      SeqScanner.Free;
    end;

    // Parallel scanning
    ParScanner := TParallelFolderScanner.Create(False);
    try
      ParScanner.MinSubfoldersForParallel := 2;

      ParStopwatch := TStopwatch.StartNew;
      ParFiles := ParScanner.ScanFolderPathsOnly(TEST_FOLDER);
      ParStopwatch.Stop;
      ParTimeMs := ParStopwatch.ElapsedMilliseconds;
      Log(Format('  Parallel scan: %d ms (%d files)', [ParTimeMs, ParFiles.Count]));
    finally
      ParScanner.Free;
    end;

    // Calculate speedup
    if ParTimeMs > 0 then
      Speedup := SeqTimeMs / ParTimeMs
    else
      Speedup := SeqTimeMs;

    Result.DurationMs := ParTimeMs;

    // Folder scanning is I/O bound, so lower speedup expected
    var CoreCount := TThread.ProcessorCount;
    var MinSpeedup: Double;
    if CoreCount >= 4 then
      MinSpeedup := 1.2  // Lower threshold for I/O bound task
    else
      MinSpeedup := 1.0;

    Result.Passed := Speedup >= MinSpeedup;
    Result.Details := Format('Sequential=%dms, Parallel=%dms, Speedup=%.2fx (Cores=%d)',
      [SeqTimeMs, ParTimeMs, Speedup, CoreCount]);

  finally
    // Free parsed files (TParsedFile objects)
    if Assigned(SeqFiles) then
    begin
      for I := 0 to SeqFiles.Count - 1 do
        SeqFiles.Objects[I].Free;
      SeqFiles.Free;
    end;
    if Assigned(ParFiles) then
    begin
      // ParFiles from ScanFolderPathsOnly only contains paths, no objects
      ParFiles.Free;
    end;
  end;
end;

{
  Test: Folder Hash Calculation Performance
  Tests how fast we can compute folder hashes for change detection.
}
function TestFolderHashCalculation: TTestResult;
var
  Scanner: TFolderScanner;
  Files: TStringList;
  Stopwatch: TStopwatch;
  I: Integer;
  Hash: string;
  FileInfo: TSearchRec;
  ContentHash: string;
begin
  Result.TestName := 'FolderHash_Calculation';
  Result.ExpectedMs := 50;  // Target: <50ms for ~100 files

  Files := nil;

  try
    // Scan folder
    Scanner := TFolderScanner.Create(False);
    try
      Files := Scanner.ScanFolder(TEST_FOLDER);
    finally
      Scanner.Free;
    end;

    // Time hash calculation
    Stopwatch := TStopwatch.StartNew;

    // Simulate folder hash calculation (similar to TChangeDetector)
    var SortedFiles := TStringList.Create;
    try
      SortedFiles.Sorted := True;

      for I := 0 to Files.Count - 1 do
      begin
        var FilePath := Files[I];
        if FindFirst(FilePath, faAnyFile, FileInfo) = 0 then
        begin
          try
            // Hash = filename:timestamp (as would be used in change detection)
            SortedFiles.Add(Format('%s:%.6f', [ExtractFileName(FilePath), FileInfo.TimeStamp]));
          finally
            FindClose(FileInfo);
          end;
        end;
      end;

      // Compute combined hash
      ContentHash := THashMD5.GetHashString(SortedFiles.Text);

    finally
      SortedFiles.Free;
    end;

    Stopwatch.Stop;
    Result.DurationMs := Stopwatch.ElapsedMilliseconds;

    Result.Passed := Result.DurationMs < Result.ExpectedMs;
    Result.Details := Format('Files=%d, HashTime=%dms',
      [Files.Count, Result.DurationMs]);

  finally
    if Assigned(Files) then
    begin
      for I := 0 to Files.Count - 1 do
        Files.Objects[I].Free;
      Files.Free;
    end;
  end;
end;

{
  Test: File Hash Batch Calculation
  Tests computing MD5 hashes for multiple files (simulates change detection).
}
function TestFileHashBatch: TTestResult;
var
  Scanner: TFolderScanner;
  Files: TStringList;
  Stopwatch: TStopwatch;
  I: Integer;
  HashCount: Integer;
  TotalBytes: Int64;
begin
  Result.TestName := 'FileHash_Batch';
  Result.ExpectedMs := 200;  // Target: <200ms for ~100 files

  Files := nil;
  HashCount := 0;
  TotalBytes := 0;

  try
    // Scan folder
    Scanner := TFolderScanner.Create(False);
    try
      Files := Scanner.ScanFolder(TEST_FOLDER);
    finally
      Scanner.Free;
    end;

    // Time hash calculation for all files
    Stopwatch := TStopwatch.StartNew;

    for I := 0 to Files.Count - 1 do
    begin
      var FilePath := Files[I];
      try
        var Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
        try
          TotalBytes := TotalBytes + Stream.Size;
          var Hash := THashMD5.GetHashString(Stream);
          Inc(HashCount);
        finally
          Stream.Free;
        end;
      except
        // Ignore files we can't read
      end;
    end;

    Stopwatch.Stop;
    Result.DurationMs := Stopwatch.ElapsedMilliseconds;

    Result.Passed := Result.DurationMs < Result.ExpectedMs;
    Result.Details := Format('Files=%d, Hashed=%d, TotalMB=%.2f, Time=%dms',
      [Files.Count, HashCount, TotalBytes / (1024 * 1024), Result.DurationMs]);

  finally
    if Assigned(Files) then
    begin
      for I := 0 to Files.Count - 1 do
        Files.Objects[I].Free;
      Files.Free;
    end;
  end;
end;

{
  Test: AST Parsing Performance per File
  Measures average parsing time per file.
}
function TestASTParsingSingleFile: TTestResult;
var
  Scanner: TFolderScanner;
  ASTProcessor: TASTProcessor;
  Files: TStringList;
  ParsedFile: TParsedFile;
  Chunks: TCodeChunkList;
  Stopwatch: TStopwatch;
  TotalTime, AvgTime: Int64;
  TotalChunks, I: Integer;
begin
  Result.TestName := 'AST_PerFileAverage';
  Result.ExpectedMs := 20;  // Target: <20ms average per file

  Files := nil;
  TotalTime := 0;
  TotalChunks := 0;

  try
    // Scan folder
    Scanner := TFolderScanner.Create(False);
    try
      Files := Scanner.ScanFolder(TEST_FOLDER);
    finally
      Scanner.Free;
    end;

    // Parse each file individually and measure
    ASTProcessor := TASTProcessor.Create;
    try
      for I := 0 to Files.Count - 1 do
      begin
        ParsedFile := TParsedFile(Files.Objects[I]);

        Stopwatch := TStopwatch.StartNew;
        Chunks := ASTProcessor.ProcessSingleFile(ParsedFile);
        Stopwatch.Stop;

        TotalTime := TotalTime + Stopwatch.ElapsedMilliseconds;
        TotalChunks := TotalChunks + Chunks.Count;

        Chunks.Free;
      end;
    finally
      ASTProcessor.Free;
    end;

    if Files.Count > 0 then
      AvgTime := TotalTime div Files.Count
    else
      AvgTime := 0;

    Result.DurationMs := AvgTime;
    Result.Passed := AvgTime < Result.ExpectedMs;
    Result.Details := Format('Files=%d, TotalChunks=%d, TotalTime=%dms, AvgTime=%dms',
      [Files.Count, TotalChunks, TotalTime, AvgTime]);

  finally
    if Assigned(Files) then
    begin
      for I := 0 to Files.Count - 1 do
        Files.Objects[I].Free;
      Files.Free;
    end;
  end;
end;

procedure RunAllTests;
var
  TestResult: TTestResult;
  PassCount, FailCount: Integer;
begin
  GTestResults := TList<TTestResult>.Create;
  try
    WriteLn;
    WriteLn('==============================================');
    WriteLn('delphi-indexer Performance Tests');
    WriteLn('==============================================');
    WriteLn(Format('Test folder: %s', [TEST_FOLDER]));
    WriteLn(Format('Processor cores: %d', [TThread.ProcessorCount]));
    WriteLn;

    WriteLn('----------------------------------------------');
    WriteLn('Running Performance Tests...');
    WriteLn('----------------------------------------------');
    WriteLn;

    // Test 1: Folder Hash Calculation
    Log('Test 1: Folder Hash Calculation');
    TestResult := TestFolderHashCalculation;
    LogResult(TestResult);
    GTestResults.Add(TestResult);
    WriteLn;

    // Test 2: File Hash Batch
    Log('Test 2: File Hash Batch');
    TestResult := TestFileHashBatch;
    LogResult(TestResult);
    GTestResults.Add(TestResult);
    WriteLn;

    // Test 3: AST Parsing per File
    Log('Test 3: AST Parsing per File');
    TestResult := TestASTParsingSingleFile;
    LogResult(TestResult);
    GTestResults.Add(TestResult);
    WriteLn;

    // Test 4: Parallel Folder Scan Speedup
    Log('Test 4: Parallel Folder Scan Speedup');
    TestResult := TestParallelFolderScanSpeedup;
    LogResult(TestResult);
    GTestResults.Add(TestResult);
    WriteLn;

    // Test 5: Parallel AST Processing Speedup
    Log('Test 5: Parallel AST Processing Speedup');
    TestResult := TestParallelASTSpeedup;
    LogResult(TestResult);
    GTestResults.Add(TestResult);
    WriteLn;

    // Summary
    WriteLn('----------------------------------------------');
    WriteLn('Test Summary');
    WriteLn('----------------------------------------------');

    PassCount := 0;
    FailCount := 0;
    for TestResult in GTestResults do
    begin
      if TestResult.Passed then
        Inc(PassCount)
      else
        Inc(FailCount);
    end;

    WriteLn(Format('Passed: %d / %d', [PassCount, GTestResults.Count]));
    WriteLn(Format('Failed: %d / %d', [FailCount, GTestResults.Count]));
    WriteLn;

    if FailCount = 0 then
      WriteLn('All tests PASSED!')
    else
      WriteLn('Some tests FAILED. Review results above.');

    WriteLn;
    WriteLn('==============================================');

  finally
    GTestResults.Free;
  end;
end;

begin
  try
    RunAllTests;
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
