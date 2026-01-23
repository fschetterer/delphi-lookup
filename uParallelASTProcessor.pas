unit uParallelASTProcessor;

{
  TParallelASTProcessor - Parallel AST processing for Pascal source files

  REQ-007: Parallel AST Processing with Worker Pool
  -------------------------------------------------
  Uses System.Threading.TTask pool to parse multiple files in parallel.
  Target: Speedup >= 3x on CPUs with 4+ cores.

  Algorithm:
  ----------
  1. Initialize thread-local TASTProcessor instances (one per worker)
  2. Partition files across workers (work stealing via TParallel.For)
  3. Each worker parses files using its own TASTProcessor (NOT thread-safe)
  4. Workers store results in thread-local TCodeChunkList
  5. After all workers complete, merge results into single output list

  Thread Safety:
  --------------
  - Each worker has its own TASTProcessor instance (parser is NOT thread-safe)
  - Each worker uses thread-local TCodeChunkList (no locks during parsing)
  - Final merge happens after TParallel.For completes (sequential)
  - Progress counters use TInterlocked for atomic updates
  - Error collection uses TCriticalSection (only for rare error cases)

  Performance Considerations:
  ---------------------------
  - TParallel.For uses TThreadPool.Default (auto-scales to CPU count)
  - Work stealing ensures balanced load even with varying file sizes
  - Memory overhead: ~1MB per worker (TASTProcessor + chunk buffer)
  - Minimum files threshold: only parallelize if 4+ files (avoid overhead)
  - Expected speedup: 3-6x on 4+ core CPUs

  Usage:
  ------
    var
      ParallelProcessor: TParallelASTProcessor;
      Chunks: TCodeChunkList;
    begin
      ParallelProcessor := TParallelASTProcessor.Create;
      try
        Chunks := ParallelProcessor.ProcessFiles(FileList);
        // Chunks now contains all parsed symbols from all files
      finally
        ParallelProcessor.Free;
      end;
    end;

  Fallback:
  ---------
  If fewer than 4 files, falls back to sequential processing
  to avoid parallel overhead for small batches.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  System.SyncObjs,
  System.Diagnostics,
  uASTProcessor,
  uFolderScanner;

type
  /// <summary>
  /// Result from parsing a single file (thread-safe transfer object)
  /// </summary>
  TFileParseResult = record
    FilePath: string;
    Chunks: TCodeChunkList;  // Owned by result, caller takes ownership
    Success: Boolean;
    ErrorMessage: string;
    ParseTimeMs: Int64;
  end;

  TFileParseResultArray = TArray<TFileParseResult>;

  /// <summary>
  /// Parallel AST processor using worker pool
  /// </summary>
  TParallelASTProcessor = class
  private
    FMinFilesForParallel: Integer;
    FMaxWorkers: Integer;
    FShowProgress: Boolean;

    // Statistics (updated atomically during processing) - use Int64 for TInterlocked
    FFilesProcessed: Int64;
    FFilesErrored: Int64;
    FTotalChunks: Int64;
    FTotalParseTimeMs: Int64;

    // Error collection (protected by critical section)
    FErrorLock: TCriticalSection;
    FErrors: TStringList;

    // Progress tracking
    FProgressLock: TCriticalSection;
    FLastProgressUpdate: TDateTime;
    FProgressUpdateIntervalMs: Integer;

    procedure UpdateProgress(ACurrent, ATotal: Integer);
    procedure CollectError(const AFilePath, AError: string);

    // Sequential processing (fallback for small batches)
    function ProcessFilesSequential(AFiles: TStringList): TCodeChunkList;

    // Parallel processing
    function ProcessFilesParallel(AFiles: TStringList): TCodeChunkList;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Process multiple files, automatically choosing parallel vs sequential
    /// based on file count and CPU cores.
    /// </summary>
    /// <param name="AFiles">TStringList with file paths and TParsedFile objects</param>
    /// <returns>TCodeChunkList with all parsed symbols (caller owns)</returns>
    function ProcessFiles(AFiles: TStringList): TCodeChunkList;

    /// <summary>Minimum number of files to trigger parallel processing.
    /// Default: 4. Set higher if parallel overhead is significant.</summary>
    property MinFilesForParallel: Integer read FMinFilesForParallel write FMinFilesForParallel;

    /// <summary>Maximum number of worker threads.
    /// Default: CPU count. Set lower to reduce memory usage.</summary>
    property MaxWorkers: Integer read FMaxWorkers write FMaxWorkers;

    /// <summary>Show progress updates during processing.
    /// Default: True.</summary>
    property ShowProgress: Boolean read FShowProgress write FShowProgress;

    // Statistics (read after processing completes)
    function GetFilesProcessed: Integer;
    function GetFilesErrored: Integer;
    function GetTotalChunks: Integer;
    property TotalParseTimeMs: Int64 read FTotalParseTimeMs;

    /// <summary>Errors collected during processing</summary>
    property Errors: TStringList read FErrors;
  end;

implementation

uses
  System.Math;

{ TParallelASTProcessor }

constructor TParallelASTProcessor.Create;
begin
  inherited Create;

  // Configuration defaults
  FMinFilesForParallel := 4;
  FMaxWorkers := TThread.ProcessorCount;
  FShowProgress := True;
  FProgressUpdateIntervalMs := 100;  // Update every 100ms max

  // Initialize synchronization
  FErrorLock := TCriticalSection.Create;
  FProgressLock := TCriticalSection.Create;
  FErrors := TStringList.Create;

  // Reset statistics
  FFilesProcessed := 0;
  FFilesErrored := 0;
  FTotalChunks := 0;
  FTotalParseTimeMs := 0;
  FLastProgressUpdate := 0;
end;

destructor TParallelASTProcessor.Destroy;
begin
  FErrors.Free;
  FProgressLock.Free;
  FErrorLock.Free;
  inherited Destroy;
end;

function TParallelASTProcessor.GetFilesProcessed: Integer;
begin
  Result := FFilesProcessed;
end;

function TParallelASTProcessor.GetFilesErrored: Integer;
begin
  Result := FFilesErrored;
end;

function TParallelASTProcessor.GetTotalChunks: Integer;
begin
  Result := FTotalChunks;
end;

procedure TParallelASTProcessor.UpdateProgress(ACurrent, ATotal: Integer);
var
  Now: TDateTime;
  Percent: Double;
  ETASec: Integer;
  ETAStr, ProgressStr: string;
  ElapsedMs, CurrentChunks: Int64;
begin
  if not FShowProgress then
    Exit;

  // Throttle updates to avoid console flooding
  FProgressLock.Enter;
  try
    Now := System.SysUtils.Now;
    if (FLastProgressUpdate > 0) and
       ((Now - FLastProgressUpdate) * 86400000 < FProgressUpdateIntervalMs) then
      Exit;
    FLastProgressUpdate := Now;
  finally
    FProgressLock.Leave;
  end;

  Percent := (ACurrent / ATotal) * 100;

  // Read statistics atomically
  // Note: TInterlocked.Read only works with Int64. For Integer, reads of
  // aligned 32-bit values are atomic on x86/x64, so direct access is safe.
  ElapsedMs := TInterlocked.Read(FTotalParseTimeMs);
  CurrentChunks := FTotalChunks;  // Aligned Integer read is atomic

  // Calculate ETA based on elapsed time
  if (ACurrent > 0) and (ElapsedMs > 0) then
  begin
    ETASec := Round((ElapsedMs / ACurrent) * (ATotal - ACurrent) / 1000);
    if ETASec >= 3600 then
      ETAStr := Format('%d:%02d:%02d', [ETASec div 3600, (ETASec mod 3600) div 60, ETASec mod 60])
    else if ETASec >= 60 then
      ETAStr := Format('%d:%02d', [ETASec div 60, ETASec mod 60])
    else
      ETAStr := Format('%ds', [ETASec]);
    ProgressStr := Format('[%d/%d %.0f%% ETA:%s]', [ACurrent, ATotal, Percent, ETAStr]);
  end
  else
    ProgressStr := Format('[%d/%d %.0f%%]', [ACurrent, ATotal, Percent]);

  Write(Format(#13'%s Parsing files (parallel)... %d chunks        ',
    [ProgressStr, CurrentChunks]));
end;

procedure TParallelASTProcessor.CollectError(const AFilePath, AError: string);
begin
  FErrorLock.Enter;
  try
    FErrors.Add(Format('%s: %s', [ExtractFileName(AFilePath), AError]));
  finally
    FErrorLock.Leave;
  end;
end;

function TParallelASTProcessor.ProcessFilesSequential(AFiles: TStringList): TCodeChunkList;
var
  Processor: TASTProcessor;
  ParsedFile: TParsedFile;
  FileChunks: TCodeChunkList;
  Chunk: TCodeChunk;
  Stopwatch: TStopwatch;
  I: Integer;
begin
  Result := TCodeChunkList.Create;

  Processor := TASTProcessor.Create;
  try
    for I := 0 to AFiles.Count - 1 do
    begin
      ParsedFile := TParsedFile(AFiles.Objects[I]);
      if not Assigned(ParsedFile) then
        Continue;

      Stopwatch := TStopwatch.StartNew;
      try
        FileChunks := Processor.ProcessSingleFile(ParsedFile);
        try
          // Move chunks to result
          while FileChunks.Count > 0 do
          begin
            Chunk := FileChunks.Extract(FileChunks[0]);
            Result.Add(Chunk);
          end;

          TInterlocked.Increment(FFilesProcessed);
          TInterlocked.Exchange(FTotalChunks, Result.Count);
        finally
          FileChunks.Free;
        end;
      except
        on E: Exception do
        begin
          TInterlocked.Increment(FFilesErrored);
          CollectError(ParsedFile.FilePath, E.Message);
        end;
      end;

      TInterlocked.Add(FTotalParseTimeMs, Stopwatch.ElapsedMilliseconds);

      if FShowProgress then
        UpdateProgress(I + 1, AFiles.Count);
    end;

    if FShowProgress then
      WriteLn(Format(#13'[AST] Processed %d files, generated %d chunks (sequential)        ',
        [GetFilesProcessed, Result.Count]));
  finally
    Processor.Free;
  end;
end;

function TParallelASTProcessor.ProcessFilesParallel(AFiles: TStringList): TCodeChunkList;
var
  FileCount: Integer;
  WorkerProcessors: TArray<TASTProcessor>;
  WorkerFileCounts: TArray<Integer>;
  WorkerChunkCounts: TArray<Integer>;
  WorkerTimeMs: TArray<Int64>;
  ProcessorLocks: TArray<TCriticalSection>;
  ResultLock: TCriticalSection;
  I: Integer;
  WorkerCount: Integer;
  Self_: TParallelASTProcessor;
  Result_: TCodeChunkList;
begin
  Result := TCodeChunkList.Create;
  FileCount := AFiles.Count;

  // Determine optimal worker count (don't create more workers than files)
  WorkerCount := Min(FMaxWorkers, FileCount);
  if WorkerCount < 1 then
    WorkerCount := 1;

  WriteLn(Format('[INFO] Parallel AST processing: %d files on %d workers',
    [FileCount, WorkerCount]));

  // Pre-allocate worker resources
  SetLength(WorkerProcessors, WorkerCount);  // One processor per WORKER
  SetLength(ProcessorLocks, WorkerCount);    // One lock per processor
  SetLength(WorkerFileCounts, WorkerCount);
  SetLength(WorkerChunkCounts, WorkerCount);
  SetLength(WorkerTimeMs, WorkerCount);

  // Initialize worker processors and their locks
  for I := 0 to WorkerCount - 1 do
  begin
    WorkerProcessors[I] := TASTProcessor.Create;
    ProcessorLocks[I] := TCriticalSection.Create;
    WorkerFileCounts[I] := 0;
    WorkerChunkCounts[I] := 0;
    WorkerTimeMs[I] := 0;
  end;

  // Lock for merging results into Result (coarse-grained: one lock per file merge)
  ResultLock := TCriticalSection.Create;

  // Capture Self and Result for anonymous method
  Self_ := Self;
  Result_ := Result;

  try
    // Parallel processing using work-stealing pool
    // Merge happens INSIDE the loop while parser is still alive (fixes memory bug)
    TParallel.For(0, FileCount - 1,
      procedure(Index: Integer)
      var
        ParsedFile: TParsedFile;
        FileChunks: TCodeChunkList;
        Chunk: TCodeChunk;
        ChunkCount: Integer;
        WorkerIndex: Integer;
        Processor: TASTProcessor;
        Lock: TCriticalSection;
        Stopwatch: TStopwatch;
        CurrentProcessed: Int64;
      begin
        // Determine which worker this thread should use
        // Use modulo to distribute evenly, with locking to ensure safety
        WorkerIndex := Index mod WorkerCount;

        // Get thread's processor and lock
        Processor := WorkerProcessors[WorkerIndex];
        Lock := ProcessorLocks[WorkerIndex];

        ParsedFile := TParsedFile(AFiles.Objects[Index]);
        if not Assigned(ParsedFile) then
          Exit;

        Stopwatch := TStopwatch.StartNew;
        try
          // Lock the processor while we use it (parser is NOT thread-safe)
          Lock.Enter;
          try
            FileChunks := Processor.ProcessSingleFile(ParsedFile);
          finally
            Lock.Leave;
          end;

          ChunkCount := FileChunks.Count;

          // Merge immediately while parser memory is still valid
          // Coarse-grained lock: one acquisition per file (not per chunk)
          ResultLock.Enter;
          try
            while FileChunks.Count > 0 do
            begin
              Chunk := FileChunks.Extract(FileChunks[0]);
              Result_.Add(Chunk);
            end;
          finally
            ResultLock.Leave;
          end;

          // Free the now-empty list
          FileChunks.Free;

          // Update statistics atomically
          CurrentProcessed := TInterlocked.Increment(FFilesProcessed);
          TInterlocked.Add(FTotalChunks, ChunkCount);
          TInterlocked.Add(FTotalParseTimeMs, Stopwatch.ElapsedMilliseconds);

          // Update worker-local stats
          TInterlocked.Increment(WorkerFileCounts[WorkerIndex]);
          TInterlocked.Add(WorkerChunkCounts[WorkerIndex], ChunkCount);
          TInterlocked.Add(WorkerTimeMs[WorkerIndex], Stopwatch.ElapsedMilliseconds);

          // Progress update (throttled)
          Self_.UpdateProgress(CurrentProcessed, FileCount);
        except
          on E: Exception do
          begin
            TInterlocked.Increment(FFilesErrored);
            Self_.CollectError(ParsedFile.FilePath, E.Message);
          end;
        end;
      end
    );

    if FShowProgress then
    begin
      WriteLn(Format(#13'[AST] Processed %d files, generated %d chunks (parallel, %d workers)        ',
        [GetFilesProcessed, Result.Count, WorkerCount]));

      // Show per-worker statistics
      for I := 0 to WorkerCount - 1 do
      begin
        if WorkerFileCounts[I] > 0 then
          WriteLn(Format('  Worker %d: %d files, %d chunks, %d ms',
            [I, WorkerFileCounts[I], WorkerChunkCounts[I], WorkerTimeMs[I]]));
      end;
    end;

  finally
    // Cleanup worker processors and locks
    for I := 0 to WorkerCount - 1 do
    begin
      WorkerProcessors[I].Free;
      ProcessorLocks[I].Free;
    end;
    ResultLock.Free;
  end;
end;

function TParallelASTProcessor.ProcessFiles(AFiles: TStringList): TCodeChunkList;
var
  Stopwatch: TStopwatch;
begin
  // Reset statistics
  FFilesProcessed := 0;
  FFilesErrored := 0;
  FTotalChunks := 0;
  FTotalParseTimeMs := 0;
  FErrors.Clear;

  if AFiles.Count = 0 then
  begin
    Result := TCodeChunkList.Create;
    Exit;
  end;

  Stopwatch := TStopwatch.StartNew;

  // Choose parallel vs sequential based on file count
  if AFiles.Count >= FMinFilesForParallel then
    Result := ProcessFilesParallel(AFiles)
  else
  begin
    if FShowProgress then
      WriteLn(Format('[INFO] Sequential AST processing: %d files (below threshold %d)',
        [AFiles.Count, FMinFilesForParallel]));
    Result := ProcessFilesSequential(AFiles);
  end;

  // Show error summary if any
  if GetFilesErrored > 0 then
  begin
    WriteLn(Format('[WARN] %d files had parsing errors:', [GetFilesErrored]));
    for var I := 0 to Min(FErrors.Count - 1, 9) do  // Show first 10 errors
      WriteLn(Format('  - %s', [FErrors[I]]));
    if FErrors.Count > 10 then
      WriteLn(Format('  ... and %d more errors', [FErrors.Count - 10]));
  end;

  Stopwatch.Stop;

  if FShowProgress then
    WriteLn(Format('[INFO] Total AST processing: %d ms (%.1f files/sec)',
      [Stopwatch.ElapsedMilliseconds,
       GetFilesProcessed / Max(1, Stopwatch.ElapsedMilliseconds / 1000)]));
end;

end.
