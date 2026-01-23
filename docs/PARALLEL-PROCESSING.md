# Parallel Processing Architecture

The delphi-indexer uses parallel processing at two levels:
1. **Folder Scanning** - `TParallelFolderScanner`
2. **AST Parsing** - `TParallelASTProcessor`

Both use `TParallel.For` from `System.Threading` for work distribution.

---

## TParallelFolderScanner

**Source**: `uParallelFolderScanner.pas`

### Architecture

```
                    +-------------------+
                    |  ScanFolder()     |
                    |  (entry point)    |
                    +--------+----------+
                             |
                             v
                    +-------------------+
                    | ScanFolderPaths   |
                    | Only()            |
                    +--------+----------+
                             |
              +--------------+--------------+
              |                             |
              v                             v
    +------------------+           +------------------+
    | Get root files   |           | Get subdirs     |
    | (sequential)     |           | (sequential)    |
    +------------------+           +--------+--------+
                                            |
                                            v
                              +--------------------------+
                              | # subdirs >= threshold?  |
                              +-----------+--------------+
                                   |            |
                          >= 2     |            |  < 2
                                   v            v
                        +------------------+  +------------------+
                        | ScanSubfolders   |  | Sequential scan  |
                        | Parallel()       |  | (fallback)       |
                        +--------+---------+  +------------------+
                                 |
               +-----------------+-----------------+
               |                 |                 |
               v                 v                 v
        +-----------+     +-----------+     +-----------+
        | Worker 0  |     | Worker 1  |     | Worker N  |
        | TStringLst|     | TStringLst|     | TStringLst|
        +-----------+     +-----------+     +-----------+
               |                 |                 |
               +-----------------+-----------------+
                                 |
                                 v
                        +------------------+
                        | Merge results    |
                        | (sequential)     |
                        +------------------+
```

### Thread Safety

| Component | Strategy |
|-----------|----------|
| `WorkerResults[i]` | Pre-allocated per worker, no sharing |
| `FExcludedFolders` | Read-only after constructor |
| Statistics | Protected by `TCriticalSection` |
| File system calls | Inherently thread-safe (OS handles) |

### Expected Speedup

| CPU Cores | Subdirectories | Expected Speedup |
|-----------|---------------|------------------|
| 2 | 4+ | ~1.5x |
| 4 | 4+ | ~2.5x |
| 8 | 8+ | ~4x |
| 16 | 16+ | ~6x (I/O bound limit) |

### Usage

```pascal
var
  Scanner: TParallelFolderScanner;
  Files: TStringList;
begin
  Scanner := TParallelFolderScanner.Create(True);
  try
    Files := Scanner.ScanFolder('W:\MyProject');
    // Process files...
  finally
    Scanner.Free;
  end;
end;
```

---

## TParallelASTProcessor

**Source**: `uParallelASTProcessor.pas`

### Architecture

```
+---------------------------+
|  TParallelASTProcessor    |
+---------------------------+
|  - FMaxWorkers            | (CPU cores)
|  - FMinFilesForParallel   | (threshold = 4)
|  - Statistics (Int64)     | (thread-safe via TInterlocked)
+---------------------------+
           |
           v
+---------------------------+
|   ProcessFiles(AFiles)    |
+---------------------------+
           |
    files < 4?
    /       \
   yes       no
   |          |
   v          v
Sequential  Parallel
   |          |
   v          v
+-------------+  +----------------------------------+
| 1 Processor |  | N Processors + N Locks           |
+-------------+  +----------------------------------+
                           |
                           v
                 +-------------------+
                 | TParallel.For     |
                 | (work-stealing)   |
                 +-------------------+
                           |
                 +---------+---------+
                 |         |         |
              Worker0   Worker1   WorkerN
              (Lock0)   (Lock1)   (LockN)
                 |         |         |
                 v         v         v
           +-------+  +-------+  +-------+
           |Proc[0]|  |Proc[1]|  |Proc[N]|
           +-------+  +-------+  +-------+
```

### Why Locking is Needed

`TASTProcessor` (and its underlying `TPasSyntaxTreeBuilder` from DelphiAST) is **NOT thread-safe**. Multiple threads cannot share a single parser instance because:

1. The parser maintains mutable internal state (stack, current position, etc.)
2. String operations during parsing are not atomic
3. Comment collection uses shared mutable lists

### Solution: Per-Worker Processors

```pascal
// Pre-allocate N processors and N locks
SetLength(WorkerProcessors, WorkerCount);
SetLength(ProcessorLocks, WorkerCount);

for I := 0 to WorkerCount - 1 do
begin
  WorkerProcessors[I] := TASTProcessor.Create;
  ProcessorLocks[I] := TCriticalSection.Create;
end;

// In TParallel.For iteration:
WorkerIndex := Index mod WorkerCount;  // Distribute files across workers
Lock := ProcessorLocks[WorkerIndex];
Processor := WorkerProcessors[WorkerIndex];

Lock.Enter;
try
  FileChunks := Processor.ProcessSingleFile(ParsedFile);
finally
  Lock.Leave;
end;
```

### Statistics

Statistics are updated using `TInterlocked` for thread-safe counters:

```pascal
// Int64 fields (required by TInterlocked.Read)
FFilesProcessed: Int64;
FFilesErrored: Int64;
FTotalChunks: Int64;
FTotalParseTimeMs: Int64;

// Usage in parallel code:
CurrentProcessed := TInterlocked.Increment(FFilesProcessed);
TInterlocked.Add(FTotalChunks, FileChunks.Count);
```

### Expected Speedup

| CPU Cores | Expected Speedup | Notes |
|-----------|------------------|-------|
| 1-2       | ~1x              | Sequential fallback |
| 4         | ~3x              | Minimum target |
| 8+        | ~4-6x            | Diminishing returns (I/O bound) |

### Usage

```pascal
var
  ParallelProcessor: TParallelASTProcessor;
  Chunks: TCodeChunkList;
begin
  ParallelProcessor := TParallelASTProcessor.Create;
  try
    ParallelProcessor.MaxWorkers := 8;
    ParallelProcessor.MinFilesForParallel := 10;
    ParallelProcessor.ShowProgress := True;

    Chunks := ParallelProcessor.ProcessFiles(FileList);

    if ParallelProcessor.Errors.Count > 0 then
      for var Error in ParallelProcessor.Errors do
        WriteLn('Error: ', Error);
  finally
    ParallelProcessor.Free;
    Chunks.Free;
  end;
end;
```

---

## Performance Tuning

### Thresholds

| Class | Property | Default | Purpose |
|-------|----------|---------|---------|
| TParallelFolderScanner | MinSubfoldersForParallel | 2 | Min subdirs for parallel scan |
| TParallelASTProcessor | MinFilesForParallel | 4 | Min files for parallel parse |

### Why Thresholds?

`TParallel.For` has ~0.1ms overhead per invocation. For small workloads, sequential execution is faster.

### Limiting Factors

1. **I/O Bound**: File system enumeration is I/O bound, not CPU bound
2. **SSD vs HDD**: SSD benefits more from parallel I/O
3. **OS Caching**: First scan slower, subsequent scans benefit from cache
4. **Parser Lock Contention**: Each worker waits for its lock (mitigated by per-worker allocation)
5. **Memory Pressure**: Each TASTProcessor uses ~1MB

---

## Comparison with Sequential

| Feature | Sequential | Parallel |
|---------|------------|----------|
| Folder scanning | TFolderScanner | TParallelFolderScanner |
| AST parsing | TASTProcessor | TParallelASTProcessor |
| Thread safety | N/A | Yes (locks + TInterlocked) |
| Statistics | None | FilesFound, FoldersScanned, etc. |
| Progress | Basic | Throttled with ETA |
| Speedup | 1x | 2-6x (depending on workload) |
