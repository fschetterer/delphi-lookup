# Code Audit: delphi-lookup

**Date:** 2026-02-20
**Scope:** ~24K lines of Pascal across 30+ source files
**Status:** Actionable — each item includes file, line, root cause, and fix

---

## How to Use This Document

Each issue has a checkbox. Work through them in order — they're prioritized by
impact. Issues within the same priority tier are independent and can be tackled
in any order or in parallel.

**Severity levels:**
- **P0 — Critical:** Broken core functionality or exploitable vulnerability
- **P1 — High:** Memory leaks, security hardening, correctness
- **P2 — Medium:** Defense-in-depth, consistency, maintainability
- **P3 — Low:** Code quality, cleanup, minor improvements

---

## P0 — Critical

### - [ ] 1. FTS5 table is built but never queried

**File:** `uQueryProcessor.pas:414-476`
**Impact:** Core search uses `LIKE '%keyword%'` full table scans instead of the FTS5 index. This defeats the entire purpose of the FTS5 infrastructure built in `uDatabaseBuilder.pas`.

**Current code (line 443):**
```pascal
FQuery.SQL.Text :=
  'SELECT * FROM symbols ' +
  'WHERE (UPPER(content) LIKE UPPER(:search_term) ' +
  '   OR UPPER(comments) LIKE UPPER(:search_term) ' +
  '   OR UPPER(name) LIKE UPPER(:search_term)) ' +
  BuildFilterClause + ...
```

**Fix:** Replace with a proper FTS5 MATCH query against `symbols_fts`:
```pascal
FQuery.SQL.Text :=
  'SELECT s.* FROM symbols s ' +
  'JOIN symbols_fts fts ON s.id = fts.rowid ' +
  'WHERE symbols_fts MATCH :query_terms ' +
  BuildFilterClause +
  'ORDER BY rank ' +
  'LIMIT :max_results';
FQuery.ParamByName('query_terms').AsString := BuildFTS5Query(Keywords);
FQuery.ParamByName('max_results').AsInteger := AMaxResults;
```

You'll need a helper `BuildFTS5Query` that joins keywords with FTS5 operators
(e.g., `keyword1 AND keyword2` or `keyword1 keyword2` for implicit AND).

**Why it matters:** This is the single highest-impact fix. Database has 480K+
symbols; LIKE scans are orders of magnitude slower than FTS5 MATCH.

---

### - [ ] 2. SQL injection via `BuildFilterClause`

**File:** `uQueryProcessor.pas:219-237`
**Impact:** Filter values from CLI args and LSP clients are string-interpolated
into SQL using `QuotedStr()` — not parameterized queries. Used in every search
path (exact, fuzzy, FTS, references).

**Current code:**
```pascal
function TQueryProcessor.BuildFilterClause: string;
begin
  Result := '';
  if FContentTypeFilter <> '' then
    Result := Result + Format(' AND content_type = %s', [QuotedStr(FContentTypeFilter)]);
  if FDomainTagsFilter <> '' then
    Result := Result + Format(' AND domain_tags LIKE %s', [QuotedStr('%' + FDomainTagsFilter + '%')]);
  // ... same pattern for FSymbolTypeFilter, FFrameworkFilter, FSourceCategoryFilter
end;
```

**Fix — Option A (recommended): Refactor to parameter-based filtering.**

Change `BuildFilterClause` to return only the SQL template, then bind values
at each call site:

```pascal
function TQueryProcessor.BuildFilterClause: string;
begin
  Result := '';
  if FContentTypeFilter <> '' then
    Result := Result + ' AND content_type = :filter_content_type';
  if FSourceCategoryFilter <> '' then
    Result := Result + ' AND source_category = :filter_source_category';
  if FDomainTagsFilter <> '' then
    Result := Result + ' AND domain_tags LIKE :filter_domain_tags';
  if FSymbolTypeFilter <> '' then
    Result := Result + ' AND type = :filter_symbol_type';
  if FFrameworkFilter <> '' then
    Result := Result + ' AND (framework = :filter_framework OR framework IS NULL)';
end;

procedure TQueryProcessor.BindFilterParams;
begin
  if FContentTypeFilter <> '' then
    FQuery.ParamByName('filter_content_type').AsString := FContentTypeFilter;
  if FSourceCategoryFilter <> '' then
    FQuery.ParamByName('filter_source_category').AsString := FSourceCategoryFilter;
  if FDomainTagsFilter <> '' then
    FQuery.ParamByName('filter_domain_tags').AsString := '%' + FDomainTagsFilter + '%';
  if FSymbolTypeFilter <> '' then
    FQuery.ParamByName('filter_symbol_type').AsString := FSymbolTypeFilter;
  if FFrameworkFilter <> '' then
    FQuery.ParamByName('filter_framework').AsString := FFrameworkFilter;
end;
```

Then at every call site, after setting `FQuery.SQL.Text` and before `FQuery.Open`:
```pascal
BindFilterParams;
```

**Fix — Option B (minimal): Validate filter values against whitelists.**

If refactoring all call sites is too large, at minimum validate filter values
when they're set:

```pascal
procedure TQueryProcessor.SetFrameworkFilter(const AValue: string);
const
  ValidFrameworks: array[0..2] of string = ('VCL', 'FMX', 'RTL');
begin
  if (AValue = '') or MatchStr(AValue, ValidFrameworks) then
    FFrameworkFilter := AValue
  else
    raise Exception.CreateFmt('Invalid framework filter: %s', [AValue]);
end;
```

---

## P1 — High

### - [ ] 3. Memory leak: `TempEmbeddings` list never freed in CHM indexing loop

**File:** `delphi-indexer.dpr:823-829`
**Impact:** Leaks one `TEmbeddingList` object per CHM chunk. Thousands of leaks for large CHM files.

**Current code:**
```pascal
var TempEmbeddings := EmbeddingGen.GenerateEmbeddings(ChunkList);
try
  if TempEmbeddings.Count > 0 then
    Embeddings.Add(TempEmbeddings[0]);
finally
  // Don't free the individual embeddings, they're now owned by Embeddings list
end;
```

**Fix:**
```pascal
var TempEmbeddings := EmbeddingGen.GenerateEmbeddings(ChunkList);
try
  if TempEmbeddings.Count > 0 then
  begin
    Embeddings.Add(TempEmbeddings[0]);
    TempEmbeddings.OwnsObjects := False;  // Prevent freeing transferred item
  end;
finally
  TempEmbeddings.Free;
end;
```

---

### - [ ] 4. Memory leak: anonymous lists passed to `BuildDatabase`

**File:** `delphi-indexer.dpr:849-851`
**Impact:** Two list objects leaked every time `IndexCHMDocumentation` runs.

**Current code:**
```pascal
DBBuilder.BuildDatabase(TCodeChunkList.Create, TEmbeddingList.Create,
  ADBPath, EmbeddingURL, EmbeddingModel, EmbeddingDimensions,
  OutputDir, False, 'help', 'official_help');
```

**Fix — use local variables with try/finally:**
```pascal
var TempChunks := TCodeChunkList.Create;
var TempEmbed := TEmbeddingList.Create;
try
  DBBuilder.BuildDatabase(TempChunks, TempEmbed,
    ADBPath, EmbeddingURL, EmbeddingModel, EmbeddingDimensions,
    OutputDir, False, 'help', 'official_help');
finally
  TempChunks.Free;
  TempEmbed.Free;
end;
```

**Alternative:** Extract schema initialization into a parameterless method
(`DBBuilder.EnsureSchema(ADBPath)`) so empty lists aren't needed at all.

---

### - [ ] 5. Memory leak: `CalculateFolderHashForStorage` missing try/finally

**File:** `uChangeDetector.pas:632-639`
**Impact:** If `CalculateFolderHash` raises, both `TStringList` out-params leak.

**Current code:**
```pascal
function TChangeDetector.CalculateFolderHashForStorage(const AFolderPath: string): string;
var
  SubfolderNames, FileHashes: TStringList;
begin
  Result := CalculateFolderHash(AFolderPath, SubfolderNames, FileHashes);
  SubfolderNames.Free;
  FileHashes.Free;
end;
```

**Fix:**
```pascal
function TChangeDetector.CalculateFolderHashForStorage(const AFolderPath: string): string;
var
  SubfolderNames, FileHashes: TStringList;
begin
  SubfolderNames := nil;
  FileHashes := nil;
  try
    Result := CalculateFolderHash(AFolderPath, SubfolderNames, FileHashes);
  finally
    SubfolderNames.Free;
    FileHashes.Free;
  end;
end;
```

---

### - [ ] 6. Command injection via `ExecuteCommand` argument quoting

**File:** `uCHMExtractor.pas:183-185`
**Impact:** Paths containing embedded double-quotes break the command line
quoting. `PYTHON_EXE` env var is also used unsanitized.

**Current code:**
```pascal
CommandLine := '"' + ACommand + '"';
for I := 0 to High(AArgs) do
  CommandLine := CommandLine + ' "' + AArgs[I] + '"';
```

**Fix:** Validate inputs before building the command line:
```pascal
// Reject paths containing double-quotes
for I := 0 to High(AArgs) do
  if Pos('"', AArgs[I]) > 0 then
    raise Exception.CreateFmt('Invalid character in argument: %s', [AArgs[I]]);

// Use lpApplicationName parameter of CreateProcess for the executable
// and pass only arguments in lpCommandLine
```

Additionally, use `CreateProcess` with a non-nil `lpApplicationName` parameter
(the resolved Python path) so that only the arguments go through shell parsing.

---

### - [ ] 7. Path traversal in LSP `ReadFileContent`

**File:** `LSP/uLSPHandlers.pas:265-271`
**Impact:** LSP server reads any file on disk from client-supplied URIs without
restricting to workspace boundaries.

**Current code:**
```pascal
function TLSPServer.ReadFileContent(const AFilePath: string): string;
begin
  if FileExists(AFilePath) then
    Result := TFile.ReadAllText(AFilePath)
  else
    Result := '';
end;
```

**Fix:** Validate against workspace root (set during `initialize`):
```pascal
function TLSPServer.ReadFileContent(const AFilePath: string): string;
begin
  if not IsPathInWorkspace(AFilePath) then
    Exit('');
  if FileExists(AFilePath) then
    Result := TFile.ReadAllText(AFilePath)
  else
    Result := '';
end;

function TLSPServer.IsPathInWorkspace(const AFilePath: string): Boolean;
var
  Normalized: string;
begin
  Normalized := ExpandFileName(AFilePath);
  Result := StartsText(FWorkspaceRoot, Normalized);
end;
```

---

### - [ ] 8. Unsafe binary deserialization with unbounded allocations

**File:** `uIndexerEmbeddings.Ollama.pas:144-196`
**Impact:** `LoadFromBinaryFile` trusts all size fields from the file. A crafted
file can trigger OOM via huge `Count`, `ChunkIDLen`, or `VectorLen`.

**Current code (no bounds checks):**
```pascal
FileStream.Read(Count, SizeOf(Integer));     // unchecked
// ...
FileStream.Read(ChunkIDLen, SizeOf(Integer)); // unchecked
SetLength(ChunkIDBytes, ChunkIDLen);          // could be 2GB
// ...
FileStream.Read(VectorLen, SizeOf(Integer));  // unchecked
SetLength(Vector, VectorLen);                 // could be 2GB
```

**Fix — add bounds validation after each read:**
```pascal
FileStream.Read(Version, SizeOf(Integer));
if Version <> 1 then
  raise Exception.CreateFmt('Unsupported binary format version: %d', [Version]);

FileStream.Read(Count, SizeOf(Integer));
if (Count < 0) or (Count > 10000000) then
  raise Exception.Create('Corrupt binary file: invalid count');

// Inside loop:
FileStream.Read(ChunkIDLen, SizeOf(Integer));
if (ChunkIDLen < 0) or (ChunkIDLen > 65536) then
  raise Exception.Create('Corrupt binary file: invalid ChunkID length');

FileStream.Read(VectorLen, SizeOf(Integer));
if (VectorLen < 0) or (VectorLen > 65536) then
  raise Exception.Create('Corrupt binary file: invalid vector length');
```

---

### - [ ] 9. `ProcessFiles` (legacy mode) missing try/finally for list cleanup

**File:** `delphi-indexer.dpr:1191-1364`
**Impact:** `ParsedFiles`, `FilesToProcess`, `CodeChunks`, `Embeddings` freed at
lines 1360-1363 without try/finally. Any exception between allocation and cleanup
leaks all four lists.

**Fix:** Wrap the processing body in try/finally:
```pascal
ParsedFiles := TStringList.Create;
FilesToProcess := TStringList.Create;
CodeChunks := TCodeChunkList.Create;
Embeddings := TEmbeddingList.Create;
try
  // ... all processing steps ...
finally
  Embeddings.Free;
  CodeChunks.Free;
  FilesToProcess.Free;
  ParsedFiles.Free;
end;
```

---

## P2 — Medium

### - [ ] 10. Unbounded `Content-Length` in LSP protocol reader

**File:** `LSP/uLSPProtocol.pas:182-201`
**Impact:** No cap on `Content-Length`. A value of `MaxInt` causes attempted 2GB
allocation.

**Fix — add a maximum check in `ReadMessage` (around line 237):**
```pascal
const
  MAX_LSP_CONTENT = 10 * 1024 * 1024;  // 10 MB

if ContentLength > MAX_LSP_CONTENT then
begin
  Log(Format('ERROR: Content-Length %d exceeds maximum %d', [ContentLength, MAX_LSP_CONTENT]));
  Exit;
end;
```

---

### - [ ] 11. Information disclosure via LSP error messages

**File:** `LSP/uLSPHandlers.pas:195-200`
**Impact:** Full exception messages (including database paths, SQL error text,
internal file paths) are sent verbatim to the LSP client.

**Fix:**
```pascal
except
  on E: Exception do
  begin
    TLSPProtocol.Log('Internal error: ' + E.Message);  // Log full detail
    if AMessage.HasID then
      TLSPProtocol.WriteError(AMessage.ID, ecInternalError, 'Internal server error');
  end;
end;
```

---

### - [ ] 12. `__DCU_CACHE` not excluded in `TFolderScanner`

**File:** `uFolderScanner.pas:65-94`
**Impact:** `uChangeDetector.pas` and `uParallelFolderScanner.pas` exclude
`__DCU_CACHE`, but `TFolderScanner.ShouldExcludeFolder` does not. Legacy-mode
scanning will process compiled unit directories.

**Fix — add to the build output exclusion block (after line 91):**
```pascal
  if SameText(FolderName, 'Win32') or
     SameText(FolderName, 'Win64') or
     SameText(FolderName, 'Debug') or
     SameText(FolderName, 'Release') or
     SameText(FolderName, '__DCU_CACHE') then  // <-- add this
    Exit(True);
```

---

### - [ ] 13. String concatenation in `load_extension` SQL

**File:** `uDatabaseConnection.pas:230`
**Impact:** `Vec0Path` is concatenated into SQL. Currently computed from the
executable directory (not user input), but the pattern is fragile.

**Current code:**
```pascal
Query.SQL.Text := 'SELECT load_extension(''' + Vec0Path + ''')';
```

**Fix:**
```pascal
Query.SQL.Text := 'SELECT load_extension(:ext_path)';
Query.ParamByName('ext_path').AsString := Vec0Path;
Query.Open;
```

---

### - [ ] 14. No TLS certificate validation on HTTP clients

**Files:**
- `uIndexerEmbeddings.Ollama.pas:207-210`
- `uLookupEmbeddings.Ollama.pas:94-97`
- `uReranker.pas:48-51`

**Impact:** Source code content sent for embedding could be intercepted if the
Ollama/reranker service runs on HTTPS (remote deployments).

**Fix — set explicit TLS policy:**
```pascal
FHttpClient := THTTPClient.Create;
FHttpClient.SecureProtocols := [THTTPSecureProtocol.TLS12, THTTPSecureProtocol.TLS13];
FHttpClient.ConnectionTimeout := 30000;
FHttpClient.ResponseTimeout := 300000;
```

---

### - [ ] 15. Unparameterized table name in `ColumnExists`

**File:** `uDatabaseBuilder.pas:1996-2013`
**Impact:** PRAGMA can't use parameters, but the method is public and accepts
arbitrary strings. Currently all callers pass hardcoded names.

**Fix — add whitelist validation:**
```pascal
function TDatabaseBuilder.ColumnExists(const ATable, AColumn: string): Boolean;
const
  ALLOWED_TABLES: array[0..5] of string = (
    'symbols', 'query_log', 'query_cache',
    'file_hashes', 'indexed_folders', 'vec_embeddings'
  );
var
  I: Integer;
begin
  // Validate table name against whitelist (PRAGMA can't use parameters)
  Result := False;
  for I := 0 to High(ALLOWED_TABLES) do
    if SameText(ATable, ALLOWED_TABLES[I]) then
    begin
      Result := True;
      Break;
    end;
  if not Result then
    raise Exception.CreateFmt('ColumnExists: invalid table name "%s"', [ATable]);

  Result := False;
  FQuery.SQL.Text := Format('PRAGMA table_info(%s)', [ATable]);
  // ... rest unchanged
```

---

### - [ ] 16. LSP log file path from environment variable without validation

**File:** `LSP/uLSPProtocol.pas:109-113`
**Impact:** `DELPHI_LSP_LOG` env var passed directly to `AssignFile`. Could
overwrite arbitrary files if environment is controlled by attacker.

**Fix — validate path before use:**
```pascal
class procedure TLSPProtocol.EnableLogging(const ALogFile: string);
var
  LogDir: string;
begin
  LogDir := ExtractFilePath(ExpandFileName(ALogFile));
  if not DirectoryExists(LogDir) then
    Exit;  // Don't create directories or write outside existing paths
  AssignFile(FLogFile, ALogFile);
  Rewrite(FLogFile);
  FLoggingEnabled := True;
end;
```

---

## P3 — Low (Code Quality)

### - [ ] 17. Dead code: `GenerateSetupScript` never called

**File:** `delphi-indexer.dpr:194-255`
**Action:** Delete the entire procedure. It references the obsolete
ChromaDB/Python architecture replaced by Ollama.

---

### - [ ] 18. `ExtractMethodSignature` duplicated with different logic

**Files:**
- `uDatabaseBuilder.pas:336-397` — parenthesis-depth tracking, stops at `begin`/`var`/`asm`
- `uResultFormatter.pas:207-237` — regex matching `procedure|function|...`

**Action:** Extract into a shared utility unit (e.g., `uPascalUtils.pas`). Keep
the `uDatabaseBuilder` version (more robust). Remove the regex version from
`uResultFormatter` and call the shared one.

---

### - [ ] 19. Global variables as implicit procedure parameters

**File:** `delphi-indexer.dpr:62-113`
**Impact:** `ProcessFiles`, `ProcessFilesHybrid`, `ProcessFilesIncremental` all
read module-level globals. `ReindexAllFolders` manually saves/restores
`FolderPath`, `ContentType`, `SourceCategory` — fragile pattern.

**Action:** Define a `TIndexerConfig` record:
```pascal
type
  TIndexerConfig = record
    FolderPath: string;
    DatabaseFile: string;
    EmbeddingURL: string;
    ContentType: string;
    SourceCategory: string;
    // ... etc
  end;
```
Pass it as a parameter to all processing procedures. This eliminates the
save/restore antipattern and makes the code testable.

---

### - [ ] 20. Commented-out glossary enrichment block

**File:** `delphi-indexer.dpr:1276-1307`
**Action:** Delete the 31-line commented block and its vestigial `Step3Ms := 0`
assignment (line 1274). Also remove the misleading "Step 3 (Comments): 0 ms"
from the timing summary (line 1353).

---

### - [ ] 21. `RevalidateCache` uses inefficient two-pass counting

**File:** `delphi-indexer.dpr:1002-1008`
**Impact:** Iterates entire result set to count candidates, resets cursor,
iterates again to process.

**Fix:** Use `Query.RecordCount` after `Open` (FireDAC materializes the result
set), or run a separate `COUNT(*)` query before the data query.

---

## Summary

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 2 | FTS5 not used, SQL injection |
| P1 | 7 | Memory leaks (×3), command injection, path traversal, unsafe deser., missing try/finally |
| P2 | 7 | DoS, info disclosure, inconsistent exclusions, TLS, SQL patterns |
| P3 | 5 | Dead code, duplication, globals, cleanup |
| **Total** | **21** | |

---

## Verification Checklist

After fixing each item, verify:

- [ ] `delphi-lookup.exe "TStringList" -n 5` returns results using FTS5 (check query plan)
- [ ] `delphi-indexer.exe --index-chm` completes without growing memory per chunk
- [ ] LSP server rejects file reads outside workspace root
- [ ] Filter values with special characters (`'`, `%`, `"`) don't cause SQL errors
- [ ] Binary file with corrupt size headers raises clean exception, doesn't OOM
- [ ] `__DCU_CACHE` folders are excluded in all three scanner implementations
