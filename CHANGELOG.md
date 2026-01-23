# Changelog

All notable changes to delphi-lookup will be documented in this file.

## [Unreleased]

### Added
- **Parallel Processing for Indexing**: Major performance improvements for delphi-indexer
  - `TParallelFolderScanner`: 8x speedup on folder scanning using `TParallel.For`
  - `TParallelASTProcessor`: Worker pool with one parser per thread (3x+ speedup)
  - Configurable worker count (default: CPU cores)
- **Merkle-style Change Detection**: Fast detection of modified/deleted files
  - `indexed_files` table tracking individual file hashes and timestamps
  - Folder-level hash comparison to skip unchanged subtrees
  - Automatic detection and cleanup of deleted files/folders
  - No-change verification in <10ms for large codebases (was 17s)
- Performance documentation: `docs/INCREMENTAL-INDEXING.md`, `docs/PARALLEL-PROCESSING.md`, `docs/PERFORMANCE.md`
- Performance test suite: `Tests/Performance/PerformanceTests.dpr`

### Changed
- **Indexer Performance**: Dramatically faster incremental updates
  - No changes: 10ms (was 17s) - 1700x faster
  - Single file change: 5.8s (was 17s) - 2.9x faster
  - `--force` flag preserved for full reindex when needed

### Added
- **Unit name in search results**: Each result now shows `// Unit: UnitName` directly below the file path
  - Eliminates need for AI agents to read source files just to determine the unit name for `uses` clauses
  - Rationale: In Delphi, unit name = filename without `.pas` extension (compiler-enforced)
  - AI agents were reading files unnecessarily to "verify" the unit name; explicit output prevents this
- **Gemini CLI Setup Guide**: One-shot configuration prompt for Gemini CLI users (`gemini/GEMINI_SETUP.md`)
- **Smart Cache Revalidation System**: Content-hash based cache validation that survives index updates
  - `content_hash` field in `symbols` table (MD5 of symbol content)
  - `query_cache` table replacing query_log for caching (one row per unique query)
  - Cache hits validate that referenced symbols still exist and content hasn't changed
  - Cache entries invalidated only when their specific symbols change (not on every reindex)
  - Support for caching "0 results" queries
- `--revalidate-cache [N]` command in delphi-indexer:
  - Revalidates invalidated queries with N+ hits (default: 3)
  - Purges obsolete cache entries (0 results >30 days, low hits >30 days)
  - Can be interrupted with Ctrl+C safely
  - Shows detailed progress and summary statistics
- `--stats` flag in delphi-lookup to show usage statistics (total queries, failure rate, avg duration)
- `--clear-cache` flag in delphi-lookup to delete all cached queries
- Cache preservation when no changes: `FinalizeFolder` no longer invalidates cache if no files were modified
- Realistic search examples documentation (`docs/HELP-SEARCH-EXAMPLES.md`)

### Changed
- **Vector Search Recommendations**: Tool is now AI-agent-only; FTS5 is the recommended mode
  - AI agents iterate fast, achieving similar quality with multiple searches while being 17x faster
  - Embedding infrastructure remains functional but not recommended for agent workflows
  - Added `BENCHMARK-embedding-quality.md` with detailed quality comparison
  - Updated CLAUDE.md with simplified recommendations
- Cache format now uses `id:hash` pairs in `result_ids` for robust validation
- `query_cache` table tracks hit counts, first/last seen timestamps, and average duration
- `InvalidateQueryCache` now invalidates both `query_cache` and `query_log` tables
- `ShowStats` displays both query_log and query_cache statistics
- `ClearCache` clears both tables
- `FinalizeFolder` now requires `AFilesModified` parameter to decide whether to invalidate cache

### Technical
- Automatic schema migration for existing databases (adds `content_hash`, creates `query_cache`)
- One-time migration from `query_log` to `query_cache` on first use
- Backwards-compatible: still reads from `query_log` as fallback

## [1.0.0] - 2025-12-15

### Added
- **delphi-indexer.exe**: Pascal source code indexer with support for:
  - Incremental indexing (only modified files)
  - Automatic framework detection (VCL/FMX/RTL)
  - Source categories (user, stdlib, third_party)
  - FTS5 full-text search
  - Optional vector embeddings (Ollama)

- **delphi-lookup.exe**: Hybrid search tool with:
  - Exact name matching
  - Fuzzy matching (Levenshtein distance)
  - FTS5 full-text search
  - Optional semantic search (vectors)
  - Query caching
  - Filters by framework, category, symbol type

- **CheckFTS5.exe**: Diagnostic tool to verify FTS5 support

- CHM documentation indexing support (partially implemented)
- Multi-tier framework detection system (comment tags, mapping files, packages, uses clause)
- Query logging for analytics (`query_log` table)

### Technical
- SQLite database with WAL mode
- FTS5 for full-text search
- sqlite-vec for vector search (optional)
- DelphiAST for Pascal code parsing
