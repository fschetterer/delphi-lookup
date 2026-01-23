# Incremental Indexing - Merkle-Style Change Detection

## Overview

`TChangeDetector` implements a Merkle tree-inspired algorithm for efficient detection of file changes in large codebases. The goal is to minimize filesystem I/O and database queries when no changes have occurred.

**Performance Target**: <100ms for 10,000 files with no changes

**Source File**: `uChangeDetector.pas`

## Algorithm

### Core Concept

The algorithm uses hierarchical hashing similar to Merkle trees:

1. Each folder has a `folder_hash` stored in the database
2. The hash is calculated from:
   - Sorted list of subfolder names (not their content - that's recursive)
   - Sorted list of file names + last-modified timestamps
3. If the calculated hash matches the stored hash, the **entire subtree is skipped**
4. If different, descend recursively but only into branches that differ

### Hash Calculation

```
folder_hash = MD5(
  "D:subfolder1\n" +
  "D:subfolder2\n" +
  ... +
  "F:file1.pas:1705968000\n" +
  "F:file2.pas:1705968001\n" +
  ...
)
```

Where:
- `D:` prefix indicates a directory entry
- `F:` prefix indicates a file entry
- File entries include name + last-modified timestamp (as file date integer)
- All entries are sorted alphabetically for deterministic hashing

### Detection Flow

```
DetectChanges(root_folder)
   |
   +-> LoadFolderCache()     // Single DB query: all folder_hash values
   +-> LoadFileHashCache()   // Single DB query: all file content hashes
   |
   +-> DescendModifiedBranches(root_folder)
        |
        +-> CalculateFolderHash(folder)
        |    |
        |    +-> Enumerate subfolders (exclude .git, __history, etc.)
        |    +-> Enumerate .pas files (exclude _TLB.pas, _IMPL.pas)
        |    +-> Sort and concatenate into hash input
        |    +-> Calculate MD5
        |
        +-> Compare with stored folder_hash
        |
        +-> IF MATCH: return (skip entire subtree)  <-- KEY OPTIMIZATION
        |
        +-> IF DIFFER:
             |
             +-> DetectDeletedSubfolders()  // Check stored vs current
             +-> For each file in folder:
             |    +-> Compare content hash if file existed
             |    +-> Add to changed_files if new or modified
             +-> For each subfolder:
                  +-> DescendModifiedBranches(subfolder)  // Recursive
```

## Example Execution

### Scenario: 10,000 files, no changes

```
Root/
  +-- LibA/         (3,000 files)
  +-- LibB/         (2,000 files)
  +-- LibC/         (5,000 files)
```

**Without Merkle detection**:
- Would need to check 10,000 file timestamps/hashes
- Time: ~2-5 seconds

**With Merkle detection**:
1. Calculate `Root/` folder_hash
2. Compare with stored hash
3. **Hash matches** -> Return immediately, skip all 10,000 files
4. Time: **<50ms** (one DB query + one folder enumeration)

### Scenario: 1 file modified in LibB

```
Root/           (hash changed)
  +-- LibA/     (hash unchanged) -> SKIP 3,000 files
  +-- LibB/     (hash changed)   -> Descend
  |    +-- sub1/  (unchanged)    -> SKIP
  |    +-- sub2/  (changed)      -> Check files
  |         +-- modified.pas     -> Detected as modified
  +-- LibC/     (hash unchanged) -> SKIP 5,000 files
```

**With Merkle detection**:
- Only checks: Root, LibB, LibB/sub2
- Skips: LibA (3,000), LibC (5,000), LibB/sub1
- Files actually checked: ~10-50
- Time: **<200ms**

## Complexity Analysis

### Time Complexity

| Scenario | Files | Complexity | Expected Time |
|----------|-------|------------|---------------|
| No changes | N | O(1) to O(depth) | <50ms |
| 1 file changed | N | O(depth + siblings) | <100ms |
| Entire tree changed | N | O(N) | O(original) |
| k files changed | N | O(k * depth) | k * 50ms |

Where:
- N = total number of files
- depth = folder tree depth (typically 3-5 levels)
- siblings = files in the changed folder

### Space Complexity

- Folder cache: O(folders) entries, typically < 1% of files
- File hash cache: O(files) entries
- Memory: ~50 bytes per file = ~500KB for 10,000 files

### Why <100ms is Guaranteed for No Changes

1. **Two DB queries** at startup:
   - `SELECT * FROM indexed_folders WHERE path LIKE 'root%'` (~10ms)
   - `SELECT * FROM indexed_files WHERE path LIKE 'root%'` (~20ms)

2. **One folder enumeration** for root:
   - `TDirectory.GetDirectories()` (~5ms)
   - `TDirectory.GetFiles('*.pas')` (~5ms)
   - MD5 hash calculation (~1ms)

3. **One string comparison**:
   - stored_hash == calculated_hash (~0ms)

4. **Total**: ~41ms + overhead = **<100ms**

## Usage

### Command Line

```bash
# Incremental reindex (uses TChangeDetector)
delphi-indexer.exe "W:\MyProject"

# Full reindex (skips TChangeDetector)
delphi-indexer.exe "W:\MyProject" --force
```

### Statistics Output

```
Folders checked: 3
Folders skipped: 47
Files checked: 12
Files skipped: 11
Changed files: 1
Deleted files: 0
Detection time: 29 ms
```

## Limitations

1. **Hash calculation requires filesystem access**: Even "no changes" needs to enumerate the root folder to calculate its hash. This is unavoidable but minimal.

2. **First run is slower**: Initial indexing cannot skip anything since no hashes are stored.

3. **Timestamp-based folder hash**: Uses file timestamps, not content, for folder hashing. A file "touched" but not modified will trigger descent into that folder, but individual file content hashes will correctly identify it as unchanged.

## Database Schema

See [DATABASE-SCHEMA.md](../DATABASE-SCHEMA.md) for:
- `indexed_folders` table with `folder_hash`, `parent_folder_path`, `subfolders_list` columns
- `indexed_files` table for per-file tracking
