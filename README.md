# xtagger

A Common Lisp command-line utility for searching files by their
extended attributes (xattr) tags.

As of now, it's particularly designed to work with KDE Dolphin's
tagging system, allowing you to find tagged files from the command
line.

## Features

- Search files by tag recursively
- List all tagged files and their attributes
- Compatible with KDE Dolphin's tagging system
- Configurable search path via environment variable

## Prerequisites

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- Required libraries:
  - cffi
  - cl-fad
  - babel
  - cl-ppcre

## Installation

1. Make sure you have SBCL and Quicklisp installed.

2. Clone or download this repository:
   ```bash
   git clone [repository-url]
   cd xtagger
   ```

3. Compile the executable:
   ```bash
   sbcl --load "xtagger.lisp" --eval "(xattr-lister:save-executable)" --quit
   ```

4. (Optional) Move the executable to your PATH:
   ```bash
   sudo mv xtagger /usr/local/bin/
   ```

## Usage

### Basic Commands

1. Find files by tag:
   ```bash
   xtagger tag <tagname>
   ```

2. List all tagged files:
   ```bash
   xtagger list
   ```

### Setting Default Search Path

You can set the default search path using the `TAGGER_PATH` environment variable:

```bash
export TAGGER_PATH="/path/to/search"
```

Add this to your `.bashrc` or `.zshrc` to make it permanent.

### Command Line Options

```
Usage:
  Find files by tag: xtagger tag <tagname> [directory]
  List all tags:     xtagger list [directory]

If directory is not specified, TAGGER_PATH environment variable will be used.
```

### Examples

1. Search for files tagged as "important":
   ```bash
   xtagger tag important
   ```

2. List all tagged files in a specific directory:
   ```bash
   xtagger list /home/user/Documents
   ```

3. Search for tagged files in a specific directory:
   ```bash
   xtagger tag work /home/user/Projects
   ```

## Using from Common Lisp REPL

You can also use the functions directly from a Common Lisp REPL:

```lisp
(ql:quickload '(:cffi :cl-fad :babel :cl-ppcre))
(load "xtagger.lisp")

;; Find files with specific tag
(xattr-lister:find-files-with-tag "/path/to/search" "important")

;; List all tagged files
(xattr-lister:list-files-with-xattrs "/path/to/search")
```

## Limitations

- Currently only works on Unix-like systems with xattr support
- Designed primarily for KDE Dolphin's tagging system
- Requires appropriate permissions to read extended attributes
