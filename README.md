# CSV Query Language (CQL)

A domain-specific programming language for querying CSV files.

## Overview

This language provides a SQL-inspired syntax for querying and manipulating CSV data.
The interpreter reads queries written in CQL, processes CSV files, and outputs the results.

## Features

- Cartesian product of tables
- Selection and filtering of rows
- Permutation and projection of columns
- Existence checks on column values
- Left merge of tables
- Support for constants and copying values

## Building

To build the project, you need GHC, Alex, and Happy installed.

```bash
# Using stack
stack build

# Using cabal
cabal build
```

## Usage

```bash
# Run a query file
stack exec comp2212-csv-query-language-exe <cql_file_path>

# The program will look for CSV files in the current directory
# based on table names referenced in the query.
```

## Syntax Examples

```sql
-- Cartesian product of A and B
CARTESIAN PRODUCT A, B;

-- Select specific columns with a condition
SELECT #1, #3 FROM A WHERE #2 = "value";

-- Perform a permutation with matching condition
PERMUTE #3, #1 FROM A WHERE MATCH #1 #2;

-- Check for non-empty values in column 2
EXISTS COL #2 FROM A;

-- Copy a column with a constant value in between
COPY #1 WITH CONSTANT "foo" FROM A;

-- Left merge two tables on the first column
LEFT MERGE P ON #1 WITH Q;
```

## Tasks

The example query files in this repository correspond to the following tasks:

- `Task 1`: Cartesian product of tables A and B
- `Task 2`: Permutation, drop, and matching
- `Task 3`: Existence check
- `Task 4`: Copying and constants
- `Task 5`: Left merge on first column
- `Task 6` : Multiway Cartesian Product
- `Task 7`: Paired Composition
- `Task 8`: Right Merge on Last Column
- `Task 9`: Paths of length three
- `Task 10`: Matching Pairs

