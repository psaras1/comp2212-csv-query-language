# COMP2212 CSV Query Language

A domain-specific programming language for querying CSV documents, developed as coursework for COMP2212 Programming Language Concepts.

## Group Members

1. **[Constantinos Psaras]** - [cp2n23]
2. **[Fatema Waly]** - [fmew1e22]
3. **[Khudeja Begum]** - [kb1n22]

## Project Overview

Goal - Create a custom query language for CSV files that allows users to:
- Filter and select data from CSV files
- Perform operations like cartesian product, projection, and joins
- Transform and manipulate data
- Output results in CSV format

Our language is implemented in Haskell and uses Alex for lexical analysis and Happy for parsing.

## Installation

### Requirements
- GHC (Glasgow Haskell Compiler)
- Stack (Haskell Tool Stack)
- Alex (lexer generator)
- Happy (parser generator)

### Setup

```bash
# Clone the repository
git clone https://github.com/[your-github-username]/comp2212-csv-query-language.git
cd comp2212-csv-query-language

# Build the project
stack build

# Run the executable
stack exec comp2212-csv-query-language-exe