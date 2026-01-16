# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the sample code repository for the Japanese book "関数型プログラミングの基礎" (Fundamentals of Functional Programming) by Akimichi Tatsukawa. The code demonstrates functional programming concepts using JavaScript, with additional examples in Scala and Haskell.

## Commands

### Running Tests

```bash
# Run all JavaScript tests (primary method)
gulp js-test

# Alternative: run tests via npm
npm test

# Run tests for other languages (requires Docker setup)
gulp scala-test
gulp haskell-test
gulp ruby-test
```

### Generating Documentation

```bash
# Generate docco documentation
rake doc

# Or via gulp
gulp doc
```

### Node.js Version

The project uses Node.js v16.20.0 (specified in `.nvmrc`). Use `nvm use` to switch to the correct version.

## Architecture

### Library Modules (`lib/`)

Core functional programming implementations:

- **monad.js** - Monad implementations (ID, Maybe, Either, IO, Cont) with Haskell-style pattern matching
- **list.js** - Immutable linked list with functional operations (map, flatMap, foldr, filter, etc.)
- **pair.js** - Pair/tuple data structure
- **parser.js** - Parser combinator library
- **evaluator.js** - Expression evaluator
- **monad_transformer.js** - Monad transformer implementations

### Test Files (`test/`)

Tests are organized by book chapters (`chap01.spec.js` through `chap08.spec.js`) plus module-specific tests. Tests use Mocha with expect.js assertions.

### Design Patterns

- **Pattern matching**: Implemented via objects with `match` methods that accept pattern objects (e.g., `{just: fn, nothing: fn}` for Maybe)
- **Curried functions**: Most library functions use curried form (e.g., `List.map(list)(transform)`)
- **Church encoding**: Algebraic data types encoded as functions that accept pattern objects
