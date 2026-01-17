"use strict";

// Type pattern for Pair
export interface TypePatterns<R> {
  pair: () => R;
}

// Match patterns for Pair
export interface PairPatterns<L, R, Result> {
  empty?: () => Result;
  cons: (left: L, right: R) => Result;
}

// Pair interface (Church-encoded)
export interface Pair<L, R> {
  type: <Result>(pattern: TypePatterns<Result>) => Result;
  match: <Result>(pattern: PairPatterns<L, R, Result>) => Result;
}

// Empty pair type
export interface EmptyPair {
  type: <Result>(pattern: TypePatterns<Result>) => Result;
  match: <Result>(pattern: { empty: () => Result }) => Result;
}

/**
 * Returns the type identifier for Pair
 */
export function type<R>(pattern: TypePatterns<R>): R {
  return pattern.pair();
}

/**
 * Creates an empty pair
 */
export function empty<L = never, R = never>(_?: any): EmptyPair {
  return {
    type: <Result>(pattern: TypePatterns<Result>): Result => {
      return pattern.pair();
    },
    match: <Result>(pattern: { empty: () => Result }): Result => {
      return pattern.empty();
    }
  };
}

/**
 * Creates a pair (cons cell) with left and right values
 */
export function cons<L, R>(left: L, right: R): Pair<L, R> {
  return {
    type: <Result>(pattern: TypePatterns<Result>): Result => {
      return pattern.pair();
    },
    match: <Result>(pattern: PairPatterns<L, R, Result>): Result => {
      return pattern.cons(left, right);
    }
  };
}

/**
 * Gets the right value from a pair
 */
export function right<L, R>(data: Pair<L, R>): R {
  return data.match({
    cons: (_left: L, right: R): R => {
      return right;
    }
  });
}

/**
 * Gets the left value from a pair
 */
export function left<L, R>(data: Pair<L, R>): L {
  return data.match({
    cons: (left: L, _right: R): L => {
      return left;
    }
  });
}

// Namespace export for module compatibility
export const Pair = {
  type,
  empty,
  cons,
  right,
  left
};

export default Pair;
