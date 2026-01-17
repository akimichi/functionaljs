"use strict";

// Type patterns for List
export interface TypePatterns<R> {
  list: () => R;
}

// Match patterns for List
export interface ListPatterns<T, R> {
  empty: () => R;
  cons: (head: T, tail: List<T>) => R;
}

// List interface (Church-encoded)
export interface List<T> {
  type: <R>(pattern: TypePatterns<R>) => R;
  match: <R>(pattern: ListPatterns<T, R>) => R;
}

/**
 * Returns the type identifier for List
 */
export function type<R>(pattern: TypePatterns<R>): R {
  return pattern.list();
}

/**
 * Pattern match helper
 */
export function match<T, R>(data: List<T>, pattern: ListPatterns<T, R>): R {
  return data.match(pattern);
}

/**
 * Creates an empty list
 */
export function empty<T>(): List<T> {
  return {
    type: <R>(pattern: TypePatterns<R>): R => {
      return pattern.list();
    },
    match: <R>(pattern: ListPatterns<T, R>): R => {
      return pattern.empty();
    }
  };
}

/**
 * Creates a cons cell (list node)
 */
export function cons<T>(head: T, tail: List<T>): List<T> {
  return {
    type: <R>(pattern: TypePatterns<R>): R => {
      return pattern.list();
    },
    match: <R>(pattern: ListPatterns<T, R>): R => {
      return pattern.cons(head, tail);
    }
  };
}

/**
 * Creates a singleton list
 */
export function unit<T>(value: T): List<T> {
  return cons(value, empty());
}

/**
 * Gets the head of a list
 */
export function head<T>(data: List<T>): T | null {
  return data.match({
    empty: () => null,
    cons: (h, _tail) => h
  });
}

/**
 * Gets the tail of a list
 */
export function tail<T>(data: List<T>): List<T> | null {
  return data.match({
    empty: () => null,
    cons: (_head, t) => t
  });
}

/**
 * Checks if a list is empty
 */
export function isEmpty<T>(list: List<T>): boolean {
  return list.match({
    empty: () => true,
    cons: () => false
  });
}

/**
 * Checks if two lists are equal
 */
export function isEqual<T>(xs: List<T>, ys: List<T>): boolean {
  if (isEmpty(xs)) {
    return isEmpty(ys);
  } else {
    if (isEmpty(ys)) {
      return false;
    } else {
      return xs.match({
        empty: () => false,
        cons: (x, xtail) => {
          return ys.match({
            empty: () => false,
            cons: (y, ytail) => {
              if (x === y) {
                return isEqual(xtail, ytail);
              } else {
                return false;
              }
            }
          });
        }
      });
    }
  }
}

/**
 * Flattens a list of lists
 * flatten :: [[a]] -> [a]
 */
export function flatten<T>(instanceMM: List<List<T>>): List<T> {
  return instanceMM.match({
    empty: () => empty(),
    cons: (h, t) => append(h)(flatten(t))
  });
}

/**
 * Maps a function over a list
 */
export function map<T>(instanceM: List<T>): <U>(transform: (item: T) => U) => List<U> {
  return <U>(transform: (item: T) => U): List<U> => {
    return instanceM.match({
      empty: () => empty<U>(),
      cons: (h, t) => cons(transform(h), map(t)(transform))
    });
  };
}

/**
 * FlatMap (bind) operation
 */
export function flatMap<T>(instanceM: List<T>): <U>(transform: (item: T) => List<U>) => List<U> {
  return <U>(transform: (item: T) => List<U>): List<U> => {
    return flatten(map(instanceM)(transform));
  };
}

/**
 * Appends two lists
 * append :: [a] -> [a] -> [a]
 */
export function append<T>(xs: List<T>): (ys: List<T>) => List<T> {
  return (ys: List<T>): List<T> => {
    return xs.match({
      empty: () => ys,
      cons: (h, t) => cons(h, append(t)(ys))
    });
  };
}

/**
 * Concatenates a list of lists (alias for flatten)
 */
export function concat<T>(xss: List<List<T>>): List<T> {
  return xss.match({
    empty: () => empty(),
    cons: (xs, rest) => append(xs)(concat(rest))
  });
}

/**
 * Gets the last element of a list
 */
export function last<T>(alist: List<T>): T | null {
  return alist.match({
    empty: () => null,
    cons: (h, t) => {
      return t.match({
        empty: () => h,
        cons: () => last(t)
      });
    }
  });
}

/**
 * Join (alias for concat)
 */
export function join<T>(list_of_list: List<List<T>>): List<T> {
  return concat(list_of_list);
}

/**
 * Right fold
 * foldr :: [a] -> b -> (a -> b -> b) -> b
 */
export function foldr<T>(alist: List<T>): <U>(accumulator: U) => (glue: (item: T) => (acc: U) => U) => U {
  return <U>(accumulator: U) => (glue: (item: T) => (acc: U) => U): U => {
    return alist.match({
      empty: () => accumulator,
      cons: (h, t) => glue(h)(foldr(t)(accumulator)(glue))
    });
  };
}

/**
 * Checks if any element satisfies the predicate
 */
export function any<T>(alist: List<T>): (predicate: (item: T) => boolean) => boolean {
  const or = (list: List<boolean>): boolean => {
    return list.match({
      empty: () => false,
      cons: (h, t) => h || or(t)
    });
  };
  return (predicate: (item: T) => boolean): boolean => {
    return or(map(alist)(predicate));
  };
}

/**
 * Checks if an element is in the list
 */
export function elem<T>(alist: List<T>): (item: T) => boolean {
  const isEq = (x: T) => (y: T): boolean => x === y;
  return (item: T): boolean => {
    return any(alist)(isEq(item));
  };
}

/**
 * Creates a list from an array
 */
export function fromArray<T>(array: T[]): List<T> {
  return array.reduce((accumulator: List<T>, item: T) => {
    return append(accumulator)(cons(item, empty()));
  }, empty<T>());
}

/**
 * Converts a list to an array
 */
export function toArray<T>(alist: List<T>): T[] {
  const toArrayAux = (list: List<T>, accumulator: T[]): T[] => {
    return list.match({
      empty: () => accumulator,
      cons: (h, t) => toArrayAux(t, accumulator.concat(h))
    });
  };
  return toArrayAux(alist, []);
}

/**
 * Gets the length of a list
 */
export function length<T>(alist: List<T>): number {
  return foldr(alist)(0)((_: T) => (accumulator: number) => 1 + accumulator);
}

/**
 * Filters a list by a predicate
 */
export function filter<T>(alist: List<T>): (predicate: (item: T) => boolean) => List<T> {
  return (predicate: (item: T) => boolean): List<T> => {
    return alist.match({
      empty: () => empty(),
      cons: (h, t) => {
        if (predicate(h) === true) {
          return cons(h, filter(t)(predicate));
        } else {
          return filter(t)(predicate);
        }
      }
    });
  };
}

/**
 * Creates a list from a string
 */
export function fromString(str: string): List<string> {
  if (str.length === 0) {
    return empty();
  } else {
    const h = str[0];
    const t = str.substring(1);
    return cons(h, fromString(t));
  }
}

// Namespace export for module compatibility
export const List = {
  type,
  match,
  empty,
  cons,
  unit,
  head,
  tail,
  isEmpty,
  isEqual,
  flatten,
  map,
  flatMap,
  append,
  concat,
  last,
  join,
  foldr,
  any,
  elem,
  fromArray,
  toArray,
  length,
  filter,
  fromString
};

export default List;
