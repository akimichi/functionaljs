"use strict";

// Type patterns interface for primitive types
export interface PrimitiveTypePatterns<R> {
  number?: (n: number) => R;
  string?: (s: string) => R;
  boolean?: (b: boolean) => R;
  function?: (f: Function) => R;
  object?: (o: object) => R;
  undefined?: (u: undefined) => R;
  [key: string]: ((value: any) => R) | undefined;
}

// Interface for data that has a type method (Church-encoded data)
export interface Typeable<P, R> {
  type: (pattern: P) => R;
}

// Interface for data that has a match method (Church-encoded data)
export interface Matchable<P, R> {
  match: (pattern: P) => R;
}

/**
 * Determines the type of data and calls the appropriate pattern handler.
 * For primitive types, uses typeof to determine the type.
 * For Church-encoded data, calls the data's type method.
 */
export function type<P extends PrimitiveTypePatterns<R>, R>(
  data: Typeable<P, R> | number | string | boolean | Function | object | undefined,
  pattern: P
): R {
  if (typeof data === 'object' && data !== null && 'type' in data && typeof (data as any).type === 'function') {
    return (data as Typeable<P, R>).type(pattern);
  } else {
    const typeOf = typeof data as keyof PrimitiveTypePatterns<R>;
    const handler = pattern[typeOf];
    if (handler) {
      return handler(data as any);
    }
    throw new Error(`No handler for type: ${typeOf}`);
  }
}

/**
 * Pattern matches on Church-encoded data by calling its match method.
 */
export function match<P, R>(data: Matchable<P, R>, pattern: P): R {
  return data.match(pattern);
}

// CommonJS compatibility export
export default {
  type,
  match
};
