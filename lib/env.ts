"use strict";

/**
 * Environment type - a function that maps variable names to values
 */
export type Environment<T> = (variable: string) => T | undefined;

/**
 * Empty environment - returns undefined for any variable
 */
export function empty<T>(_variable: string): T | undefined {
  return undefined;
}

/**
 * Look up a variable's value in an environment
 * lookupEnv:: (STRING, ENV) => M[VALUE]
 */
export function lookup<T>(identifier: string, environment: Environment<T>): T | undefined {
  return environment(identifier);
}

/**
 * Extend an environment with a new variable binding
 * extendEnv:: (STRING, VALUE, ENV) => ENV
 */
export function extend<T>(
  identifier: string,
  value: T,
  environment: Environment<T>
): Environment<T> {
  return (queryIdentifier: string): T | undefined => {
    if (identifier === queryIdentifier) {
      return value;
    } else {
      return lookup(queryIdentifier, environment);
    }
  };
}

// Namespace export for module compatibility
export const Env = {
  empty,
  lookup,
  extend
};

export default Env;
