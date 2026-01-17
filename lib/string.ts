"use strict";

import { List, cons, empty } from './list';

/**
 * Gets the first character of a string
 */
export function head(str: string): string {
  return str[0];
}

/**
 * Gets all but the first character of a string
 */
export function tail(str: string): string {
  return str.substring(1);
}

/**
 * Checks if a string is empty
 */
export function isEmpty(str: string): boolean {
  return str.length === 0;
}

/**
 * Checks if a string is a single character
 */
export function isChar(str: string): boolean {
  return str.length === 1;
}

/**
 * Converts a string to a list of characters
 */
export function toList(str: string): List<string> {
  if (isChar(str) === true) {
    return cons(str, empty());
  } else {
    return cons(head(str), toList(tail(str)));
  }
}

// Namespace export for module compatibility
export const Text = {
  head,
  tail,
  isEmpty,
  isChar,
  toList
};

export default Text;
