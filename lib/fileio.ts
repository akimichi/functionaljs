"use strict";

import * as fs from 'fs';

/**
 * Read file contents synchronously
 */
export function read(path: string): string {
  return fs.readFileSync(path, 'utf8');
}

/**
 * Write content to file synchronously
 */
export function write(path: string, content: string): void {
  fs.writeFileSync(path, content);
}

// Namespace export for module compatibility
export const FileIO = {
  read,
  write
};

export default FileIO;
