"use strict";

// Turing machine instruction
export interface Instruction {
  write: string;
  move: number;
  next: string;
}

// State transition function
export interface StateTransitions {
  [symbol: string]: Instruction;
}

// Program is a mapping from states to transitions
export interface Program {
  [state: string]: StateTransitions;
}

// Tape is a sparse array indexed by position
export interface Tape {
  [position: string]: string;
}

/**
 * Turing Machine simulator
 *
 * @param program - The program (state transition table)
 * @param tape - The initial tape contents
 * @param initState - The initial state
 * @param endState - The halting state
 * @returns The final tape contents, or false if the machine fails
 */
export function machine(
  program: Program,
  tape: Tape,
  initState: string,
  endState: string
): Tape | false {
  // Head position
  let position = 0;
  // Machine state
  let state = initState;
  // Current instruction
  let currentInstruction: Instruction | undefined;

  // Execute until reaching the end state
  while (state !== endState) {
    const cell = tape[String(position)];
    if (cell) {
      currentInstruction = program[state]?.[cell];
    } else {
      currentInstruction = program[state]?.B;
    }

    if (!currentInstruction) {
      return false;
    } else {
      // Write to tape
      tape[String(position)] = currentInstruction.write;
      // Move head
      position += currentInstruction.move;
      // Transition to next state
      state = currentInstruction.next;
    }
  }

  return tape;
}

export default machine;
