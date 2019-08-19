"use strict";

var machine = (program,tape,initState, endState) => {
  /* ヘッドの位置 */
  var position = 0;
  /* 機械の状態 */
  var state = initState;
  /* 実行する命令 */
  var currentInstruction = undefined;
  /*
     以下のwhileループにて、
     現在の状態が最終状態に到達するまで命令を繰り返す
  */
  while(state != endState) {
    var cell = tape[String(position)];
    if (cell)
      currentInstruction = program[state][cell];
    else
      currentInstruction = program[state].B;
    if (!currentInstruction) {
      return false;
    } else {
      /* テープに印字する */
      tape[String(position)] = currentInstruction.write; 
      /* ヘッドを動かす */
      position += currentInstruction.move;
      /* 次の状態に移る */
      state = currentInstruction.next;
    }
  }
  return tape;
};

module.exports = machine;
