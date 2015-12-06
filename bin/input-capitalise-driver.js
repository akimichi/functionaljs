"use strict";

// ストリームでユーザー入力を制御する

var expect = require('expect.js');
var readline = require('readline');

var input = require('./input-capitalise.js');

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

var question = rl.question("英単語を入力してください ", (answer) => {
  // TODO: Log the answer in a database
  var capitalise = (ch) => {
    return ch.toUpperCase();
  };
  console.log("あなたの入力: ", input.stream.toString.call(input,
                                                           input.stream.map.call(input,
                                                                                 input.stream.fromString(answer))(capitalise)));
  
  rl.close();
});
