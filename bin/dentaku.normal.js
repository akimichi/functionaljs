"use strict";

var Dentaku = require('../lib/dentaku.normal.js');
var readline = require('readline');
var prompt = "input> ";

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

rl.on('line', (input) => {
    console.log(Dentaku.evaluate(input));
    rl.setPrompt(prompt, prompt.length);
    rl.prompt();
});

rl.setPrompt(prompt, prompt.length);
rl.prompt();

// rl.question("dentaku> ", (input) => {
//   // TODO: Log the answer in a database
//   console.log(Dentaku.evaluate(input));
//   rl.close();
// });
