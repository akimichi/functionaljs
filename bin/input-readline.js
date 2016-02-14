// ストリームでユーザー入力を制御する

var readline = require('readline');
var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

rl.question("何か入力してください ", function(answer) {
  // TODO: Log the answer in a database
  console.log("あなたの入力: ", answer);
  
  rl.close();
});
