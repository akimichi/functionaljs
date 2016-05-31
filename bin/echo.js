/* #@range_begin(stdin_stream) */
"use strict";

process.stdin.resume();
process.stdin.setEncoding('utf8');

var fragment = "";

var prompt = (_) => {  
  process.stdout.write("> ");
};

var mkLines = (chunk) => {
  var lines = chunk.split("\n");
  lines[0] = fragment + lines[0];
  fragment = lines.pop();
  return lines;
};

prompt();

process.stdin.on('data', (chunk) => {
  mkLines(chunk).forEach((line) => {
    console.log(line);
  });
  prompt();
});
 
// ストリーム終了時に呼ばれる.
process.stdin.on('end', function(){
  console.log("EXIT");
});
/* #@range_end(stdin_stream) */
