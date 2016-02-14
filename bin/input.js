"use strict";

var net = require('net');
var socket = net.connect({
  host: '127.0.0.1',
  port: 3000
});


process.stdin.setEncoding('utf8');

/* #@range_begin(input_js) */
socket.on('connect', () => {
  process.stdin.on('readable', () => {
    var chunk = process.stdin.read();
    if (chunk !== null) {
      var maybeInt = parseInt(chunk,10); // 読み込んだ文字列を数値に変換する
      if(isNaN(maybeInt)){ // 数値でない場合、大文字に変換する
        // process.stdout.write(chunk);
        process.stdout.write(chunk.toUpperCase());
      } else { // taraiサーバーにリクエストする
        socket.write(maybeInt + '\r\n');
      };
    }
  });
});
socket.on('data', (chunk) => { // taraiサーバーからの返信イベントハンドラ
  process.stdout.write(chunk.toString());
});
/* #@range_end(input_js) */

process.stdin.on('end', () => {
  process.stdout.write('end');
});


// var tarai = (x,y,z) => {
//   if(x > y) {
//     return tarai(tarai(x - 1, y, z), tarai(y - 1, z, x), tarai(z - 1, x, y));
//   } else {
//     return y;
//   }
// };
// process.stdin.resume();
// process.stdin.setEncoding('utf8');

// process.stdin.on('data', (chunk) => {
//   var maybeInt = parseInt(chunk,10);
//   if(isNaN(maybeInt)){
//     process.stdout.write('string: ' + chunk.toUpperCase());
//     process.stdout.write('\n');
//   } else {
//     process.stdout.write('number: ' + tarai(maybeInt * 2, maybeInt, 0));
//     process.stdout.write('\n');
//   }

// });

// process.stdin.on('end',  () => {
//  process.stdout.write('end');
// });
