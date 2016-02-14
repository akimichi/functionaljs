/* #@range_begin(tarai_server) */
"use strict";

var net = require('net');
var localhost = '127.0.0.1';

/* tarai関数 */
var tarai = (x,y,z) => {
  if(x > y) {
    return tarai(tarai(x - 1, y, z), tarai(y - 1, z, x), tarai(z - 1, x, y));
  } else {
    return y;
  }
};

net.createServer((socket) => {
  // 'data' イベントハンドラー
  socket.on('data', (incommingData) => {
    var number = parseInt(incommingData,10); // クライアントからデータを数値に変換します
    console.log(number);
    socket.write(tarai(number * 2, number, 0) + '\r\n'); // tarai関数を計算して、クライアントに返します
  });
  // 'close'イベントハンドラー
  socket.on('close', (error) => {
    console.log("connection closed");
  });
}).listen(3000, localhost);
/* #@range_end(tarai_server) */

var sleep = (sec) => {
  var until = new Date().getTime() + (sec * 1000);
  while (new Date().getTime() <= until) {
    ;
  }
};

    // if(isNaN(maybeInt)){ // データが数値でなければ 0 を返します
    //   socket.write('0\r\n');
    // } else { // データが数値ならば、 tarai(maybeInt * 2, maybeInt, 0) を返します
    //   socket.write(tarai(maybeInt * 2, maybeInt, 0) + '\r\n');
    // };
