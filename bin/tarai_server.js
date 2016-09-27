"use strict";

/* tarai関数 */
var tarai = (x,y,z) => {
  if(x > y) {
    return tarai(tarai(x - 1, y, z), 
                 tarai(y - 1, z, x), 
                 tarai(z - 1, x, y));
  } else {
    return y;
  }
};
/* #@range_begin(tarai_server) */
var net = require('net');
var localhost = '127.0.0.1';

net.createServer((socket) => {
  /* dataイベントハンドラ */
  socket.on('data', (incomingData) => {
    /* クライアントからデータを数値に変換する */
    var number = parseInt(incomingData,10); 
    console.log(number);
    /* tarai関数を計算して、クライアントに返す */
    socket.write(tarai(number * 2, number, 0) + '\r\n'); 
  });
  /* closeイベントハンドラ */
  socket.on('close', (error) => {
    console.log("connection closed");
  });
}).listen(3000, localhost);
/* #@range_end(tarai_server) */

