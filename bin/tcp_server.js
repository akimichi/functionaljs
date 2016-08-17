/* #@range_begin(tcp_server) */
"use strict";

var net = require('net');
var localhost = '127.0.0.1';

// サーバインスタンスを生成
net.createServer((socket) => {
  // 'data' イベントハンドラ
  socket.on('data', (incomingData) => {
    socket.write(incomingData);
  });
  // 'close'イベントハンドラ
  socket.on('close', (error) => {
    console.log("connection closed");
  });
}).listen(3000, localhost);
/* #@range_end(tcp_server) */
