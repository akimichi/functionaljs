/* #@range_begin(tcp_server) */
"use strict";

var net = require('net');
var localhost = '127.0.0.1';

// サーバーインスタンスを生成
net.createServer((socket) => {
  // 'data' イベントハンドラー
  socket.on('data', (incommingData) => {
    socket.write(incommingData);
  });
  // 'close'イベントハンドラー
  socket.on('close', (error) => {
    console.log("connection closed");
  });
}).listen(3000, localhost);
/* #@range_end(tcp_server) */
