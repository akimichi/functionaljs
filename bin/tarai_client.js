"use strict";

process.stdin.setEncoding('utf8');

/* #@range_begin(tarai_client) */
var net = require('net');
var socket = net.connect({
  host: '127.0.0.1',
  port: 3000
});

/* 
 taraiサーバと接続したときに起動されるconnectイベントハンドラ 
 */
socket.on('connect', () => {
  // コンソールの標準入力を読み出すためのreadableイベントハンドラ
  process.stdin.on('readable', () => {
    var chunk = process.stdin.read();
    if (chunk !== null) {
      /* 読み込んだ文字列を数値に変換する */
      var maybeInt = parseInt(chunk,10); 
      if(isNaN(maybeInt)){ 
        /* 数値でない場合、大文字に変換する */
        process.stdout.write(chunk.toUpperCase()); 
      } else { 
        /* 数値の場合、taraiサーバに計算をリクエストする */
        socket.write(maybeInt + '\r\n'); 
      };
    }
  });
});

/* 
 taraiサーバからの計算結果を受け取るdataイベントハンドラ 
 */
socket.on('data', (chunk) => { 
  process.stdout.write(chunk.toString());
});
/* #@range_end(tarai_client) */

process.stdin.on('end', () => {
  process.stdout.write('end');
});
