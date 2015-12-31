"use strict";

// unit:: a -> IO a
var unit = function(any) {
  return function(_) {
    return any;
  };
};

// flatMap:: IO a -> (a -> IO b) -> IO b
var flatMap = function(instanceA) {
  return function(actionAB) { // actionAB:: a -> IO b
    return unit(run(actionAB(run(instanceA))));
  };
};

// run:: IO A -> A
var run = function(instance) {
  return instance();
};

var readFile = function(path) {
  return function(io) {
    var fs = require('fs');
    var content = fs.readFileSync(path, 'utf8');
    return content;
  };
};

var writeFile = function(content) {
  return function(io) {
    console.log(content);
    return null;
  };
};


var println = function(message) {
  return function(io) {
    console.log(message);
    return null;
  };
};
var readln = function(io) {
  var fs = require('fs');
  var length = 80;
  var buffer = new Buffer(length);
  fs.readSync(process.stdin.fd, buffer, 0, length)
  var line = buffer.split(/\r?\n/);
  return line;
  // var lines = [];
  // var readline = require('readline');
  // var reader = require('readline').createInterface({
  //   input: process.stdin,
  //   output: process.stdout
  // });
  // reader.on('line', function (line) {
  //   lines.push(line);
  // });
  // process.stdin.on('end', function () {
  //   //do something
  // });
  // var input = '';
  // process.stdin.resume();
  // process.stdin.setEncoding('utf8');
  // // 標準入力がくると発生するイベント
  // process.stdin.on('data', function (chunk) {
    
  // });
  // // EOFがくると発生するイベント
  // process.stdin.on('end', function () {
};

run(println("test"));
