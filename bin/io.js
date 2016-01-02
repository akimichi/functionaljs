"use strict";

// unit:: a -> IO a
var unit = (any) => {
  return (_) =>  {
    return any;
  };
};

// done:: T -> IO T
var done = (any) => {
  return unit();
};

// flatMap:: IO a -> (a -> IO b) -> IO b
var flatMap = (instanceA) => {
  return (actionAB) => { // actionAB:: a -> IO b
    return unit(run(actionAB(run(instanceA))));
  };
};

// run:: IO A -> A
var run = (instance) => {
  return instance();
};

var readFile = (path) => {
  return (io) => {
    var fs = require('fs');
    var content = fs.readFileSync(path, 'utf8');
    return content;
  };
};

var writeFile = (content) => {
  return (io) => {
    console.log(content);
    return null;
  };
};


var println = (message) => {
  return (io) => {
    console.log(message);
    return null;
  };
};

// var readln = function(io) {
//   var fs = require('fs');
//   var length = 80;
//   var buffer = new Buffer(length);
//   fs.readSync(process.stdin.fd, buffer, 0, length)
//   var line = buffer.split(/\r?\n/);
//   return line;
// };

run(println("test"));
run(readFile("./io.js"));

flatMap(readFile("./io.js"))((content) => {
  return flatMap(println(content))((_) => {
    return done();
  });
});
