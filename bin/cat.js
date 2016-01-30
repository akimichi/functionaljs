"use strict";

var IO = {
  // unit:: a -> IO a
  unit: (any) => {
    return (_) =>  {
      return any;
    };
  },
  // done:: T -> IO T
  done: (any) => {
    return IO.unit();
  },
  // flatMap:: IO a -> (a -> IO b) -> IO b
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return IO.unit(IO.run(actionAB(IO.run(instanceA))));
    };
  },
  // run:: IO A -> A
  run: (instance) => {
    return instance();
  },
  readFile: (path) => {
    return (io) => {
      var fs = require('fs');
      var content = fs.readFileSync(path, 'utf8');
      return content;
    };
  },
  println: (message) => {
    return (io) => {
      console.log(message);
      return null;
    };
  }
};


 
/* #@range_begin(io_monad_combined) */
var initialWorld = null;
var path = process.argv[2];

IO.flatMap(IO.readFile(path))((content) => {
  return IO.flatMap(IO.println(content))((_) => {
    return IO.done(_);
  });
})(initialWorld);
/* #@range_end(io_monad_combined) */
