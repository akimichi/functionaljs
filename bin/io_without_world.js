"use strict";

/*
  外界を明示しないIOモナド

*/


var expect = require('expect.js');
var pair = require('./pair');

module.exports = {
  /* #@range_begin(io_monad_definition_with_world) */
  // unit:: T -> IO T
  // ただし、IO T は Nothing => T とする。 
  unit: (any) => {
    return (_) =>  {
      return any;
    };
  },
  /* #@range_begin(io_monad_definition_with_world_helper_function) */
  // done:: T -> IO T
  done: (any) => {
    var self = this;
    return self.unit(any);
  },
  // flatMap:: IO T -> (T -> IO S) -> IO S
  flatMap: (instanceA) => {
    var self = this;
    return (actionAB) => { // actionAB:: T -> IO S
      return self.unit(self.run(actionAB(self.run(instanceA))));
    };
  },
  // run:: IO A -> A
  run: (instance) => {
    return instance();
  },
  /* #@range_end(io_monad_definition_with_world_helper_function) */
  /* #@range_begin(io_actions) */
  // readFile:: STRING => IO[STRING]
  readFile: (path) => {
    return () => {
      var fs = require('fs');
      var content = fs.readFileSync(path, 'utf8');
      expect(content).to.a('string');
      return content;
    };
  },
  // println:: STRING => IO[null]
  println: (message) => {
    return () => {
      console.log(message);
      return null;
    };
  }
  /* #@range_end(io_actions) */
};
/* #@range_end(io_monad_definition_with_world) */

