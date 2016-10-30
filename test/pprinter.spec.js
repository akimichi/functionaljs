"use strict";

var expect = require('expect.js');
var Data = require('../lib/data.js');
var Pair = require('../lib/pair.js');
var PP = require('../lib/pprinter.js');

describe('PretterPrinter', () => {
  it("print", (next) => {
    var pair = Pair.cons(1,2);
    expect(
      PP.print(pair)
    ).to.eql(
      "(1,2)"
    );
    next();
  });
});
