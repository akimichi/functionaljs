"use strict";

var expect = require('expect.js');
var Data = require('../lib/data.js');
var Pair = require('../lib/pair.js');
var List = require('../lib/list.js');
var PP = require('../lib/pprinter.js');

describe('PrettyPrinter', () => {
  it("print", (next) => {
    var pair = Pair.cons(1,2);
    expect(
      PP.print(pair)
    ).to.eql(
      "(1,2)"
    );
    expect(
      PP.print(List.cons(1, List.cons(2,List.empty())))
    ).to.eql(
      "[1,2,nil]"
    );
    expect(
      PP.print(List.cons("a", List.cons("b",List.empty())))
    ).to.eql(
      "[a,b,nil]"
    );
    expect(
      PP.print(List.empty())
    ).to.eql(
      "[]"
    );
    expect(
      PP.print(List.cons(Pair.cons(1,2), List.empty()))
    ).to.eql(
      "[(1,2),nil]"
    );
    next();
  });
});
