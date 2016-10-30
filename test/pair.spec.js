"use strict";

var expect = require('expect.js');
var Pair = require('../lib/pair.js');
var List = require('../lib/list.js');
var Data = require('../lib/data.js');


describe('Pair', () => {
  var data = Pair.cons(1,2);
  it("match", (next) => {
    data.match({
      cons: (left, right) => {
        expect(
          left
        ).to.eql(
          1
        );
      }
    });
    next();
  });
  it("left", (next) => {
    expect(
      Pair.left(data)
    ).to.eql(
      1
    );
    next();
  });
  it("right", (next) => {
    expect(
      Pair.right(data)
    ).to.eql(
      2
    );
    next();
  });
});
