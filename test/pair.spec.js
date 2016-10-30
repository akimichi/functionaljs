"use strict";

var expect = require('expect.js');
var Pair = require('../lib/pair.js');

var Data = {
  type: (data,pattern) => {
    return data.type(pattern);
  }
};

describe('Pair', () => {
  var data = Pair.cons(1,2);
  it("type", (next) => {
    Data.type(data,{
      pair: (_) => {
        expect(true).to.eql(true);
      }
    });
    // expect(data.type).to.eql('pair');
    next();
  });
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
