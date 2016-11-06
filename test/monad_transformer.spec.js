"use strict";


var fs = require('fs');
var expect = require('expect.js');
// Pair型の読込
var Pair = require('../lib/pair.js');
// String型の読込
var String = require('../lib/string.js');

var Maybe = require('../lib/monad.js').Maybe;
var ID = require('../lib/monad.js').ID;
var MaybeT = require('../lib/monad_transformer.js').MaybeT;

// ### MaybeTモナドのテスト
describe("MaybeTモナドをテストする",() => {
  it("MaybeT#unitをテストする", (next) => {
    Maybe.match(MaybeT.unit(1)(ID),{
      nothing: (_) => {
        return expect.fail();
      },
      just: (v) => {
        expect(
          v
        ).to.eql(
          1
        );
      }
    });
    next();
  });
  it("MaybeT#flatMapをテストする", (next) => {
    var instanceM = MaybeT.unit(1)(ID);
    var identity = (x) => {
      return x;
    };
    var double = (x) => {
      return 2 * x;
    };
    expect(
      MaybeT.flatMap(instanceM)(double)(ID)
    ).to.eql(
      2
    );
    next();
  });
});
