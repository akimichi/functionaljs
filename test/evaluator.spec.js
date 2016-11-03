"use strict";

var expect = require('expect.js');
var Env = require('../lib/env.js');
var emptyEnv = Env.empty;

// ## IDモナドによる評価器
describe('IDモナドによる評価器', () => {
  var I = require('../lib/evaluator.js').ID;
  it('数値を評価する', (next) => {
    expect(
      I.evaluate(I.Exp.num(2), emptyEnv) 
    ).to.eql(
      2
    );
    next();
  });
});
