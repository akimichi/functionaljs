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
  it('変数の評価のテスト', (next) => {
    /* 変数xを1に対応させた環境を作る */
    var newEnv = Env.extend("x", 1, emptyEnv); 
    expect(
      I.evaluate(I.Exp.variable("x"), newEnv)
    ).to.eql(
      1
    );
    expect(
      I.evaluate(I.Exp.variable("y"), newEnv)
    ).to.be(
      undefined
    );
    next();
  });
  it('足し算の評価のテスト', (next) => {
    /* add(1,2) */
    var addition = I.Exp.add(I.Exp.num(1),I.Exp.num(2));
    expect(
      I.evaluate(addition, emptyEnv)
    ).to.eql(
      3 
    );
    next();
  });
  describe('関数を評価する', () => {
    it('identity関数を評価する', (next) => {
      var identity = I.Exp.lambda(I.Exp.variable("x"),
                                  I.Exp.variable("x"));
      expect(
        I.evaluate(identity, emptyEnv)(1)
      ).to.eql(
        1
      );
      next();
    });
    it('カリー化関数の評価', (next) => {
      // ~~~js
      // ((n) => {
      //    return (m) => {
      //       return n + m;
      //    };
      // })(2)(3)
      // ~~~
      var expression = I.Exp.app(
        I.Exp.app(
          I.Exp.lambda(I.Exp.variable("n"),
                     I.Exp.lambda(I.Exp.variable("m"),
                                I.Exp.add(
                                  I.Exp.variable("n"),I.Exp.variable("m")))),
          I.Exp.num(2)),
        I.Exp.num(3));
      expect(
        I.evaluate(expression, emptyEnv)
      ).to.eql(
        5
      );
      /* #@range_end(curried_function_evaluation_test) */
      next();
    });
  });
});
