"use strict";

var expect = require('expect.js');
var Env = require('../lib/env.js');
var emptyEnv = Env.empty;

// ## IDモナドによる評価器のテスト
describe('IDモナドによる評価器のテスト', () => {
  // 恒等モナドをIとして読み込む
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
  describe('演算のテスト', () => {
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
      it('かけ算の評価のテスト', (next) => {
          /* multiply(1,2) */
          var multiplication = I.Exp.add(I.Exp.num(2),I.Exp.num(3));
          expect(
              I.evaluate(multiplication, emptyEnv)
              ).to.eql(
                  5 
                  );
          next();
      });
  });
  describe('関数を評価する', () => {
    it('identity関数を評価する', (next) => {
      var identity = I.Exp.app(I.Exp.lambda(I.Exp.variable("x"),
                                    I.Exp.variable("x")),
            I.Exp.num(2));
      expect(
        I.evaluate(identity, emptyEnv)
      ).to.eql(
        2
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
      next();
    });
  });
});

// ## Contモナドによる継続渡し評価器のテスト
describe('Contモナドによる評価器', () => {
  var K = require('../lib/evaluator.js').Cont;
  var identity = (any) => {
    return any;
  };
  it('数値を評価する', (next) => {
    expect(
      K.evaluate(K.Exp.num(2), emptyEnv)(identity)
    ).to.eql(
      2
    );
    next();
  });
  it('変数を評価する', (next) => {
    /* 変数xを1に対応させた環境を作る */
    var newEnv = Env.extend("x", 1, emptyEnv); 
    expect(
      K.evaluate(K.Exp.variable("x"), newEnv)(identity)
    ).to.eql(
      1
    );
    expect(
      K.evaluate(K.Exp.variable("y"), newEnv)(identity)
    ).to.be(
      undefined
    );
    next();
  });
  it('足し算の評価のテスト', (next) => {
    /* add(1,2) */
    var addition = K.Exp.add(K.Exp.num(1),K.Exp.num(2));
    expect(
      K.evaluate(addition, emptyEnv)(identity)
    ).to.eql(
      3 
    );
    next();
  });
  describe('関数を評価する', () => {
    it('identity関数を評価する', (next) => {
      var id = K.Exp.lambda(K.Exp.variable("x"),
                                  K.Exp.variable("x"));
      expect(
        K.evaluate(K.Exp.app(id,K.Exp.num(1)), emptyEnv)(identity)
      ).to.eql(
        1
      );
      next();
    });
    it('カリー化関数の評価', (next) => {
      var expression = K.Exp.app(
        K.Exp.app(
          K.Exp.lambda(K.Exp.variable("n"),
                     K.Exp.lambda(K.Exp.variable("m"),
                                K.Exp.add(
                                  K.Exp.variable("n"),K.Exp.variable("m")))),
          K.Exp.num(2)),
        K.Exp.num(3));
      expect(
        K.evaluate(expression, emptyEnv)(identity)
      ).to.eql(
        5
      );
      next();
    });
  });
});
