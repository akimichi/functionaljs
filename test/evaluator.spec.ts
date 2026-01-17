"use strict";

import { empty as emptyEnv, extend } from '../lib/env';
import { IDEvaluator as I, ContEvaluator as K } from '../lib/evaluator';

describe('IDモナドによる評価器のテスト', () => {
  it('数値を評価する', () => {
    expect(I.evaluate(I.Exp.num(2), emptyEnv)).toEqual(2);
  });

  it('変数の評価のテスト', () => {
    const newEnv = extend("x", 1, emptyEnv);
    expect(I.evaluate(I.Exp.variable("x"), newEnv)).toEqual(1);
    expect(I.evaluate(I.Exp.variable("y"), newEnv)).toBe(undefined);
  });

  describe('演算のテスト', () => {
    it('足し算の評価のテスト', () => {
      const addition = I.Exp.add(I.Exp.num(1), I.Exp.num(2));
      expect(I.evaluate(addition, emptyEnv)).toEqual(3);
    });

    it('かけ算の評価のテスト', () => {
      const multiplication = I.Exp.add(I.Exp.num(2), I.Exp.num(3));
      expect(I.evaluate(multiplication, emptyEnv)).toEqual(5);
    });
  });

  describe('関数を評価する', () => {
    it('identity関数を評価する', () => {
      const identity = I.Exp.app(
        I.Exp.lambda(I.Exp.variable("x"), I.Exp.variable("x")),
        I.Exp.num(2)
      );
      expect(I.evaluate(identity, emptyEnv)).toEqual(2);
    });

    it('カリー化関数の評価', () => {
      const expression = I.Exp.app(
        I.Exp.app(
          I.Exp.lambda(
            I.Exp.variable("n"),
            I.Exp.lambda(
              I.Exp.variable("m"),
              I.Exp.add(I.Exp.variable("n"), I.Exp.variable("m"))
            )
          ),
          I.Exp.num(2)
        ),
        I.Exp.num(3)
      );
      expect(I.evaluate(expression, emptyEnv)).toEqual(5);
    });
  });
});

describe('Contモナドによる評価器', () => {
  const identity = <T>(any: T): T => any;

  it('数値を評価する', () => {
    expect(K.evaluate(K.Exp.num(2), emptyEnv)(identity)).toEqual(2);
  });

  it('変数を評価する', () => {
    const newEnv = extend("x", 1, emptyEnv);
    expect(K.evaluate(K.Exp.variable("x"), newEnv)(identity)).toEqual(1);
    expect(K.evaluate(K.Exp.variable("y"), newEnv)(identity)).toBe(undefined);
  });

  it('足し算の評価のテスト', () => {
    const addition = K.Exp.add(K.Exp.num(1), K.Exp.num(2));
    expect(K.evaluate(addition, emptyEnv)(identity)).toEqual(3);
  });

  describe('関数を評価する', () => {
    it('identity関数を評価する', () => {
      const id = K.Exp.lambda(K.Exp.variable("x"), K.Exp.variable("x"));
      expect(K.evaluate(K.Exp.app(id, K.Exp.num(1)), emptyEnv)(identity)).toEqual(1);
    });

    it('カリー化関数の評価', () => {
      const expression = K.Exp.app(
        K.Exp.app(
          K.Exp.lambda(
            K.Exp.variable("n"),
            K.Exp.lambda(
              K.Exp.variable("m"),
              K.Exp.add(K.Exp.variable("n"), K.Exp.variable("m"))
            )
          ),
          K.Exp.num(2)
        ),
        K.Exp.num(3)
      );
      expect(K.evaluate(expression, emptyEnv)(identity)).toEqual(5);
    });
  });
});
