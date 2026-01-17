"use strict";

// 第3章 心の準備
// ========

import * as assert from 'assert';

// ## 3.1 <section id='DRY-principle'>DRY原則</section>
describe('DRY原則', () => {
  const add = (x: number, y: number) => {
    return x + y;
  };

  // **リスト3.1** 冗長なコード
  it('冗長なコード', () => {
    const timesForMultiply = (count: number, arg: number, memo: number): number => {
      if (count > 1) {
        return timesForMultiply(count - 1, arg, arg + memo);
      } else {
        return arg + memo;
      }
    };
    const multiply = (n: number, m: number) => {
      return timesForMultiply(n, m, 0);
    };
    const timesForExponential = (count: number, arg: number, memo: number): number => {
      if (count > 1) {
        return timesForExponential(count - 1, arg, arg * memo);
      } else {
        return arg * memo;
      }
    };
    const exponential = (n: number, m: number) => {
      return timesForExponential(m, n, 1);
    };
    expect(
      multiply(2, 3)
    ).toEqual(
      6
    );
    expect(
      exponential(2, 3)
    ).toEqual(
      8
    );
  });

  it('DRYを適用する', () => {
    // **リスト3.3** DRYなtimes関数
    const times = (count: number, arg: number, memo: number, fun: (a: number, b: number) => number): number => { // 引数funを追加
      if (count > 1) {
        return times(count - 1, arg, fun(arg, memo), fun);
      } else {
        return fun(arg, memo);
      }
    };

    // **リスト3.4** DRYなかけ算とべき乗
    const add = (n: number, m: number) => {
      return n + m;
    };
    /* times関数を利用してmultiply関数を定義する */
    const multiply = (n: number, m: number) => {
      return times(m, n, 0, add);
    };
    /* times関数を利用してexponential関数を定義する */
    const exponential = (n: number, m: number) => {
      return times(m, n, 1, multiply);
    };
    expect(
      multiply(2, 3)
    ).toEqual(
      6
    );
    expect(
      exponential(2, 3)
    ).toEqual(
      8
    );
    expect(
      multiply(-2, 3)
    ).toEqual(
      -6
    );
  });
});

// ## 3.2 <section id='abstraction-oriented'>抽象化への指向</section>
describe('抽象化への指向', () => {
  // **リスト3.5** 関数という抽象化
  it('関数という抽象化', () => {
    const succ = (n: number) => {
      return n + 1;
    };
  });

  describe('高階関数による抽象化', () => {
    const anArray = [2, 3, 5, 7, 11, 13];

    // **リスト3.6** for文によるsum関数
    it('for文によるsum関数', () => {
      const anArray = [2, 3, 5, 7];
      const sum = (array: number[]) => {
        let result = 0;
        for (let index = 0; index < array.length; index++) {
          result = result + array[index];
        }
        return result;
      };
      sum(anArray);
      expect(
        sum(anArray)
      ).toEqual(
        17
      );
    });

    // **リスト3.7** forEachによるsum関数
    it('forEachによるsum関数', () => {
      const sum = (array: number[]) => {
        /* 結果を格納する変数result */
        let result = 0;
        array.forEach((item) => {
          result = result + item;
        });
        return result;
      };
      expect(
        sum(anArray)
      ).toEqual(
        41
      );
    });

    // **リスト3.8** reduceによるsum関数
    it('reduceによるsum関数', () => {
      const sum = (array: number[]) => {
        return array.reduce((x, y) => {
          return x + y;
        });
      };
      expect(
        sum(anArray)
      ).toEqual(
        41
      );
    });
  });
});

// ## 3.3 <section id='semantics-conscious'>セマンティクスを意識する</section>
describe('セマンティクスを意識する', () => {
  // **リスト3.9** 環境という仕組み
  it('環境という仕組み', () => {
    /* merge関数は、引数にわたされた2つのオブジェクトを併合する */
    const merge = <T extends object, U extends object>(obj1: T, obj2: U): T & U => {
      const mergedObject: any = {};
      for (const attrname in obj1) { mergedObject[attrname] = obj1[attrname]; }
      for (const attrname in obj2) { mergedObject[attrname] = obj2[attrname]; }
      return mergedObject;
    };
    /* 空の環境 */
    const emptyEnv: { [key: string]: any } = {};
    /* 環境を拡張する */
    const extendEnv = (binding: { [key: string]: any }, oldEnv: { [key: string]: any }) => {
      /* merge(obj1, obj2) は
         obj1とobj2のオブジェクトをマージする関数のこと */
      return merge(binding, oldEnv);
    };
    /* 変数名に対応する値を環境から取り出す */
    const lookupEnv = (name: string, env: { [key: string]: any }) => {
      return env[name];
    };
    expect((() => {
      // **リスト3.11** リスト 3.10のセマンティクス
      /* 空の辞書を作成する */
      const initEnv = emptyEnv;
      /* var a = 1 を実行して、辞書を拡張する */
      const firstEnv = extendEnv({ "a": 1 }, initEnv);
      /* var b = 3 を実行して、辞書を拡張する */
      const secondEnv = extendEnv({ "b": 3 }, firstEnv);
      /* 辞書から b の値を参照する */
      lookupEnv("b", secondEnv);
      return lookupEnv("b", secondEnv);
    })()).toEqual(
      3
    );
  });
});

// ## 3.4 <section id='test-driven'>テストに親しむ</section>
describe('テストに親しむ', () => {
  // ### 単体テストの仕組み
  describe('単体テストの仕組み', () => {
    // **リスト3.12** アサート関数の例
    //
    // assertライブラリを使う場合
    it('assertによる表明', () => {
      assert.equal(1 + 2, 3);
    });

    // expectライブラリを使う場合
    it('expectによる表明', () => {
      expect(
        1 + 2
      ).toEqual(
        3
      );
    });
  });
});

// [目次に戻る](index.html) [次章に移る](chap04.spec.html)
