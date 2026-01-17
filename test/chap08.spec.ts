"use strict";

// 第8章 関数型言語を作る
// ========

// Pair型の定義
type Pair<L, R> = (pattern: PairPattern<L, R>) => any;

interface PairPattern<L, R> {
  cons: (left: L, right: R) => any;
}

// **pairモジュール**
const pair = {
  match: <L, R, T>(data: Pair<L, R>, pattern: { cons: (left: L, right: R) => T }): T => {
    return data.call(pair, pattern);
  },
  cons: <L, R>(left: L, right: R): Pair<L, R> => {
    return (pattern: PairPattern<L, R>) => {
      return pattern.cons(left, right);
    };
  },
  right: <L, R>(tuple: Pair<L, R>): R => {
    return pair.match(tuple, {
      cons: (left: L, right: R) => {
        return right;
      }
    });
  },
  left: <L, R>(tuple: Pair<L, R>): L => {
    return pair.match(tuple, {
      cons: (left: L, right: R) => {
        return left;
      }
    });
  }
};

// リスト型の定義
type List<T> = (pattern: ListPattern<T>) => any;

interface ListPattern<T> {
  empty: () => any;
  cons: (head: T, tail: List<T>) => any;
}

// **listモジュール**
const list = {
  match: <T, R>(data: List<T>, pattern: { empty: () => R; cons: (head: T, tail: List<T>) => R }): R => {
    return data.call(list, pattern);
  },
  empty: <T>(): List<T> => {
    return (pattern: ListPattern<T>) => {
      return pattern.empty();
    };
  },
  cons: <T>(head: T, tail: List<T>): List<T> => {
    return (pattern: ListPattern<T>) => {
      return pattern.cons(head, tail);
    };
  },
  head: <T>(alist: List<T>): T | undefined => {
    return list.match(alist, {
      empty: () => {
        return undefined;
      },
      cons: (head: T, tail: List<T>) => {
        return head;
      }
    });
  },
  tail: <T>(alist: List<T>): List<T> | undefined => {
    return list.match(alist, {
      empty: () => {
        return undefined;
      },
      cons: (head: T, tail: List<T>) => {
        return tail;
      }
    });
  },
  /* append:: LIST[T] -> LIST[T] -> LIST[T] */
  append: <T>(xs: List<T>) => {
    return (ys: List<T>): List<T> => {
      return list.match(xs, {
        empty: () => {
          return ys;
        },
        cons: (head: T, tail: List<T>) => {
          return list.cons(head, list.append(tail)(ys));
        }
      });
    };
  },
  /* foldr:: LIST[T] -> T -> FUN[T -> LIST] -> T */
  foldr: <T, R>(alist: List<T>) => {
    return (accumulator: R) => {
      return (glue: (item: T) => (acc: R) => R): R => {
        return list.match<T, R>(alist, {
          empty: () => {
            return accumulator;
          },
          cons: (head: T, tail: List<T>) => {
            return glue(head)(list.foldr<T, R>(tail)(accumulator)(glue));
          }
        });
      };
    };
  },
  toArray: <T>(alist: List<T>): T[] => {
    return list.foldr<T, T[]>(alist)([])((item: T) => {
      return (accumulator: T[]): T[] => {
        return [item].concat(accumulator);
      };
    });
  }
};

// ## 8.2 <section id='abstract-syntax-tree'>抽象構文木を作る</section>
describe('抽象構文木を作る', () => {
  // **リスト8.2** 式の代数的データ構造
  describe('式の代数的データ構造', () => {
    // 式の型定義
    type Exp = (pattern: ExpPattern) => any;

    interface ExpPattern {
      num: (value: number) => any;
      variable: (name: string) => any;
      lambda: (variable: Exp, body: Exp) => any;
      app: (lambda: Exp, arg: Exp) => any;
      add: (expL: Exp, expR: Exp) => any;
    }

    const exp = {
      /* 式のパターンマッチ関数 */
      match: (data: Exp, pattern: ExpPattern): any => {
        return data(pattern);
      },
      /* 数値の式 */
      num: (value: number): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.num(value);
        };
      },
      /* 変数の式 */
      variable: (name: string): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.variable(name);
        };
      },
      /* 関数定義の式(λ式) */
      lambda: (variable: Exp, body: Exp): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.lambda(variable, body);
        };
      },
      /* 関数適用の式 */
      app: (lambda: Exp, arg: Exp): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.app(lambda, arg);
        };
      },
      // **リスト8.3** 演算の定義
      /* 足し算の式 */
      add: (expL: Exp, expR: Exp): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.add(expL, expR);
        };
      }
    };

    describe('式をテストする', () => {
      it("\\x.\\y.x", () => {
        /* λx.λy.x */
        exp.match(exp.lambda(exp.variable("x"), exp.lambda(exp.variable("y"), exp.variable("x"))), {
          lambda: (variable: Exp, arg: Exp) => {
            expect(
              typeof variable
            ).toBe('function');
          },
          num: () => { },
          variable: () => { },
          app: () => { },
          add: () => { }
        });
      });
    });
  });
});

// ## 8.3 <section id='environment'>環境を作る</section>
describe('環境を作る', () => {
  // **リスト8.5** クロージャーによる「環境」の定義
  type Env = (variable: string) => any;

  const env = {
    /* 空の環境を作る */
    /* empty:: STRING => VALUE */
    empty: (variable: string): undefined => {
      return undefined;
    },
    /* 変数名に対応する値を環境から取り出す */
    /* lookup:: (STRING, ENV) => VALUE */
    lookup: (name: string, environment: Env): any => {
      return environment(name);
    },
    /* 環境を拡張する */
    /* extend:: (STRING, VALUE, ENV) => ENV */
    extend: (identifier: string, value: any, environment: Env): Env => {
      return (queryIdentifier: string): any => {
        if (identifier === queryIdentifier) {
          return value;
        } else {
          return env.lookup(queryIdentifier, environment);
        }
      };
    }
  };

  // **リスト8.7** 変数バインディングにおける環境のセマンティクス
  it('変数バインディングにおける環境のセマンティクス', () => {
    expect((() => {
      /* 空の環境からnewEnv環境を作る */
      const newEnv = env.extend("a", 1, env.empty);
      /* newEnv環境を利用して a の値を求める */
      return env.lookup("a", newEnv);
    })()).toEqual(
      1
    );
    expect((() => {
      const initEnv = env.empty;                      // 空の辞書を作成する
      /* var a = 1 を実行して、辞書を拡張する */
      const firstEnv = env.extend("a", 1, initEnv);
      /* var b = 3 を実行して、辞書を拡張する */
      const secondEnv = env.extend("b", 3, firstEnv);
      /* 辞書から b の値を参照する */
      return env.lookup("b", secondEnv);
    })()).toEqual(
      3
    );

    // **リスト8.9** クロージャーにおける環境のセマンティクス
    expect((() => {
      /* 空の辞書を作成する */
      const initEnv = env.empty;
      /* 空の辞書から outerEnv環境を作る */
      const outerEnv = env.extend("x", 1, initEnv);

      /* closureEnv環境を作る */
      const closureEnv = env.extend("y", 2, outerEnv);
      /* closureEnv環境を利用してx+yを計算する */
      return env.lookup("x", closureEnv) + env.lookup("y", closureEnv);
    })()).toEqual(
      3
    );
  });
});

// ## 8.4 <section id='evaluator'>評価器を作る</section>
describe('評価器を作る', () => {
  /* 「環境」モジュール */
  type Env = (variable: string) => any;

  const env = {
    empty: (variable: string): undefined => {
      return undefined;
    },
    lookup: (name: string, environment: Env): any => {
      return environment(name);
    },
    extend: (identifier: string, value: any, environment: Env): Env => {
      return (queryIdentifier: string): any => {
        if (identifier === queryIdentifier) {
          return value;
        } else {
          return env.lookup(queryIdentifier, environment);
        }
      };
    }
  };
  const emptyEnv = env.empty;

  // ### <section id='identity-monad-evaluator'>恒等モナドによる評価器</section>
  describe('恒等モナドによる評価器', () => {
    // 式の型定義
    type Exp = (pattern: ExpPattern) => any;

    interface ExpPattern {
      num: (value: number) => any;
      variable: (name: string) => any;
      lambda: (variable: Exp, body: Exp) => any;
      app: (lambda: Exp, arg: Exp) => any;
      add: (expL: Exp, expR: Exp) => any;
    }

    const exp = {
      match: (data: Exp, pattern: ExpPattern): any => { // 式のパターンマッチ関数
        return data(pattern);
      },
      num: (value: number): Exp => {             // 数値の式
        return (pattern: ExpPattern) => {
          return pattern.num(value);
        };
      },
      variable: (name: string): Exp => {        // 変数の式
        return (pattern: ExpPattern) => {
          return pattern.variable(name);
        };
      },
      lambda: (variable: Exp, body: Exp): Exp => { // 関数定義の式(λ式)
        return (pattern: ExpPattern) => {
          return pattern.lambda(variable, body);
        };
      },
      app: (lambda: Exp, arg: Exp): Exp => {       // 関数適用の式
        return (pattern: ExpPattern) => {
          return pattern.app(lambda, arg);
        };
      },
      add: (expL: Exp, expR: Exp): Exp => {        // 足し算の式
        return (pattern: ExpPattern) => {
          return pattern.add(expL, expR);
        };
      }
    };

    // ### 恒等モナド
    const ID = {
      unit: <T>(value: T): T => {
        return value;
      },
      flatMap: <T, U>(instance: T) => {
        return (transform: (x: T) => U): U => {
          return transform(instance);
        };
      }
    };

    // **リスト8.10** 恒等モナド評価器の定義
    /* evaluate:: (EXP, ENV) => ID[VALUE] */
    const evaluate = (anExp: Exp, environment: Env): any => {
      return exp.match(anExp, {
        // **リスト8.11** 数値の評価
        num: (numericValue: number) => {
          return ID.unit(numericValue);
        },
        // **リスト8.13** 変数の評価
        variable: (name: string) => {
          return ID.unit(env.lookup(name, environment));
        },
        /* 関数定義（λ式）の評価  */
        lambda: (variable: Exp, body: Exp) => {
          return exp.match(variable, {
            variable: (name: string) => {
              return ID.unit((actualArg: any) => {
                return evaluate(body, env.extend(name, actualArg, environment));
              });
            },
            num: () => { },
            lambda: () => { },
            app: () => { },
            add: () => { }
          });
        },
        /* 関数適用の評価 */
        app: (lambda: Exp, arg: Exp) => {
          return ID.flatMap(evaluate(lambda, environment))((closure: (arg: any) => any) => {
            return ID.flatMap(evaluate(arg, environment))((actualArg: any) => {
              return closure(actualArg);
            });
          });
        },
        // **リスト8.15**  足し算の評価
        add: (expL: Exp, expR: Exp) => {
          return ID.flatMap(evaluate(expL, environment))((valueL: number) => {
            return ID.flatMap(evaluate(expR, environment))((valueR: number) => {
              return ID.unit(valueL + valueR);
            });
          });
        }
      });
    };

    // **リスト8.12** 数値の評価のテスト
    it('数値の評価のテスト', () => {
      expect(
        evaluate(exp.num(2), env.empty)
      ).toEqual(
        ID.unit(2)
      );
    });

    // **リスト8.14** 変数の評価のテスト
    it('変数の評価のテスト', () => {
      /* 変数xを1に対応させた環境を作る */
      const newEnv = env.extend("x", 1, env.empty);
      /* 拡張したnewEnv環境を用いて変数xを評価する */
      expect(
        evaluate(exp.variable("x"), newEnv)
      ).toEqual(
        ID.unit(1)
      );
      expect(
        evaluate(exp.variable("y"), newEnv)
      ).toBe(
        ID.unit(undefined)
      );
    });

    // **リスト8.16** 足し算の評価のテスト
    it('足し算の評価のテスト', () => {
      /* add(1,2) */
      const addition = exp.add(exp.num(1), exp.num(2));
      expect(
        evaluate(addition, env.empty)
      ).toEqual(
        ID.unit(3)
      );
    });

    it('恒等モナド評価器で演算を評価する', () => {
      expect(
        evaluate(exp.add(exp.num(1), exp.num(2)), emptyEnv)
      ).toBe(
        ID.unit(3)
      );
    });

    // #### 関数の評価
    it('ID評価器で関数を評価する', () => {
      // ((x) => {
      //   return x;
      // })(1)
      const expression = exp.lambda(exp.variable("x"),
        exp.variable("x"));
      expect(
        evaluate(expression, emptyEnv)(1)
      ).toBe(
        1
      );
    });

    it('関数適用の評価のテスト', () => {
      // **リスト8.17** 関数適用の評価のテスト
      // ((n) => {
      //   return n + 1;
      // })(2)
      const expression = exp.app(         /* 関数適用 */
        exp.lambda(exp.variable("n"),   /* λ式 */
          exp.add(exp.variable("n"),
            exp.num(1))),
        exp.num(2));                    /* 引数の数値2 */
      expect(
        evaluate(expression, env.empty)
      ).toEqual(
        ID.unit(3)
      );
    });

    it('ID評価器で関数適用 \\x.add(x,x)(2)を評価する', () => {
      // ((x) => {
      //   return x + x;
      // })(2)
      const expression = exp.app(exp.lambda(exp.variable("x"),
        exp.add(exp.variable("x"), exp.variable("x"))),
        exp.num(2));
      expect(
        evaluate(expression, env.empty)
      ).toEqual(
        4
      );
    });

    it('カリー化関数の評価', () => {
      // **リスト8.19**カリー化関数の評価
      // ((n) => {
      //    return (m) => {
      //       return n + m;
      //    };
      // })(2)(3)
      const expression = exp.app(
        exp.app(
          exp.lambda(exp.variable("n"),
            exp.lambda(exp.variable("m"),
              exp.add(
                exp.variable("n"), exp.variable("m")))),
          exp.num(2)),
        exp.num(3));
      expect(
        evaluate(expression, env.empty)
      ).toEqual(
        ID.unit(5)
      );
    });
  });

  // ### <section id='logger-monad-evaluator'>ログ出力評価器</section>
  describe('ログ出力評価器', () => {
    // **リスト8.20** ログ出力評価器の式
    // 式の型定義
    type Exp = (pattern: ExpPattern) => any;

    interface ExpPattern {
      log: (anExp: Exp) => any;
      num: (value: number) => any;
      variable: (name: string) => any;
      lambda: (variable: Exp, body: Exp) => any;
      app: (lambda: Exp, arg: Exp) => any;
      add: (exp1: Exp, exp2: Exp) => any;
      mul: (exp1: Exp, exp2: Exp) => any;
    }

    const exp = {
      log: (anExp: Exp): Exp => { // ログ出力用の式
        return (pattern: ExpPattern) => {
          return pattern.log(anExp);
        };
      },
      match: (data: Exp, pattern: ExpPattern): any => {
        return data.call(exp, pattern);
      },
      num: (value: number): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.num(value);
        };
      },
      variable: (name: string): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.variable(name);
        };
      },
      lambda: (variable: Exp, body: Exp): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.lambda(variable, body);
        };
      },
      app: (variable: Exp, arg: Exp): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.app(variable, arg);
        };
      },
      add: (exp1: Exp, exp2: Exp): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.add(exp1, exp2);
        };
      },
      mul: (exp1: Exp, exp2: Exp): Exp => {
        return (pattern: ExpPattern) => {
          return pattern.mul(exp1, exp2);
        };
      }
    };

    // **リスト8.21** LOGモナドの定義
    /* LOG[T] = PAIR[T, LIST[STRING]] */
    type LOG<T> = Pair<T, List<string>>;

    const LOG = {
      /* unit:: VALUE => LOG[VALUE] */
      unit: <T>(value: T): LOG<T> => {
        /* 値とログのPair型を作る */
        return pair.cons(value, list.empty<string>());
      },
      /* flatMap:: LOG[T] => FUN[T => LOG[T]] => LOG[T] */
      flatMap: <T, U>(instanceM: LOG<T>) => {
        return (transform: (x: T) => LOG<U>): LOG<U> => {
          return pair.match(instanceM, {
            /* Pair型に格納されている値の対を取り出す */
            cons: (value: T, log: List<string>) => {
              /* 取り出した値で計算する */
              const newInstance = transform(value);
              /* 計算の結果をPairの左側に格納し、
                 新しいログをPairの右側に格納する */
              return pair.cons(pair.left(newInstance),
                list.append(log)(pair.right(newInstance)));
            }
          });
        };
      },
      /* 引数 value をログに格納する */
      /* output:: VALUE => LOG[()] */
      output: (value: any): LOG<null> => {
        return pair.cons(null,
          list.cons(String(value), list.empty<string>()));
      }
    };

    // **リスト8.22** LOGモナド評価器
    /* evaluate:: (EXP, ENV) => LOG[VALUE] */
    const evaluate = (anExp: Exp, environment: Env): LOG<any> => {
      return exp.match(anExp, {
        /* log式の評価 */
        log: (anExp: Exp) => {
          /* 式を評価する */
          return LOG.flatMap(evaluate(anExp, environment))((value: any) => {
            /* value をログに格納する */
            return LOG.flatMap(LOG.output(value))((_: any) => {
              return LOG.unit(value);
            });
          });
        },
        /* 数値の評価 */
        num: (value: number) => {
          return LOG.unit(value);
        },
        /* 変数の評価 */
        variable: (name: string) => {
          return LOG.unit(env.lookup(name, environment));
        },
        /* λ式の評価 */
        lambda: (variable: Exp, body: Exp) => {
          return exp.match(variable, {
            variable: (name: string) => {
              return LOG.unit((actualArg: any) => {
                return evaluate(body, env.extend(name, actualArg, environment));
              });
            },
            log: () => { },
            num: () => { },
            lambda: () => { },
            app: () => { },
            add: () => { },
            mul: () => { }
          });
        },
        /* 関数適用の評価 */
        app: (lambda: Exp, arg: Exp) => {         // 関数適用の評価
          return LOG.flatMap(evaluate(lambda, environment))((closure: (arg: any) => LOG<any>) => {
            return LOG.flatMap(evaluate(arg, environment))((actualArg: any) => {
              return closure(actualArg);
            });
          });
        },
        add: (expL: Exp, expR: Exp) => {
          return LOG.flatMap(evaluate(expL, environment))((valueL: number) => {
            return LOG.flatMap(evaluate(expR, environment))((valueR: number) => {
              return LOG.unit(valueL + valueR);
            });
          });
        },
        mul: (expL: Exp, expR: Exp) => {
          return LOG.flatMap(evaluate(expL, environment))((valueL: number) => {
            return LOG.flatMap(evaluate(expR, environment))((valueR: number) => {
              return LOG.unit(valueL * valueR);
            });
          });
        }
      });
    };

    // ### ログ出力評価器のテスト
    describe('ログ出力評価器のテスト', () => {
      it('LOG評価器で数値を評価する', () => {
        pair.match(evaluate(exp.log(exp.num(2)), env.empty), {
          cons: (value: number, log: List<string>) => {
            expect( // 結果の値をテストする
              value
            ).toBe(
              2
            );
            expect( // 保存されたログを見る
              list.toArray(log)
            ).toEqual(
              ['2']
            );
          }
        });
      });

      it('LOG評価器で変数を評価する', () => {
        const newEnv = env.extend("x", 1, env.empty);
        pair.match(evaluate(exp.log(exp.variable("x")), newEnv), {
          cons: (value: number, log: List<string>) => {
            expect( // 結果の値をテストする
              value
            ).toEqual(
              1
            );
            expect( // 保存されたログを見る
              list.toArray(log)
            ).toEqual(
              ['1']
            );
          }
        });
      });

      it('LOG評価器で演算を評価する', () => {
        pair.match(evaluate(exp.log(exp.add(exp.num(1), exp.num(2))), env.empty), {
          cons: (value: number, log: List<string>) => {
            expect(
              value
            ).toBe(
              3
            );
            expect(
              list.toArray(log)
            ).toEqual(
              ['3']
            );
          }
        });
        pair.match(evaluate(exp.log(exp.add(exp.log(exp.num(1)), exp.log(exp.num(2)))), env.empty), {
          cons: (value: number, log: List<string>) => {
            expect(
              value
            ).toBe(
              3 // 1 + 2 = 3
            );
            expect(
              list.toArray(log)
            ).toEqual(
              ['1', '2', '3']
            );
          }
        });

        // **リスト8.25** ログ出力評価器による評価戦略の確認
        // ((n) => {
        //    return add(1)(n)
        // })(2);
        const theExp = exp.log(exp.app(exp.lambda(exp.variable("n"),
          exp.add(exp.log(exp.num(1)),
            exp.variable("n"))),
          exp.log(exp.num(2))));
        pair.match(evaluate(theExp, env.empty), {
          /* パターンマッチで結果を取り出す */
          cons: (value: number, log: List<string>) => {
            expect(
              value
            ).toEqual(
              3
            );
            expect(
              list.toArray(log)
            ).toEqual(
              ['2', '1', '3']
            );
          }
        });
      });

      it('LOG評価器で関数適用を評価する', () => {
        // ((x) => {
        //    return add(x)(x)
        // })(2);
        const expression = exp.app(exp.lambda(exp.variable("x"),
          exp.add(exp.variable("x"), exp.variable("x"))),
          exp.num(2));
        expect(
          pair.left(evaluate(expression, env.empty))
        ).toEqual(
          4
        );
        expect(
          list.toArray(pair.right(evaluate(expression, env.empty)))
        ).toEqual(
          []
        );
      });

      it('LOG評価器でカリー化関数を評価する', () => {
        // ((x) => {
        //   return (y) => {
        //       return add(x)(y)
        //   };
        // })(2)(3);
        const expression = exp.app(
          exp.app(exp.lambda(exp.variable("x"),
            exp.lambda(exp.variable("y"),
              exp.add(exp.variable("x"), exp.variable("y")))),
            exp.num(2)),
          exp.num(3));
        expect(
          pair.left(evaluate(expression, emptyEnv))
        ).toBe(
          5
        );
      });
    });
  });
});

// [目次に戻る](index.html)
