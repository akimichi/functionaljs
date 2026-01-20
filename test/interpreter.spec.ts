"use strict";

// # 継続渡し評価器のサンプルコード


// ## 環境モジュール
const env = {
  // 空の環境
  empty: (variable: string): any => {
    return undefined;
  },
  /* 変数名に対応する値を環境から取りだす */
  // lookupEnv:: (STRING, ENV) => M[VALUE]
  lookup: (identifier: string, environment: (id: string) => any): any => {
    return environment(identifier);
  },
  /* 環境を拡張する */
  // extendEnv:: (STRING, VALUE, ENV) => ENV
  extend: (identifier: string, value: any, environment: (id: string) => any): ((id: string) => any) => {
    expect(typeof identifier).toBe('string');
    return (queryIdentifier: string): any => {
      expect(typeof queryIdentifier).toBe('string');
      if (identifier === queryIdentifier) {
        return value;
      } else {
        return env.lookup(queryIdentifier, environment);
      }
    };
  }
};


// ## 例外捕捉評価器
describe('例外捕捉評価器', () => {
  // 「環境」モジュール
  const env = {
    // empty:: STRING => VALUE
    empty: (variable: string): any => {                        // 空の環境を作る
      return undefined;
    },
    // lookup:: (STRING, ENV) => VALUE
    lookup: (name: string, environment: (id: string) => any): any => {       // 変数名に対応する値を環境から取りだす
      return environment(name);
    },
    // extend:: (STRING, VALUE, ENV) => ENV
    extend: (identifier: string, value: any, environment: (id: string) => any): ((id: string) => any) => { // 環境を拡張する
      return (queryIdentifier: string): any => {
        if (identifier === queryIdentifier) {
          return value;
        } else {
          return env.lookup(queryIdentifier, environment);
        }
      };
    }
  };

  // ### 式の代数的データ構造
  interface ExpPattern {
    exception?: (message: string) => any;
    raise?: (exception: Error) => any;
    tryWith?: (anExp: any, exception: Error, raisedExp: any) => any;
    num?: (value: number) => any;
    variable?: (name: string) => any;
    lambda?: (variable: any, body: any) => any;
    app?: (variable: any, arg: any) => any;
  }

  /* #@range_begin(continuation_passing_interpreter_expression) */
  const exp = {
    match: (data: any, pattern: ExpPattern): any => {
      return data.call(exp, pattern);
    },
    exception: (message: string) => {
      return (pattern: ExpPattern) => {
        return pattern.exception!(message);
      };
    },
    raise: (exception: Error) => {
      return (pattern: ExpPattern) => {
        return pattern.raise!(exception);
      };
    },
    tryWith: (anExp: any, exception: Error, raisedExp: any) => {
      return (pattern: ExpPattern) => {
        return pattern.tryWith!(anExp, exception, raisedExp);
      };
    },
    /* #@range_end(continuation_passing_interpreter_expression) */
    num: (value: number) => {
      expect(typeof value).toBe('number');
      return (pattern: ExpPattern) => {
        return pattern.num!(value);
      };
    },
    variable: (name: string) => {
      expect(typeof name).toBe('string');
      return (pattern: ExpPattern) => {
        return pattern.variable!(name);
      };
    },
    lambda: (variable: any, body: any) => {
      expect(typeof variable).toBe('function');
      expect(typeof body).toBe('function');
      return (pattern: ExpPattern) => {
        return pattern.lambda!(variable, body);
      };
    },
    app: (variable: any, arg: any) => {
      return (pattern: ExpPattern) => {
        return pattern.app!(variable, arg);
      };
    }
  }; // exp

  // ### 例外捕捉評価器の評価関数
  /* #@range_begin(continuation_passing_interpreter_evaluate) */
  // evaluateCPS: (EXP, ENV, FUNC[VALUE -> VALUE]) -> VALUE
  const evaluateCPS = (anExp: any, environment: any, continues: (value: any) => any, continuesInFailure: (exception: Error) => any): any => {
    // c.f. Programming Language Concepts, p.208
    return exp.match(anExp, {
      /* 例外の評価 */
      raise: (exception: Error) => {
        return continuesInFailure(exception);
      },
      tryWith: (anExp: any, caughtException: Error, failSafeExp: any) => {
        const newContinuesInFailure = (thrownException: Error) => {
          if (thrownException.message === caughtException.message) {
            return evaluateCPS(failSafeExp, environment, continues, continuesInFailure);
          } else {
            return continuesInFailure(thrownException);
          }
        };
        return evaluateCPS(anExp, environment, continues, newContinuesInFailure);
      },
      /* 数値の評価 */
      num: (answer: number) => {
        return continues(answer);
      },
      /* 変数の評価 */
      variable: (name: string) => {
        const found = env.lookup(name, environment);
        if (found === undefined) {
          return continuesInFailure(new Error(name + " not found"));
        } else {
          return continues(found);
        }
      },
      /* λ式の評価 */
      lambda: (anExp: any, bodyExp: any) => {
        /* クロージャーを返す */
        return (actualArg: any) => {
          return exp.match(anExp, {
            variable: (name: string) => {
              return continues(evaluateCPS(bodyExp, env.extend(name, actualArg, environment), continues, continuesInFailure));
            },
            num: (value: number) => {
              return continuesInFailure(new Error("lambdaの引数が数値になっています"));
            },
            lambda: ($$: any, $$$: any) => {
              return continuesInFailure(new Error("lambdaの引数がlambbaになっています"));
            }
          });
        };
      },
      /* 関数適用の評価 */
      app: (anExp: any, arg: any) => {
        const rator = evaluateCPS(anExp, environment, continues, continuesInFailure);
        const rand = evaluateCPS(arg, environment, continues, continuesInFailure);
        return continues(rator(rand));
      }
    });
    /* #@range_end(continuation_passing_interpreter_evaluate) */
  };

  describe('例外捕捉評価器をテストする', () => {
    // 「環境」モジュール
    const env = {
      // empty:: STRING => VALUE
      empty: (variable: string): any => {                        // 空の環境を作る
        return undefined;
      },
      // lookup:: (STRING, ENV) => VALUE
      lookup: (name: string, environment: (id: string) => any): any => {       // 変数名に対応する値を環境から取りだす
        return environment(name);
      },
      // extend:: (STRING, VALUE, ENV) => ENV
      extend: (identifier: string, value: any, environment: (id: string) => any): ((id: string) => any) => { // 環境を拡張する
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
    const continuesNormally = (result: any) => {
      return result;
    };
    const continuesAbnormally = (exception: Error) => {
      return exception;
    };

    it('数値を評価する', () => {
      expect(
        evaluateCPS(exp.num(2), emptyEnv, continuesNormally, continuesAbnormally)
      ).toEqual(2);
    });

    it('変数を評価する', () => {
      const newEnv = env.extend("x", 1, emptyEnv);
      expect(
        evaluateCPS(exp.variable("x"), newEnv, continuesNormally, continuesAbnormally)
      ).toEqual(
        1
      );
      // 自由変数の場合は、 例外が返る
      expect(
        evaluateCPS(exp.variable("y"), newEnv, continuesNormally, continuesAbnormally)
      ).toEqual(
        new Error("y not found")
      );
    });

    it('constant関数', () => {
      const constant = exp.lambda(exp.variable("x"), exp.num(1));
      expect(
        typeof evaluateCPS(constant, emptyEnv, continuesNormally, continuesAbnormally)
      ).toBe(
        'function'
      );
      // (λx.1)(2)
      const applied = exp.app(constant, exp.num(2));
      expect(
        evaluateCPS(applied, emptyEnv, continuesNormally, continuesAbnormally)
      ).toEqual(
        1
      );
    });

    it('identity関数をテストする', () => {
      /* λx.x */
      const identity = exp.lambda(exp.variable("x"), exp.variable("x"));
      expect(
        typeof evaluateCPS(identity, emptyEnv, continuesNormally, continuesAbnormally)
      ).toBe(
        'function'
      );
      // (λx.x)(1) = 1 */
      const appliedExpression = exp.app(identity, exp.num(1));
      expect(
        evaluateCPS(appliedExpression, emptyEnv, continuesNormally, continuesAbnormally)
      ).toEqual(
        1
      );
    });

    it('ブール型を評価する', () => {
      /* λx.λy.x */
      const trueFun = exp.lambda(exp.variable("x"), exp.lambda(exp.variable("y"), exp.variable("x")));
      /* λx.λy.y */
      const falseFun = exp.lambda(exp.variable("x"), exp.lambda(exp.variable("y"), exp.variable("y")));
      const not = exp.lambda(exp.variable("x"),
        exp.app(
          exp.app(
            exp.variable("x"), falseFun),
          trueFun));
      const and = exp.lambda(exp.variable("x"),
        exp.lambda(exp.variable("y"),
          exp.app(
            exp.app(exp.variable("x"), exp.variable("y")),
            falseFun)));
      const or = exp.lambda(exp.variable("x"),
        exp.lambda(exp.variable("y"),
          exp.app(
            exp.app(exp.variable("x"), trueFun),
            exp.variable("y"))));
      const cond = exp.lambda(exp.variable("pred"),
        exp.lambda(exp.variable("x"),
          exp.lambda(exp.variable("y"),
            exp.app(
              exp.app(exp.variable("pred"), exp.variable("x")), exp.variable("y")))));
      // (λx.λy.x)(1)(0) = 1
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun, exp.num(1)),
            exp.num(0)),
          emptyEnv,
          continuesNormally,
          continuesAbnormally)
      ).toEqual(
        1
      );
      // (λx.λy.x)(1)(z) = 1
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun, exp.num(1)),
            exp.variable("z")),
          emptyEnv,
          continuesNormally,
          continuesAbnormally)
      ).toEqual(
        1
      );
      // (λx.λy.x)(z)(0) = error
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun, exp.variable("z")),
            exp.num(0)),
          emptyEnv,
          continuesNormally,
          continuesAbnormally)
      ).toEqual(
        new Error("z not found")
      );
    }, 1000);

    it('投げられた例外を捕捉する', () => {
      /* #@range_begin(continuation_passing_interpreter_trycatch) */
      const tryExpression = exp.tryWith(
        exp.raise(new Error("exception")), // exp
        new Error("exception"), // caughtException
        exp.num(1) // failSafeExp
      );
      expect(
        evaluateCPS(tryExpression, emptyEnv, continuesNormally, continuesAbnormally)
      ).toEqual(
        1
      );
      // (λx.tryWith(raise, exception , 1))(0) = 1
      const catchException = exp.app(exp.lambda(exp.variable("x"),
        exp.tryWith(
          exp.raise(new Error("exception")),
          new Error("exception"),
          exp.num(1)
        )),
        exp.num(0));
      expect(
        evaluateCPS(catchException, emptyEnv, continuesNormally, continuesAbnormally)
      ).toEqual(
        1
      );
      /* #@range_end(continuation_passing_interpreter_trycatch) */
    });
  });
});

describe('継続渡し評価器', () => {
  // ## 式の代数的データ構造
  interface ExpPattern {
    raise?: (exp: string) => any;
    number?: (value: number) => any;
    variable?: (name: string) => any;
    lambda?: (variable: any, body: any) => any;
    app?: (variable: any, arg: any) => any;
  }

  const exp = {
    match: (data: any, pattern: ExpPattern): any => {
      return data.call(exp, pattern);
    },
    raise: (exp: string) => {
      return (pattern: ExpPattern) => {
        return pattern.raise!(exp);
      };
    },
    number: (value: number) => {
      expect(typeof value).toBe('number');
      return (pattern: ExpPattern) => {
        return pattern.number!(value);
      };
    },
    variable: (name: string) => {
      expect(typeof name).toBe('string');
      return (pattern: ExpPattern) => {
        return pattern.variable!(name);
      };
    },
    lambda: (variable: any, body: any) => {
      expect(typeof variable).toBe('function');
      expect(typeof body).toBe('function');
      return (pattern: ExpPattern) => {
        return pattern.lambda!(variable, body);
      };
    },
    app: (variable: any, arg: any) => {
      return (pattern: ExpPattern) => {
        return pattern.app!(variable, arg);
      };
    }
  }; // exp

  // ## 式の評価関数
  // evaluateCPS: (EXP, ENV, FUNC[VALUE -> VALUE]) -> VALUE
  const evaluateCPS = (anExp: any, environment: any, continues: (value: any) => any): any => {
    return exp.match(anExp, {
      /* 例外の評価 */
      raise: (message: string) => {
        return new Error(message);
      },
      /* 数値の評価 */
      number: (answer: number) => {
        return continues(answer);
      },
      /* 変数の評価 */
      variable: (name: string) => {
        const found = env.lookup(name, environment);
        if (found === undefined) {
          return new Error(name + " not found");
        } else {
          return continues(found);
        }
      },
      /* λ式の評価 */
      lambda: (anExp: any, bodyExp: any) => {
        /* クロージャーを返す */
        return (actualArg: any) => {
          return exp.match(anExp, {
            variable: (name: string) => {
              return continues(evaluateCPS(bodyExp, env.extend(name, actualArg, environment), continues));
            },
            number: (value: number) => {
              return new Error("lambdaの引数が数値になっています");
            },
            lambda: ($$: any, $$$: any) => {
              return new Error("lambdaの引数がlambbaになっています");
            }
          });
        };
      },
      /* 関数適用の評価 */
      app: (anExp: any, arg: any) => {
        const rator = evaluateCPS(anExp, environment, continues);
        const rand = evaluateCPS(arg, environment, continues);
        return continues(rator(rand));
      }
    });
  };

  // ## 単体テスト
  describe('継続渡し評価器をテストする', () => {
    const returns = (result: any) => {
      return result;
    };

    it('数値を評価する', () => {
      expect(
        evaluateCPS(exp.number(2), env.empty, returns)
      ).toEqual(2);
    });

    it('変数を評価する', () => {
      const environment = env.extend("x", 1, env.empty);
      expect(
        evaluateCPS(exp.variable("x"), environment, returns)
      ).toEqual(
        1
      );
      // 自由変数の場合は、 abortが返る
      expect(
        evaluateCPS(exp.variable("y"), environment, returns)
      ).toEqual(
        new Error("y not found")
      );
    });

    it('constant関数', () => {
      const constant = exp.lambda(exp.variable("x"), exp.number(1));
      expect(
        typeof evaluateCPS(constant, env.empty, returns)
      ).toBe(
        'function'
      );
      // (λx.1)(2)
      const applied = exp.app(constant, exp.number(2));
      expect(
        evaluateCPS(applied, env.empty, returns)
      ).toEqual(
        1
      );
    });

    it('identity関数をテストする', () => {
      /* λx.x */
      const identity = exp.lambda(exp.variable("x"), exp.variable("x"));
      expect(
        typeof evaluateCPS(identity, env.empty, returns)
      ).toBe(
        'function'
      );
      // (λx.x)(1) = 1 */
      const appliedExpression = exp.app(identity, exp.number(1));
      expect(
        evaluateCPS(appliedExpression, env.empty, returns)
      ).toEqual(
        1
      );
    });

    it('ブール型を評価する', () => {
      // /* λx.λy.x */
      const trueFun = exp.lambda(exp.variable("x"), exp.lambda(exp.variable("y"), exp.variable("x")));
      /* λx.λy.y */
      const falseFun = exp.lambda(exp.variable("x"), exp.lambda(exp.variable("y"), exp.variable("y")));
      const not = exp.lambda(exp.variable("x"),
        exp.app(
          exp.app(
            exp.variable("x"), falseFun),
          trueFun));
      const and = exp.lambda(exp.variable("x"),
        exp.lambda(exp.variable("y"),
          exp.app(
            exp.app(exp.variable("x"), exp.variable("y")),
            falseFun)));
      const or = exp.lambda(exp.variable("x"),
        exp.lambda(exp.variable("y"),
          exp.app(
            exp.app(exp.variable("x"), trueFun),
            exp.variable("y"))));
      const cond = exp.lambda(exp.variable("pred"),
        exp.lambda(exp.variable("x"),
          exp.lambda(exp.variable("y"),
            exp.app(
              exp.app(exp.variable("pred"), exp.variable("x")), exp.variable("y")))));
      // (λx.λy.x)(1)(0) = 1
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun, exp.number(1)),
            exp.number(0)),
          env.empty,
          returns)
      ).toEqual(
        1
      );
      // (λx.λy.x)(1)(z) = 1
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun, exp.number(1)),
            exp.variable("z")),
          env.empty,
          returns)
      ).toEqual(
        1
      );
      // (λx.λy.x)(z)(0) = error
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun, exp.variable("z")),
            exp.number(0)),
          env.empty,
          returns)
      ).toEqual(
        new Error("z not found")
      );
    }, 1000);

    it('raiseを使う', () => {
      // (λx.raise)(2)
      const applied = exp.app(exp.lambda(exp.variable("x"), exp.raise("exception")),
        exp.number(2));
      expect(
        evaluateCPS(applied, env.empty, returns)
      ).toEqual(
        new Error("exception")
      );
    });
  });
});
