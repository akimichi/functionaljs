"use strict";

var expect = require('expect.js');

// # 継続渡し評価器のサンプルコード


// ## 環境モジュール
var env = {
  // 空の環境
  empty: (variable) => {
    return undefined;
  },
  /* 変数名に対応する値を環境から取りだす */
  // lookupEnv:: (STRING, ENV) => M[VALUE]
  lookup: (identifier, environment) => {
    return environment(identifier);
  },
  /* 環境を拡張する */
  // extendEnv:: (STRING, VALUE, ENV) => ENV 
  extend: (identifier, value, environment) => {
    expect(identifier).to.a('string');
    return (queryIdentifier) => {
      expect(queryIdentifier).to.a('string');
      if(identifier === queryIdentifier) {
        return value;
      } else {
        return env.lookup(queryIdentifier, environment);
      }
    };
  }
};

describe('継続渡し評価器', () => {
  // ## 式の代数的データ構造
  var exp = {
    match : (data, pattern) => {
      return data.call(exp, pattern);
    },
    raise: (exp) => {
      return (pattern) => {
        return pattern.raise(exp);
      };
    },
    number: (value) => {
      expect(value).to.a('number');
      return (pattern) => {
        return pattern.number(value);
      };
    },
    variable: (name) => {
      expect(name).to.a('string');
      return (pattern) => {
        return pattern.variable(name);
      };
    },
    lambda: (variable, body) => {
      expect(variable).to.a('function');
      expect(body).to.a('function');
      return (pattern) => {
        return pattern.lambda(variable, body);
      };
    },
    app: (variable, arg) => {
      return (pattern) => {
        return pattern.app(variable, arg);
      };
    }
  }; // exp
  // ## 式の評価関数
  // evaluateCPS: (EXP, ENV, FUNC[VALUE -> VALUE]) -> VALUE
  var evaluateCPS = (anExp, environment, continues) => {
    return exp.match(anExp,{
      /* 例外の評価 */
      raise: (message) => {
        return new Error(message);
      },
      /* 数値の評価 */
      number: (answer) => {
        return continues(answer);
      },
      /* 変数の評価 */
      variable: (name) => {
        var found = env.lookup(name, environment);
        if(found === undefined){
          return new Error(name + " not found");
        } else {
          return continues(found);
        }
      },
      /* λ式の評価 */
      lambda: (anExp, bodyExp) => {
        /* クロージャーを返す */
        return (actualArg) => {
          return exp.match(anExp,{
            variable: (name) => {
              return continues(evaluateCPS(bodyExp, env.extend(name, actualArg ,environment), continues));
            },
            number: (value) => {
              return new Error("lambdaの引数が数値になっています");
            },
            lambda: ($$,$$$) => {
              return new Error("lambdaの引数がlambbaになっています");
            }
          });
        };
      },
      /* 関数適用の評価 */
      app: (anExp, arg) => {
        var rator = evaluateCPS(anExp, environment, continues);
        var rand = evaluateCPS(arg, environment, continues);
        return continues(rator(rand));
      }
    });
  };
  // ## 単体テスト
  describe('継続渡し評価器をテストする', () => {
    var returns = (result) => {
      return result;
    };
    it('数値を評価する', (next) => {
      expect(
        evaluateCPS(exp.number(2), env.empty, returns)
      ).to.eql(2);
      next();
    });
    it('変数を評価する', (next) => {
      var environment = env.extend("x",1, env.empty, returns);
      expect(
        evaluateCPS(exp.variable("x"), environment, returns)
      ).to.eql(
        1
      );
      // 自由変数の場合は、 abortが返る
      expect(
        evaluateCPS(exp.variable("y"), environment, returns)
      ).to.eql(
        new Error("y not found")
      );
      next();
    });
    it('constant関数', (next) => {
      var constant = exp.lambda(exp.variable("x"),exp.number(1));
      expect(
        evaluateCPS(constant, env.empty, returns)
      ).to.a(
        'function'
      );
      // (λx.1)(2)
      var applied = exp.app(constant, exp.number(2));
      expect(
        evaluateCPS(applied, env.empty, returns)
      ).to.eql(
        1
      );
      next();
    });
    it('identity関数をテストする', (next) => {
      /* λx.x */
      var identity = exp.lambda(exp.variable("x"),exp.variable("x"));
      expect(
        evaluateCPS(identity, env.empty, returns)
      ).to.a(
        'function'
      );
      // (λx.x)(1) = 1 */
      var appliedExpression = exp.app(identity, exp.number(1));
      expect(
        evaluateCPS(appliedExpression, env.empty, returns)
      ).to.eql(
        1
      );
      next();
    });
    it('ブール型を評価する', (next) => {
      this.timeout(1000);
      /* λx.λy.x */
      var trueFun = exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("x")));
      /* λx.λy.y */
      var falseFun = exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("y")));
      var not = exp.lambda(exp.variable("x"),
                           exp.app(
                             exp.app(
                               exp.variable("x"),falseFun),
                             trueFun));
      var and = exp.lambda(exp.variable("x"),
                           exp.lambda(exp.variable("y"),
                                      exp.app(
                                        exp.app(exp.variable("x"),exp.variable("y")),
                                        falseFun)));
      var or = exp.lambda(exp.variable("x"),
                          exp.lambda(exp.variable("y"),
                                     exp.app(
                                       exp.app(exp.variable("x"),trueFun),
                                       exp.variable("y"))));
      var cond = exp.lambda(exp.variable("pred"),
                            exp.lambda(exp.variable("x"),
                                       exp.lambda(exp.variable("y"),
                                                  exp.app(
                                                    exp.app(exp.variable("pred"),exp.variable("x")),exp.variable("y")))));
      // (λx.λy.x)(1)(0) = 1
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun,exp.number(1)),
            exp.number(0)),
          env.empty,
          returns)
      ).to.eql(
        1
      );
      // (λx.λy.x)(1)(z) = 1
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun,exp.number(1)),
            exp.variable("z")),
          env.empty,
          returns)
      ).to.eql(
        1
      );
      // (λx.λy.x)(z)(0) = error
      expect(
        evaluateCPS(
          exp.app(
            exp.app(trueFun,exp.variable("z")),
            exp.number(0)),
          env.empty,
          returns)
      ).to.eql(
        new Error("z not found")
      );
      next();
    });
    it('raiseを使う', (next) => {
      // (λx.raise)(2)
      var applied = exp.app(exp.lambda(exp.variable("x"),exp.raise("exception")),
                            exp.number(2));
      expect(
        evaluateCPS(applied, env.empty, returns)
      ).to.eql(
        new Error("exception")
      );
      next();
    });
  });
});
