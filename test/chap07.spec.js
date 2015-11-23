"use strict";

var expect = require('expect.js');

// 関数型言語を作る
// ========
describe('関数型言語を作る', () => {
  describe('環境を作る', () => {
    /* #@range_begin(environment) */
    // ## 環境
    var emptyEnv = (variable) => {
      return undefined;
    };
    /* 変数名に対応する値を環境から取りだす */
    var lookupEnv = (identifier, env) => {
      return env(identifier);
    };
    /* 環境を拡張する */
    var extendEnv = (identifier, value, env) => {
      expect(identifier).to.a('string');
      return (queryIdentifier) => {
        expect(queryIdentifier).to.a('string');
        if(identifier === queryIdentifier) {
          return value;
        } else {
          return lookupEnv(queryIdentifier,env);
        }
      };
    };
    /* #@range_end(environment) */
	describe('環境をテストする', () => {
      it('extendEnvで環境を作り、 lookupEnv で環境を探る', (next) => {
		var newEnv = extendEnv('a',1, emptyEnv);
		expect(
          lookupEnv("a", newEnv)
		).to.be(
          1
		);
		next();
      });
    });
    describe('式を作る', () => {
      // ## 式の代数的データ構造
      var match = (exp, pattern) => {
        return exp.call(pattern, pattern);
      };
      /* #@range_begin(expression_algaraic_datatype) */
      var number = (value) => {
        expect(value).to.a('number');
        return (pattern) => {
          return pattern.number(value);
        };
      };
      var variable = (name) => {
        expect(name).to.a('string');
        return (pattern) => {
          return pattern.variable(name);
        };
      };
      var lambda = (variable, body) => {
        expect(variable).to.a('function');
        expect(body).to.a('function');
        return (pattern) => {
          return pattern.lambda(variable, body);
        };
      };
      var application = (variable, arg) => {
        return (pattern) => {
          return pattern.application(variable, arg);
        };
      };
      it('式をテストする', (next) => {
        // λx.λy.x
        match(lambda(variable("x"),lambda(variable("y"),variable("x"))),{
          lambda: (variable, arg) => {
            expect(
              variable
            ).to.a('function');
          }
        });
        next();
      });
      /* #@range_end(expression_algaraic_datatype) */
      describe('評価器を作る', () => {
        // ## 式の評価関数
        /* #@range_begin(evaluation_function) */
        var evaluate = (exp, env) => {
          return match(exp,{
            /* 数値の評価 */
            number: (value) => {
              return value;
            },
            /* 変数の評価 */
            variable: (name) => {
              return lookupEnv(name, env);
            },
            /* λ式の評価 */
            lambda: (variable, bodyExp) => {
              /* クロージャーを返す */
              return (actualArg) => {
                return match(variable,{ // maybeを返すべきか？
                  variable: (name) => {
                    return evaluate(bodyExp, extendEnv(name, actualArg ,env));
                  }
                });
              };
            },
            /* 関数適用の評価 */
            application: (variable, arg) => {
              var rator = evaluate(variable, env);
              var rand = evaluate(arg, env);
              return rator(rand);
            }
          });
        };
        /* #@range_end(evaluation_function) */
        describe('evaluate関数で式を評価する', () => {
          it('数値を評価する', (next) => {
            expect(
              evaluate(number(2), emptyEnv)
            ).to.be(
              2
            );
            next();
          });
          it('変数を評価する', (next) => {
            var env = extendEnv("x",1, emptyEnv);
            expect(
              evaluate(variable("x"), env)
            ).to.be(
              1
            );
            expect(
              evaluate(variable("y"), env)
            ).to.be(
              undefined
            );
            next();
          });
          it('constant関数', (next) => {
            var constant = lambda(variable("x"),number(1))
            expect(
              evaluate(constant, emptyEnv)
            ).to.a(
              'function'
            );
            var applied = application(constant, variable("y"))
            expect(
              evaluate(applied, emptyEnv)
            ).to.eql(
              1
            );
            next();
          });
          it('id関数', (next) => {
            /* λx.x */
            var id = lambda(variable("x"),variable("x"));
            expect(
              evaluate(id, emptyEnv)
            ).to.a(
              'function'
            );
            expect(
              evaluate(application(id, number(1)), emptyEnv)
            ).to.eql(
              1
            );
            next();
          });
          it('ブール型を評価する', (next) => {
            /* λx.λy.x */
            var trueFun = lambda(variable("x"),lambda(variable("y"),variable("x")));
            /* λx.λy.y */
            var falseFun = lambda(variable("x"),lambda(variable("y"),variable("y")));
            var not = lambda(variable("x"),
                             application(
                               application(
                                 variable("x"),falseFun),
                               trueFun));
            var and = lambda(variable("x"),
                             lambda(variable("y"),
                                    application(
                                      application(variable("x"),variable("y")),
                                      falseFun)));
            var or = lambda(variable("x"),
                            lambda(variable("y"),
                                   application(
                                     application(variable("x"),trueFun),
                                     variable("y"))));
            var cond = lambda(variable("pred"),
                              lambda(variable("x"),
                                     lambda(variable("y"),
                                            application(
                                              application(variable("pred"),variable("x")),variable("y")))));
            expect(
              evaluate(
                application(
                  application(trueFun,number(1)),
                  number(0)),
                emptyEnv)
            ).to.eql(
              1
            );
            expect(
              evaluate(
                application(
                  application(
                    application(not, trueFun), number(1)), number(0)),emptyEnv)
            ).to.eql(
              0
            );
            expect(
              evaluate(
                application(
                  application(
                    application(
                      application(and,
                                  trueFun),
                      trueFun),
                    number(1)),
                  number(0)),emptyEnv)
            ).to.be(
              1
            );
            expect(
              evaluate(
                application(
                  application(
                    application(
                      application(
                        application(cond, trueFun),
                        falseFun),
                      trueFun),
                    number(1)),
                  number(0)),
                emptyEnv)
            ).to.eql(
              0
            );
            next();
          });
        });
      });
    }); // 
    describe('継続渡し評価器', () => {
      // ## 式の代数的データ構造
      var match = (exp, pattern) => {
        return exp.call(pattern, pattern);
      };
	  var exp = {
		raise: (exp) => {
          return (pattern) => {
			return pattern.raise(exp);
          };
		},
		tryWith: (exp, exception, raisedExp) => {
          return (pattern) => {
			return pattern.tryWith(exp, exception, raisedExp);
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
		application: (variable, arg) => {
          return (pattern) => {
			return pattern.application(variable, arg);
          };
		}
	  }; // exp
	  var result = {
		value: (answer) => {
          return (pattern) => {
			return pattern.value(answer);
          };
		},
		abort: (message) => {
          return (pattern) => {
			return pattern.abort(message);
          };
		}
	  };
	  // ## 式の評価関数
	  // evaluateCPS: (EXP, ENV, FUNC[VALUE -> RESULT]) -> RESULT
      var evaluateCPS = (exp, env, continues) => {
        return match(exp,{
          /* 例外の評価 */
          raise: (exp) => {
            return result.abort("exception raised");
          },
          /* 数値の評価 */
          number: (answer) => {
            return continues(result.value(answer));
          },
          /* 変数の評価 */
          variable: (name) => {
			var found = lookupEnv(name, env);
            if(found === undefined){
			  return result.abort(name + " not found");
			} else {
			  return continues(result.value(found));
			}
          },
          /* λ式の評価 */
          lambda: (variable, bodyExp) => {
            /* クロージャーを返す */
            return (actualArg) => {
              return match(variable,{ // maybeを返すべきか？
                variable: (name) => {
                  return continues(evaluateCPS(bodyExp, extendEnv(name, actualArg ,env)));
                }
              });
            };
          },
          /* 関数適用の評価 */
          application: (variable, arg) => {
            var rator = evaluateCPS(variable, env);
            var rand = evaluateCPS(arg, env);
            return continues(rator(rand));
          }
        });
      };
	  describe('継続渡し評価器で式を評価する', () => {
		var returns = (result) => {
		  return result;
		};
        it('数値を評価する', (next) => {
		  match(evaluateCPS(exp.number(2), emptyEnv, returns), {
			value: (answer) => {
			  expect(answer).to.eql(2);
			},
			abort: (message) => {
			  expect().fail();
			}
		  });
		  next();
        });
        it('変数を評価する', (next) => {
          var env = extendEnv("x",1, emptyEnv, returns);
		  match(evaluateCPS(exp.variable("x"), env, returns), {
			value: (answer) => {
			  expect(answer).to.eql(1);
			},
			abort: (message) => {
			  expect().fail();
			}
		  });
		  match(evaluateCPS(exp.variable("y"), env, returns), {
			value: (answer) => {
			  expect().fail();
			  
			},
			abort: (message) => {
			  expect(message).to.eql("y not found");
			}
		  });
          next();
        });
	  });
    });
  });
  // describe('失敗継続渡し評価器', () => {
  // });
});
