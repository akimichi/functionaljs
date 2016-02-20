"use strict";

var expect = require('expect.js');
var sys = require('sys');
var fs = require('fs');


var pair = {
  match : (data, pattern) => {
    return data.call(pair, pattern);
  },
  cons: (left, right) => {
    return (pattern) => {
      return pattern.cons(left, right);
    };
  },
  right: (tuple) => {
    return pair.match(tuple, {
      cons: (left, right) => {
        return right;
      }
    });
  },
  left: (tuple) => {
    return pair.match(tuple, {
      cons: (left, right) => {
        return left;
      }
    });
  }
};

var list  = {
  match : (data, pattern) => {
    return data.call(list, pattern);
  },
  empty: (_) => {
    return (pattern) => {
      return pattern.empty();
    };
  },
  cons: (head, tail) => {
    return (pattern) => {
      return pattern.cons(head, tail);
    };
  },
  unit : (value) => {
    return list.cons(value, list.empty());
  },
  head: (alist) => {
    return list.match(alist, {
      empty: (_) => {
        return undefined;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: (alist) => {
    return list.match(alist, {
      empty: (_) => {
        return undefined;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  // append:: LIST[T] -> LIST[T] -> LIST[T]
  append: (xs) => {
    return (ys) => {
      return list.match(xs, {
        empty: (_) => {
          return ys;
        },
        cons: (head, tail) => {
          return list.cons(head, list.append(tail)(ys)); 
        }
      });
    };
  },
  // list#concat
  concat: (xss) => {
    return list.match(xss,{
      empty: (_) => {
        return list.empty();
      },
      cons: (xs,xss) => {
        return list.append(xs,xss);
      }
    });
  },
  // join:: LIST[LIST[T]] -> LIST[T]
  join: (list_of_list) => {
    return list.concat(list_of_list);
  },
  // map:: LIST[T] -> FUNC[T -> T] -> LIST[T]
  map: (alist) => {
    return (transform) => {
      return list.match(alist,{
        empty: (_) => {
          return list.empty();
        },
        cons: (head,tail) => {
          return list.cons(transform(head),list.map(tail)(transform));
        }
      });
    };
  },
  // foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T
  foldr: (alist) => {
    return (accumulator) => {
      return (glue) => {
        expect(glue).to.a('function');
        return list.match(alist,{
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            return glue(head)(list.foldr(tail)(accumulator)(glue));
          }
        });
      };
    };
  },
  toArray: (alist) => {
    return list.foldr(alist)([])((item) => {
      return (accumulator) => {
        return [item].concat(accumulator); 
      };
    });
  }
};

// 関数型言語を作る
// ========
describe('関数型言語を作る', () => {
  describe('環境を作る', () => {
    /* #@range_begin(environment) */
    // 「環境」モジュール
    var env = {
      // 空の環境
      empty: (variable) => {
        return undefined;
      },
      /* 変数名に対応する値を環境から取りだす */
      // lookup:: (STRING, ENV) => M[VALUE]
      lookup : (identifier, environment) => {
        return environment(identifier);
      },
      /* 環境を拡張する */
      // extend:: (STRING, VALUE, ENV) => ENV 
      extend: (identifier, value, environment) => {
        expect(identifier).to.a('string');
        return (queryIdentifier) => {
          expect(queryIdentifier).to.a('string');
          if(identifier === queryIdentifier) {
            return value;
          } else {
            return env.lookup(queryIdentifier,environment);
          }
        };
      }
    };
    /* #@range_end(environment) */
    describe('環境をテストする', () => {
      it('extendEnvで環境を作り、 lookupEnv で環境を探る', (next) => {
        /* #@range_begin(environment_test) */
        expect(((_) => {
          var newEnv = env.extend('a',1, env.empty);
          return env.lookup("a", newEnv);
        })()).to.be(
          1
        );
        expect(((_) => {
          // 空の辞書を作成する
          var initEnv = env.empty;
          // var a = 1 を実行して、辞書を拡張する
          var firstEnv = env.extend("a", 1, initEnv);
          // var b = 3 を実行して、辞書を拡張する
          var secondEnv = env.extend("b",3, firstEnv);
          // 辞書から b の値を参照する
          return env.lookup("b",secondEnv);
        })()).to.eql(
          3
        );
        expect(((_) => {
          // 空の辞書を作成する
          var initEnv = env.empty;
          // var x = 1 を実行して、辞書を拡張する
          var xEnv = env.extend("x", 1, initEnv);
          // var z = 2 を実行して、辞書を拡張する
          var zEnv = env.extend("z", 2, xEnv);
          // 内部のスコープで var x = 3 を実行して、辞書を拡張する
          var xEnvInner = env.extend("x",3, zEnv);
          // 内部のスコープで var y = 4 を実行して、辞書を拡張する
          var innerMostEnv = env.extend("y",4, xEnvInner);
          // 一番内側のスコープを利用して x + y + z を計算する
          return env.lookup("x",innerMostEnv) + env.lookup("y",innerMostEnv) + env.lookup("z",innerMostEnv) ;
        })()).to.eql(
          3 + 4 + 2
        );
        /* #@range_end(environment_test) */
        next();
      });
    });
    describe('プログラムの構成要素(式と値)を作る', () => {
      // ## 値の代数的データ構造
      /* #@range_begin(value_algaraic_datatype) */
      // var value = {
      //   number : (numberValue) => {
      //     expect(numberValue).to.a('number');
      //     return () => {
      //       return numberValue;
      //     };
      //   },
      //   closure: (lambdaExpression) => {
      //     expect(lambdaExpression).to.a('function');
      //     return () => {
      //       return lambdaExpression;
      //     };
      //   },
      //   // 補助関数
      //   match : (data, pattern) => {
      //     return data.call(value, pattern);
      //   }
      // };
      /* #@range_end(value_algaraic_datatype) */
      // ## 式の代数的データ構造
      var exp = {
      /* #@range_begin(expression_algebraic_datatype) */
        // 式のパターンマッチ関数
        match : (data, pattern) => {
          return data(pattern);
        },
        // 数値の式
        num: (value) => {
          expect(value).to.a('number');
          return (pattern) => {
            return pattern.num(value);
          };
        },
        // 変数の式
        variable : (name) => {
          return (pattern) => {
            return pattern.variable(name);
          };
        },
        // 関数定義の式(λ式)
        lambda : (variable, body) => {
          return (pattern) => {
            return pattern.lambda(variable, body);
          };
        },
        // 関数適用の式
        app : (variable, arg) => {
          return (pattern) => {
            return pattern.app(variable, arg);
          };
        },
      /* #@range_end(expression_algebraic_datatype) */
      /* #@range_begin(expression_arithmetic) */
        add : (exp1,exp2) => {
          return (pattern) => {
            return pattern.add(exp1, exp2);
          };
        },
        // mul : (exp1,exp2) => {
        //   return (pattern) => {
        //     return pattern.mul(exp1, exp2);
        //   };
        // },
        div : (exp1,exp2) => {
          return (pattern) => {
            return pattern.div(exp1, exp2);
          };
        }
      /* #@range_end(expression_arithmetic) */
      };
      describe('式をテストする', () => {
        it("\\x.\\y.x", (next) => {
          // λx.λy.x
          exp.match(exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("x"))),{
            lambda: (variable, arg) => {
              expect(
                variable
              ).to.a('function');
            }
          });
          next();
        });
      });
      describe('モナド的評価器を作る', () => {
        var emptyEnv = env.empty;
        describe('恒等モナド的評価器を作る', () => {
          // ~~~haskell
          // eval :: Monad m => Exp -> m Int
          // eval (Const x) = return x
          // eval (Div t u) = do { x <- eval t
          //                       y <- eval u
          //                       return (x div y)}
          // ~~~
          /* #@range_begin(identity_monad) */
          var ID = {
            unit: (value) => {
              return value;
            },
            flatMap: (instance) => {
              return (transform) => {
                expect(transform).to.a('function');
                return transform(instance);
              };
            }
          };
          /* #@range_end(identity_monad) */
          /* #@range_begin(identity_monad_evaluator) */
          // evaluate:: (EXP, ENV) => ID[VALUE]
          var evaluate = (anExp, environment) => {
            return exp.match(anExp,{
              /* 数値の評価 */
              num: (numericValue) => {
                return ID.unit(numericValue);
              },
              /* 変数の評価 */
              variable: (name) => {
                return env.lookup(name, environment);
              },
              /* λ式の評価 */
              lambda: (variable, body) => {
                /* クロージャーを返す */
                return exp.match(variable,{
                  variable: (name) => {
                    return ID.unit((actualArg) => {
                      return evaluate(body, env.extend(name, actualArg, environment));
                    });
                  }
                });
              },
              /* 関数適用の評価 */
              app: (lambda, arg) => {
                return ID.flatMap(evaluate(lambda, environment))((closure) => {
                  return ID.flatMap(evaluate(arg, environment))((actualArg) => {
                    return ID.unit(closure(actualArg)); 
                  });
                });
              },
              /* 足し算の評価 */
              add: (expL, expR) => {
                return ID.flatMap(evaluate(expL, environment))((valueR) => {
                  return ID.flatMap(evaluate(expR, environment))((valueL) => {
                    return ID.unit(valueL + valueR); 
                  });
                });
              },
              /* かけ算の評価 */
              // mul: (expL, expR) => {
              //   return ID.flatMap(evaluate(expL, environment))((valueR) => {
              //     return ID.flatMap(evaluate(expR, environment))((valueL) => {
              //       return ID.unit(valueL * valueR); 
              //     });
              //   });
              // },
              // /* 割り算の評価 */
              // div: (expL, expR) => {
              //   return ID.flatMap(evaluate(expL, environment))((valueR) => {
              //     return ID.flatMap(evaluate(expR, environment))((valueL) => {
              //       return ID.unit(valueL / valueR); 
              //     });
              //   });
              // }
            });
          };
          /* #@range_end(identity_monad_evaluator) */
          it('ID評価器で数値を評価する', (next) => {
            /* #@range_begin(number_evaluation_test) */
            expect(
              evaluate(exp.num(2), emptyEnv)
            ).to.be(
              ID.unit(2)
            );
            /* #@range_end(number_evaluation_test) */
            next();
          });
          it('ID評価器で演算を評価する', (next) => {
            expect(
              evaluate(exp.add(exp.num(1),exp.num(2)), emptyEnv)
            ).to.be(
              ID.unit(3)
            );
            // expect(
            //   evaluate(exp.mul(exp.num(2),exp.num(3)), emptyEnv)
            // ).to.be(
            //   ID.unit(6)
            // );
            next();
          });
          it('ID評価器で変数を評価する', (next) => {
            /* #@range_begin(variable_evaluation_test) */
            var newEnv = env.extend("x",1, emptyEnv);
            expect(
              evaluate(exp.variable("x"), newEnv)
            ).to.be(
              ID.unit(1)
            );
            expect(
              evaluate(exp.variable("y"), newEnv)
            ).to.be(
              ID.unit(undefined)
            );
            /* #@range_end(variable_evaluation_test) */
            next();
          });
          it('ID評価器で関数を評価する', (next) => {
            // \x.x
            var expression = exp.lambda(exp.variable("x"),
                                                        exp.variable("x"));
            expect(
              evaluate(expression, emptyEnv)(1)
            ).to.be(
              1
            );
            next();
          });
          it('ID評価器で関数適用を評価する', (next) => {
            /* #@range_begin(application_evaluation_test) */
            // \x.add(x,x)(2)
            var expression = exp.app(exp.lambda(exp.variable("x"),
                                                        exp.add(exp.variable("x"),exp.variable("x"))),
                                             exp.num(2));
            expect(
              evaluate(expression, emptyEnv)
            ).to.be(
              4
            );
            /* #@range_end(application_evaluation_test) */
            next();
          });
          it('ID評価器で高階関数を評価する', (next) => {
            /* #@range_begin(curried_function_evaluation_test) */
            // (\x.
            //    \y.
            //       x+y)(2)(3)
            var expression = exp.app(
              exp.app(exp.lambda(exp.variable("x"),
                                         exp.lambda(exp.variable("y"),
                                                    exp.add(exp.variable("x"),exp.variable("y")))),
                              exp.num(2)),
              exp.num(3));
            expect(
              evaluate(expression, emptyEnv)
            ).to.be(
              5
            );
            /* #@range_end(curried_function_evaluation_test) */
            next();
          });
        });
        describe('ログ出力用評価器を作る', () => {
          /* #@range_begin(expression_logger_interpreter) */
          var exp = {
            log: (anExp) => { // ログ出力用の式
              expect(anExp).to.a('function');
              return (pattern) => {
                return pattern.log(anExp);
              };
            },
            /* #@range_end(expression_logger_interpreter) */
            match : (data, pattern) => {
              return data.call(exp, pattern);
            },
            number : (value) => {
              expect(value).to.a('number');
              return (pattern) => {
                return pattern.number(value);
              };
            },
            closure: (lambdaExpssion) => {
              expect(lambdaExpssion).to.a('function');
              return (pattern) => {
                return pattern.closure(lambdaExpssion);
              };
            },
            variable : (name) => {
              expect(name).to.a('string');
              return (pattern) => {
                return pattern.variable(name);
              };
            },
            lambda : (variable, body) => {
              expect(variable).to.a('function');
              expect(body).to.a('function');
              return (pattern) => {
                return pattern.lambda(variable, body);
              };
            },
            app : (variable, arg) => {
              return (pattern) => {
                return pattern.app(variable, arg);
              };
            },
            add : (exp1,exp2) => {
              return (pattern) => {
                return pattern.add(exp1, exp2);
              };
            },
            mul : (exp1,exp2) => {
              return (pattern) => {
                return pattern.mul(exp1, exp2);
              };
            },
            div : (exp1,exp2) => {
              return (pattern) => {
                return pattern.div(exp1, exp2);
              };
            }
          };
          /* #@range_begin(logger_monad) */
          var LOG = {
            unit: (value) => {
              return pair.cons(value, list.empty());
            },
            flatMap: (instance) => {
              return (transform) => {
                expect(transform).to.a('function');
                return pair.match(instance,{
                  cons: (value, message) => {
                    var newInstance = transform(value);
                    return pair.cons(
                      pair.left(newInstance),
                      list.append(message)(pair.right(newInstance)));
                  }
                });
              };
            },
            // 引数 value をログに格納する
            output: (value) => {
              return pair.cons(null, list.unit(String(value)));
            }
          };
          /* #@range_end(logger_monad) */
          /* #@range_begin(logger_monad_evaluator) */
          // evaluate:: (EXP, ENV) => ID[NUM]
          var evaluate = (anExp, environment) => {
            return exp.match(anExp,{
              log: (anExp) => {
                return LOG.flatMap(evaluate(anExp, environment))((value) => {
                  return LOG.flatMap(LOG.output(value))((_) => {
                    return LOG.unit(value); 
                  });
                });
              },
          /* #@range_end(logger_monad_evaluator) */
              /* 数値の評価 */
              number: (value) => {
                return LOG.unit(value);
              },
              /* 変数の評価 */
              variable: (name) => {
                return LOG.unit(env.lookup(name, environment));
              },
              /* λ式の評価 */
              lambda: (variable, bodyExp) => {
                return LOG.unit(exp.closure((arg) => { /* クロージャーを返す */
                  return exp.match(variable,{ 
                    variable: (name) => {
                      return evaluate(bodyExp, env.extend(name, arg ,environment));
                    }
                  });
                }));
              },
              /* 関数適用の評価 */
              app: (func, arg) => {
                return LOG.flatMap(evaluate(func, environment))((closure) => {
                  return LOG.flatMap(evaluate(arg, environment))((actualArg) => {
                    return exp.match(closure,{ 
                      closure: (lambdaExpssion) => {
                        return lambdaExpssion(actualArg);
                      }
                    });
                  });
                });
              },
              add: (expL, expR) => {
                return LOG.flatMap(evaluate(expL, environment))((valueR) => {
                  return LOG.flatMap(evaluate(expR, environment))((valueL) => {
                    return LOG.unit(valueL + valueR); 
                  });
                });
              },
              mul: (expL, expR) => {
                return LOG.flatMap(evaluate(expL, environment))((valueR) => {
                  return LOG.flatMap(evaluate(expR, environment))((valueL) => {
                    return LOG.unit(valueL * valueR); 
                  });
                });
              },
              div: (expL, expR) => {
                return LOG.flatMap(evaluate(expL, environment))((valueR) => {
                  return LOG.flatMap(evaluate(expR, environment))((valueL) => {
                    return LOG.unit(valueL / valueR); 
                  });
                });
              }
            });
          };
          it('LOG評価器で数値を評価する', (next) => {
            /* #@range_begin(log_interpreter_number) */
            pair.match(evaluate(exp.log(exp.number(2)), emptyEnv),{
              cons: (value, log) => {
                expect( // 結果の値をテストする
                  value
                ).to.be(
                  2
                );
                expect( // 保存されたログを見る
                  list.toArray(log)
                ).to.eql(
                  [2]
                );
              }
            });
            /* #@range_end(log_interpreter_number) */
            // expect(
            //   pair.right(evaluate(
            //     exp.log(exp.number(2)), 
            //     emptyEnv))
            // ).to.be(
            //   2
            // );
            // expect(
            //   list.toArray(pair.left(evaluate(exp.log(exp.number(2)), emptyEnv)))
            // ).to.eql(
            //   [2]
            // );
            next();
          });
          it('LOG評価器で演算を評価する', (next) => {
            /* #@range_begin(log_interpreter_evaluation_strategy) */
            pair.match(evaluate(exp.log(exp.add(exp.number(1),exp.number(2))), emptyEnv),{
              cons: (value, log) => {
                expect(
                  value
                ).to.be(
                  3
                );
                expect(
                  list.toArray(log)
                ).to.eql(
                  [3]
                );
              }
            });
            pair.match(evaluate(exp.log(exp.add(exp.log(exp.number(1)),exp.log(exp.number(2)))), emptyEnv),{
              cons: (value, log) => {
                expect(
                  value
                ).to.be(
                  3
                );
                expect(
                  list.toArray(log)
                ).to.eql(
                  [1,2,3]
                );
              }
            });
            /* #@range_end(log_interpreter_evaluation_strategy) */
            next();
          });
          it('LOG評価器で変数を評価する', (next) => {
            var newEnv = env.extend("x",1, emptyEnv);
            expect(
              pair.left(evaluate(exp.variable("x"), newEnv))
            ).to.be(
              1
            );
            expect(
              pair.left(evaluate(exp.variable("y"), newEnv))
            ).to.be(
              undefined
            );
            next();
          });
          it('LOG評価器で関数適用を評価する', (next) => {
            // \x.add(x,x)(2)
            var expression = exp.app(exp.lambda(exp.variable("x"),
                                                        exp.add(exp.variable("x"),exp.variable("x"))),
                                             exp.number(2));
            expect(
              pair.left(evaluate(expression, emptyEnv))
            ).to.be(
              4
            );
            next();
          });
          it('LOG評価器でlogを評価する', (next) => {
            // (\x.
            //    \y.
            //       x*y)(2)(3)
            // 
            var expression = exp.app(
              exp.app(exp.lambda(exp.variable("x"),
                                         exp.lambda(exp.variable("y"),
                                                    exp.mul(exp.variable("x"),exp.variable("y")))),
                              exp.number(2)),
              exp.number(3));
            expect(
              pair.left(evaluate(expression, emptyEnv))
            ).to.be(
              6
            );
            next();
          });
        });
      });
      //     it('ブール型を評価する', (next) => {
      //       /* λx.λy.x */
      //       var trueFun = exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("x")));
      //       /* λx.λy.y */
      //       var falseFun = exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("y")));
      //       var not = exp.lambda(exp.variable("x"),
      //                        exp.app(
      //                          exp.app(
      //                            exp.variable("x"),falseFun),
      //                          trueFun));
      //       var and = exp.lambda(exp.variable("x"),
      //                        exp.lambda(exp.variable("y"),
      //                               exp.app(
      //                                 exp.app(exp.variable("x"),exp.variable("y")),
      //                                 falseFun)));
      //       var or = exp.lambda(exp.variable("x"),
      //                       exp.lambda(exp.variable("y"),
      //                              exp.app(
      //                                exp.app(exp.variable("x"),trueFun),
      //                                exp.variable("y"))));
      //       var cond = exp.lambda(exp.variable("pred"),
      //                         exp.lambda(exp.variable("x"),
      //                                exp.lambda(exp.variable("y"),
      //                                       exp.app(
      //                                         exp.app(exp.variable("pred"),exp.variable("x")),exp.variable("y")))));
      //       expect(
      //         evaluate(
      //           exp.app(
      //             exp.app(trueFun,exp.number(1)),
      //             exp.number(0)),
      //           emptyEnv)
      //       ).to.eql(
      //         1
      //       );
      //       expect(
      //         evaluate(
      //           exp.app(
      //             exp.app(
      //               exp.app(not, trueFun), exp.number(1)), exp.number(0)),emptyEnv)
      //       ).to.eql(
      //         0
      //       );
      //       expect(
      //         evaluate(
      //           exp.app(
      //             exp.app(
      //               exp.app(
      //                 exp.app(and,
      //                             trueFun),
      //                 trueFun),
      //               exp.number(1)),
      //             exp.number(0)),emptyEnv)
      //       ).to.be(
      //         1
      //       );
      //       expect(
      //         evaluate(
      //           exp.app(
      //             exp.app(
      //               exp.app(
      //                 exp.app(
      //                   exp.app(cond, trueFun),
      //                   falseFun),
      //                 trueFun),
      //               exp.number(1)),
      //             exp.number(0)),
      //           emptyEnv)
      //       ).to.eql(
      //         0
      //       );
      //       next();
      //     });
      //   });
      //   describe('プリティプリンタを作る', () => {
      //     var prettyPrint = (anExp) => {
      //       return exp.match(anExp,{
      //         /* 数値 */
      //         number: (value) => {
      //           return {
      //             number: value
      //           };
      //         },
      //         /* 変数 */
      //         variable: (name) => {
      //           return {
      //             variable: name
      //           };
      //         },
      //         /* λ式 */
      //         lambda: (variable, bodyExp) => {
      //           return {
      //             lambda: {
      //               variable: variable,
      //               bodyExp: prettyPrint(bodyExp)
      //             }
      //           };
      //         },
      //         /* 関数適用評価 */
      //         app: (variable, arg) => {
      //           return {
      //             app: {
      //               variable: variable,
      //               arg: prettyPrint(arg)
      //             }
      //           };
      //         }
      //       });
      //     };
      //     it('prettyPrintをテストする', (next) => {
      //       expect(
      //         prettyPrint(exp.number(2))
      //       ).to.eql(
      //         {"number" : 2}
      //       );
      //       next();
      //     });
      //     describe('ローダーを作る', () => {
      //       var load = (object) => {
      //         //            return (callback) => {
      //         for (var key in object) {
      //           switch (key){
      //           case "number":
      //             return exp.number(parseInt(object[key],10));
      //             break;
      //           default:
      //             throw new Error();
      //             break;
      //           }
      //         }
      //         //            };
      //       };
      //       it('ローダーをテストする', (next) => {
      //         expect(
      //           evaluate(load({"number" : 2}, emptyEnv))
      //         ).to.eql(
      //           2
      //         );
      //         next();
      //       });
      //   // describe('ファイル操作', () => {
      //   //   it('数値を評価する', (next) => {
      //   //     fs.writeFileSync('/tmp/nodejs-labo-test.json',  JSON.stringify(number(2), null, '    '));
      //   //     expect(
      //   //       JSON.parse(fs.readFileSync('/tmp/nodejs-labo-test.json', 'utf8'))
      //   //     ).to.eql(
      //   //       2
      //   //     );
      //   //     next();
      //   //   });
      //   // });
      // });
    }); // 
    describe('例外捕捉評価器', () => {
      // ## 式の代数的データ構造
      /* #@range_begin(continuation_passing_interpreter_expression) */
      var exp = {
        match : (data, pattern) => {
          return data.call(exp, pattern);
        },
        exception: (message) => {
          return (pattern) => {
            return pattern.exception(message);
          };
        },
        raise: (exception) => {
          return (pattern) => {
            return pattern.raise(exception);
          };
        },
        tryWith: (anExp, exception, raisedExp) => {
          return (pattern) => {
            return pattern.tryWith(anExp, exception, raisedExp);
          };
        },
        /* #@range_end(continuation_passing_interpreter_expression) */
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
      /* #@range_begin(continuation_passing_interpreter_evaluate) */
      // evaluateCPS: (EXP, ENV, FUNC[VALUE -> VALUE]) -> VALUE
      var evaluateCPS = (anExp, environment, continues, continuesInFailure) => {
        // c.f. Programming Language Concepts, p.208
        return exp.match(anExp,{
          /* 例外の評価 */
          raise: (exception) => {
            return continuesInFailure(exception);
          },
          tryWith: (anExp, caughtException, failSafeExp) => {
            var newContinuesInFailure = (thrownException) => {
              if (thrownException.message === caughtException.message) {
                return evaluateCPS(failSafeExp, environment, continues, continuesInFailure);
              } else {
                return continuesInFailure(thrownException);
              }
            };
            return evaluateCPS(anExp, environment, continues, newContinuesInFailure);
          },
          /* 数値の評価 */
          number: (answer) => {
            return continues(answer);
          },
          /* 変数の評価 */
          variable: (name) => {
            var found = env.lookup(name, environment);
            if(found === undefined){
              return continuesInFailure(new Error(name + " not found"));
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
                  return continuesInFailure(new Error("lambdaの引数が数値になっています"));
                },
                lambda: ($$,$$$) => {
                  return continuesInFailure(new Error("lambdaの引数がlambbaになっています"));
                }
              });
            };
          },
          /* 関数適用の評価 */
          app: (anExp, arg) => {
            var rator = evaluateCPS(anExp, environment, continues, continuesInFailure);
            var rand = evaluateCPS(arg, environment, continues, continuesInFailure);
            return continues(rator(rand));
          }
        });
        /* #@range_end(continuation_passing_interpreter_evaluate) */
      };
      describe('例外捕捉評価器をテストする', () => {
        var emptyEnv = env.empty;
        var continuesNormally = (result) => {
          return result;
        };
        var continuesAbnormally = (exception) => {
          return exception;
        };
        it('数値を評価する', (next) => {
          expect(
            evaluateCPS(exp.number(2), emptyEnv, continuesNormally, continuesAbnormally)
          ).to.eql(2);
          next();
        });
        it('変数を評価する', (next) => {
          var newEnv = env.extend("x",1, emptyEnv, continuesNormally, continuesAbnormally);
          expect(
            evaluateCPS(exp.variable("x"), newEnv, continuesNormally, continuesAbnormally)
          ).to.eql(
            1
          );
          // 自由変数の場合は、 例外が返る
          expect(
            evaluateCPS(exp.variable("y"), newEnv, continuesNormally, continuesAbnormally)
          ).to.eql(
            new Error("y not found")
          );
          next();
        });
        it('constant関数', (next) => {
          var constant = exp.lambda(exp.variable("x"),exp.number(1));
          expect(
            evaluateCPS(constant, emptyEnv, continuesNormally, continuesAbnormally)
          ).to.a(
            'function'
          );
          // (λx.1)(2)
          var applied = exp.app(constant, exp.number(2));
          expect(
            evaluateCPS(applied, emptyEnv, continuesNormally, continuesAbnormally)
          ).to.eql(
            1
          );
          next();
        });
        it('identity関数をテストする', (next) => {
          /* λx.x */
          var identity = exp.lambda(exp.variable("x"),exp.variable("x"));
          expect(
            evaluateCPS(identity, emptyEnv, continuesNormally, continuesAbnormally)
          ).to.a(
            'function'
          );
          // (λx.x)(1) = 1 */
          var appliedExpression = exp.app(identity, exp.number(1));
          expect(
            evaluateCPS(appliedExpression, emptyEnv, continuesNormally, continuesAbnormally)
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
              emptyEnv,
              continuesNormally,
              continuesAbnormally)
          ).to.eql(
            1
          );
          // (λx.λy.x)(1)(z) = 1
          expect(
            evaluateCPS(
              exp.app(
                exp.app(trueFun,exp.number(1)),
                exp.variable("z")),
              emptyEnv,
              continuesNormally,
              continuesAbnormally)
          ).to.eql(
            1
          );
          // (λx.λy.x)(z)(0) = error
          expect(
            evaluateCPS(
              exp.app(
                exp.app(trueFun,exp.variable("z")),
                exp.number(0)),
              emptyEnv,
              continuesNormally, 
              continuesAbnormally)
          ).to.eql(
            new Error("z not found")
          );
          next();
        });
        it('投げられた例外を捕捉する', (next) => {
          /* #@range_begin(continuation_passing_interpreter_trycatch) */
          var tryExpression = exp.tryWith(
            exp.raise(new Error("exception")), // exp
            new Error("exception"), // caughtException
            exp.number(1) // failSafeExp
          );
          expect(
            evaluateCPS(tryExpression, emptyEnv, continuesNormally, continuesAbnormally)
          ).to.eql(
            1
          );
          // (λx.tryWith(raise, exception , 1))(0) = 1
          var catchException = exp.app(exp.lambda(exp.variable("x"),
                                                          exp.tryWith(
                                                            exp.raise(new Error("exception")),
                                                            new Error("exception"),
                                                            exp.number(1)
                                                          )),
                                               exp.number(0));
          expect(
            evaluateCPS(catchException, emptyEnv, continuesNormally, continuesAbnormally)
          ).to.eql(
            1
          );
          /* #@range_end(continuation_passing_interpreter_trycatch) */
          next();
        });
      });
    });
  });
});


