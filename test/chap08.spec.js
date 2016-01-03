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
    // 空の環境
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
      /* #@range_begin(expression_algaraic_datatype) */
      var exp = {
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
        application : (variable, arg) => {
          return (pattern) => {
            return pattern.application(variable, arg);
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
      /* #@range_end(expression_algaraic_datatype) */
      describe('モナド的評価器を作る', () => {
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
          // evaluate:: (EXP, ENV) => ID[NUM]
          var evaluate = (anExp, env) => {
            return exp.match(anExp,{
              /* 数値の評価 */
              number: (value) => {
                return ID.unit(value);
              },
              /* 変数の評価 */
              variable: (name) => {
                return ID.unit(lookupEnv(name, env));
              },
              /* λ式の評価 */
              lambda: (variable, bodyExp) => {
                return ID.unit(exp.closure((arg) => { /* クロージャーを返す */
                  return exp.match(variable,{ 
                    variable: (name) => {
                      return evaluate(bodyExp, extendEnv(name, arg ,env));
                    }
                  });
                }));
              },
              /* 関数適用の評価 */
              application: (func, arg) => {
                return ID.flatMap(evaluate(func, env))((closure) => {
                  return ID.flatMap(evaluate(arg, env))((actualArg) => {
                    return exp.match(closure,{ 
                      closure: (lambdaExpssion) => {
                        return lambdaExpssion(actualArg);
                      }
                    });
                  });
                });
              },
              add: (expL, expR) => {
                return ID.flatMap(evaluate(expL, env))((valueR) => {
                  return ID.flatMap(evaluate(expR, env))((valueL) => {
                    return ID.unit(valueL + valueR); 
                  });
                });
              },
              mul: (expL, expR) => {
                return ID.flatMap(evaluate(expL, env))((valueR) => {
                  return ID.flatMap(evaluate(expR, env))((valueL) => {
                    return ID.unit(valueL * valueR); 
                  });
                });
              },
              div: (expL, expR) => {
                return ID.flatMap(evaluate(expL, env))((valueR) => {
                  return ID.flatMap(evaluate(expR, env))((valueL) => {
                    return ID.unit(valueL / valueR); 
                  });
                });
              }
            });
          };
          /* #@range_end(identity_monad_evaluator) */
          it('ID評価器で数値を評価する', (next) => {
            expect(
              evaluate(exp.number(2), emptyEnv)
            ).to.be(
              2
            );
            next();
          });
          it('ID評価器で演算を評価する', (next) => {
            expect(
              evaluate(exp.add(exp.number(1),exp.number(2)), emptyEnv)
            ).to.be(
              3
            );
            expect(
              evaluate(exp.mul(exp.number(2),exp.number(3)), emptyEnv)
            ).to.be(
              6
            );
            next();
          });
          it('ID評価器で変数を評価する', (next) => {
            var env = extendEnv("x",1, emptyEnv);
            expect(
              evaluate(exp.variable("x"), env)
            ).to.be(
              1
            );
            expect(
              evaluate(exp.variable("y"), env)
            ).to.be(
              undefined
            );
            next();
          });
          it('ID評価器で関数適用を評価する', (next) => {
            // \x.add(x,x)(2)
            var expression = exp.application(exp.lambda(exp.variable("x"),
                                                        exp.add(exp.variable("x"),exp.variable("x"))),
                                             exp.number(2));
            expect(
              evaluate(expression, emptyEnv)
            ).to.be(
              4
            );
            next();
          });
          it('ID評価器で高階関数を評価する', (next) => {
            // (\x.
            //    \y.
            //       x*y)(2)(3)
            // 
            var expression = exp.application(
              exp.application(exp.lambda(exp.variable("x"),
                                         exp.lambda(exp.variable("y"),
                                                    exp.mul(exp.variable("x"),exp.variable("y")))),
                              exp.number(2)),
              exp.number(3));
            expect(
              evaluate(expression, emptyEnv)
            ).to.be(
              6
            );
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
            application : (variable, arg) => {
              return (pattern) => {
                return pattern.application(variable, arg);
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
              return pair.cons(list.empty(),value);
            },
            flatMap: (instance) => {
              return (transform) => {
                expect(transform).to.a('function');
                return pair.match(instance,{
                  cons: (message, value) => {
                    var newInstance = transform(value);
                    return pair.cons(
                      list.append(message)(pair.left(newInstance)),
                      pair.right(newInstance));
                  }
                });
                // var currentLogMessage = pair.left(instance);
                // var currentValue = pair.right(instance);
                // var newPair = transform(currentValue);
                // return pair.cons(
                //   list.append(currentLogMessage)(pair.left(newPair)),
                //   pair.right(newPair));
              };
            },
            output: (value) => {
              return pair.cons(list.unit(String(value)), null);
              // return pair.cons(list.unit(value), null);
            }
          };
          /* #@range_end(logger_monad) */
          /* #@range_begin(logger_monad_evaluator) */
          // evaluate:: (EXP, ENV) => ID[NUM]
          var evaluate = (anExp, env) => {
            return exp.match(anExp,{
              log: (anExp) => {
                return LOG.flatMap(evaluate(anExp, env))((value) => {
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
                return LOG.unit(lookupEnv(name, env));
              },
              /* λ式の評価 */
              lambda: (variable, bodyExp) => {
                /* クロージャーを返す */
                return LOG.unit(exp.closure((arg) => { /* クロージャーを返す */
                  return exp.match(variable,{ 
                    variable: (name) => {
                      return evaluate(bodyExp, extendEnv(name, arg ,env));
                    }
                  });
                }));
              },
              /* 関数適用の評価 */
              application: (func, arg) => {
                return LOG.flatMap(evaluate(func, env))((closure) => {
                  return LOG.flatMap(evaluate(arg, env))((actualArg) => {
                    return exp.match(closure,{ 
                      closure: (lambdaExpssion) => {
                        return lambdaExpssion(actualArg);
                      }
                    });
                  });
                });
              },
              add: (expL, expR) => {
                return LOG.flatMap(evaluate(expL, env))((valueR) => {
                  return LOG.flatMap(evaluate(expR, env))((valueL) => {
                    return LOG.unit(valueL + valueR); 
                  });
                });
              },
              mul: (expL, expR) => {
                return LOG.flatMap(evaluate(expL, env))((valueR) => {
                  return LOG.flatMap(evaluate(expR, env))((valueL) => {
                    return LOG.unit(valueL * valueR); 
                  });
                });
              },
              div: (expL, expR) => {
                return LOG.flatMap(evaluate(expL, env))((valueR) => {
                  return LOG.flatMap(evaluate(expR, env))((valueL) => {
                    return LOG.unit(valueL / valueR); 
                  });
                });
              }
            });
          };
          it('LOG評価器で数値を評価する', (next) => {
            /* #@range_begin(log_interpreter_number) */
            pair.match(evaluate(exp.log(exp.number(2)), emptyEnv),{
              cons: (log, value) => {
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
              cons: (log, value) => {
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
              cons: (log, value) => {
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
            // expect(
            //   pair.right(evaluate(exp.add(exp.number(1),exp.number(2)), emptyEnv))
            // ).to.be(
            //   3
            // );
            // expect(
            //   pair.right(evaluate(exp.mul(exp.number(2),exp.number(3)), emptyEnv))
            // ).to.be(
            //   6
            // );
            next();
          });
          it('LOG評価器で変数を評価する', (next) => {
            var env = extendEnv("x",1, emptyEnv);
            expect(
              pair.right(evaluate(exp.variable("x"), env))
            ).to.be(
              1
            );
            expect(
              pair.right(evaluate(exp.variable("y"), env))
            ).to.be(
              undefined
            );
            next();
          });
          it('LOG評価器で関数適用を評価する', (next) => {
            // \x.add(x,x)(2)
            var expression = exp.application(exp.lambda(exp.variable("x"),
                                                        exp.add(exp.variable("x"),exp.variable("x"))),
                                             exp.number(2));
            expect(
              pair.right(evaluate(expression, emptyEnv))
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
            var expression = exp.application(
              exp.application(exp.lambda(exp.variable("x"),
                                         exp.lambda(exp.variable("y"),
                                                    exp.mul(exp.variable("x"),exp.variable("y")))),
                              exp.number(2)),
              exp.number(3));
            expect(
              pair.right(evaluate(expression, emptyEnv))
            ).to.be(
              6
            );
            next();
          });
        });
      });
      describe('評価器を作る', () => {
        // ## 式の評価関数
        /* #@range_begin(evaluation_function) */
        var evaluate = (anExp, env) => {
          return exp.match(anExp,{
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
                return exp.match(variable,{ // maybeを返すべきか？
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
            },
            add: (exp1, exp2) => {
              return exp.number(evaluate(exp1, env) + evaluate(exp2, env));
            },
            mul: (exp1, exp2) => {
              return exp.number(evaluate(exp1, env) * evaluate(exp2, env));
            }
          });
        };
        /* #@range_end(evaluation_function) */
        describe('evaluate関数で式を評価する', () => {
          it('数値を評価する', (next) => {
            expect(
              evaluate(exp.number(2), emptyEnv)
            ).to.be(
              2
            );
            next();
          });
          // it('演算子を評価する', (next) => {
          //   expect(
          //     evaluate(add(number(2),number(3), emptyEnv))
          //   ).to.be(
          //     2
          //   );
          //   next();
          // });
          it('変数を評価する', (next) => {
            var env = extendEnv("x",1, emptyEnv);
            expect(
              evaluate(exp.variable("x"), env)
            ).to.be(
              1
            );
            expect(
              evaluate(exp.variable("y"), env)
            ).to.be(
              undefined
            );
            next();
          });
          it('constant関数', (next) => {
            // λx.1
            var constant = exp.lambda(exp.variable("x"),exp.number(1));
            expect(
              evaluate(constant, emptyEnv)
            ).to.a(
              'function'
            );
            var applied = exp.application(constant, exp.variable("y"));
            expect(
              evaluate(applied, emptyEnv)
            ).to.eql(
              1
            );
            next();
          });
          it('identity関数', (next) => {
            /* λx.x */
            var identity = exp.lambda(exp.variable("x"),exp.variable("x"));
            expect(
              evaluate(identity, emptyEnv)
            ).to.a(
              'function'
            );
            expect(
              evaluate(exp.application(identity, exp.number(1)), emptyEnv)
            ).to.eql(
              1
            );
            next();
          });
          // it('例題', (next) => {
          //   /* add(1,2) == 3 */
            
          //   expect(
          //     evaluate(application(identity, number(1)), emptyEnv)
          //   ).to.eql(
          //     1
          //   );
          //   next();
          // });
          it('ブール型を評価する', (next) => {
            /* λx.λy.x */
            var trueFun = exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("x")));
            /* λx.λy.y */
            var falseFun = exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("y")));
            var not = exp.lambda(exp.variable("x"),
                             exp.application(
                               exp.application(
                                 exp.variable("x"),falseFun),
                               trueFun));
            var and = exp.lambda(exp.variable("x"),
                             exp.lambda(exp.variable("y"),
                                    exp.application(
                                      exp.application(exp.variable("x"),exp.variable("y")),
                                      falseFun)));
            var or = exp.lambda(exp.variable("x"),
                            exp.lambda(exp.variable("y"),
                                   exp.application(
                                     exp.application(exp.variable("x"),trueFun),
                                     exp.variable("y"))));
            var cond = exp.lambda(exp.variable("pred"),
                              exp.lambda(exp.variable("x"),
                                     exp.lambda(exp.variable("y"),
                                            exp.application(
                                              exp.application(exp.variable("pred"),exp.variable("x")),exp.variable("y")))));
            expect(
              evaluate(
                exp.application(
                  exp.application(trueFun,exp.number(1)),
                  exp.number(0)),
                emptyEnv)
            ).to.eql(
              1
            );
            expect(
              evaluate(
                exp.application(
                  exp.application(
                    exp.application(not, trueFun), exp.number(1)), exp.number(0)),emptyEnv)
            ).to.eql(
              0
            );
            expect(
              evaluate(
                exp.application(
                  exp.application(
                    exp.application(
                      exp.application(and,
                                  trueFun),
                      trueFun),
                    exp.number(1)),
                  exp.number(0)),emptyEnv)
            ).to.be(
              1
            );
            expect(
              evaluate(
                exp.application(
                  exp.application(
                    exp.application(
                      exp.application(
                        exp.application(cond, trueFun),
                        falseFun),
                      trueFun),
                    exp.number(1)),
                  exp.number(0)),
                emptyEnv)
            ).to.eql(
              0
            );
            next();
          });
        });
        describe('プリティプリンタを作る', () => {
          var prettyPrint = (anExp) => {
            return exp.match(anExp,{
              /* 数値 */
              number: (value) => {
                return {
                  number: value
                };
              },
              /* 変数 */
              variable: (name) => {
                return {
                  variable: name
                };
              },
              /* λ式 */
              lambda: (variable, bodyExp) => {
                return {
                  lambda: {
                    variable: variable,
                    bodyExp: prettyPrint(bodyExp)
                  }
                };
              },
              /* 関数適用評価 */
              application: (variable, arg) => {
                return {
                  application: {
                    variable: variable,
                    arg: prettyPrint(arg)
                  }
                };
              }
            });
          };
          it('prettyPrintをテストする', (next) => {
            expect(
              prettyPrint(exp.number(2))
            ).to.eql(
              {"number" : 2}
            );
            next();
          });
          describe('ローダーを作る', () => {
            var load = (object) => {
              //            return (callback) => {
              for (var key in object) {
                switch (key){
                case "number":
                  return exp.number(parseInt(object[key],10));
                  break;
                default:
                  throw new Error();
                  break;
                }
              }
              //            };
            };
            it('ローダーをテストする', (next) => {
              expect(
                evaluate(load({"number" : 2}, emptyEnv))
              ).to.eql(
                2
              );
              next();
            });
            // var eventSystem = (_) => {
            //   var object = _; 
            //   var listener = [];
            //   return {
            //     on: (key, callback) => { 
            //       return callback(object[key]);
            //     },
            //     listen: (target) => {
            //       object = target;
            //     }
            //   };
            // };
            // var event = eventSystem();
            // event.on("number",(value) => {
            //   return number(value);
            // }, (exp) => {
            
            // });
            // event.on("variable",(value) => {
            //   return variable(value);
            // });
            // event.listen(object);
            // it('ローダーをテストする', (next) => {
            //   expect(
            //     evaluate(event.listen({"number" : 2}, emptyEnv))
            //   ).to.eql(
            //     1
            //   );
            //   next();
            // });
          });
        });
        // describe('ファイル操作', () => {
        //   it('数値を評価する', (next) => {
        //     fs.writeFileSync('/tmp/nodejs-labo-test.json',  JSON.stringify(number(2), null, '    '));
        //     expect(
        //       JSON.parse(fs.readFileSync('/tmp/nodejs-labo-test.json', 'utf8'))
        //     ).to.eql(
        //       2
        //     );
        //     next();
        //   });
        // });
      });
    }); // 
    // describe('式と値を分離した評価器', () => {
    //   // ## 式の代数的データ構造
    //   var exp = {
    //     number: (value) => {
    //       expect(value).to.a('number');
    //       return (pattern) => {
    //         return pattern.number(value);
    //       };
    //     },
    //     variable: (name) => {
    //       expect(name).to.a('string');
    //       return (pattern) => {
    //         return pattern.variable(name);
    //       };
    //     },
    //     lambda: (variable, body) => {
    //       expect(variable).to.a('function');
    //       expect(body).to.a('function');
    //       return (pattern) => {
    //         return pattern.lambda(variable, body);
    //       };
    //     },
    //     application: (variable, arg) => {
    //       return (pattern) => {
    //         return pattern.application(variable, arg);
    //       };
    //     }
    //   }; // exp
    //   // RESULT
    //   var result = {
    //     value: (answer) => {
    //       return (pattern) => {
    //         return pattern.value(answer);
    //       };
    //     },
    //     // closure: (func) => {
    //     //   return (pattern) => {
    //     //  return pattern.closure(func);
    //     //   };
    //     // }
    //   };
    //   var evaluate = (exp, env) => {
    //     return exp.match(exp,{
    //       /* 数値の評価 */
    //       number: (value) => {
    //         return result.value(value);
    //       },
    //       /* 変数の評価 */
    //       variable: (name) => {
    //         return result.value(lookupEnv(name, env));
    //       },
    //       /* λ式の評価 */
    //       lambda: (variable, bodyExp) => {
    //         /* クロージャーを返す */
    //         return result.value((actualArg) => {
    //           return exp.match(variable,{
    //             variable: (name) => {
    //               return exp.match(evaluate(bodyExp, extendEnv(name, actualArg ,env)),{
    //                 value: (value) => {
    //                   return value;
    //                 }
    //               });
    //             }
    //           });
    //         });
    //       },
    //       /* 関数適用の評価 */
    //       application: (func, arg) => {
    //         return exp.match(evaluate(func, env),{
    //           value: (closure) => {
    //             var rand = evaluate(arg, env);
    //             return closure(rand);
    //           }
    //         });
    //       }
    //     });
    //   };
    //   it('数値を評価する', (next) => {
    //     exp.match(evaluate(exp.number(2), emptyEnv), {
    //       value: (answer) => {
    //         expect(answer).to.eql(2);
    //       },
    //       abort: (message) => {
    //         expect().fail();
    //       }
    //     });
    //     next();
    //   });
    //   it('変数を評価する', (next) => {
    //     var env = extendEnv("x",1, emptyEnv);
    //   	exp.match(evaluate(exp.variable("x"), env), {
    //   	  value: (answer) => {
    //   		expect(answer).to.eql(1);
    //   	  }
    //   	});
    //   	// 自由変数の場合
    //   	exp.match(evaluate(exp.variable("y"), env), {
    //   	  value: (answer) => {
    //   		expect(answer).to.eql(undefined);
    //   	  }
    //   	});
    //     next();
    //   });
    //   it('constant関数', (next) => {
    //     // λx.1
    //     var constant = exp.lambda(exp.variable("x"),exp.number(1));
    //     expect(
    //       evaluate(constant, emptyEnv)
    //     ).to.a(
    //       'function'
    //     );
    //     // (λx.1)(2)
    //     var appliedExpression = exp.application(constant, exp.number(2));
    //   	exp.match(evaluate(appliedExpression, emptyEnv), {
    //   	  value: (answer) => {
    //   		expect(answer).to.eql(undefined);
    //   	  }
    //   	});
    //     next();
    //   });
    // }); // 式と値を分離した評価器
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
        application: (variable, arg) => {
          return (pattern) => {
            return pattern.application(variable, arg);
          };
        }
      }; // exp
      // ## 式の評価関数
      // evaluateCPS: (EXP, ENV, FUNC[VALUE -> VALUE]) -> VALUE
      var evaluateCPS = (anExp, env, continues) => {
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
            var found = lookupEnv(name, env);
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
                  return continues(evaluateCPS(bodyExp, extendEnv(name, actualArg ,env), continues));
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
          application: (anExp, arg) => {
            var rator = evaluateCPS(anExp, env, continues);
            var rand = evaluateCPS(arg, env, continues);
            return continues(rator(rand));
          }
        });
      };
      describe('継続渡し評価器をテストする', () => {
        var returns = (result) => {
          return result;
        };
        it('数値を評価する', (next) => {
          expect(
            evaluateCPS(exp.number(2), emptyEnv, returns)
          ).to.eql(2);
          next();
        });
        it('変数を評価する', (next) => {
          var env = extendEnv("x",1, emptyEnv, returns);
          expect(
            evaluateCPS(exp.variable("x"), env, returns)
          ).to.eql(
            1
          );
          // 自由変数の場合は、 abortが返る
          expect(
            evaluateCPS(exp.variable("y"), env, returns)
          ).to.eql(
            new Error("y not found")
          );
          next();
        });
        it('constant関数', (next) => {
          var constant = exp.lambda(exp.variable("x"),exp.number(1));
          expect(
            evaluateCPS(constant, emptyEnv, returns)
          ).to.a(
            'function'
          );
          // (λx.1)(2)
          var applied = exp.application(constant, exp.number(2));
          expect(
            evaluateCPS(applied, emptyEnv, returns)
          ).to.eql(
            1
          );
          next();
        });
        it('identity関数をテストする', (next) => {
          /* λx.x */
          var identity = exp.lambda(exp.variable("x"),exp.variable("x"));
          expect(
            evaluateCPS(identity, emptyEnv, returns)
          ).to.a(
            'function'
          );
          // (λx.x)(1) = 1 */
          var appliedExpression = exp.application(identity, exp.number(1));
          expect(
            evaluateCPS(appliedExpression, emptyEnv, returns)
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
                               exp.application(
                                 exp.application(
                                   exp.variable("x"),falseFun),
                                 trueFun));
          var and = exp.lambda(exp.variable("x"),
                               exp.lambda(exp.variable("y"),
                                          exp.application(
                                            exp.application(exp.variable("x"),exp.variable("y")),
                                            falseFun)));
          var or = exp.lambda(exp.variable("x"),
                              exp.lambda(exp.variable("y"),
                                         exp.application(
                                           exp.application(exp.variable("x"),trueFun),
                                           exp.variable("y"))));
          var cond = exp.lambda(exp.variable("pred"),
                                exp.lambda(exp.variable("x"),
                                           exp.lambda(exp.variable("y"),
                                                      exp.application(
                                                        exp.application(exp.variable("pred"),exp.variable("x")),exp.variable("y")))));
          // (λx.λy.x)(1)(0) = 1
          expect(
            evaluateCPS(
              exp.application(
                exp.application(trueFun,exp.number(1)),
                exp.number(0)),
              emptyEnv,
              returns)
          ).to.eql(
            1
          );
          // (λx.λy.x)(1)(z) = 1
          expect(
            evaluateCPS(
              exp.application(
                exp.application(trueFun,exp.number(1)),
                exp.variable("z")),
              emptyEnv,
              returns)
          ).to.eql(
            1
          );
          // (λx.λy.x)(z)(0) = error
          expect(
            evaluateCPS(
              exp.application(
                exp.application(trueFun,exp.variable("z")),
                exp.number(0)),
              emptyEnv,
              returns)
          ).to.eql(
            new Error("z not found")
          );
          next();
        });
        it('raiseを使う', (next) => {
          // (λx.raise)(2)
          var applied = exp.application(exp.lambda(exp.variable("x"),exp.raise("exception")),
                                        exp.number(2));
          expect(
            evaluateCPS(applied, emptyEnv, returns)
          ).to.eql(
            new Error("exception")
          );
          next();
        });
      });
    });
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
        application: (variable, arg) => {
          return (pattern) => {
            return pattern.application(variable, arg);
          };
        }
      }; // exp
      // ## 式の評価関数
      /* #@range_begin(continuation_passing_interpreter_evaluate) */
      // evaluateCPS: (EXP, ENV, FUNC[VALUE -> VALUE]) -> VALUE
      var evaluateCPS = (anExp, env, continues, continuesInFailure) => {
        // c.f. Programming Language Concepts, p.208
        return exp.match(anExp,{
          /* 例外の評価 */
          raise: (exception) => {
            return continuesInFailure(exception);
          },
          tryWith: (anExp, caughtException, failSafeExp) => {
            var newContinuesInFailure = (thrownException) => {
              if (thrownException.message === caughtException.message) {
                return evaluateCPS(failSafeExp, env, continues, continuesInFailure);
              } else {
                return continuesInFailure(thrownException);
              }
            };
            return evaluateCPS(anExp, env, continues, newContinuesInFailure);
          },
          /* 数値の評価 */
          number: (answer) => {
            return continues(answer);
          },
          /* 変数の評価 */
          variable: (name) => {
            var found = lookupEnv(name, env);
            if(found === undefined){
              return continuesInFailure(new Error(name + " not found"));
              // return new Error(name + " not found");
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
                  return continues(evaluateCPS(bodyExp, extendEnv(name, actualArg ,env), continues));
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
          application: (anExp, arg) => {
            var rator = evaluateCPS(anExp, env, continues, continuesInFailure);
            var rand = evaluateCPS(arg, env, continues, continuesInFailure);
            return continues(rator(rand));
          }
        });
        /* #@range_end(continuation_passing_interpreter_evaluate) */
      };
      describe('例外捕捉評価器をテストする', () => {
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
          var env = extendEnv("x",1, emptyEnv, continuesNormally, continuesAbnormally);
          expect(
            evaluateCPS(exp.variable("x"), env, continuesNormally, continuesAbnormally)
          ).to.eql(
            1
          );
          // 自由変数の場合は、 例外が返る
          expect(
            evaluateCPS(exp.variable("y"), env, continuesNormally, continuesAbnormally)
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
          var applied = exp.application(constant, exp.number(2));
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
          var appliedExpression = exp.application(identity, exp.number(1));
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
                               exp.application(
                                 exp.application(
                                   exp.variable("x"),falseFun),
                                 trueFun));
          var and = exp.lambda(exp.variable("x"),
                               exp.lambda(exp.variable("y"),
                                          exp.application(
                                            exp.application(exp.variable("x"),exp.variable("y")),
                                            falseFun)));
          var or = exp.lambda(exp.variable("x"),
                              exp.lambda(exp.variable("y"),
                                         exp.application(
                                           exp.application(exp.variable("x"),trueFun),
                                           exp.variable("y"))));
          var cond = exp.lambda(exp.variable("pred"),
                                exp.lambda(exp.variable("x"),
                                           exp.lambda(exp.variable("y"),
                                                      exp.application(
                                                        exp.application(exp.variable("pred"),exp.variable("x")),exp.variable("y")))));
          // (λx.λy.x)(1)(0) = 1
          expect(
            evaluateCPS(
              exp.application(
                exp.application(trueFun,exp.number(1)),
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
              exp.application(
                exp.application(trueFun,exp.number(1)),
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
              exp.application(
                exp.application(trueFun,exp.variable("z")),
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
          var catchException = exp.application(exp.lambda(exp.variable("x"),
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


