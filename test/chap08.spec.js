"use strict";

// 関数型言語を作る
// ========
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

describe('環境を作る', () => {
  /* #@range_begin(environment) */
  // ## 「環境」モジュール
  var env = {
    // empty:: STRING => VALUE
    /* 空の環境を作る */
    empty: (variable) => {                        
      return undefined;
    },
    // lookup:: (STRING, ENV) => VALUE
    /* 変数名に対応する値を環境から取りだす */
    lookup : (name, environment) => {       
      return environment(name);
    },
    // extend:: (STRING, VALUE, ENV) => ENV
    /* 環境を拡張する */
    extend: (identifier, value, environment) => { 
      return (queryIdentifier) => {
        if(identifier === queryIdentifier) {
          return value;
        } else {
          return env.lookup(queryIdentifier,environment);
        }
      };
    }
  };
  /* #@range_end(environment) */
  it('環境をテストする', (next) => {
    /* #@range_begin(environment_code_test) */
    expect(((_) => {
      /* 空の環境からnewEnv環境を作る */
      var newEnv = env.extend('a',1, env.empty); 
      /* newEnv環境を利用して a の値を求める */
      return env.lookup("a", newEnv);            
    })()).to.eql(
      1
    );
    /* #@range_end(environment_code_test) */
    expect(((_) => {
      var initEnv = env.empty;                      // 空の辞書を作成する
      /* var a = 1 を実行して、辞書を拡張する */  
      var firstEnv = env.extend("a", 1, initEnv);
      /* var b = 3 を実行して、辞書を拡張する */
      var secondEnv = env.extend("b",3, firstEnv);
      /* 辞書から b の値を参照する */
      return env.lookup("b",secondEnv);
    })()).to.eql(
      3
    );
    // ~~~js
    // var x = 1;
    // var y = 2;
    // var closure = () => {
    //   var z = 3;
    //   return x + y + z;
    // };
    // closure() 
    // ~~~
    /* #@range_begin(environment_extend_test) */
    expect(((_) => {
      /* 空の辞書を作成する */
      var initEnv = env.empty;                   
      /* 空の辞書から outerEnv環境を作る */
      var outerEnv = env.extend("x", 1, initEnv);    

      /* closureEnv環境を作る */
      var closureEnv = env.extend("y",2, outerEnv);  
      /* closureEnv環境を利用してx+yを計算する */
      return env.lookup("x",closureEnv) + env.lookup("y",closureEnv);
    })()).to.eql(
      3
    );
    /* #@range_end(environment_extend_test) */
    next();
  });
  // ## 式の代数的データ構造
  describe('式の代数的データ構造', () => {
    var exp = {
      /* #@range_begin(expression_algebraic_datatype) */
      /* 式のパターンマッチ関数 */
      match : (data, pattern) => { 
        return data(pattern);
      },
      /* 数値の式 */
      num: (value) => {             
        return (pattern) => {
          return pattern.num(value);
        };
      },
      /* 変数の式 */
      variable : (name) => {        
        return (pattern) => {
          return pattern.variable(name);
        };
      },
      /* 関数定義の式(λ式) */
      lambda: (variable, body) => { 
        return (pattern) => {
          return pattern.lambda(variable, body);
        };
      },
      /* 関数適用の式 */
      app: (lambda, arg) => {       
        return (pattern) => {
          return pattern.app(lambda, arg);
        };
      },
      /* #@range_end(expression_algebraic_datatype) */
      /* #@range_begin(expression_arithmetic) */
      /* 足し算の式 */
      add : (expL,expR) => {        
        return (pattern) => {
          return pattern.add(expL, expR);
        };
      }
      /* #@range_end(expression_arithmetic) */
      /*
      mul : (exp1,exp2) => {        // かけ算の式
        return (pattern) => {
          return pattern.mul(exp1, exp2);
        };
      }
      div : (exp1,exp2) => {
        return (pattern) => {
          return pattern.div(exp1, exp2);
        };
      },
      equal: (expL,expR) => {
        return (pattern) => {
          return pattern.equal(expL, expR);
        };
      }
      */
    };
    describe('式をテストする', () => {
      it("\\x.\\y.x", (next) => {
        /* λx.λy.x */
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
  });
});
// ## 恒等モナド評価器
describe('恒等モナド評価器を作る', () => {
  // 「環境」モジュール
  var env = {
    /* empty:: STRING => VALUE */ 
    /* 空の環境を作る */
    empty: (variable) => {                        
      return undefined;
    },
    /* lookup:: (STRING, ENV) => VALUE
       変数名に対応する値を環境から取りだす
    */
    lookup : (name, environment) => {       
      return environment(name);
    },
    /* extend:: (STRING, VALUE, ENV) => ENV 
       環境を拡張する
    */
    extend: (identifier, value, environment) => { 
      return (queryIdentifier) => {
        if(identifier === queryIdentifier) {
          return value;
        } else {
          return env.lookup(queryIdentifier,environment);
        }
      };
    }
  };
  var emptyEnv = env.empty;
  var exp = {
    match : (data, pattern) => { // 式のパターンマッチ関数
      return data(pattern);
    },
    num: (value) => {             // 数値の式
      return (pattern) => {
        return pattern.num(value);
      };
    },
    variable : (name) => {        // 変数の式
      return (pattern) => {
        return pattern.variable(name);
      };
    },
    lambda: (variable, body) => { // 関数定義の式(λ式)
      return (pattern) => {
        return pattern.lambda(variable, body);
      };
    },
    app: (lambda, arg) => {       // 関数適用の式
      return (pattern) => {
        return pattern.app(lambda, arg);
      };
    },
    add : (expL,expR) => {        // 足し算の式
      return (pattern) => {
        return pattern.add(expL, expR);
      };
    }
  };
  // ~~~haskell
  // eval :: Monad m => Exp -> m Int
  // eval (Const x) = return x
  // eval (Div t u) = do { x <- eval t
  //                       y <- eval u
  //                       return (x div y)}
  // ~~~
  // ### 恒等モナド
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
  // ### 恒等モナド評価器の評価関数
  /* #@range_begin(identity_monad_evaluator) */
  /* evaluate:: (EXP, ENV) => ID[VALUE] */
  var evaluate = (anExp, environment) => {
    return exp.match(anExp,{
      /* 数値の評価 */
      num: (numericValue) => {        
        return ID.unit(numericValue);
      },
      /* 変数の評価  */
      variable: (name) => {           
        return ID.unit(env.lookup(name, environment));
      },
      /* λ式の評価  */
      lambda: (variable, body) => {   
        return exp.match(variable,{
          variable: (name) => {
            return ID.unit((actualArg) => {
              return evaluate(body, 
                              env.extend(name, actualArg, environment));
            });
          }
        });
      },
      /* 関数適用の評価 */
      app: (lambda, arg) => {         
        return ID.flatMap(evaluate(lambda, environment))((closure) => {
          return ID.flatMap(evaluate(arg, environment))((actualArg) => {
            return closure(actualArg); 
          });
        });
      },
      /* 足し算の評価 */
      add: (expL, expR) => {          
        return ID.flatMap(evaluate(expL, environment))((valueL) => {
          return ID.flatMap(evaluate(expR, environment))((valueR) => {
            return ID.unit(valueL + valueR); 
          });
        });
      }
    });
  };
  /* #@range_end(identity_monad_evaluator) */
  /*
  equal: (expL, expR) => {
    return ID.flatMap(evaluate(expL, environment))((valueR) => {
      return ID.flatMap(evaluate(expR, environment))((valueL) => {
        return ID.unit(valueL === valueR); 
      });
    });
  }
  div: (expL, expR) => {
    return ID.flatMap(evaluate(expL, environment))((valueR) => {
      return ID.flatMap(evaluate(expR, environment))((valueL) => {
        return ID.unit(valueL / valueR); 
      });
    });
  }
  */
  it('ID評価器で数値を評価する', (next) => {
    /* #@range_begin(number_evaluation_test) */
    expect(
      evaluate(exp.num(2), env.empty)
    ).to.eql(
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
    next();
  });
  it('ID評価器で変数を評価する', (next) => {
    /* #@range_begin(variable_evaluation_test) */
    /* 変数xを1に対応させた環境を作る */
    var newEnv = env.extend("x",1, env.empty); 
    /* 拡張したnewEnv環境を用いて変数xを評価する */
    expect(
      evaluate(exp.variable("x"), newEnv)
    ).to.eql(
      ID.unit(1)
    );
    /* #@range_end(variable_evaluation_test) */
    expect(
      evaluate(exp.variable("y"), newEnv)
    ).to.be(
      ID.unit(undefined)
    );
    next();
  });
  it('ID評価器でaddを評価する', (next) => {
    /* add(1,2) */
    /* #@range_begin(add_evaluation_test) */
    var addition = exp.add(exp.num(1),exp.num(2));
    expect(
      evaluate(addition, env.empty)
    ).to.eql(
      ID.unit(3)
    );
    /* #@range_end(add_evaluation_test) */
    next();
  });
  it('ID評価器で関数を評価する', (next) => {
    // ~~~js
    // ((x) => {
    //   return x; 
    // })(1)
    // ~~~
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
    // ~~~js
    // ((n) => {
    //   return n + 1; 
    // })(2)
    // ~~~
    /* #@range_begin(application_evaluation_test) */
    var expression = exp.app(         /* 関数適用 */
      exp.lambda(exp.variable("n"),   /* λ式 */
                 exp.add(exp.variable("n"),
                         exp.num(1))),
      exp.num(2));                    /* 引数の数値2 */
    expect(
      evaluate(expression, env.empty)
    ).to.eql(
      ID.unit(3)
    );
    /* #@range_end(application_evaluation_test) */
    next();
  });
  it('ID評価器で関数適用 \\x.add(x,x)(2)を評価する', (next) => {
    // ~~~js
    // ((x) => {
    //   return x + x; 
    // })(2)
    // ~~~
    var expression = exp.app(exp.lambda(exp.variable("x"),
                                        exp.add(exp.variable("x"),exp.variable("x"))),
                             exp.num(2));
    expect(
      evaluate(expression, env.empty)
    ).to.eql(
      4
    );
    next();
  });
  it('ID評価器で高階関数を評価する', (next) => {
    // ~~~js
    // ((n) => {
    //    return (m) => {
    //       return n + m;
    //    };
    // })(2)(3)
    // ~~~
    /* #@range_begin(curried_function_evaluation_test) */
    var expression = exp.app(
      exp.app(
        exp.lambda(exp.variable("n"),
                   exp.lambda(exp.variable("m"),
                              exp.add(
                                exp.variable("n"),exp.variable("m")))),
        exp.num(2)),
      exp.num(3));
    expect(
      evaluate(expression, env.empty)
    ).to.eql(
      ID.unit(5)
    );
    /* #@range_end(curried_function_evaluation_test) */
    next();
  });
  it('Yコンビネータで再帰を実行する', (next) => {
    // ~~~js
    // var Y = (F) => {
    //   return ((g) => {
    //     return (x) =>  {
    //       return F(g(g))(x);
    //     };
    //   })((g) =>  {
    //     return (x) => {
    //       return F(g(g))(x);
    //     };
    //   });
    // };
    // ~~~
    /*
    var Y = exp.lambda(exp.variable("F"),
                       exp.app(
                         exp.lambda(exp.variable("g"),
                                    exp.lambda(exp.variable("x"),
                                               exp.app(exp.app(exp.variable("F"), // F(g(g))(x)
                                                       exp.app(exp.variable("g"),exp.variable("g"))),
                                                       exp.variable("x")))),
                         exp.lambda(exp.variable("g"),
                                    exp.lambda(exp.variable("x"),
                                               // F(g(g))(x);
                                               exp.app(exp.app(exp.variable("F"),
                                                       exp.app(exp.variable("g"),exp.variable("g"))),
                                                       exp.variable("x"))))
                       ));
    var factorial = Y((fact) => {
      return (n) => {
        if (n == 0) {
          return 1;
        } else {
          return n * fact(n - 1);
        }
      };
    });

    var factorial = exp.app(
      exp.app(Y, exp.lambda(exp.variable("fact"),
                            exp.lambda(exp.variable("n"),
  
    expect(
      evaluate(factorial, emptyEnv)
    ).to.be(
      5
    );
    */
    next();
  });
});
// ## LOGモナド評価器
describe('ログ出力用評価器を作る', () => {
  // 「環境」モジュール
  var env = {
    // empty:: STRING => VALUE 
    empty: (variable) => {                        // 空の環境を作る
      return undefined;
    },
    // lookup:: (STRING, ENV) => VALUE
    lookup : (name, environment) => {       // 変数名に対応する値を環境から取りだす
      return environment(name);
    },
    // extend:: (STRING, VALUE, ENV) => ENV 
    extend: (identifier, value, environment) => { // 環境を拡張する
      return (queryIdentifier) => {
        if(identifier === queryIdentifier) {
          return value;
        } else {
          return env.lookup(queryIdentifier,environment);
        }
      };
    }
  };
  var emptyEnv = env.empty;
  /* #@range_begin(expression_logger_interpreter) */
  var exp = {
    log: (anExp) => { // ログ出力用の式
      return (pattern) => {
        return pattern.log(anExp);
      };
    },
    /* #@range_end(expression_logger_interpreter) */
    match : (data, pattern) => {
      return data.call(exp, pattern);
    },
    num : (value) => {
      expect(value).to.a('number');
      return (pattern) => {
        return pattern.num(value);
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
    }
  };
  // ### LOGモナド
  /* #@range_begin(logger_monad) */
  /* LOG[T] = PAIR[T, LIST[STRING]] */
  var LOG = {
    // unit:: VALUE => LOG[VALUE]
    unit: (value) => {
      /* 値とログのPair型を作る */
      return pair.cons(value, list.empty()); 
    },
    // flatMap:: LOG[T] => FUN[T => LOG[T]] => LOG[T]
    flatMap: (instanceM) => {
      return (transform) => {
        return pair.match(instanceM,{
          /* Pair型に格納されている値の対を取り出す */
          cons: (value, log) => {
            /* 取り出した値で計算する */
            var newInstance = transform(value); 
            /* 計算の結果をPairの左側に格納し、
               新しいログをPairの右側に格納する */
            return pair.cons(pair.left(newInstance),
                             list.append(log)(pair.right(newInstance)));
          }
        });
      };
    },
    // output:: VALUE => LOG[()]
    /* 引数 value をログに格納する */
    output: (value) => {
      return pair.cons(null, 
                       list.cons(String(value), list.empty()));
    }
  };
  /* #@range_end(logger_monad) */
  // #### LOGモナド評価器の評価関数
  /* #@range_begin(logger_monad_evaluator) */
  // evaluate:: (EXP, ENV) => LOG[VALUE]
  var evaluate = (anExp, environment) => {
    return exp.match(anExp,{
      /* log式の評価 */
      log: (anExp) => {
        /* 式を評価する */
        return LOG.flatMap(evaluate(anExp, environment))((value) => {
          /* value をログに格納する */
          return LOG.flatMap(LOG.output(value))((_) => { 
            return LOG.unit(value); 
          });
        });
      },
      /* #@range_end(logger_monad_evaluator) */
      /* 数値の評価 */
      num: (value) => {
        return LOG.unit(value);
      },
      /* 変数の評価 */
      variable: (name) => {
        return LOG.unit(env.lookup(name, environment));
      },
      /* λ式の評価 */
      lambda: (variable, body) => {
        return exp.match(variable,{
          variable: (name) => {
            return LOG.unit((actualArg) => {
              return evaluate(body, env.extend(name, actualArg, environment));
            });
          }
        });
      },
      /* 関数適用の評価 */
      app: (lambda, arg) => {         // 関数適用の評価
        return LOG.flatMap(evaluate(lambda, environment))((closure) => {
          return LOG.flatMap(evaluate(arg, environment))((actualArg) => {
            return closure(actualArg); 
          });
        });
      },
      add: (expL, expR) => {
        return LOG.flatMap(evaluate(expL, environment))((valueL) => {
          return LOG.flatMap(evaluate(expR, environment))((valueR) => {
            return LOG.unit(valueL + valueR); 
          });
        });
      }
    });
  };
  it('LOG評価器で数値を評価する', (next) => {
    /* #@range_begin(log_interpreter_number) */
    pair.match(evaluate(exp.log(exp.num(2)), env.empty),{
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
    next();
  });
  it('LOG評価器で変数を評価する', (next) => {
    /* #@range_begin(log_interpreter_variable) */
    var newEnv = env.extend("x",1, env.empty);
    pair.match(evaluate(exp.log(exp.variable("x")), newEnv), {
      cons: (value, log) => {
        expect( // 結果の値をテストする
          value
        ).to.eql(
          1
        );
        expect( // 保存されたログを見る
          list.toArray(log)
        ).to.eql(
          [1]
        );
      }
    });
    /* #@range_end(log_interpreter_variable) */
    next();
  });
  it('LOG評価器で演算を評価する', (next) => {
    pair.match(evaluate(exp.log(exp.add(exp.num(1),exp.num(2))), env.empty),{
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
    pair.match(evaluate(exp.log(exp.add(exp.log(exp.num(1)),exp.log(exp.num(2)))), env.empty),{
      cons: (value, log) => {
        expect(
          value
        ).to.be(
          3 // 1 + 2 = 3
        );
        expect(
          list.toArray(log)
        ).to.eql(
          [1,2,3]
        );
      }
    });
    /* #@range_begin(log_interpreter_evaluation_strategy) */
    /* ((n) => { return 1 + n})(2) の評価 */
    var theExp = exp.log(exp.app(exp.lambda(exp.variable("n"),
                                            exp.add(exp.log(exp.num(1)), 
                                                    exp.variable("n"))),
                                 exp.log(exp.num(2))));
    pair.match(evaluate(theExp, env.empty),{
      /* パターンマッチで結果を取り出す */
      cons: (value, log) => {
        expect(
          value
        ).to.eql(
          3
        );
        expect(
          list.toArray(log)
        ).to.eql(
          [2,1,3]
        );
      }
    });
    /* #@range_end(log_interpreter_evaluation_strategy) */
    next();
  });
  it('LOG評価器で関数適用を評価する', (next) => {
    // ~~~
    // \x.add(x,x)(2)
    // ~~~
    var expression = exp.app(exp.lambda(exp.variable("x"),
                                        exp.add(exp.variable("x"),exp.variable("x"))),
                             exp.num(2));
    expect(
      pair.left(evaluate(expression, env.empty))
    ).to.eql(
      4
    );
    expect(
      list.toArray(pair.right(evaluate(expression, env.empty)))
    ).to.eql(
      []
    );
    next();
  });
  it('LOG評価器でカリー化関数を評価する', (next) => {
    // ~~~
    // (\x.
    //    \y.
    //       x+y)(2)(3)
    // ~~~ 
    var expression = exp.app(
      exp.app(exp.lambda(exp.variable("x"),
                         exp.lambda(exp.variable("y"),
                                    exp.add(exp.variable("x"),exp.variable("y")))),
              exp.num(2)),
      exp.num(3));
    expect(
      pair.left(evaluate(expression, emptyEnv))
    ).to.be(
      5
    );
    next();
  });
});
/*
    it('ブール型を評価する', (next) => {
      
      var trueFun = exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("x"))); // λx.λy.x 
      
      var falseFun = exp.lambda(exp.variable("x"),exp.lambda(exp.variable("y"),exp.variable("y"))); // λx.λy.y 
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
      expect(
        evaluate(
          exp.app(
            exp.app(trueFun,exp.num(1)),
            exp.num(0)),
          emptyEnv)
      ).to.eql(
        1
      );
      expect(
        evaluate(
          exp.app(
            exp.app(
              exp.app(not, trueFun), exp.num(1)), exp.num(0)),emptyEnv)
      ).to.eql(
        0
      );
      expect(
        evaluate(
          exp.app(
            exp.app(
              exp.app(
                exp.app(and,
                            trueFun),
                trueFun),
              exp.num(1)),
            exp.num(0)),emptyEnv)
      ).to.be(
        1
      );
      expect(
        evaluate(
          exp.app(
            exp.app(
              exp.app(
                exp.app(
                  exp.app(cond, trueFun),
                  falseFun),
                trueFun),
              exp.num(1)),
            exp.num(0)),
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
        number: (value) => {
          return {
            number: value
          };
        },
        variable: (name) => {
          return {
            variable: name
          };
        },
        lambda: (variable, bodyExp) => {
          return {
            lambda: {
              variable: variable,
              bodyExp: prettyPrint(bodyExp)
            }
          };
        },
        app: (variable, arg) => {
          return {
            app: {
              variable: variable,
              arg: prettyPrint(arg)
            }
          };
        }
      });
    };
    it('prettyPrintをテストする', (next) => {
      expect(
        prettyPrint(exp.num(2))
      ).to.eql(
        {"number" : 2}
      );
      next();
    });
    describe('ローダーを作る', () => {
      var load = (object) => {
        for (var key in object) {
          switch (key){
          case "number":
            return exp.num(parseInt(object[key],10));
            break;
          default:
            throw new Error();
            break;
          }
        }
      };
      it('ローダーをテストする', (next) => {
        expect(
          evaluate(load({"number" : 2}, emptyEnv))
        ).to.eql(
          2
        );
        next();
      });
  describe('ファイル操作', () => {
    it('数値を評価する', (next) => {
      fs.writeFileSync('/tmp/nodejs-labo-test.json',  JSON.stringify(num(2), null, '    '));
      expect(
        JSON.parse(fs.readFileSync('/tmp/nodejs-labo-test.json', 'utf8'))
      ).to.eql(
        2
      );
      next();
    });
  });
});
*/



