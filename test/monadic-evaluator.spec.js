"use strict";

// モナド的評価器
// =============
// ここでは、書籍で扱かったモナドによる評価器の実装を、さらに発展させます。
// 
// なお、本ページのコードは、書籍で採用された node.js 0.12版では動作しません。
// できるだけ新しいバージョンのnode.jsで実行してください。
// 当方の環境では、v8.11.1でテストが成功することを確認しています。

// ## 小目次
// <div class="toc">
// <ul class="toc">
//   <li><a href="http://akimichi.github.io/functionaljs/monadic-evaluator.spec.html#exp> 式の定義</a>
//   <li><a href="http://akimichi.github.io/functionaljs/monadic-evaluator.spec.html#env> 環境の定義</a>
//   <li><a href="http://akimichi.github.io/functionaljs/monadic-evaluator.spec.html#maybe> Maybeモナド</a>
//   <li><a href="http://akimichi.github.io/functionaljs/monadic-evaluator.spec.html#reader> Readerモナド</a>
//   <li><a href="http://akimichi.github.io/functionaljs/monadic-evaluator.spec.html#evaluator> 評価関数の定義</a>
// </ul>
// </div>


const expect = require('expect.js');

// 拙作 kansuu.jsのコードを利用します
const kansuu = require('kansuu.js'),
  pair = kansuu.pair,
  array = kansuu.array;


// ## <section id='exp'>式の定義</section>
const Exp = {
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
  /* Let式 */
  let: (variable, declaration, body) => {
    return (pattern) => {
      return pattern.let(variable, declaration, body);
    };
  },
  // 演算の定義
  /* 足し算の式 */
  add : (expL,expR) => {
    return (pattern) => {
      return pattern.add(expL, expR);
    };
  },
  /* 引き算の式 */
  subtract: (expL,expR) => {
    return (pattern) => {
      return pattern.subtract(expL, expR);
    };
  }
};


// ## <section id='env'>環境の定義</section>
//
// 書籍で紹介した「環境」とは異なる方法で定義しています。
const Env = {
  empty: () => {
    return []; 
  },
  /* **Env#lookup**
    lookup:: (STRING) -> Map[String, VALUE] => Maybe[VALUE] 
  */
  lookup : (key) =>  (env) => {
    const answer = array.find(aPair => {
      return pair.match(aPair, {
        cons: (left, right) => {
          return left === key;
        }
      })
    })(env);
    if(answer === undefined) {
      return Maybe.nothing(`変数 ${key} は未定義です`);
    } else {
      return Maybe.just(pair.right(answer));
    }
  },
  /* 
  /* **Env#extend**
    extend:: (STRING, VALUE) => ENV => ENV 
    環境を拡張する
  */
  extend: (key, value) => (env) => { 
    return array.cons(pair.cons(key, value), env);
  }
};

// ## 環境のテスト
describe("環境をテストする",() => {
  // **Env.empty**をテストする
  it("Env.emptyをテストする",(done) => {
    Maybe.match(Env.lookup('a')(Env.empty()),{
      nothing: (_) => {
        expect(true).to.be.ok();
      },
      just: (value) => {
        expect().fail();
      }
    })
    done();
  });
  // **Env.extend**をテストする
  it("Env.extendをテストする",(done) => {
    const env = Env.extend('a', 1)(Env.empty());
    expect(
      pair.left(array.head(env))
    ).to.eql('a');
    done(); 
  });
  // **Env.lookup**をテストする
  it("Env.lookupをテストする",(done) => {
    const env = Env.extend('a', 1)(Env.empty());
    Maybe.match(Env.lookup('a')(env),{
      nothing: (_) => {
        expect().fail();
      },
      just: (value) => {
        expect(value).to.eql(1);
      }
    })
    done(); 
  });
});

// ## <section id='maybe'>Maybeモナドの定義</section>
//
// 書籍で紹介した「Maybeモナド」とほとんど同じですが、nothingにエラー情報を格納できるよう変更しました。
const Maybe = {
  match: (data, pattern) => {
    return data(pattern);
  },
  just : (value) => {
    return (pattern) => {
      return pattern.just(value);
    };
  },
  nothing : (message) => {
    return (pattern) => {
      return pattern.nothing(message);
    };
  },
  // **Maybe#unit**
  unit : (value) => {
    return Maybe.just(value);
  },
  // **Maybe#flatMap**
  flatMap : (maybeInstance) => {
    return (transform) => {
      expect(transform).to.a('function');
      return Maybe.match(maybeInstance,{
        just: (value) => {
          return transform(value);
        },
        nothing: (message) => {
          return Maybe.nothing(message);
        }
      });
    };
  }
};

// ## <section id='reader'>Readerモナドの定義</section>
// 
//  Readerモナドは、共有された環境から値を読み込む計算を抽象化します。
//  書籍で紹介された評価器では、全ての評価関数が環境を引数として受けとることになっていました。
//  Readerモナドを使えば、この引数としての環境を隠蔽できます。

const Reader = {
  // **Reader#unit**
  unit: (a) => {
    return {
      run: (_) => { // runReader :: Reader r a -> r -> a
        return a;
      }
    };
  },
  // **Reader#flatMap**
  flatMap: (m) => {
    expect(m).to.be.an('object')
    return (f) => {
      return {
        run: (env) => {
          return f(m.run(env)).run(env);
        }
      };
    };
  },
  // **Reader#ask**
  // 環境から値を取りだします。
  ask: {
    run: (env) => {
      return env;
    }
  },
  // **Reader#local**
  local: (f) => (reader) => {
    return {
      run: (env) => {
        return reader.run(f(env));
      }
    };
  }
};

// ## <section id='evaluator'>評価関数の定義</section>
//
// Semantics.evaluator関数が、評価器となります。
// この評価器の特徴は、内部に複数のモナドを利用している点です。
// 使われているモナドは、MaybeモナドとReaderモナドです。
const Semantics = {
  // 2項演算の評価 
  binary: (operator) => (expL, expR) => {
    return Reader.flatMap(Semantics.evaluate(expL))(maybeL => {
      return Reader.flatMap(Semantics.evaluate(expR))(maybeR => {
        const maybeAnswer = Maybe.flatMap(maybeL)(valueL => {
          return Maybe.flatMap(maybeR)(valueR => {
            return Maybe.just(operator(valueL,valueR)); 
          });
        });
        return Reader.unit(maybeAnswer);
      });
    });
  },
  // **Semantics#evaluate**
  /* evaluate:: Exp -> Reader[Maybe[Value]] */
  evaluate: (anExp) => {
    return Exp.match(anExp,{
      // 数値の評価
      num: (numericValue) => {
        return Reader.unit(Maybe.just(numericValue));
      },
      // 変数の評価
      variable: (name) => {
        /* Reader.askを使って、現在の環境の情報を取りだす */
        return Reader.flatMap(Reader.ask)(dictionary => {
          return Reader.unit(Env.lookup(name)(dictionary));
        });
      },
      // 関数定義（λ式）の評価
      /* lambda:: (Var, Exp) -> FUN[VALUE -> Reader[Maybe[Value]] */
      lambda: (identifier, body) => {
        return Exp.match(identifier,{
          variable: (name) => {
            const closure = (actualArg => {
              const localEnv = (env) => {
                 return Env.extend(name, actualArg)(env);
              };
              /* Reader.localを使ってクロージャーで使われるローカルな環境を作りだす */
              return Reader.local(localEnv)(Semantics.evaluate(body));
            });
            return closure; 
          }
        });
      },
      // 関数適用の評価
      /* app: (Lambda, Exp) -> Reader[Maybe[Value]] */
      app: (lambda, arg) => {
        const closure = Semantics.evaluate(lambda);
        return Reader.flatMap(Semantics.evaluate(arg))(maybeArg => {
          return Maybe.flatMap(maybeArg)(actualArg => {
            return closure(actualArg); 
          });
        });
      },
      // Letの評価
      /* let: (Variable, Exp, Exp) -> Reader[Maybe[Value]] */
      let: (variable, declaration, body) => {
        /* Let式は、関数適用の糖衣構文にすぎない */
        return Semantics.evaluate(Exp.app(Exp.lambda(variable, body), declaration));
      },
      //  足し算の評価
      add: (expL, expR) => {
        const operator = (operandR, operandL) => {
          return operandR + operandL; 
        };
        return Semantics.binary(operator)(expL, expR);
      },
      // 引き算の評価 
      subtract: (expL, expR) => {
        const operator = (operandR, operandL) => {
          return operandR - operandL; 
        };
        return Semantics.binary(operator)(expL, expR);
      }
    });
  }
};


// ### 評価関数のテスト
describe("評価関数をテストする",() => {
  // ####  数値を評価する
  describe("数値を評価する",() => {
    // **evaluate(1)は、Maybe.just(1)を返す**
    it("evaluate(1)は、Maybe.just(1)を返す",(done) => {
      const num = Exp.num(1);

      Maybe.match(Semantics.evaluate(num).run(Env.empty()),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(1);
          done(); 
        }
      })
    });
    // **数値の評価は、環境の情報に影響を受けない**
    it("数値の評価は、環境の情報に影響を受けない",(done) => {
      const num = Exp.num(1),
        initialEnv = Env.extend('x',1)(Env.empty())

      Maybe.match(Semantics.evaluate(num).run(initialEnv),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(1);
          done(); 
        }
      })
    });
  });
  // ####  変数を評価する
  describe("変数を評価する",() => {
    // **evaluateでMaybe.justを返す**
    it("evaluateでMaybe.justを返す",(done) => {
      const variable = Exp.variable('a');
      const initialEnv = Env.extend('a',1)(Env.empty())

      Maybe.match(Semantics.evaluate(variable).run(initialEnv),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(1);
          done(); 
        }
      })
    });
    // **evaluateで、Maybe.nothingを返す**
    it("evaluateで、Maybe.nothingを返す",(done) => {
      const variable = Exp.variable('b'),
        initialEnv = Env.extend('a',1)(Env.empty())

      Maybe.match(Semantics.evaluate(variable).run(initialEnv),{
        nothing: (message) => {
          expect(message).to.eql('変数 b は未定義です');
          done(); 
        },
        just: (value) => {
          expect().fail();
        }
      })
    });
  });
  // ####  演算子を評価する
  describe("演算子を評価する",() => {
    // **add(1,2)は、Maybe.just(3)を返す**
    it("add(1,2)は、Maybe.just(3)を返す",(done) => {
      const one = Exp.num(1),
        two = Exp.num(2);
      const initialEnv = Env.empty()

      Maybe.match(Semantics.evaluate(Exp.add(one, two)).run(initialEnv),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(3);
          done(); 
        }
      })
    });
    // **add(x,1)で、Maybe.justを返す**
    it("add(x,1)で、Maybe.justを返す",(done) => {
      const x = Exp.variable('x');
      const one = Exp.num(1);
      const initialEnv = Env.extend('x',1)(Env.empty())

      Maybe.match(Semantics.evaluate(Exp.add(x, one)).run(initialEnv),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(2);
          done(); 
        }
      })
    });
    // **add(y,1)は、Maybe.nothingを返す**
    it("add(y,1)は、Maybe.nothingを返す",(done) => {
      const y = Exp.variable('y'),
        one = Exp.num(1),
        initialEnv = Env.extend('x',1)(Env.empty());

      Maybe.match(Semantics.evaluate(Exp.add(y, one)).run(initialEnv),{
        nothing: (_) => {
          expect(true).to.be.ok();
          done(); 
        },
        just: (value) => {
          expect().fail();
        }
      })
    });
    // **subtract(2,1)は、Maybe.just(1)を返す**
    it("subtract(2,1)は、Maybe.just(1)を返す",(done) => {
      const one = Exp.num(1),
        two = Exp.num(2);
      const initialEnv = Env.empty()

      Maybe.match(Semantics.evaluate(Exp.subtract(two, one)).run(initialEnv),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(1);
          done(); 
        }
      })
    });
    // **add(subtract(2,1),2)は、Maybe.just(3)を返す**
    it("add(subtract(2,1),2)は、Maybe.just(3)を返す",(done) => {
      const one = Exp.num(1),
        two = Exp.num(2);
      const initialEnv = Env.empty()

      const exp = Exp.add(Exp.subtract(two, one),two);
      Maybe.match(Semantics.evaluate(exp).run(initialEnv),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(3);
          done(); 
        }
      })
    });
    // **add(subtract(2,y),2)は、Maybe.nothing()を返す**
    it("add(subtract(2,y),2)は、Maybe.nothing()を返す",(done) => {
      const y = Exp.variable('y'),
        two = Exp.num(2);
      const initialEnv = Env.empty()

      const exp = Exp.add(Exp.subtract(two, y),two);
      Maybe.match(Semantics.evaluate(exp).run(initialEnv),{
        nothing: (message) => {
          expect(message).to.eql('変数 y は未定義です');
          done(); 
        },
        just: (value) => {
          expect().fail();
          done(); 
        }
      })
    });
  });
  // ####  クロージャーを評価する
  describe("クロージャーを評価する",() => {
    // **(x => (x + 1))(1) は、Maybe.just(2)を返す**
    it("(x => (x + 1))(1) は、Maybe.just(2)を返す",(done) => {
      const x = Exp.variable('x'),
        one = Exp.num(1);
      const application = Exp.app(Exp.lambda(x, Exp.add(x, one)), one);
      Maybe.match(Semantics.evaluate(application).run(Env.empty()),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(2);
          done(); 
        }
      })
    });
    // **(x => (x + 1))(1)は、Maybe.just(2)を返す**
    it("(x => (x + 1))(1)は、Maybe.just(2)を返す",(done) => {
      const x = Exp.variable('x'),
        one = Exp.num(1);
      const application = Exp.app(Exp.lambda(x, Exp.add(x, one)), one);
      Maybe.match(Semantics.evaluate(application).run(Env.empty()),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(2);
          done(); 
        }
      })
    });
    // **(x => (x + y))(2)は、Maybe.nothingを返す**
    it("(x => (x + y))(2)は、Maybe.nothingを返す",(done) => {
      const x = Exp.variable('x'),
        y = Exp.variable('y'),
        two = Exp.num(2);
      const application = Exp.app(Exp.lambda(x, Exp.add(x, y)), two);
      Maybe.match(Semantics.evaluate(application).run(Env.empty()),{
        nothing: (message) => {
          expect(message).to.eql('変数 y は未定義です');
          done(); 
        },
        just: (value) => {
          expect(value).to.eql(4);
          done(); 
        }
      })
    });
  });
  // ####  Let式を評価する
  describe("Let式を評価する",() => {
    // **(let x = 1 in (x + x)は、Maybe.just(2)を返す**
    it("(let x = 1 in (x + x)は、Maybe.just(2)を返す",(done) => {
      const x = Exp.variable('x'),
        one = Exp.num(1);
      const letExp = Exp.let(x, one, Exp.add(x,x)); 
      Maybe.match(Semantics.evaluate(letExp).run(Env.empty()),{
        nothing: (_) => {
          expect().fail();
        },
        just: (value) => {
          expect(value).to.eql(2);
          done(); 
        }
      })
    });
  });
});
// [目次に戻る](index.html) 
