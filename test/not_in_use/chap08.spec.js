
// var sys = require('sys');
// var fs = require('fs');
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

    // /* 「環境」モジュール */
    // var env = {
    //   /* empty:: STRING => VALUE  */
    //   empty: (variable) => {       
    //     return undefined;
    //   },
    //   /* 変数名に対応する値を環境から取りだす */
    //   /* lookup:: (STRING, ENV) => VALUE */
    //   lookup : (name, environment) => {       
    //     return environment(name);
    //   },
    //   /* 環境を拡張する */
    //   /* extend:: (STRING, VALUE, ENV) => ENV */
    //   extend: (identifier, value, environment) => { 
    //     return (queryIdentifier) => {
    //       if(identifier === queryIdentifier) {
    //         return value;
    //       } else {
    //         return env.lookup(queryIdentifier,environment);
    //       }
    //     };
    //   }
    // };
    // var emptyEnv = env.empty;

  /* list#concat */
  // concat: (xss) => {
  //   return list.match(xss,{
  //     empty: (_) => {
  //       return list.empty();
  //     },
  //     cons: (xs,xss) => {
  //       return list.append(xs,xss);
  //     }
  //   });
  // },
  /* join:: LIST[LIST[T]] -> LIST[T] */
  // join: (list_of_list) => {
  //   return list.concat(list_of_list);
  // },
  /* map:: LIST[T] -> FUNC[T -> T] -> LIST[T] */
  // map: (alist) => {
  //   return (transform) => {
  //     return list.match(alist,{
  //       empty: (_) => {
  //         return list.empty();
  //       },
  //       cons: (head,tail) => {
  //         return list.cons(transform(head),list.map(tail)(transform));
  //       }
  //     });
  //   };
  // },
