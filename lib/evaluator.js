"use strict";

var expect = require('expect.js');
var fs = require('fs');
var List = require('./list.js');
var Pair = require('../lib/pair.js');
var String = require('../lib/string.js');
var PP = require('../lib/pprinter.js');
var Env = require('../lib/env.js');
var IO = require('../lib/monad.js').IO;
var ID = require('../lib/monad.js').ID;
var Cont = require('../lib/monad.js').Cont;

const Evaluator = {
  ID: {
    Exp: {
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
      /* 足し算の式 */
      add : (expL,expR) => {        
        return (pattern) => {
          return pattern.add(expL, expR);
        };
      },
      /* 足し算の式 */
      subtract : (expL,expR) => {        
        return (pattern) => {
          return pattern.subtract(expL, expR);
        };
      },
      /* 足し算の式 */
      divide : (expL,expR) => {        
        return (pattern) => {
          return pattern.divide(expL, expR);
        };
      },
      /* かけ算の式 */
      multiply : (expL,expR) => {        
        return (pattern) => {
          return pattern.multiply(expL, expR);
        };
      }
    },
    evaluate: (anExp, environment) => {
      return Evaluator.ID.Exp.match(anExp,{
        // 数値の評価
        num: (numericValue) => {        
          return ID.unit(numericValue);
        },
        // 変数の評価
        variable: (name) => {           
          return ID.unit(Env.lookup(name, environment));
        },
        /* 関数定義（λ式）の評価  */
        lambda: (variable, body) => {   
          return Evaluator.ID.Exp.match(variable,{
            variable: (name) => {
              return ID.unit((actualArg) => {
                return Evaluator.ID.evaluate(body, 
                                Env.extend(name, actualArg, environment));
              });
            }
          });
        },
        /* 関数適用の評価 */
        app: (lambda, arg) => {         
          return ID.flatMap(Evaluator.ID.evaluate(lambda, environment))((closure) => {
            return ID.flatMap(Evaluator.ID.evaluate(arg, environment))((actualArg) => {
              return closure(actualArg); 
            });
          });
        },
        // 足し算の評価 
        add: (expL, expR) => {          
          return ID.flatMap(Evaluator.ID.evaluate(expL, environment))((valueL) => {
            return ID.flatMap(Evaluator.ID.evaluate(expR, environment))((valueR) => {
              return ID.unit(valueL + valueR); 
            });
          });
        },
        // 足し算の評価 
        subtract: (expL, expR) => {          
          return ID.flatMap(self.evaluate(expL, environment))((valueL) => {
            return ID.flatMap(self.evaluate(expR, environment))((valueR) => {
              return ID.unit(valueL - valueR); 
            });
          });
        },
        // 足し算の評価 
        multiply: (expL, expR) => {          
          return ID.flatMap(self.evaluate(expL, environment))((valueL) => {
            return ID.flatMap(self.evaluate(expR, environment))((valueR) => {
              return ID.unit(valueL * valueR); 
            });
          });
        },
        // 足し算の評価 
        divide: (expL, expR) => {          
          return ID.flatMap(self.evaluate(expL, environment))((valueL) => {
            return ID.flatMap(self.evaluate(expR, environment))((valueR) => {
              return ID.unit(valueL / valueR); 
            });
          });
        }
      });
    }
  },
  Cont: {
    Exp: {
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
      /* 足し算の式 */
      add : (expL,expR) => {        
        return (pattern) => {
          return pattern.add(expL, expR);
        };
      },
      /* callccの式 */
      callcc: (name, exp) => {
        return (pattern) => {
          return pattern.callcc(name, exp);
        };
      }
    },
    evaluate: (anExp, environment) => {
      return Evaluator.Cont.Exp.match(anExp,{
        // 数値の評価
        num: (numericValue) => {        
          return Cont.unit(numericValue);
        },
        // 変数の評価
        variable: (name) => {           
          return Cont.unit(Env.lookup(name, environment));
        },
        // 足し算の評価 
        add: (expL, expR) => {          
          return Cont.flatMap(Evaluator.Cont.evaluate(expL, environment))((valueL) => {
            return Cont.flatMap(Evaluator.Cont.evaluate(expR, environment))((valueR) => {
              return Cont.unit(valueL + valueR); 
            });
          });
        },
        /* 関数定義（λ式）の評価  */
        lambda: (variable, body) => {   
          return Evaluator.Cont.Exp.match(variable,{
            variable: (name) => {
              return Cont.unit((actualArg) => {
                return Evaluator.Cont.evaluate(body, 
                                     Env.extend(name, actualArg, environment));
              });
            }
          });
        },
        /* 関数適用の評価 */
        app: (lambda, arg) => {         
          return Cont.flatMap(Evaluator.Cont.evaluate(lambda, environment))((closure) => {
            return Cont.flatMap(Evaluator.Cont.evaluate(arg, environment))((actualArg) => {
              return closure(actualArg); 
            });
          });
        },
        // callcc: (name, exp) => {
        //   return Cont.flatMap()((_) => {

        //   };
        // }
      });
    }
  }
};

module.exports = Evaluator
