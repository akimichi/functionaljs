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

module.exports = {
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
      }
    },
    evaluate: (anExp, environment) => {
      var self = this;
      return self.Exp.match(anExp,{
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
          return self.Exp.match(variable,{
            variable: (name) => {
              return ID.unit((actualArg) => {
                return self.evaluate(body, 
                                Env.extend(name, actualArg, environment));
              });
            }
          });
        },
        /* 関数適用の評価 */
        app: (lambda, arg) => {         
          return ID.flatMap(self.evaluate(lambda, environment))((closure) => {
            return ID.flatMap(self.evaluate(arg, environment))((actualArg) => {
              return closure(actualArg); 
            });
          });
        },
        // 足し算の評価 
        add: (expL, expR) => {          
          return ID.flatMap(self.evaluate(expL, environment))((valueL) => {
            return ID.flatMap(self.evaluate(expR, environment))((valueR) => {
              return ID.unit(valueL + valueR); 
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
      }
    },
    // Env: {
    //   // 空の環境
    //   empty: (variable) => {
    //     return undefined;
    //   },
    //   /* 変数名に対応する値を環境から取りだす */
    //   // lookupEnv:: (STRING, ENV) => M[VALUE]
    //   lookup: (identifier, environment) => {
    //     return environment(identifier);
    //   },
    //   /* 環境を拡張する */
    //   // extendEnv:: (STRING, VALUE, ENV) => ENV 
    //   extend: (identifier, value, environment) => {
    //     var self = this;
    //     expect(identifier).to.a('string');
    //     return (queryIdentifier) => {
    //       expect(queryIdentifier).to.a('string');
    //       if(identifier === queryIdentifier) {
    //         return value;
    //       } else {
    //         return self.lookup(queryIdentifier, environment);
    //       }
    //     };
    //   }
    // },
    evaluate: (anExp, environment) => {
      var self = this;
      return self.Exp.match(anExp,{
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
          return Cont.flatMap(self.evaluate(expL, environment))((valueL) => {
            return Cont.flatMap(self.evaluate(expR, environment))((valueR) => {
              return Cont.unit(valueL + valueR); 
            });
          });
        },
        /* 関数定義（λ式）の評価  */
        lambda: (variable, body) => {   
          return self.Exp.match(variable,{
            variable: (name) => {
              return Cont.unit((actualArg) => {
                return self.evaluate(body, 
                                     Env.extend(name, actualArg, environment));
              });
            }
          });
        },
        /* 関数適用の評価 */
        app: (lambda, arg) => {         
          return Cont.flatMap(self.evaluate(lambda, environment))((closure) => {
            return Cont.flatMap(self.evaluate(arg, environment))((actualArg) => {
              return closure(actualArg); 
            });
          });
        }
      });
    }
  }
};
