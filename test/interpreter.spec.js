"use strict";

var expect = require('expect.js');

describe('λ計算機', () => {
  // ## 環境
  var emptyEnv = (variable) => {
    return undefined;
  };
  /* 変数名に対応する値を環境から取りだす */
  var lookupEnv = (variable, env) => {
    return env(variable);
  };
  /* 環境を拡張する */
  var extendEnv = (variable, value, env) => {
    return (queryVariable) => {
      if(variable === queryVariable) {
        return value;
      } else {
        return lookupEnv(queryVariable,env)
      }
    }
  };
  // ## 式の代数的データ構造
  var match = (exp, pattern) => { 
    return exp.call(pattern, pattern);
  };
  var number = (value) => {
    return (pattern) => {
      return pattern.number(value);
    };
  };
  var variable = (name) => {
    return (pattern) => {
      return pattern.variable(value);
    };
  };
  var lambda = (arg, body) => {
    return (pattern) => {
      return pattern.lambda(arg, body);
    };
  };
  var application = (name, arg) => {
    return (pattern) => {
      return pattern.application(name,arg);
    };
  };
  var closure = (id, body, env) => {
    return (arg) => {
      return evaluate(body, extendEnv(id, body,env));
    };
  };
  // ## 式の評価関数
  var evaluate = (exp, env) => {
	return match(exp,{
	  number: (value) => {
		return value;
	  },
	  variable: (name) => {
        return lookupEnv(name, env);
	  },
	  lambda: (arg, body) => {
		return (actualArg) => {
		  return evaluate(body, extendEnv(arg, actualArg ,env));
		};
		// return closure(arg, body, env);
	  },
	  application: (name, arg) => {
        var rator = evaluate(name, env)
        var rand = evaluate(arg, env)
        return rator(rand);
      }
    });
  };
  describe('環境', () => {
    it('extendEnvで環境を作り、 lookupEnv で環境を探る', (next) => {
      var newEnv = extendEnv('a',1, emptyEnv);
      expect(
		lookupEnv("a", newEnv)
	  ).to.be(
		1
	  );
	  next();
    });
    // it('gets a value with applyEnv', function() {
    //   var env, mapping, newEnv, newMapping;
    //   mapping = {
    //     x: 1,
    //     y: 2
    //   };
    //   env = intp.extendEnv(mapping, intp.emptyEnv());
    //   (expect(intp.applyEnv(env, "x"))).to.be(1);
    //   newMapping = {
    //     z: 3
    //   };
    //   newEnv = intp.extendEnv(newMapping, env);
    //   (expect(intp.applyEnv(newEnv, "z"))).to.be(3);
    //   return (expect(intp.applyEnv(newEnv, "y"))).to.be(2);
    // });
  });
});
