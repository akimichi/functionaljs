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
  var lambda = (arg, body) => {
	expect(arg).to.a('function');
	expect(body).to.a('function');
    return (pattern) => {
      return pattern.lambda(arg, body);
    };
  };
  var application = (name, arg) => {
    return (pattern) => {
      return pattern.application(name,arg);
    };
  };
  // var closure = (id, body, env) => {
  //   return (arg) => {
  //     return evaluate(body, extendEnv(id, body,env));
  //   };
  // };
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
	  application: (func, arg) => {
        var rator = evaluate(func, env)
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
  describe('eval', () => {
    it('evaluate number', (next) => {
      expect(
		evaluate(number(2), emptyEnv)
	  ).to.be(
		2
	  );
	  next();
    });
    it('evaluate variable', (next) => {
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
    it('evaluate a constant function application with eval', (next) => {
      var constant = lambda(variable("x"),number(1))
      expect(
		evaluate(constant, emptyEnv)
	  ).to.a(
		'function'
	  );
	  next();
    });
  //   it('evaluate id function as an object with eval', function() {
  //     var func, id;
  //     id = {
  //       arg: "x",
  //       body: "x"
  //     };
  //     func = intp["eval"](id, intp.emptyEnv());
  //     (expect(fj.isFunction(func))).to.be(true);
  //     (expect(func(1))).to.be(1);
  //     return (expect(intp["eval"]([id, 1], intp.emptyEnv()))).to.be(1);
  //   });
  //   return it('evaluate logical function application with eval', function() {
  //     var andFun, falseFun, ifFun, notFun, orFun, trueFun;
  //     trueFun = {
  //       arg: "x",
  //       body: {
  //         arg: "y",
  //         body: "x"
  //       }
  //     };
  //     falseFun = {
  //       arg: "x",
  //       body: {
  //         arg: "y",
  //         body: "y"
  //       }
  //     };
  //     notFun = {
  //       arg: "x",
  //       body: [["x", falseFun], trueFun]
  //     };
  //     andFun = {
  //       arg: "x",
  //       body: {
  //         arg: "y",
  //         body: [["x", "y"], falseFun]
  //       }
  //     };
  //     orFun = {
  //       arg: "x",
  //       body: {
  //         arg: "y",
  //         body: [["x", trueFun], "y"]
  //       }
  //     };
  //     ifFun = {
  //       arg: "pred",
  //       body: {
  //         arg: "x",
  //         body: {
  //           arg: "y",
  //           body: [["pred", "x"], "y"]
  //         }
  //       }
  //     };
  //     (expect(intp["eval"]([[trueFun, 1], 0], intp.emptyEnv()))).to.be(1);
  //     (expect(intp["eval"]([[[notFun, trueFun], 1], 0], intp.emptyEnv()))).to.be(0);
  //     (expect(intp["eval"]([[[[andFun, trueFun], trueFun], 1], 0], intp.emptyEnv()))).to.be(1);
  //     return (expect(intp["eval"]([[[[[ifFun, trueFun], falseFun], trueFun], 1], 0], intp.emptyEnv()))).to.be(0);
  //   });
  // });
  // return describe('consBinding', function() {
  //   it('constructs new binding with consBinding', function() {
  //     var emptyBinding, firstBinding, secondBinding;
  //     emptyBinding = {};
  //     firstBinding = intp.consBinding("a", 1)(emptyBinding);
  //     (expect(firstBinding['a'])).to.be(1);
  //     secondBinding = intp.consBinding("b", 2)(firstBinding);
  //     (expect(secondBinding['b'])).to.be(2);
  //     return (expect(secondBinding['a'])).to.be(1);
  //   });
  //   it('can be composed', function() {
  //     var composedBinding, emptyBinding, firstBinding, secondBinding, thirdBinding;
  //     firstBinding = intp.consBinding("a", 1);
  //     secondBinding = intp.consBinding("b", 2);
  //     thirdBinding = intp.consBinding("c", 3);
  //     emptyBinding = {};
  //     composedBinding = fj.compose(fj.compose(firstBinding, secondBinding), thirdBinding)(emptyBinding);
  //     (expect(composedBinding['a'])).to.be(1);
  //     (expect(composedBinding['b'])).to.be(2);
  //     return (expect(composedBinding['c'])).to.be(3);
  //   });
  //   return it('pipes consBinding with pipe', function() {
  //     var combinedBinding, emptyBinding, firstBinding, secondBinding, thirdBinding;
  //     firstBinding = intp.consBinding("a", 1);
  //     secondBinding = intp.consBinding("b", 2);
  //     thirdBinding = intp.consBinding("c", 3);
  //     emptyBinding = {};
  //     combinedBinding = fj.pipe([firstBinding, secondBinding, thirdBinding])(emptyBinding);
  //     (expect(combinedBinding['a'])).to.be(1);
  //     (expect(combinedBinding['b'])).to.be(2);
  //     return (expect(combinedBinding['c'])).to.be(3);
  //   });
  });
});
