"use strict";

var expect = require('expect.js');

describe('λ計算機', () => {
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
      if(identifier === queryIdentifier) {
        return value;
      } else {
        return lookupEnv(queryIdentifier,env)
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
	  lambda: (variable, bodyExp) => {
		return (actualArg) => {
		  //expect(actualArg).to.a('string');
		  return match(variable,{
			variable: (name) => {
			  return evaluate(bodyExp, extendEnv(name, actualArg ,env));
			}
		  })
		};
		// return closure(arg, body, env);
	  },
	  application: (variable, arg) => {
        var rator = evaluate(variable, env)
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
  describe('evaluate関数で式を評価する', () => {
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
    // it('id関数', (next) => {
	//   /* λx.x */
    //   var id = lambda(variable("x"),variable("x"))
    //   expect(
	// 	evaluate(id, emptyEnv)
	//   ).to.a(
	// 	'function'
	//   );
	//   var applied = application(id, number(1))
    //   expect(
	// 	evaluate(applied, emptyEnv)
	//   ).to.eql(
	// 	1
	//   );
	//   next();
    // });
	// it('evaluate id function as an object with eval', (next) => {
	  
	// });
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
