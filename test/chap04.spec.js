"use strict";

var expect = require('expect.js');
var util = require('util');

describe('値、型、変数', () => {
  describe('型とは何か', () => {
    it('整数の構成', (next) => {
      /* #@range_begin(integer_construction) */
      var succ = (n) => {
        return n + 1;
      };
      expect(
        succ(0)
      ).to.eql(
        1
      );
      expect(
        succ(succ(0))
      ).to.eql(
        2
      );
      /* #@range_end(integer_construction) */
      next();
    });
    it('真理値型は不変である', (next) => {
      /* #@range_begin(truth_is_immutable) */
	  var truth = true;
      expect(
        ! truth
      ).to.eql(
        false
      );
      expect(
        truth
      ).to.eql(
        truth
      );
      /* #@range_end(truth_is_immutable) */
      next();
    });
    it('数値型は不変である', (next) => {
      /* #@range_begin(number_is_immutable) */
      expect(
		1
	  ).to.eql(
		1
	  );
      var one = 1;
      one = one + 1;
      expect(
		one
	  ).to.eql(
		2
	  );
      /* #@range_end(number_is_immutable) */
      next();
    });
    it('文字列は不変である', function(next) {
      /* #@range_begin(string_is_immutable) */
      var str = "to be, or not to be";
      expect(
		str.toUpperCase()
	  ).to.eql(
		"TO BE, OR NOT TO BE"
	  );
      expect(
		str
	  ).to.eql(
		"to be, or not to be"
	  );
      /* #@range_end(string_is_immutable)  */
      next();
    });
  });
  describe('合成型', () => {
	describe('オブジェクト型', () => {
	  it('for in で反復処理する', (next) => {
		/* ##@range_begin(for_in_object) */
		var obj = {
		  one: 1,
		  two: 2
		};
		var results = [];
		for (var key in obj) {
		  results.push(obj[key])
		}
		expect(
		  results
		).to.eql(
		  [1,2]
		);
		/* ##@range_end(for_in_object)     */
		next();
	  });
	  it('値に関数をいれる', (next) => {
		/* ##@range_begin(object_can_embed_function) */
		var natural = {
		  zero: 0,
		  succ: (n) => {
			return n + 1;
		  }
		};
		expect(
		  natural.succ(natural.zero)
		).to.eql(
		  1
		);
		/* ##@range_end(object_can_embed_function) */
		next();
	  });
	  it('オブジェクト型の入れ子', (next) => {
		/* ##@range_begin(object_can_embed_object) */
		var expression = {
		  add: {
			x: 1,
			y: {
			  multiply: {
				x: 2,
				y: 3
			  }
			}
		  }
		};
		expect(
		  expression.add.y.multiply.x
		).to.eql(
		  2
		);
		/* ##@range_end(object_can_embed_object) */
		next();
	  });
	});
	describe('抽象データ型', () => {
	  it('抽象データ型としてのスタック', (next) => {
        var push = (n, stack) => {
          return [n].concat(stack);
        };
		var top = (stack) => {
		  return stack[0];
		}
        var pop = (stack) => {
		  return stack.slice(1,stack.length);
        };
        var empty = [];
		/* #@range_begin(stack_as_abstract_type) */
		expect(
		  top(pop(pop(push(3,push(2,push(1,empty))))))
		).to.eql(
		  1
		);
		/* #@range_end(stack_as_abstract_type) */
		next();
	  });
	  it('抽象データ型としてのオブジェクト型', (next) => {
		/* #@range_begin(object_as_abstract_type) */
		var objects = {
		  empty: {
		  },
		  set: (key,value,obj) => {
			expect(obj).to.an('object');
			obj[key] = value;
			return obj;
		  },
		  get: (key,obj) => {
			expect(obj).to.an('object');
			return obj[key];
		  },
		  isEmpty: (obj) => {
			expect(obj).to.an('object');
			var hasOwnProperty = Object.prototype.hasOwnProperty;
			for(var key in obj){
			  if(hasOwnProperty.call(obj, key))
				return false;
			}
		  },
		  isNotEmpty: (obj) => {
			expect(obj).to.an('object');
			return ! this.objects.isEmpty(obj);
		  },
		};
		expect(
		  objects.set("HAL9000","2001: a space odessay",objects.empty)
		).to.eql(
		  {
			"HAL9000": "2001: a space odessay"
		  }
		)
		/* #@range_end(object_as_abstract_type) */
		next();
	  });
	  it('抽象データ型としての配列型', (next) => {
		/* #@range_begin(array_as_abstract_type) */
		var arrays = {
		  empty: [],
		  cons: (any,array) => {
			expect(array).to.an('array');
			return [any].concat(array);
		  },
		  head: (ary) => {
			expect(ary).to.an('array');
			return ary[0];
		  },
		  tail: (ary) => {
			expect(ary).to.an('array');
			expect(self.isNonEmpty(ary)).to.be.ok();
			return ary.slice(1,ary.length);
		  },
		  get: (index,ary) => {
			  expect(index).to.be.a('number');
			expect(ary).to.an('array');
			return ary[index];
		  },
		  isEmpty: (ary) => {
			expect(ary).to.an('array');
			return self.equal.call(self,ary.length)(0);
		  },
		  isNotEmpty: (ary) => {
			expect(ary).to.an('array');
			return self.not.call(self,self.arrays.isEmpty(ary));
		  }
		};
		expect(
		  arrays.cons(1,arrays.empty)
		).to.eql(
		  [1]
		)
		/* #@range_end(array_as_abstract_type) */
		next();
	  });
	});
	  
	it('代数的データ型', (next) => {
      /* #@range_begin(algebraic_datatype) */
	  var match = (exp, pattern) => {
		return exp.call(pattern, pattern);
	  };
	  var num = (n) => {
		return (pattern) => {
		  return pattern.num(n);
		};
	  };
	  var add = (x, y) => {
		return (pattern) => {
		  return pattern.add(x, y);
		};
	  };
	  var mul = (x, y) => {
		return (pattern) => {
		  return pattern.mul(x, y);
		};
	  };
	  var exp = add(num(1), mul(num(2), num(3)));
	  var evaluate = (exp) => {
		return match(exp, {
		  num: (x) => {
			return x;
		  },
		  add: (x, y) => {
			return evaluate(x) + evaluate(y);
		  },
		  mul: (x, y) => {
			return evaluate(x) * evaluate(y);
		  }
		});
	  };
	  expect(
		evaluate(exp)
	  ).to.eql(
		7
	  )
      /* #@range_end(algebraic_datatype) */
	  next();
	});
	describe('関数型', () => {
	  
	});
  });
});
