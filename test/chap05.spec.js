"use strict";

var expect = require('expect.js');
var util = require('util');

describe('制御構造', () => {
  describe('条文分岐', () => {
    it('逆数の例', (next) => {
      /* ##@range_begin(inverse_function)*/
      var inverse = (n) => {
        if(n === 0) {
          return 0;
        } else {
          return 1 / n;
        }
      };
      /* ##@range_end(inverse_function)*/
      expect(
        inverse(2)
      ).to.be(
        0.5
      );
      expect(
        inverse(0)
      ).to.be(
        0
      );
      next();
    });
    describe('真理値', () => {
      it('チャーチの真理値', (next) => {
        /* ##@range_begin(church_truth) */
        var _true = (x) => {
          return (y) => {
            return x;
          };
        };
        var _false = (x) => {
          return (y) => {
            return y;
          };
        };
        var _ifElse = (pred) => {
          return (x) => {
            return (y) => {
              return pred(x)(y);
            };
          };
        };
        var _not = (x) => {
          return (x(_false))(_true);
        };
        var _and = (x) => {
          return (y) => {
            return (x(y))(_false);
          };
        };
        var _or = (x) => {
          return (y) => {
            return (x(_true))(y);
          };
        };
        /* ##@range_end(church_truth) */
        /* ##@range_begin(church_truth_test) */
        expect(
          _true(1)(0)
        ).to.be(
          1
        );
        expect(
          _not(_true)(1)(0)
        ).to.be(
          0
        );
        expect(
          _and(_true)(_true)(1)(0)
        ).to.be(
          1
        );
        expect(
          _ifElse(_true)(_false)(_true)(1)(0)
        ).to.be(
          0
        );

        /* ##@range_end(church_truth_test) */
        next();
      });
    });
  });
  describe('パターンマッチ', () => {
    it("compare", function(next) {
      /* #@range_begin(compare) */
      var compare =  function(n,m){
        if (n > m) {
          return 1;
        } else {
          if(n === m) {
            return 0;
          } else {
            return -1;
          }
        }
      };
      expect(
		compare(3,2)
	  ).to.eql(
		1
	  );
      expect(
		compare(1,1)
	  ).to.eql(
		0
	  );
      expect(
		compare(2,3)
	  ).to.eql(
		  -1
	  );
      /* #@range_end(compare) */
      next();
    });
  });
  describe('代数的データ型', () => {
    it('男女の別をsum型で表現する', (next) => {
      var BMI = (weight /* kg */, height /* cm */) => {
        var height_in_meter = height / 100.0;
        return weight / (height_in_meter * height_in_meter);
      };
      var match = (exp, pattern) => {
        return exp.call(pattern, pattern);
      };
      var male = (data) => {
        return (pattern) => {
          return pattern.male(data);
        }
      };
      var female = (data) => {
        return (pattern) => {
          return pattern.female(data);
        };
      }
      var evaluate = (person) => {
        var self = this;
        return match(person, {
          male: (data) => {
            return {
              BMI: BMI(data.weight, data.height),
              // total body water
              TBW: data.weight * 0.6,
              // estimated blood volume
              EBV: 0.168 * Math.pow(data.height,3) + 0.050 * data.weight + 0.444
            };
          },
          female: (data) => {
            return {
              BMI: BMI(data.weight, data.height),
              TBW: data.weight * 0.5,
              // estimated blood volume = 0.250 \times height^3 + 0.625 \times weight - 0.662
              EBV: 0.250 * Math.pow(data.height,3) + 0.625 * data.weight - 0.662
            };
          },
        });
      }
      var man = male({
        weight: 72,
        height: 175
      });
      var woman = female({
        weight: 54,
        height: 160
      });
      expect(
        evaluate(man).BMI
      ).to.be(23.510204081632654)
      expect(
        evaluate(woman).TBW
      ).to.be(27)
      next();
    });
    it('数式の例', (next) => {
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
      var calculate = (exp) => {
        return match(exp, { // パターンマッチを実行する
          num: (x) => {
            return x;
          },
          add: (x, y) => {
            return calculate(x) + calculate(y);
          },
          mul: (x, y) => {
            return calculate(x) * calculate(y);
          }
        });
      };
      var exp = add(num(1), mul(num(2), num(3)));
      expect(
        calculate(exp)
      ).to.eql(
        7
      )
      /* #@range_end(algebraic_datatype) */
      next();
    });
  });
  describe("反復文", function() {
    /* #@range_begin(while_example) */
    it("while文", function(next) {
      var counter = 0;
      while (counter < 10) {
        counter += 1;
      }
      expect(counter).to.eql(10);
      next();
    });
    /* #@range_end(while_example) */
    /* #@range_begin(for_example) */
    it("for文", function(next) {
      var counter;
      for (counter = 0; counter < 10; counter += 1) {
        ;
      }
      expect(counter).to.eql(10);
      next();
    });
    /* #@range_end(for_example) */
    /* #@range_begin(forEach_example) */
    it("forEach文", function(next) {
      var array = [1,2,3,4,5];
      var sum = 0;
      array.forEach(function(element){
        sum += element;
      });
      expect(sum).to.eql(15);
      next();
    });
    /* #@range_end(forEach_example) */
	describe('再帰処理', () => {
	  var list = {
		empty: [],
		cons: (n, list) => {
          return [n].concat(list);
		},
		head: (list) => {
          return list[0];
		},
		tail: (list) => {
          return list.slice(1,list.length);
		},
		isEmpty: (list) => {
          return list.length === 0;
		}
	  };
	  describe('mapの例', () => {
		/* #@range_begin(recursive_map) */
		var map = (alist, transform) => {
		  if (list.isEmpty(alist)) {
			return list.empty;
		  } else {
			var x = list.head(alist);
			var xs = list.tail(alist);
			return list.cons(transform(x), map(xs, transform));
		  }
		};
		/* #@range_end(recursive_map) */
		var alist = list.cons(1,
							  list.cons(2,
										list.cons(3,
												  list.empty)));
		var double = (n) => {
		  return n * 2;
		};
		var doubledList = map(alist, double);
		expect(
		  list.head(doubledList)
		).to.eql(
		  2
		);
		expect(
		  list.head(list.tail(doubledList))
		).to.eql(
		  4
		);
		expect(
		  list.head(list.tail(list.tail(doubledList)))
		).to.eql(
		  6
		);
      });
	  describe('sumの例', () => {
		/* #@range_begin(recursive_sum) */
	  	var sum = (alist, accumulator) => {
	  	  if (list.isEmpty(alist)) {
	  		return accumulator;
	  	  } else {
			var x = list.head(alist);
	  		var xs = list.tail(alist);
	  		return sum(xs, accumulator + x);
	  	  }
	  	};
		/* #@range_end(recursive_sum) */
		var alist = list.cons(1,
							  list.cons(2,
										list.cons(3,
												  list.empty)));
		expect(
		  sum(alist, 0)
		).to.eql(
		  6
		);
      });
	  describe('maximumの例', () => {
		/* #@range_begin(recursive_maximum) */
	  	var maximum = (alist, accumulator) => {
	  	  if (list.isEmpty(alist)) {
	  		return accumulator;
	  	  } else {
			var x = list.head(alist);
	  		var xs = list.tail(alist);
			if(x > accumulator) {
			  return maximum(xs, x);
			} else {
	  		  return maximum(xs, accumulator);
			}
	  	  }
	  	};
		/* #@range_end(recursive_maximum) */
		var alist = list.cons(2,
							  list.cons(4,
										list.cons(1,
												  list.empty)));
		expect(
		  maximum(alist, 0)
		).to.eql(
		  4
		);
      });
	  describe('factorialの例', () => {
		it("factorial by recursive process", function(next) {
		  /* #@range_begin(recursive_factorial) */
		  var factorial = (n) => {
			if (n === 1) {
			  return 1;
			} else {
			  return n * factorial(n - 1);
			}
		  };
		  /* #@range_end(recursive_factorial) */
		  expect(
			factorial(6)
		  ).to.eql(
			720
		  );
		  next();
		});
		it("factorial by iterative process", function(next) {
		  /* #@range_begin(iterative_factorial) */
		  var factorial = (n) => {
			return fact_iter(1, 1, n);
		  };
		  var fact_iter = (product, counter, max_count) => {
			if (counter > max_count) {
			  return product;
			} else {
			  return fact_iter(counter * product, counter + 1, max_count);
			}
		  };
		  expect(factorial(6)).to.eql(720);
		  /* #@range_end(iterative_factorial) */
		  next();
		});
		it('命令的factorial', function(next) {
		  /* #@range_begin(imperative_factorial) */
		  var factorial = function(n) {
			var index, result;
			result = 1;
			index = 1;
			while (index < n + 1) {
			  result = result * index;
			  index = index + 1;
			}
			return result;
		  };
		  /* #@range_end(imperative_factorial) */
		  expect(
			factorial(5)
		  ).to.eql(
			120
		  );
		  next();
		});
	  });
	});
  });
});
