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
  });
});
