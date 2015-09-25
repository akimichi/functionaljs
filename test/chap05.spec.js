"use strict";

var expect = require('expect.js');
var util = require('util');

describe('制御構造', () => {
  describe('真理値', () => {
	
  });
  describe('条文文', () => {
	
  });
  describe('パターンマッチ', () => {
	
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
  });
});
