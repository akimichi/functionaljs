"use strict";

var expect = require('expect.js');

describe('再帰処理', () => {
  it('Y combinator', function(next) {
	/* #@range_begin(Y_combinator) */
	var Y = (F) => {
	  return (function(g) {
		return function(x) {
		  return F(g(g))(x);
		};
	  })(function(g) {
		return function(x) {
		  return F(g(g))(x);
		};
	  });
	};
	/* #@range_end(Y_combinator)  */
	/* #@range_begin(Y_combinator_test) */
	var factorial = function(fact) {
	  return function(n) {
		if (n == 0) {
		  return 1;
		} else {
		  return n * fact(n - 1);
		}
	  };
	};
 	var fact = Y(factorial);
	expect(
	  fact(3)
	).to.eql(
	  6
	);
	// Y x = x(Y x)
	/* #@range_end(Y_combinator_test) */
	next();
  });

});
