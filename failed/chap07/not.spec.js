"use strict";

var expect = require('expect.js');
var util = require('util');

it('!演算子はコンビネータではない', (next) => {
  var multiplyOf = (n) => {
    return (m) => {
      if(m % n === 0) {
        return true;
      } else {
        return false;
      }
    };
  };
  var even = multiplyOf(2);
  /* #@range_begin(not_operator_is_not_combinator) */
  var not = (predicate) => { 
    return ! predicate; // !演算子で論理を反転させる
  };
  var odd = not(even);
  /* #@range_end(not_operator_is_not_combinator) */
  /* #@range_begin(not_operator_is_not_combinator_test) */
  expect(
    odd(3)
  ).to.eql(
    true
  );
  /* #@range_end(not_operator_is_not_combinator_test) */
  next();
}); 
