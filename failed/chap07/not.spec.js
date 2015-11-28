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
  var odd = ! even;
  expect(
    odd(3)
  ).to.eql(
    true
  );
  /* #@range_end(not_operator_is_not_combinator) */
  next();
}); 
