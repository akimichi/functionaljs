"use strict";

var expect = require('expect.js');
var util = require('util');

it('依然、かけ算は失敗する', (next) => {
  var succ = (n) => {
    return n + 1;
  };
  var prev = (n) => {
    return n - 1;
  };
  var add = (x,y) => {
    if(y === 0){
      return x;
    } else {
      if(y < 0){
        return add(prev(x), succ(y)); // yが負の場合
      } else {
        return add(succ(x), prev(y)); // yが正の場合
      }
    }
  };
  var times = (count,fun,arg, memo) => {
    if(count > 1) {
      return times(count-1,fun,arg, fun(memo,arg));
    } else {
      return fun(memo,arg);
    }
  };
  var exponential = (n,m) => {
    return times(m, multiply, n, 1);
  };
  /* #@range_begin(multiply_failed_again_test) */
  var multiply = (n,m) => {
    return times(m, add, n, 0);
  };
  expect(
    multiply(-2,3)
  ).to.eql(
      -6
  );
  expect(
    multiply(2,-3)
  ).to.eql(
      -6
  );
  /* #@range_end(multiply_failed_again_test) */
  next();
});
