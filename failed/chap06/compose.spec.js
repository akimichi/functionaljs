"use strict";

var expect = require('expect.js');
var util = require('util');

it('乗算と否定の合成は失敗する', (next) => {
  /* #@range_begin(compose_negate_multiply) */
  var compose = (f, g) => {
    return (arg) => {
      return f(g(arg));
    };
  };
  var negate = (x) => {
    return - x;
  };
  var multiply = (x, y) => {
    return x * y;
  };
  expect(
    compose(negate, multiply)(2,3)
  ).to.eql(
    -6
  );
  /* #@range_end(compose_negate_multiply) */
  next();
});
