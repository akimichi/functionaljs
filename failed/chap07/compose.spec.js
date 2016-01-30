"use strict";

var expect = require('expect.js');
var util = require('util');

it('乗算と否定の合成は失敗する', (next) => {
  /* #@range_begin(compose_opposite_add) */
  var compose = (f, g) => {
    return (arg) => {
      return f(g(arg));
    };
  };
  var opposite = (x) => {
    return - x;
  };
  var add = (x, y) => {
    return x + y;
  };
  expect(
    compose(opposite, add)(2,3)
  ).to.eql(
    -6
  );
  /* #@range_end(compose_opposite_add) */
  next();
});
