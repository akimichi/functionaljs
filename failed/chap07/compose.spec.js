"use strict";

var expect = require('expect.js');
var util = require('util');

it('加算と反数の合成は失敗する', (next) => {
  var compose = (f,g) => {
    return (arg) => {
      return f(g(arg));
    };
  };
  /* #@range_begin(compose_opposite_add) */
  var opposite = (x) => {
    return - x;
  };
  var add = (x, y) => {
    return x + y;
  };
  expect(
    compose(opposite,add)(2,3)
  ).to.eql(
    -5 // -(2 + 3) = -5
  );
  /* #@range_end(compose_opposite_add) */
  next();
});
