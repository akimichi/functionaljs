"use strict";

var expect = require('expect.js');
var util = require('util');

it('型の違いで失敗する', (next) => {
  /* #@range_begin(type_failed_test) */
  var number = "three"
  expect(
    4 * 2 + Math.abs(number)
  ).to.eql(
      11   // 4 * 2 + Math.abs(3) = 11 を期待する
  );
  /* #@range_end(type_failed_test) */
  next();
});


