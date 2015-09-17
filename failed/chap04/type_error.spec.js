"use strict";

var expect = require('expect.js');
var util = require('util');

it('型の違いで失敗する', (next) => {
  var number = "three"
  expect(
    4 * 2 + Math.abs(number)
  ).to.eql(
      11   /* 4 * 2 + Math.abs(3) = 11 を期待する */
  );
  next();
});


