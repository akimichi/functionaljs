"use strict";

var expect = require('expect.js');
var util = require('util');

it('型の違いで失敗する', (next) => {
  // var number = "three"
  // expect(
  //   4 * 2 + Math.abs(number)
  // ).to.eql(
  //     11   /* 4 * 2 + Math.abs(3) = 11 を期待する */
  // );
  /* #@range_begin(type_error) */
  var one = 1;
  var two = 2;
  var three = "three";
  var four = 4;
  expect(
	one * two * (three + four)
  ).to.eql(
	11   /* 1 * 2 + (3 + 4) = 10 を期待する */
  );
  /* #@range_end(type_error) */
  next();
});


