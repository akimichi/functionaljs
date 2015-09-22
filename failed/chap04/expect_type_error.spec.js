"use strict";

var expect = require('expect.js');

it('expectで型エラーを検出する', (next) => {
  /* #@range_begin(expect_type_error) */
  var one = 1;
  var two = 2;
  var three = "three";
  var four = 4;
  var add = (n,m) => {
	expect(n).to.be.a('number');
	expect(m).to.be.a('number');
	return n + m;
  };
  var multiply = (n,m) => {
	expect(n).to.be.a('number');
	expect(m).to.be.a('number');
	return n * m;
  };
  expect(
	multiply(add(one,two),add(three,four))
  ).to.eql(
	10   /* 1 * 2 + (3 + 4) = 10 を期待するが、NaNが返る */
  );
  /* #@range_end(expect_type_error) */
  next();
});




