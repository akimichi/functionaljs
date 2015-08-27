"use strict";

var expect = require('expect.js');
var util = require('util');

it('負の足し算で失敗する', (next) => {
  var succ = (n) => {
    return n + 1;
  };
  var prev = (n) => {
    return n - 1;
  };
  var add = (x,y) => { // add関数の定義
    if(y < 1){
      return x;
    } else {
      return add(succ(x), prev(y)); // add関数の再帰呼び出し
    }
  };
  /* #@range_begin(add_failed_test) */
  expect(
    add(1,-2)
  ).to.eql(
      -1 // -1 になるべきが、-1ではない
  );
  /* #@range_end(add_failed_test) */
  next();
});
