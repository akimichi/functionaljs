"use strict";

var expect = require('expect.js');
var util = require('util');

describe('値、型、変数', () => {
  describe('型とは何か', () => {
    it('整数の構成', (next) => {
      /* #@range_begin(integer_construction) */
      var succ = (n) => {
        return n + 1;
      };
      expect(
        succ(0)
      ).to.eql(
        1
      );
      expect(
        succ(succ(0))
      ).to.eql(
        2
      );
      /* #@range_end(integer_construction) */
      next();
    });
  });
});
