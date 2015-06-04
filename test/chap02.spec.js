"use strict";

var expect = require('expect.js');
var util = require('util');

describe('なぜ関数型プログラミングが重要か', () => {
  describe('関数の評価戦略', () => {
    it('succ関数の定義', function(next) {
      /* #@range_begin(succ_definition) */
      var succ = (n) => {
        return n + 1;
      };
      /* #@range_end(succ_definition) */
      /* succ関数のテスト */
      expect(
        succ(0)
      ).to.eql(
        1
      );
      expect(
        succ(1)
      ).to.eql(
          2
      );
      next();
    });
  });
});


/* #@range_begin(sample_test) */
/* #@range_end(sample_test) */
