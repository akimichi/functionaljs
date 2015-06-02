/* #@range_begin(sample_test) */
var assert = require("assert");

describe('assertによる表明', function(){
  describe('add関数', function(){
    var add = (x,y) => {
      return x + y;
    };
    it('add(1,1)は 2 を返す', function(){
      assert.equal(2, add(1,1));
    });
  });
});
/* #@range_end(sample_test) */

/* #@range_begin(sample_test_expect) */
var expect = require('expect.js');

describe('expectによる表明', function(){
  describe('add関数', function(){
    it('add(1,1)は 3 を返す', function(){
      var add = (x,y) => {
        return x + y;
      };
      expect(
        add(1,2)
      ).to.eql(
        3
      );
    });
  });
});
/* #@range_end(sample_test_expect) */
