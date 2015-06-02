/* #@range_begin(sample_test) */
var assert = require("assert");

describe('Assertモジュール', function(){
  describe('add', function(){
    var add = (x,y) => {
      return x + y;
    };
    it('should return 2 when add(1,1)', function(){
      assert.equal(2, add(1,1));
    });
  });
});
/* #@range_end(sample_test) */

/* #@range_begin(sample_test_expect) */
var expect = require('expect.js');

describe('Expectモジュール', function(){
  describe('add', function(){
    it('should return 3 when add(1,2)', function(){
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
