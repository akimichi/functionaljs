"use strict";

// 心の準備
// ========

var expect = require('expect.js');
var util = require('util');

describe('心の準備', () => {
  describe('DRY原則', () => {
    var add = (x,y) => {
      return x + y;
    };
    it('冗長なコードの例', (next) => {
      /* #@range_begin(redundant_code) */
      var timesForMultiply = (count,arg, memo) => {
        if(count > 1) {
          return timesForMultiply(count-1,arg, arg + memo);
        } else {
          return arg + memo;
        }
      };
      var multiply = (n,m) => {
        return timesForMultiply(n, m, 0);
      };
      var timesForExponential = (count,arg, memo) => {
        if(count > 1) {
          return timesForExponential(count-1,arg, arg * memo);
        } else {
          return arg * memo;
        }
      };
      var exponential = (n,m) => {
        return timesForExponential(m, n, 1);
      };
      /* #@range_end(redundant_code) */
      expect(
        multiply(2,3)
      ).to.eql(
        6
      );
      expect(
        exponential(2,3)
      ).to.eql(
        8
      );
      next();
    });
    it('DRYを適用する', (next) => {
      /* #@range_begin(dry_code) */
      var times = (count,fun,arg, memo) => {
        if(count > 1) {
          return times(count-1,fun,arg, fun(arg,memo));
        } else {
          return fun(arg,memo);
        }
      };
      var add = (n, m) => {
        return n + m;
      };
      var multiply = (n,m) => {
        return times(m, add, n, 0); // add関数を渡す
      };
      var exponential = (n,m) => {
        return times(m, multiply, n, 1); // multiply関数を渡す
      };
      /* #@range_end(dry_code) */
      expect(
        multiply(2,3)
      ).to.eql(
        6
      );
      expect(
        exponential(2,3)
      ).to.eql(
        8
      );
      expect(
        multiply(-2,3)
      ).to.eql(
        -6
      );
      next();
    });
  });
  describe('抽象化への指向', () => {
    it('関数抽象の例としてのλ式', (next) => {
      /* #@range_begin(function_abstraction_example) */
      var succ = (n) => {
        return n + 1;
      };
      /* #@range_end(function_abstraction_example) */
      next();
    });
    describe('関数抽象の例としての高階関数', () => {

      var sequence = [2,3,5,7,11,13];
      it('for文によるsum', (next) => {
        /* #@range_begin(sum_for) */
        var sequence = [2,3,5,7,11,13];
        var sum = (seq) => {
          var result = 0;
          for(var index = 0; index < seq.length; index++){
            result = result + seq[index];
          }
          return result;
        };
        sum(sequence);
        /* #@range_end(sum_for) */
        expect(
          sum(sequence)
        ).to.eql(
          41
        );
        next();
      });
      it('forEach関数によるsum', (next) => {
        /* #@range_begin(sum_forEach) */
        var sum = (seq) => {
          /* 結果を格納する変数result */
          var result = 0;
          seq.forEach((item) => {
            result = result + item;
          });
          return result;
        };
        /* #@range_end(sum_forEach) */
        expect(
          sum(sequence)
        ).to.eql(
          41
        );
        next();
      });
      it('reduce関数によるsum', (next) => {
        /* #@range_begin(sum_reduce) */
        var sum = (seq) => {
          return seq.reduce((x,y) => {
            return x + y;
          });
        };
        /* #@range_end(sum_reduce) */
        expect(
          sum(sequence)
        ).to.eql(
          41
        );
        next();
      });
      // it('times関数によるsum', (next) => {
      //   var times = (count,fun,arg, memo) => {
      //     if(count > 1) {
      //       return times(count-1,fun,arg, fun(memo,arg));
      //     } else {
      //       return fun(memo,arg);
      //     }
      //   };

      //   var sum = (seq) => {
      //     var add = (seq) => {
      //       return (index,memo)  => {
      //       return seq[index] + memo;
      //     };
      //     return times(seq.length, add(seq), 0,0);
      //   };
      //   expect(
      //     sum(sequence)
      //   ).to.eql(
      //     41
      //   );
      //   next();
      // });
    });
  });
  describe('意味論の存在', () => {
    it('環境の例', (next) => {
      var merge = (obj1,obj2) => {
        var mergedObject = {};
        for (var attrname in obj1) { mergedObject[attrname] = obj1[attrname]; }
        for (var attrname in obj2) { mergedObject[attrname] = obj2[attrname]; }
        return mergedObject;
      };
      /* #@range_begin(environment_example) */
      /* 空の環境 */
      var emptyEnv = {};
      /* 環境を拡張する */
      var extendEnv = (binding, oldEnv) => {
        /* merge(obj1,obj2) は
           obj1とobj2のオブジェクトをマージする関数のこと */
        return merge(binding, oldEnv); 
      };
      /* 変数名に対応する値を環境から取りだす */
      var lookupEnv = (name, env) => {
        return env[name];
      };
      /* #@range_end(environment_example) */
      expect(((_) => {
        /* #@range_begin(environment_example_usage) */
        /* 空の辞書を作成する */
        var initEnv = emptyEnv;                       
        /* var a = 1 を実行して、辞書を拡張する */
        var firstEnv = extendEnv({"a": 1}, initEnv);  
        /* var b = 3 を実行して、辞書を拡張する */
        var secondEnv = extendEnv({"b": 3}, firstEnv); 
        /* 辞書から b の値を参照する */
        lookupEnv("b",secondEnv);                
        /* #@range_end(environment_example_usage) */
        return lookupEnv("b",secondEnv);                 
      })()).to.eql(
        3
      );
      next();
    });
  });
  describe('テストに親しむ', () => {
    describe('アサーション', () => {
      it('assertによる表明', function(){
        /* #@range_begin(assert_assertion) */
        var assert = require("assert");
        assert.equal(1 + 2, 3);
        /* #@range_end(assert_assertion) */
      });
      it('expectによる表明', function(){
        /* #@range_begin(expect_assertion) */
        var expect = require('expect.js');
        expect(
          1+2
        ).to.eql(
          3
        );
        /* #@range_end(expect_assertion) */
      });
    });
    it('add,multiply,exponential関数再び', (next) => {
      var succ = (n) => {
        return n + 1;
      };
      var prev = (n) => {
        return n - 1;
      };
      /* #@range_begin(arithmetic_again) */
      var add = (x, y) => {
        if(y < 1){
          return x;
        } else {
          return add(succ(x),prev(y));
        }
      };
      var times = (count,fun,arg, memo) => {
        if(count > 1) {
          return times(count-1,fun,arg, fun(memo,arg));
        } else {
          return fun(memo,arg);
        }
      };
      var multiply = (n,m) => {
        return times(m, add, n, 0);
      };
      var exponential = (n,m) => {
        return times(m, multiply, n, 1);
      };
      /* #@range_end(arithmetic_again) */
      /* #@range_begin(multiply_test_example) */
      expect(
        multiply(2,3) // テストする式を書く
      ).to.eql(
        6        // 期待する値を書く
      );
      /* #@range_end(multiply_test_example) */
      next();
    });
    describe('テストによるコードの改良', () => {
      var succ = (n) => {
        return n + 1;
      };
      var prev = (n) => {
        return n - 1;
      };
      var add = (x, y) => {
        if(y < 1){
          return x;
        } else {
          return add(succ(x),prev(y));
        }
      };
      var times = (count,fun,arg, memo) => {
        if(count > 1) {
          return times(count-1,fun,arg, fun(arg,memo));
        } else {
          return fun(arg,memo);
        }
      };
      var multiply = (n,m) => {
        return times(m, add, n, 0);
      };
      var exponential = (n,m) => {
        return times(m, multiply, n, 1);
      };
      it('multiply関数が失敗する例', (next) => {
        /* #@range_begin(multiply_failed_test) */
        expect(
          multiply(2,3)
        ).to.eql(
          6
        );
        expect(
          multiply(-2,3)
        ).to.not.eql(
          -6  // -6になるはずが、-6ではない
        );
        expect(
          multiply(2,-3)
        ).to.not.eql(
          -6 // -6になるはずが、-6ではない
        );
        expect(
          multiply(-2,-3)
        ).to.not.eql(
          6 // 6になるはずが、6ではない
        );
        /* #@range_end(multiply_failed_test) */
        next();
      });
      it('multiplyの改良後', (next) => {
        var add = (x,y) => {
          if(y === 0){
            return x;
          } else {
            if(y < 0){
              return add(prev(x), succ(y));
            } else {
              return add(succ(x), prev(y));
            }
          }
        };
        var times = (count,fun,arg, memo) => {
          if(count > 1) {
            return times(count-1,fun,arg, fun(memo,arg)); // times関数を再帰呼出し
          } else {
            return fun(memo, arg);
          }
        };
        /* #@range_begin(multiply_improved) */
        var subtract = (n,m) => {
          return add(n, -m);
        };
        var multiply = (x,y) => {
          if(y === 0) {
            return 0;
          } else if(y < 0) {
            /* (0 - (x - (x - ... ))) */
            return times(-y, subtract, x, 0); 
          } else {
            /* (0 + (x + (x + ... ))) */
            return times(y, add, x, 0);       
          }
        };
        /* #@range_end(multiply_improved) */
        expect(
          multiply(3,0)
        ).to.eql(
          0
        );
        expect(
          multiply(0,3)
        ).to.eql(
          0
        );
        /* #@range_begin(multiply_improved_test) */
        expect(
          multiply(-2,-3)
        ).to.eql(
          6
        );
        expect(
          multiply(2,-3)
        ).to.eql(
          -6
        );
        expect(
          multiply(-2,3)
        ).to.eql(
          -6
        );
        /* #@range_end(multiply_improved_test) */
        next();
      });
    });
    describe('add関数のリファクタリング', () => {
      it('リファクタリング前', (next) => {
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
          add(0,2)
        ).to.eql(
          2
        );
        expect(
          add(-1,2)
        ).to.eql(
          1
        );
        // expect(
        //   add(3,-2)
        // ).to.eql(
        //   1
        // );
        expect(
          add(1,-2)
        ).to.not.eql(
          -1 // -1 になるべきが、-1ではない
        );
        /* #@range_end(add_failed_test) */
        next();
      });
      it('リファクタリング後', (next) => {
        var succ = (n) => {
          return n + 1;
        };
        var prev = (n) => {
          return n - 1;
        };
        /* #@range_begin(add_improved) */
        var add = (x,y) => {
          if(y === 0){
            return x;                       // yが0の場合
          } else {
            if(y < 0){
              return add(prev(x), succ(y)); // yが負の場合
            } else {
              return add(succ(x), prev(y)); // yが正の場合
            }
          }
        };
        /* #@range_end(add_improved) */
        /* #@range_begin(add_improved_test) */
        expect(
          add(-1,2)
        ).to.eql(
          1
        );
        expect(
          add(1,-2)
        ).to.eql(
          -1
        );
        expect(
          add(-1,-2)
        ).to.eql(
          -3
        );
        /* #@range_end(add_improved_test) */
        expect(
          add(0,-2)
        ).to.eql(
          -2
        );
        expect(
          add(-2,0)
        ).to.eql(
          -2
        );
        expect(
          add(0,2)
        ).to.eql(
          2
        );
        expect(
          add(0,0)
        ).to.eql(
          0
        );
        next();
      });
    });
    describe('multiply関数のリファクタリング', () => {
      it('リファクタリング前', (next) => {
        var succ = (n) => {
          return n + 1;
        };
        var prev = (n) => {
          return n - 1;
        };
        var add = (x,y) => {
          if(y === 0){
            return x;
          } else {
            if(y < 0){
              return add(prev(x), succ(y));
            } else {
              return add(succ(x), prev(y));
            }
          }
        };
        var times = (count,fun,arg, memo) => {
          if(count > 1) {
            return times(count-1,fun,arg, fun(arg,memo)); // times関数を再帰呼出し
          } else {
            return fun(arg,memo);
          }
        };
        var multiply = (n,m) => {
          return times(m, add, n, 0); // 2番目の引数にadd関数を渡している
        };
        expect(
          multiply(2,3)
        ).to.eql(
          6
        );
        expect(
          multiply(-2,3)
        ).to.eql(
          -6
        );
        expect(
          multiply(-2,-3)
        ).to.not.eql(
          6
        );
        expect(
          multiply(2,-3)
        ).to.not.eql(
          -6
        );
        next();
      });
      it('リファクタリング後', (next) => {
        var succ = (n) => {
          return n + 1;
        };
        var prev = (n) => {
          return n - 1;
        };
        var add = (x,y) => {
          if(y === 0){
            return x;
          } else {
            if(y < 0){
              return add(prev(x), succ(y));
            } else {
              return add(succ(x), prev(y));
            }
          }
        };
        var subtract = (n,m) => {
          return add(n, -m);
        };
        var times = (count,fun,arg, memo) => {
          if(count > 1) {
            return times(count-1,fun,arg, fun(memo,arg)); // times関数を再帰呼出し
          } else {
            return fun(memo, arg);
          }
        };
        var multiply = (n,m) => {
          if(m === 0) {
            return 0;
          } else {
            if(m < 0) {
              return times(-m, subtract, n, 0);
            } else {
              return times(m, add, n, 0);
            }
          }
        };
        expect(
          multiply(3,0)
        ).to.eql(
          0
        );
        expect(
          multiply(0,3)
        ).to.eql(
          0
        );
        // -2 * -3 = ((0 - (-2)) - (-2)) - (-2)
        expect(
          multiply(-2,-3)
        ).to.eql(
          6
        );
        // 2 * -3 = ((0 - 2) - 2) -2
        expect(
          multiply(2,-3)
        ).to.eql(
          -6
        );
        expect(
          multiply(2,3)
        ).to.eql(
          6
        );
        expect(
          multiply(-2,3)
        ).to.eql(
          -6
        );
        next();
      });
    });
  });
});
