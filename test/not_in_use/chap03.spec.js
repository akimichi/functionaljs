
    /*
      it('times関数によるsum', (next) => {
      var times = (count, fun, arg, memo) => {
      if(count > 1) {
      return times(count-1, fun, arg, fun(memo,arg));
      } else {
      return fun(memo,arg);
      }
      };

      var sum = (seq) => {
      var add = (seq) => {
      return (index, memo)  => {
      return seq[index] + memo;
      };
      return times(seq.length, add(seq), 0, 0);
      };
      expect(
      sum(sequence)
      ).to.eql(
      41
      );
      next();
      });
    */

  // ### 単体テストの実践
  describe('単体テストの実践', () => {
    it('add, multiply, exponential関数再び', (next) => {
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
          return add(succ(x), prev(y));
        }
      };
      var times = (count, fun, arg, memo) => {
        if(count > 1) {
          return times(count-1, fun, arg, fun(memo,arg));
        } else {
          return fun(memo,arg);
        }
      };
      var multiply = (n, m) => {
        return times(m, add, n, 0);
      };
      var exponential = (n, m) => {
        return times(m, multiply, n, 1);
      };
      /* #@range_end(arithmetic_again) */
      /* #@range_begin(multiply_test_example) */
      expect(
        multiply(2, 3) // テストする式を書く
      ).to.eql(
        6             // 期待する値を書く
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
          return add(succ(x), prev(y));
        }
      };
      var times = (count, fun, arg, memo) => {
        if(count > 1) {
          return times(count-1, fun, arg, fun(arg,memo));
        } else {
          return fun(arg,memo);
        }
      };
      var multiply = (n, m) => {
        return times(m, add, n, 0);
      };
      var exponential = (n, m) => {
        return times(m, multiply, n, 1);
      };
      it('multiply関数が失敗する例', (next) => {
        /* #@range_begin(multiply_failed_test) */
        expect(
          multiply(2, 3)
        ).to.eql(
          6
        );
        expect(
          multiply(-2, 3)
        ).to.not.eql(
            -6  // -6になるはずが、-6ではない
        );
        expect(
          multiply(2, -3)
        ).to.not.eql(
            -6 // -6になるはずが、-6ではない
        );
        expect(
          multiply(-2, -3)
        ).to.not.eql(
          6 // 6になるはずが、6ではない
        );
        /* #@range_end(multiply_failed_test) */
        next();
      });
      it('multiplyの改良後', (next) => {
        var add = (x, y) => {
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
        var times = (count, fun, arg, memo) => {
          if(count > 1) {
            return times(count-1, fun, arg, fun(memo,arg)); // times関数を再帰呼出し
          } else {
            return fun(memo, arg);
          }
        };
        /* #@range_begin(multiply_improved) */
        var subtract = (n, m) => {
          return add(n, -m);
        };
        var multiply = (x, y) => {
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
          multiply(3, 0)
        ).to.eql(
          0
        );
        expect(
          multiply(0, 3)
        ).to.eql(
          0
        );
        /* #@range_begin(multiply_improved_test) */
        expect(
          multiply(-2, -3)
        ).to.eql(
          6
        );
        expect(
          multiply(2, -3)
        ).to.eql(
            -6
        );
        expect(
          multiply(-2, 3)
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
        var add = (x, y) => { // add関数の定義
          if(y < 1){
            return x;
          } else {
            return add(succ(x), prev(y)); // add関数の再帰呼び出し
          }
        };
        /* #@range_begin(add_failed_test) */
        expect(
          add(0, 2)
        ).to.eql(
          2
        );
        expect(
          add(-1, 2)
        ).to.eql(
          1
        );
        expect(
          add(1, -2)
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
        var add = (x, y) => {
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
          add(-1, 2)
        ).to.eql(
          1
        );
        expect(
          add(1, -2)
        ).to.eql(
            -1
        );
        expect(
          add(-1, -2)
        ).to.eql(
            -3
        );
        /* #@range_end(add_improved_test) */
        expect(
          add(0, -2)
        ).to.eql(
            -2
        );
        expect(
          add(-2, 0)
        ).to.eql(
            -2
        );
        expect(
          add(0, 2)
        ).to.eql(
          2
        );
        expect(
          add(0, 0)
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
        var add = (x, y) => {
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
        var times = (count, fun, arg, memo) => {
          if(count > 1) {
            return times(count-1, fun, arg, fun(arg,memo)); // times関数を再帰呼出し
          } else {
            return fun(arg,memo);
          }
        };
        var multiply = (n, m) => {
          return times(m, add, n, 0); // 2番目の引数にadd関数を渡している
        };
        expect(
          multiply(2, 3)
        ).to.eql(
          6
        );
        expect(
          multiply(-2, 3)
        ).to.eql(
            -6
        );
        expect(
          multiply(-2, -3)
        ).to.not.eql(
          6
        );
        expect(
          multiply(2, -3)
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
        var add = (x, y) => {
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
        var subtract = (n, m) => {
          return add(n, -m);
        };
        var times = (count, fun, arg, memo) => {
          if(count > 1) {
            return times(count-1, fun, arg, fun(memo,arg)); // times関数を再帰呼出し
          } else {
            return fun(memo, arg);
          }
        };
        var multiply = (n, m) => {
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
          multiply(3, 0)
        ).to.eql(
          0
        );
        expect(
          multiply(0, 3)
        ).to.eql(
          0
        );
        /* -2 * -3 = ((0 - (-2)) - (-2)) - (-2) */
        expect(
          multiply(-2, -3)
        ).to.eql(
          6
        );
        /* 2 * -3 = ((0 - 2) - 2) -2 */
        expect(
          multiply(2, -3)
        ).to.eql(
            -6
        );
        expect(
          multiply(2, 3)
        ).to.eql(
          6
        );
        expect(
          multiply(-2, 3)
        ).to.eql(
            -6
        );
        next();
      });
    });
  });
