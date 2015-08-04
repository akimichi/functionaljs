"use strict";

var expect = require('expect.js');
var util = require('util');

describe('「計算」とは', () => {
  describe('チューリング機械', () => {
    describe('チューリング機械 turing machine', () => {
      // c.f. http://swizec.com/blog/a-turing-machine-in-133-bytes-of-javascript/swizec/3069
      /* #@range_begin(turing) */
      var machine = (code,tape,initState, endState) => {
        var position = 0;                          // ヘッドの位置
        var state = initState;                     // 機械の状態
        var currentInstruction = undefined;        // 実行する命令
        /*
         以下のwhileループにて、現在の状態が最終状態に到達するまで命令を繰り返す
         */
        while(state != endState) {
          var cell = tape[String(position)];
          if (cell)
            currentInstruction = code[state][cell];
          else
            currentInstruction = code[state].B;
          if (!currentInstruction) {
            return false;
          } else {
            tape[String(position)] = currentInstruction.write; // テープに印字する
            position += currentInstruction.move;                // ヘッドを動かす
            state = currentInstruction.next;                    // 次の状態に移る
          }
        }
        return tape;
      };
      /* #@range_end(turing)  */
      describe('turingをテストする', () => {
        it('入力を複製する', (next) => {
          /* #@range_begin(turing_example) */
          var code = {
            'q0': {"1": {"write": "B", "move": 1, "next": 'q1'}},
            'q1': {"1": {"write": "1", "move": 1, "next": 'q1'},
                   "0": {"write": "0", "move": 1, "next": 'q2'},
                   "B": {"write": "0", "move": 1, "next": 'q2'}},
            'q2': {"1": {"write": "1", "move": 1, "next": 'q2'},
                   "B": {"write": "1", "move": -1, "next": 'q3'}},
            'q3': {"1": {"write": "1", "move": -1, "next": 'q3'},
                   "0": {"write": "0", "move": -1, "next": 'q3'},
                   "B": {"write": "1", "move": 1, "next": 'q4'}},
            'q4': {"1": {"write": "B", "move": 1, "next": 'q1'},
                   "0": {"write": "0", "move": 1, "next": 'q5'}}
          };
          var tape = {
            '0': '1',
            '1':'1',
            '2':'1'
          };
          var result = machine(code,// 命令コード
                               tape,        // テープ
                               'q0',        // 初期状態
                               'q5');       // 終了状態
          expect(
            result
          ).to.eql(
            ['1','1','1','0','1','1','1' ]
          );
          /* #@range_end(turing_example) */
          next();
        });
        it('最下位桁までヘッドを移動する', (next) => {
          var code = {
            'q0': {"1": {"write": "1", "move": 1, "next": 'q0'},
                   "0": {"write": "0", "move": 1, "next": 'q0'},
                   "B": {"write": "B", "move": -1, "next": 'q1'}}
          };
          expect(
            machine(code,// 命令コード
                    ['0'],        // テープ
                    'q0',        // 初期状態
                    'q1')       // 終了状態
          ).to.eql(
            ['0','B']
          );
          expect(
            machine(code,// 命令コード
                    ['1'],        // テープ
                    'q0',        // 初期状態
                    'q1')       // 終了状態
          ).to.eql(
            ['1','B']
          );
          next();
        });
        it('succ関数を実装する', (next) => {
          /* #@range_begin(turing_example_succ) */
          var tape = {
            '0':'1',
            '1':'0'
          };
          var code = {
            'q0': {"1": {"write": "1", "move": 1, "next": 'q0'},
                   "0": {"write": "0", "move": 1, "next": 'q0'},
                   "B": {"write": "B", "move": -1, "next": 'q1'}},
            'q1': {"1": {"write": "0", "move": -1, "next": 'q1'},
                   "0": {"write": "1", "move": -1, "next": 'q2'},
                   "B": {"write": "1", "move": -1, "next": 'q3'}},
            'q2': {"1": {"write": "1", "move": -1, "next": 'q2'},
                   "0": {"write": "0", "move": -1, "next": 'q2'},
                   "B": {"write": "B", "move": 1, "next": 'q4'}},
            'q3': {"1": {"write": "1", "move": 1, "next": 'q4'},
                   "0": {"write": "0", "move": 1, "next": 'q4'},
                   "B": {"write": "B", "move": 1, "next": 'q4'}}
          };
          /* #@range_end(turing_example_succ) */
          /* #@range_begin(turing_example_succ_test) */
          expect(
            machine(code,// 命令コード
                    tape,        // テープ
                    'q0',        // 初期状態
                    'q4')       // 終了状態
          ).to.eql(
            {
              '-1': 'B',
              '0': '1',
              '1': '1',
              '2': 'B'
            }
            //['1','1','B']
          );
          /* #@range_end(turing_example_succ_test) */
          next();
        });
      });
    });
  });
  describe('関数型モデル', () => {
    describe('置換モデル', () => {
      it('単純なλ式の簡約', (next) => {
        /* #@range_begin(succ) */
        var succ = (n) => {
          return n + 1;
        };
        /* #@range_end(succ) */
        expect(
          succ(1)
        ).to.eql(
          2
        );
        next();
      });
      it('sumsquaresとsquareの簡約', (next) => {
        /* #@range_begin(sumsquares) */
        var sumsquares = (x,y) => {
          return square(x) + square(y);
        };
        var square = (x) => {
          return x * x;
        };
        /* #@range_end(sumsquares) */
        expect(
          sumsquares(3,4)
        ).to.eql(
          25
        );
        next();
      });
      it('足し算 add の簡約', (next) => {
        /* #@range_begin(add) */
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
        /* #@range_end(add) */
        expect(
          add(2,1)
        ).to.eql(
          3
        );
        next();
      });
      it('再帰と漸化式', (next) => {
        /* #@range_begin(recursion) */
		var a = (n) => {
		  if(n === 1) {
			return 1;
		  } else {
			return a(n-1) + 3;
		  }
		};
        /* #@range_end(recursion) */
        expect(
          a(1)
        ).to.eql(
          1
        );
        expect(
          a(2)
        ).to.eql(
          4
        );
        expect(
          a(3)
        ).to.eql(
          7
        );
        next();
      });
      it('チャーチ数', (next) => {
        /* #@range_begin(church_numeral) */
        var zero = (f) => {
          return (x) => {
            return x;
          };
        };
        var one = (f) => {
          return (x) => {
            return f(x);
          };
        };
        var two = (f) => {
          return (x) => {
            return f(f(x));
          };
        };
        var three = (f) => {
          return (x) => {
            return f(f(f(x)));
          };
        };
        var succ = (n) => {
          return (f) => {
            return (x) => {
              return f(n(f)(x));
            };
          };
        };
        var add = (m) => {
          return (n) => {
            return (f) => {
              return (x) => {
                return m(f)(n(f)(x));
              };
            };
          };
        };
        /*#@range_end(church_numeral) */

        /* #@range_begin(closure_as_counter) */
        var counter = (init) => {
          var _init = init;
          return (dummy) => {
            _init = _init + 1;
            return _init;
          };
        };
        expect(one(counter(0))()).to.eql(1);
        expect(two(counter(0))()).to.eql(2);
        expect(three(counter(0))()).to.eql(3);
        expect(succ(one)(counter(0))()).to.eql(2);
        expect(succ(two)(counter(0))()).to.eql(3);
        expect(add(zero)(one)(counter(0))()).to.eql(1);
        expect(add(one)(one)(counter(0))()).to.eql(2);
        expect(add(one)(two)(counter(0))()).to.eql(3);
        expect(add(two)(three)(counter(0))()).to.eql(5);
        /* #@range_end(closure_as_counter) */
        next();
      });
      // it('変数の置換規則', (next) => {
      //   /* #@range_begin(variable_reduction) */
      //   var foo = 3; // 変数fooを3に束縛する
      //   expect(
      //     foo        // 束縛にもとづいて変数fooを評価する
      //   ).to.eql(
      //     3
      //   );
      //   /* #@range_end(variable_reduction) */
      //   next();
      // });
      it('変数の置換規則', (next) => {
        /* #@range_begin(variable_and_function) */
        var foo = () => {
          return 3;
        };
        expect(
          foo()
        ).to.eql(
          3
        );
        /* #@range_end(variable_and_function) */
        /* #@range_begin(local_variable_usage_example) */
        var bar = () => {
          var foo = 3;     // 変数fooに値3を束縛する
          return foo * 10; // 変数fooの値を10倍にする
        };
        expect(
          bar()
        ).to.eql(
          30
        );
        /* #@range_end(local_variable_usage_example) */
        /* #@range_begin(variable_and_closure) */
        var bar = () => {
          var foo = 3;
          return foo * 10;
        };
        var baz = ((foo) => {
          return foo * 10;
        });
        expect(
          bar()
        ).to.eql(
          baz(3)          // 仮引数fooに値3を束縛する
        );
        /* #@range_end(variable_and_closure) */
        next();
      });
      it('関数の置換規則', (next) => {
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
            return add(succ(x),prev(y)); // add関数の再帰呼び出し
          }
        };
        /* #@range_begin(multiply) */
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
        /* #@range_end(multiply) */
        expect(
          multiply(4,6)
        ).to.eql(
          24
        );
        expect(
          multiply(1,1)
        ).to.eql(
          1
        );
        expect(
          multiply(0,1)
        ).to.eql(
          0
        );
        next();
      });
    });
    it('べき乗の定義', (next) => {
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
      /* #@range_begin(exponential) */
      var exponential = (n,m) => {
        return times(m, multiply, n, 1);
      };
      expect(
        exponential(2,3)
      ).to.eql(
        8
      );
      /* #@range_end(exponential) */
      next();
    });
  });
});
