"use strict";

// 「計算」とは
// ========


var expect = require('expect.js');
var util = require('util');

// ## 命令型モデル
describe('命令型モデル', () => {
  // ### チューリング機械
  describe('チューリング機械 turing machine', () => {
    /* c.f. http://swizec.com/blog/a-turing-machine-in-133-bytes-of-javascript/swizec/3069 */
    // [![IMAGE ALT TEXT](http://img.youtube.com/vi/lf1KZmYOG_g/0.jpg)](https://www.youtube.com/watch?v=lf1KZmYOG_g "チューリングマシンの動画")
    /* #@range_begin(turing) */
    var machine = (program,tape,initState, endState) => {
      /* ヘッドの位置 */
      var position = 0;                          
      /* 機械の状態 */
      var state = initState;                     
      /* 実行する命令 */
      var currentInstruction = undefined;        
      /*
        以下のwhileループにて、
        現在の状態が最終状態に到達するまで命令を繰り返す
      */
      while(state != endState) {
        var cell = tape[String(position)];
        if (cell)
          currentInstruction = program[state][cell];
        else
          currentInstruction = program[state].B;
        if (!currentInstruction) {
          return false;
        } else {
          /* テープに印字する */
          tape[String(position)] = currentInstruction.write; 
          /* ヘッドを動かす */
          position += currentInstruction.move;                
          /* 次の状態に移る */
          state = currentInstruction.next;                    
        }
      }
      return tape;
    };
    /* #@range_end(turing)  */
    // ### チューリング機械のテスト
    describe('チューリング機械をテストする', () => {
      it('入力を複製する', (next) => {
        /* #@range_begin(turing_example) */
        var program = {
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
        var result = machine(program,     // プログラム
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
        var program = {
          'q0': {"1": {"write": "1", "move": 1, "next": 'q0'},
                 "0": {"write": "0", "move": 1, "next": 'q0'},
                 "B": {"write": "B", "move": -1, "next": 'q1'}}
        };
        expect(
          machine(program,    // プログラム 
                  ['0'],      // テープ
                  'q0',       // 初期状態
                  'q1')       // 終了状態
        ).to.eql(
          ['0','B']
        );
        expect(
          machine(program,    // プログラム
                  ['1'],      // テープ
                  'q0',       // 初期状態
                  'q1')       // 終了状態
        ).to.eql(
          ['1','B']
        );
        next();
      });
      it('succ関数を実装する', (next) => {
        /* #@range_begin(turing_example_succ) */
        /* 2進法で10、つまり10進法で2を表すテープ */
        var tape = { 
          '0':'1',
          '1':'0'
        };
        /* チューリング機械に与えるプログラム */
        var program = {
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
        expect(
          /* #@range_begin(turing_example_succ_test) */
          machine(program,     // プログラム
                  tape,        // テープ
                  'q0',        // 初期状態
                  'q4')        // 終了状態
          /* #@range_end(turing_example_succ_test) */
        ).to.eql(
          /* #@range_begin(turing_example_succ_test_result) */
          {
            '-1': 'B',
            '0': '1',
            '1': '1',
            '2': 'B'
          }
          /* #@range_end(turing_example_succ_test_result) */
        );
        // #### <a name="turing_demo"> **チューリング機械の挙動** </a>
        // 
        // ![チューリング機械の挙動](images/turing_succ.gif) 
        next();
      });
    });
  });
});
// ## 関数型モデル
describe('関数型モデル', () => {
  describe('さまざまなバインド関数 ', () => {
    it('JavaScriptのbind関数', (next) => {
      /* #@range_begin(bind_in_javascript) */
      var foo = (y) => {
        return this.x + y; 
      };
      var object = { x : 1 };
      var bar = foo.bind(object);
      /* #@range_end(bind_in_javascript) */
      expect(
        bar(2)
      ).to.eql(
        3
      );
      next();
    });
    it('monadのbind関数', (next) => {
      /* #@range_begin(bind_in_monad) */
      var bind = (instance) => {
        return (transform) => {
          return transform(instance);
        };
      };
      /* #@range_end(bind_in_monad) */
      expect(
        /* #@range_begin(bind_in_monad_usage) */
        bind(2 * 3)((n) => {
          return n * 4;
        })
        /* #@range_end(bind_in_monad_usage) */
      ).to.eql(
        /* #@range_begin(bind_in_monad_usage_answer) */
        24 
        /* #@range_end(bind_in_monad_usage_answer) */
      );
      next();
    });
  });
  // ### 置換ルール
  describe('置換ルール', () => {
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
    it('再帰関数としてのadd', (next) => {
      /* #@range_begin(recursive_add) */
      var succ = (n) => {
        return n + 1;
      };
      var prev = (n) => {
        return n - 1;
      };
      /* add関数の定義 */
      var add = (x,y) => { 
        if(y < 1){
          return x;
        } else {
          /* add関数の再帰呼び出し */
          return add(succ(x), prev(y)); 
        }
      };
      /* #@range_end(recursive_add) */
      // #### <a name="add_reduction_demo"> **add(3,2)の簡約** </a>
      // ![add関数の簡約](images/add_reduction.gif) 
      expect(
        add(2,1)
      ).to.eql(
        3
      );
      next();
    });
    it('命令的なadd関数', (next) => {
        // var result = x; // 結果を保存する変数result
        // while(y > 0) {  // 反復処理を実行する
        //   result = result + 1; // 変数resultを更新する 
        //   y = y - 1;           // 変数yを更新する
        // }
        // return result;
      /* add関数の定義 */
      /* #@range_begin(imperative_add) */
      var add = (x,y) => { 
        while(y > 0) {  // yが0より大きい間、反復処理を実行する
          x = x + 1;    // 変数xを更新する 
          y = y - 1;    // 変数yを更新する
        }
        return x;
      };
      /* #@range_end(imperative_add) */
      expect(
        add(1,2)
      ).to.eql(
        3
      );
      expect(
        add(2,3)
      ).to.eql(
        5
      );
      expect(
        add(0,2)
      ).to.eql(
        2
      );
      next();
    });
    it('再帰と漸化式', (next) => {
      /* #@range_begin(recursion) */
      var a = (n) => {
        if(n === 1) {
          return 1; // 初項は1
        } else {
          return a(n-1) + 3; // 公差は3
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
    // #### 変数の置換ルール
    it('変数の置換ルール', (next) => {
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
        var foo = 3;     // 変数fooに値3をバインドする
        return foo * 10; // 変数fooの値を10倍にする
      };
      /* #@range_end(local_variable_usage_example) */
      expect(
        bar()
      ).to.eql(
        30
      );
      /* #@range_begin(variable_and_closure) */
      var bar = () => {
        var foo = 3;
        return foo * 10;
      };
      var baz = ((foo) => {
        return foo * 10;
      });
      /* #@range_end(variable_and_closure) */
      expect(
        bar()
      ).to.eql(
        baz(3)          // 仮引数fooに値3をバインドする
      );
      next();
    });
    // #### 関数の置換ルール
    it('関数の置換ルール', (next) => {
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
          /* times関数を再帰呼び出し */
          return times(count-1,fun,arg, fun(memo,arg)); 
        } else {
          return fun(memo,arg);
        }
      };
      var multiply = (n,m) => {
        /* 2番目の引数にadd関数を渡している */
        return times(m, add, n, 0); 
      };
      /* #@range_end(multiply) */
      // #### <a name="multiply_reduction_demo"> **multiply(2,3)の簡約** </a>
      // ![multiply関数の簡約](images/multiply_reduction.gif) 
      expect(
        add(2,3)
      ).to.eql(
        5
      );
      expect(
        multiply(2,3)
      ).to.eql(
        6
      );
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
    /* #@range_end(exponential) */
    expect(
      exponential(2,3)
    ).to.eql(
      8
    );
    next();
  });
});
