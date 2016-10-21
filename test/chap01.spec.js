"use strict";

// 第1章 「計算」とは
// ========

// ## 小目次
// <div class="toc">
// <ul class="toc">
//   <li><a href="http://akimichi.github.io/functionaljs/chap01.spec.html#imperative_model">1.2 命令型モデル</a>
//     <ul>
//        <li><a href="http://akimichi.github.io/functionaljs/chap01.spec.html#turing_machine">チューリング機械</a></li>
//     </ul></li>
//   <li><a href="http://akimichi.github.io/functionaljs/chap01.spec.html#functional_model">1.3 関数型モデル</a>
//      <ul>
//        <li><a href="http://akimichi.github.io/functionaljs/chap01.spec.html#substitution_model">置換ルール</a></li>
//        <li><a href="http://akimichi.github.io/functionaljs/chap01.spec.html#function_definition">関数の定義</a></li>
//        <li><a href="http://akimichi.github.io/functionaljs/chap01.spec.html#advantages_of_functional_model">関数型モデルを使うメリット</a></li>
//      </ul>
// </ul>
// </div>

/* アサートライブラリー expect.js の読み込み */
var expect = require('expect.js');

// ## 1.2 <section id='imperative_model'>命令型モデル</section>
describe('命令型モデル', () => {
  // ###  <section id='turing_machine'>チューリング機械</section>
  describe('チューリング機械 turing machine', () => {
    // > 参考資料: チューリング機械を現実の装置として製作したもの
    // [![IMAGE ALT TEXT](http://img.youtube.com/vi/E3keLeMwfHY/0.jpg)](https://www.youtube.com/watch?v=E3keLeMwfHY "チューリングマシンの動画")
    /* [A turing machine in 133 bytes of javascript](http://swizec.com/blog/a-turing-machine-in-133-bytes-of-javascript/swizec/3069) を参考にしている */

    // **リスト 1.1 JavaScriptによるチューリング機械 **
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

    // **リスト 1.2 チューリング機械の実行例 **
    describe('チューリング機械の実行例', () => {
      // #### <a name="turing_demo"> **チューリング機械の挙動** </a>
      // ![チューリング機械の挙動](images/turing_succ.gif) 
      it('チューリング機械でsucc関数を実装する', (next) => {
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
          // **リスト 1.3 1を加えるチューリング機械の実行 **
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
        next();
      });
    });
  });
});
// ## 1.3 <section id='functional_model'>関数型モデル</section>
describe('関数型モデル', () => {
  // ###  <section id='substitution_model'>置換ルール</section>
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
    // **リスト 1.5 add関数 **
    it('add関数', (next) => {
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
      // <a name="add_reduction_demo"> add(3,2)の簡約 </a>
      // ![add関数の簡約](images/add-reduction.gif) 
      expect(
        add(3,2)
      ).to.eql(
        5
      );
      next();
    });
    // #### コラム 再帰と漸化式
    it('再帰と漸化式', (next) => {
      // **リスト 1.6 漸化式の例 **
      // 
      // $$ a(n) = a(n-1) + 3$$
      // 
      // ただし、 $$ a(1) = 1 $$
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
    // **リスト 1.7 while文を利用したadd関数 **
    it('while文を利用したadd関数', (next) => {
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
  });
  // ### <section id='function_definition'>関数の定義</section>
  describe('関数の定義', () => {
    // **リスト 1.8 かけ算の定義 **
    it('かけ算の定義', (next) => {
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
  // ### <section id='advantages_of_functional_model'>関数型モデルを使うメリット</section>
  describe('関数型モデルを使うメリット', () => {
    // **リスト 1.9 べき乗の定義 **
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
});

// [目次に戻る](index.html) [次章に移る](chap02.spec.html) 
