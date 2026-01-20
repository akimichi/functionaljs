"use strict";

// 第1章 「計算」とは
// ========

// ## 1.2 <section id='imperative_model'>命令型モデル</section>
describe('命令型モデル', () => {
  // ###  <section id='turing_machine'>チューリング機械</section>
  describe('チューリング機械 turing machine', () => {
    interface Instruction {
      write: string;
      move: number;
      next: string;
    }

    interface Program {
      [state: string]: {
        [symbol: string]: Instruction;
      };
    }

    interface Tape {
      [position: string]: string;
    }

    // **リスト 1.1 JavaScriptによるチューリング機械 **
    /* #@range_begin(turing) */
    const machine = (program: Program, tape: Tape, initState: string, endState: string): Tape | false => {
      /* ヘッドの位置 */
      let position = 0;
      /* 機械の状態 */
      let state = initState;
      /* 実行する命令 */
      let currentInstruction: Instruction | undefined = undefined;
      /*
        以下のwhileループにて、
        現在の状態が最終状態に到達するまで命令を繰り返す
      */
      while (state != endState) {
        const cell = tape[String(position)];
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
      it('チューリング機械でsucc関数を実装する', () => {
        /* #@range_begin(turing_example_succ) */
        /* 2進法で10、つまり10進法で2を表すテープ */
        const tape: Tape = {
          '0': '1',
          '1': '0'
        };
        /* チューリング機械に与えるプログラム */
        const program: Program = {
          'q0': { "1": { "write": "1", "move": 1, "next": 'q0' },
            "0": { "write": "0", "move": 1, "next": 'q0' },
            "B": { "write": "B", "move": -1, "next": 'q1' } },
          'q1': { "1": { "write": "0", "move": -1, "next": 'q1' },
            "0": { "write": "1", "move": -1, "next": 'q2' },
            "B": { "write": "1", "move": -1, "next": 'q3' } },
          'q2': { "1": { "write": "1", "move": -1, "next": 'q2' },
            "0": { "write": "0", "move": -1, "next": 'q2' },
            "B": { "write": "B", "move": 1, "next": 'q4' } },
          'q3': { "1": { "write": "1", "move": 1, "next": 'q4' },
            "0": { "write": "0", "move": 1, "next": 'q4' },
            "B": { "write": "B", "move": 1, "next": 'q4' } }
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
        ).toEqual(
          /* #@range_begin(turing_example_succ_test_result) */
          {
            '-1': 'B',
            '0': '1',
            '1': '1',
            '2': 'B'
          }
          /* #@range_end(turing_example_succ_test_result) */
        );
      });
    });
  });
});

// ## 1.3 <section id='functional_model'>関数型モデル</section>
describe('関数型モデル', () => {
  // ###  <section id='substitution_model'>置換ルール</section>
  describe('置換ルール', () => {
    it('単純なλ式の簡約', () => {
      /* #@range_begin(succ) */
      const succ = (n: number) => {
        return n + 1;
      };
      /* #@range_end(succ) */
      expect(
        succ(1)
      ).toEqual(
        2
      );
    });

    // **リスト 1.5 add関数 **
    it('add関数', () => {
      /* #@range_begin(recursive_add) */
      const succ = (n: number) => {
        return n + 1;
      };
      const prev = (n: number) => {
        return n - 1;
      };
      /* add関数の定義 */
      const add = (x: number, y: number): number => {
        if (y < 1) {
          return x;
        } else {
          /* add関数の再帰呼び出し */
          return add(succ(x), prev(y));
        }
      };
      /* #@range_end(recursive_add) */
      expect(
        add(3, 2)
      ).toEqual(
        5
      );
    });

    // #### コラム 再帰と漸化式
    it('再帰と漸化式', () => {
      // **リスト 1.6 漸化式の例 **
      /* #@range_begin(recursion) */
      const a = (n: number): number => {
        if (n === 1) {
          return 1; // 初項は1
        } else {
          return a(n - 1) + 3; // 公差は3
        }
      };
      /* #@range_end(recursion) */
      expect(
        a(1)
      ).toEqual(
        1
      );
      expect(
        a(2)
      ).toEqual(
        4
      );
      expect(
        a(3)
      ).toEqual(
        7
      );
    });

    // **リスト 1.7 while文を利用したadd関数 **
    it('while文を利用したadd関数', () => {
      /* add関数の定義 */
      /* #@range_begin(imperative_add) */
      const add = (x: number, y: number) => {
        while (y > 0) {  // yが0より大きい間、反復処理を実行する
          x = x + 1;    // 変数xを更新する
          y = y - 1;    // 変数yを更新する
        }
        return x;
      };
      /* #@range_end(imperative_add) */
      expect(
        add(1, 2)
      ).toEqual(
        3
      );
      expect(
        add(2, 3)
      ).toEqual(
        5
      );
      expect(
        add(0, 2)
      ).toEqual(
        2
      );
    });
  });

  // ### <section id='function_definition'>関数の定義</section>
  describe('関数の定義', () => {
    // **リスト 1.8 かけ算の定義 **
    it('かけ算の定義', () => {
      const succ = (n: number) => {
        return n + 1;
      };
      const prev = (n: number) => {
        return n - 1;
      };
      const add = (x: number, y: number): number => { // add関数の定義
        if (y < 1) {
          return x;
        } else {
          return add(succ(x), prev(y)); // add関数の再帰呼び出し
        }
      };
      /* #@range_begin(multiply) */
      const times = (count: number, fun: (a: number, b: number) => number, arg: number, memo: number): number => {
        if (count > 1) {
          /* times関数を再帰呼び出し */
          return times(count - 1, fun, arg, fun(memo, arg));
        } else {
          return fun(memo, arg);
        }
      };
      const multiply = (n: number, m: number) => {
        /* 2番目の引数にadd関数を渡している */
        return times(m, add, n, 0);
      };
      /* #@range_end(multiply) */
      expect(
        add(2, 3)
      ).toEqual(
        5
      );
      expect(
        multiply(2, 3)
      ).toEqual(
        6
      );
      expect(
        multiply(4, 6)
      ).toEqual(
        24
      );
      expect(
        multiply(1, 1)
      ).toEqual(
        1
      );
      expect(
        multiply(0, 1)
      ).toEqual(
        0
      );
    });
  });

  // ### <section id='advantages_of_functional_model'>関数型モデルを使うメリット</section>
  describe('関数型モデルを使うメリット', () => {
    // **リスト 1.9 べき乗の定義 **
    it('べき乗の定義', () => {
      const succ = (n: number) => {
        return n + 1;
      };
      const prev = (n: number) => {
        return n - 1;
      };
      const add = (x: number, y: number): number => {
        if (y < 1) {
          return x;
        } else {
          return add(succ(x), prev(y));
        }
      };
      const times = (count: number, fun: (a: number, b: number) => number, arg: number, memo: number): number => {
        if (count > 1) {
          return times(count - 1, fun, arg, fun(arg, memo));
        } else {
          return fun(arg, memo);
        }
      };
      const multiply = (n: number, m: number) => {
        return times(m, add, n, 0);
      };
      /* #@range_begin(exponential) */
      const exponential = (n: number, m: number) => {
        return times(m, multiply, n, 1);
      };
      /* #@range_end(exponential) */
      expect(
        exponential(2, 3)
      ).toEqual(
        8
      );
    });
  });
});

// [目次に戻る](index.html) [次章に移る](chap02.spec.html)
