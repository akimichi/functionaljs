
      it('チューリング機械で入力を複製する', (next) => {
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

      it('adderからsucc関数を作る', (next) => {
        /* #@range_begin(succ_from_adder) */
        var adder = (m) => {
          return (n) => {
            return m + n;
          };
        };
        var succ = adder(1);
        /* #@range_end(succ_from_adder)  */
        expect(
          succ(1)
        ).to.eql(
          2
        );
        next();
      });
