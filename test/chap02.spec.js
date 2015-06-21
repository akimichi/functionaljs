"use strict";

var expect = require('expect.js');
var util = require('util');

describe('なぜ関数型プログラミングが重要か', () => {
  describe('関数型言語と関数型プログラミング', () => {
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
    describe('関数型プログミングとは', function() {
      describe('第1級市民としての関数', function() {
        it('数値は第1級市民である', function(next) {
          /*  #@range_begin(number_as_first_class_citizen) */
          // 値を変数に束縛する
          var zero = 0;
          var name = "Haskell Curry";
          // 値をデータ構造に埋めこむ
          var birthday = {
            year: 1999,
            month: 1,
            day: 12
          };
          // 値を関数に渡す
          expect(
            Math.sqrt(1)
          ).to.eql(
            1
          );
          // 関数から値を返す
          var birthYear = (birthdayObject) => {
            return birthdayObject.year;
          };
          expect(
            birthYear(birthday)
          ).to.eql(
            1999
          );
          /* #@range_end(number_as_first_class_citizen) */
          next();
        });
        it('関数は変数に束縛できる', function(next) {
          /* #@range_begin(function_bound_to_variable) */
          var succ = (n) => {
            return n + 1;
          };
          expect(
            succ(1) // 変数succを用いてλ式を呼びだす
          ).to.eql(
            2
          );
          /* #@range_end(function_bound_to_variable) */
          next();
        });
        it('関数をオブジェクトに埋めこむ', (next) => {
          /* #@range_begin(function_embedded_in_object) */
          var math = {
            add: (n,m) => {
              return n + m;
            }
          };
          expect(
            math.add(1,2)
          ).to.eql(
            3
          );
          /* #@range_end(function_embedded_in_object) */
          next();
        });
      });
      it('Array.reduceによるsumの定義', (next) => {
        /* #@range_begin(sum_in_array_reduce) */
        var add = (x, y) => {
          return x + y;
        };
        var sum = (array) => {
          return array.reduce(add,0); // reduceにadd関数を渡している
        };
        /* #@range_end(sum_in_array_reduce)  */
        expect(
          sum([1,2,3,4])
        ).to.eql(
          10
        );
        next();
      });
      it('関数を返す', (next) => {
        /* #@range_begin(function_returning_function) */
        var reduce = (init,glue) => {
          return (array) => { // 関数が返る
            if(array.length === 0){
              return init;
            } else {
              var accumulator = glue(array[0], init);
              var tail = array.slice(1,array.length);
              return reduce(accumulator,glue)(tail);
            }
          };
        };
        /* #@range_end(function_returning_function) */
        /* #@range_begin(function_returning_function_test) */
        var adder = (x,y) => {
          return x + y;
        };
        var sum = reduce(0,adder);
        expect(
          sum([1,2,3,4])
        ).to.eql(
          10
        );
        /* #@range_end(function_returning_function_test) */

        // var returnFunction = (func) => {
        //    return (args) => {
        //      return func.apply(this, args);
        //    };
        // };
        // adder . multiplyier
        // expect(
        //    compose(adder)(multiplyier)(2,3)
        // ).to.eql(
        //    10
        // );
        // // expo([2,3,4,...]) = 2 ^ 3 ^ 4 ^ .... = (2 * 2 * 2) * (2 * 2 * 2) * (2 * 2 * 2) * (2 * 2 * 2) * ...
        // var expo = (array) => {
        //   return array.reduce(returnFunctionFromFunction(returnFunctionFromFunction(multiplyier)),1);
        // };
        // expect(
        //    expo([2,3,4])
        // ).to.eql(
        //    24
        // );
        next();
      });
      describe('関数の参照透明性', () => {
        it('算術関数は参照透明性を持つ', (next) => {
          /* #@range_begin(square_definition) */
          var square = (x) => {
            return x * x;
          };
          /*  #@range_end(square_definition) */
          /* #@range_begin(square_is_transparent) */
          expect(
            square(2) // 1回目の実行
          ).to.eql(
            4
          );
          expect(
            square(2) // 2回目の実行
          ).to.eql(
            4
          );
          expect(
            square(3) === square(3)
          ).to.eql(
            true
          );
          /* #@range_end(square_is_transparent) */
          /* #@range_begin(multiply_definition) */
          var multiply = (x,y) => {
            return x * y;
          };
          /*  #@range_end(multiply_definition) */
          /* #@range_begin(multiply_is_transparent) */
          expect(
            multiply(2,3)
          ).to.eql(
            6
          );
          expect(
            multiply(2,3)
          ).to.eql(
            6
          );
          expect(
            multiply(2,3) === multiply(2,3)
          ).to.eql(
            true
          );
          /* #@range_end(multiply_is_transparent) */
          next();
        });
        it('Date.now関数は参照透明性を持たない', (next) => {
          /* #@range_begin(datenow_is_not_transparent) */
          var a = Date.now();

          /* 時間を1秒進める */
          var sleep = require('sleep');
          sleep.sleep(1);

          expect(
            a
          ).to.not.eql( /* 等しくないことをテストしている */
            Date.now()
          );
          /* #@range_end(datenow_is_not_transparent) */
          next();
        });
        // it('console.log関数は参照透明性を持たない', (next) => {
        //   /* #@range_begin(consolelog_is_not_transparent) */
        //   var a = console.log();
        //   expect(
        //     a
        //   ).to.eql( /* 等しくないことをテストしている */
        //     console.log()
        //   );
        //   /* #@range_end(consolelog_is_not_transparent) */
        //   next();
        // });
        describe('参照透明な関数では同値なものは置換可能である', () => {
          it('同じ引数のsquare関数は置換可能である', (next) => {
            /* #@range_begin(equals_replacement)  */
            var square = (n) => {
              return n * n;
            };
            var b = 1;
            var a = b;
            expect(
              square(a)
            ).to.eql(
              square(b)
            );
            expect(
              square(b)
            ).to.eql(
              square(a)
            );
            expect(
              square(a)
            ).to.eql(
              square(a)
            );
            expect(
              square(b)
            ).to.eql(
              square(b)
            );
            /* #@range_end(equals_replacement)  */
            next();
          });
        });
        it('参照不透明な関数では同値なものは置換できない', (next) => {
          /* #@range_begin(unequals_replacement)  */
          var wrapper = (f) => {
            return (args) => {
              return f.call(f, args);
            };
          };
          var now = (_) => {
            return Date.now();
          };
          var b = 1;
          var a = b;
          expect(
            wrapper(now)(a)
          ).to.not.eql(
            wrapper(now)(b)
          );
          /* #@range_end(unequals_replacement)  */
          next();
        });
      });
      describe('変数の参照透明性', () => {
        it('値は参照透明性を持つ', (next) => {
          /* #@range_begin(any_value_has_referential_transparency) */
          expect(
            2
          ).to.eql(
            2
          );
          /* #@range_end(any_value_has_referential_transparency) */
          next();
        });
        it('変数は参照透明性を持つとは限らない', (next) => {
          /* #@range_begin(variable_isnt_referential_transparent) */
          var foo = 2;
          var bar = foo;
          foo = 3;
          expect(
            foo
          ).to.not.eql(
            bar
          );
          /* #@range_end(variable_isnt_referential_transparent) */
          next();
        });
      });
    });
    describe('副作用の種類', () => {
      describe('副作用としての代入', () => {
        it('代入操作は参照透明性を破壊する', (next) => {
          /* #@range_begin(assignment_breaks_referential_transparency) */
          var add = (x, y) => {
            return x + y;
          };
          var x = 1;
          expect(
            add(x, 2)
          ).to.eql(
            3
          );
          x = 2; // ここで変数xに2を代入している
          expect(
            add(x, 2)
          ).to.eql(
            4
          );
          /* #@range_end(assignment_breaks_referential_transparency)  */
          next();
        });
        it('配列は参照透明性を破壊する', (next) => {
          /* #@range_begin(array_destroys_referential_transparency) */
          var array = [];
          array.push(1);
          expect(
            array.pop()
          ).to.eql(
            1
          );
          array.push(2);
          expect(
            array.pop()
          ).to.eql(
            2
          );
          /* #@range_end(array_destroys_referential_transparency) */
          // var add = (n,m) => {
          //   return n + m;
          // };
          // var array = [];
          // array.push(1);
          // expect(
          //   add(array.pop(),2)
          // ).to.eql(
          //   3
          // );
          // array.push(2);
          // expect(
          //   add(array.pop(),2)
          // ).to.eql(
          //   4
          // );
          next();
        });
        it('配列の状態を表示する', (next) => {
          /* #@range_begin(array_destroys_referential_transparency_log) */
          var add = (n,m) => {
            return n + m;
          };
          var array = [];
          array.push(1);
          console.log(array); // [ 1 ]
          expect(
            add(array.pop(),2)
          ).to.eql(
            3
          );
          array.push(2);
          console.log(array); // [ 2 ]
          expect(
            add(array.pop(),2)
          ).to.eql(
            4
          );
          /* #@range_end(array_destroys_referential_transparency_log) */
          next();
        });
      });
      describe('副作用と入出力', () => {
        it('ファイル入出力が参照透明性を破壊する例', (next) => {
          /* #@range_begin(fileio_breaks_referential_transparency)  */
          var fs = require('fs');
          var read = (path) => {
            var readValue = fs.readFileSync(path);
            return parseInt(readValue);
          };
          var write = (path, n) => {
            fs.writeFileSync(path, n);
            return n;
          };
          write('/tmp/io.txt', 1);
          expect(
            read('/tmp/io.txt')
          ).to.eql(
            1
          );
          write('/tmp/io.txt', 2); // ここでファイルに値を書きこむ
          expect(
            read('/tmp/io.txt')
          ).to.eql(
            2
          );
          /* #@range_end(fileio_breaks_referential_transparency)  */
          next();
        });
      });
      describe('副作用への対処', () => {
        it('命令型プログラミングによる乗算', function(next) {
          /* #@range_begin(imperative_addition) */
          var add = function(x,y){
            var times = 0;
            while(times < y){
              x = x + 1;
              times = times + 1;
            };
            return x;
          };
          expect(
            add(2,3)
          ).to.eql(
            5
          );
          /* #@range_end(imperative_addition) */
          var x = 4;
          var y = 5;
          expect(
            add(x,y)
          ).to.eql(
            9
          );
          expect(
            x
          ).to.eql(
            4
          );
          next();
        });
        it('関数型プログラミングによる乗算', function(next) {
          /* #@range_begin(functional_addition) */
          var add = function(x,y){
            if(y < 1){
              return x;
            } else {
              return add(x + 1, y - 1);
            }
          };
          /* #@range_end(functional_addition) */
          expect(add(2,1)).to.eql(3);
          expect(add(2,2)).to.eql(4);
          expect(add(3,2)).to.eql(5);
          expect(add(12,5)).to.eql(17);
          next();
        });
      });
    });
  });
});
