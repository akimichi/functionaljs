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
            succ(1)
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
        var add = (accumulator, currentItem) => {
          return currentItem + accumulator;
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
            square(2)
          ).to.eql(
            4
          );
          expect(
            square(2)
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
        it('参照透明性な関数では同値なものは置換可能である', (next) => {
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
          /* #@range_end(equals_replacement)  */
          next();
        });
      });
    });
    describe('副作用の種類', () => {
      describe('副作用としての代入', () => {
        it('配列は参照透明性を破壊する', (next) => {
          /* #@range_begin(array_destroys_referential_transparency) */
          var add = (n,m) => {
            return n + m;
          };
          var array = [];
          array.push(1);
          expect(
            add(array.pop(),2)
          ).to.eql(
            3
          );
          array.push(2);
          expect(
            add(array.pop(),2)
          ).to.eql(
            4
          );
          /* #@range_end(array_destroys_referential_transparency) */
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
    });
  });
});
