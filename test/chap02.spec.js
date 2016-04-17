"use strict";

var expect = require('expect.js');
var util = require('util');

var compose = (f,g) => {
  return (arg) => {
    return f(g(arg));
  };
};

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
          // 値を変数にバインドする
          var zero = 0;
          var name = "Haskell Curry";
          // 値をデータ構造に埋めこむ
          var birthday = {
            year: 1999,
            month: 1,
            day: 12
          };
          // 関数から値を返す
          var birthYear = (birthdayObject) => {
            return birthdayObject.year;
          };
          /* #@range_end(number_as_first_class_citizen) */
          // 値を関数に渡す
          expect(
            Math.sqrt(1)
          ).to.eql(
            1
          );
          expect(
            birthYear(birthday)
          ).to.eql(
            1999
          );
          next();
        });
        it('関数は変数にバインドできる', (next) => {
          /* #@range_begin(function_bound_to_variable) */
          var succ = (n) => {
            return n + 1;
          };
          /* #@range_end(function_bound_to_variable) */
          expect(
            succ(1) // 変数succを用いてλ式を呼びだす
          ).to.eql(
            2
          );
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
      it('Array.forEachによるsumの定義', (next) => {
        /* #@range_begin(sum_forEach) */
        var sum = (array) => {
          var result = 0;
          array.forEach((item) => { // forEachに関数を渡す
            result = result + item;
          });
          return result;
        };
        /* #@range_end(sum_forEach)  */
        expect(
          sum([1,2,3,4])
        ).to.eql(
          10
        );
        next();
      });
      describe('関数を返す', () => {
        it('adderを定義する', (next) => {
          /* #@range_begin(adder_definition) */
          var adder = (n) => {
            return (m) => { // 関数を返す
              return n + m;
            };
          };
          /* #@range_end(adder_definition) */
          var succ = adder(1);
          expect(
            succ(0)
          ).to.eql(
            1
          );
          next();
        });
        // it('forEachを定義する', (next) => {
        //   /* #@range_begin(forEach_definition) */
        //   var forEach = (array) => {
        //     return (func) => {
        //       if(array.length === 0){
        //         return null;
        //       } else {
        //         func(array[0]);
        //         var tail = array.slice(1,array.length);
        //         return forEach(tail);
        //       }
        //     };
        //   };
        //   /* #@range_end(forEach_definition) */
        //   next();
        // });
        it('reduceを定義する', (next) => {
          /* #@range_begin(reduce_definition) */
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
          /* #@range_end(reduce_definition) */
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
          var sleep = require('sleep-async')();
          //var sleep = require('sleep');
          //sleep.sleep(1);
          sleep.sleep(2000, () => {
            expect(
              a
            ).to.not.eql( /* 等しくないことをテストしている */
              Date.now()
            );
          });

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
          var aResult = wrapper(now)(a);
          // expect(
          //   aResult
          // ).to.not.eql(
          //   wrapper(now)(b)
          // );
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
        it('変数が参照透明性を持つ場合', (next) => {
          /* #@range_begin(variable_is_referential_transparent) */
          var x = 1;
          expect(
            x 
          ).to.eql(
            x
          );
          var y = x;
          expect(
            y
          ).to.eql(
            1
          );
          /* #@range_end(variable_is_referential_transparent) */
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
          /* #@range_end(fileio_breaks_referential_transparency)  */
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
          next();
        });
      });
      describe('副作用への対処', () => {
        it('命令型プログラミングによる乗算', (next) => {
          /* #@range_begin(imperative_addition) */
          var add = (x,y) => {
            var times = 0;          // times変数は反復の回数を数えるための変数
            var result = x;         // result変数は足し算の結果を保持するための変数

            while(times < y){       // while文は反復を処理する
              result = result + 1;
              times = times + 1;    // times変数を代入で更新する
            };
            return result;
          };
          /* #@range_end(imperative_addition) */
          expect(
            add(2,3)
          ).to.eql(
            5
          );
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
          var add = (x,y) => {
            if(y < 1){
              return x;
            } else {
              return add(x + 1, y - 1); // 新しい引数でadd関数を再帰的に呼び出す
            }
          };
          /* #@range_end(functional_addition) */
          expect(add(2,1)).to.eql(3);
          expect(add(2,2)).to.eql(4);
          expect(add(3,2)).to.eql(5);
          expect(add(12,5)).to.eql(17);
          next();
        });
        describe('不変なデータ', () =>  {
          it('不変なデータ型', (next) => {
            /* #@range_begin(immutable_datatype) */
            var empty =  (_) => {
              return null;
            };
            var get = (key, obj) => {
              return obj(key);
            };
            var set = (key, value, obj) => {
              return (key2) => {
                if(key === key2) {
                  return value;
                } else {
                  return get(key2,obj);
                }
              };
            };
            /* #@range_end(immutable_datatype) */
            next();
          });
          describe('命令的なstackの実装', () => {
            var object = {
              empty: (_) => {
                return null;
              },
              get: (key, obj) => {
                return obj(key);
              },
              set: (key, value, obj) => {
                return (key2) => {
                  if(key === key2) {
                    return value;
                  } else {
                    return object.get(key2,obj);
                  }
                };
              }
            };
          });
          describe('命令的なstackの実装', () => {
            /* #@range_begin(imperative_stack) */
            var stack = [];
            var add = (stack) => {
              var x = stack.pop();
              var y = stack.pop();
              return stack.push(x+y);
            };
            var subtract = (stack) => {
              var x = stack.pop();
              var y = stack.pop();
              return stack.push(x-y);
            };
            /* #@range_end(imperative_stack) */
            it('(2 + 3) * 2', function(next) {
              /* #@range_begin(imperative_stack_test) */
              stack.push(2);
              stack.push(3);
              add(stack);
              stack.push(2);
              add(stack);
              expect(
                stack.pop()
              ).to.eql(
                7
              );
              /* #@range_end(imperative_stack_test) */
              next();
            });
            it('(2 + 3) + 2 logged', function(next) {
              var stack = [];
              /* #@range_begin(imperative_stack_log) */
              stack.push(2);
              console.log(stack); // [ 2 ]
              stack.push(3);
              console.log(stack); // [ 2, 3 ]
              add(stack);
              console.log(stack); // [ 5 ]
              stack.push(2);
              console.log(stack); // [ 5, 2 ]
              add(stack);
              console.log(stack); // [ 7 ]
              expect(
                stack.pop()
              ).to.eql(
                7
              );
              console.log(stack); // []
              /* #@range_end(imperative_stack_log) */
              next();
            });
          });
          describe('関数的なstackの実装', () => {
            /* #@range_begin(functional_stack) */
            var push = (n, stack) => {
              return [n].concat(stack);
            };
            var pop = (stack) => {
              return {
                value: stack[0],
                rest: stack.slice(1,stack.length)
              };
            };
            var empty = [];
            var add = (stack) => {
              var x = stack[0];
              var y = stack[1];
              var rest = stack.slice(2,stack.length);
              return push(x+y,rest);
            };
            var subtract = (stack) => {
              var x = stack[0];
              var y = stack[1];
              var rest = stack.slice(2,stack.length);
              return push(x-y,rest);
            };
            var multiply = (stack) => {
              var x = stack[0];
              var y = stack[1];
              var rest = stack.slice(2,stack.length);
              return push(x*y,rest);
            };
            var divide = (stack) => {
              var x = stack[0];
              var y = stack[1];
              var rest = stack.slice(2,stack.length);
              return push(x/y,rest);
            };
            /* #@range_end(functional_stack) */
            it('(2 + 3) * 4', function(next) {
              /* #@range_begin(functional_stack_test) */
              var s0 = empty;
              var s1 = push(2, s0);
              var s2 = push(3, s1);
              var s3 = add(s2);
              var s4 = push(2,s3);
              var s5 = multiply(s4);
              expect(
                pop(s5).value
              ).to.eql(
                10
              );
              /* #@range_end(functional_stack_test) */
              next();
            });
          });
        });
        describe('副作用への対処', () =>  {
          it('副作用が分離されていないコード', (next) => {
            /* #@range_begin(age_sideeffect) */
            var age = (birthYear) => {
              var today = new Date();
              var thisYear = today.getFullYear(); // getFullYear関数は現時点での西暦を返す
              return thisYear - birthYear;
            };
            /* #@range_end(age_sideeffect) */
            next();
          });
          it('副作用が分離されているコード', (next) => {
            /* #@range_begin(age_without_sideeffect) */
            var age = (birthYear, thisYear) => {
              return thisYear - birthYear;
            };
            /* #@range_end(age_without_sideeffect) */
            next();
          });
          it('画面出力を分離する', (next) => {
            /* #@range_begin(tap_console_log) */
            var tap = (target, sideEffect) => {
              sideEffect(target);
              return target;
            };
            var logger = (value) =>{
              console.log(value);
            };
            /* #@range_end(tap_console_log) */
            next();
          });
          describe('副作用を関数のスコープに閉じこめる', () => {
            /* #@range_begin(action) */
            var action = (io) => {
              return (_) => { // 入出力を関数で包み込む
                 return io;
              };
            };
            /* #@range_end(action) */
            var logger = (value) => {
              return action(console.log(value));
            };
            // expect(
            //   action(logger(2))
            // ).to.eql(
            //   2
            // );
            /* #@range_begin(reader_and_writer) */
            var fs = require('fs'); // ファイルを操作するライブラリーfsをロードする
            var read = (path) => { // ファイルを読み込む操作を関数で包みこむ
              return fs.readFileSync(path, 'utf8');
            };
            var write = (path, content) => { // ファイルを書き込む操作を関数で包みこむ
              return fs.writeFileSync(path,content);
            };
            var reader = (path) => { // ファイルを読み込む操作を関数で包みこむ
              return action(fs.readFileSync(path, 'utf8'));
            };
            var writer = (path, content) => { // ファイルを書き込む操作を関数で包みこむ
              return action(fs.writeFileSync(path,content));
            };
            /* #@range_end(reader_and_writer) */
            /* #@range_begin(fileio_actions) */
            var fileio_actions = () => {
              write('/tmp/test.txt', 1);
              read('/tmp/test.txt');
              write('/tmp/test.txt', 2);
              return read('/tmp/test.txt');
            };
            /* #@range_end(fileio_actions) */
            expect(
              fileio_actions()
            ).to.eql(
              2
            );
            expect(
              fileio_actions()
            ).to.eql(
              2
            );
          });
          describe('状態モナド', () => {
            var bind = (operate, continues) => {
              return (stack) => {
                var newState = operate(stack);
                return continues(newState.value)(newState.stack);
              };
            };
            var unit = (n) => {
              return (stack) => {
                return {
                  value: n,
                  stack: stack
                };
              };
            };
            var push = (n) => {
              return (stack) => {
                return unit(undefined)([n].concat(stack));
              };
            };
            var pop = () => {
              return (stack) => {
                return unit(stack[0])(stack.slice(1,stack.length));
              };
            };
            var run = (operate, initState) => {
              return operate(initState);
            };
            var empty = [];

            it('状態を明示化する', (next) =>{
              /* #@range_begin(explicit_state) */
              var push = (n,stack) => {
                return {
                  value: undefined,
                  stack: [n].concat(stack)
                };
              };
              var pop = (stack) => {
                return {
                  value: stack[0],
                  stack: stack.slice(1,stack.length)
                };
              };
              var empty = [];
              var state1 = push(2, empty);
              var state2 = push(3, state1.stack);
              var state3 = pop(state2.stack);
              var state4 = pop(state3.stack);
              expect(
                state4.value
              ).to.eql(
                2
              );
              /* #@range_end(explicit_state) */
              next();
            });
            it('継続を用いる', (next) =>{
              var push = (n,stack) => {
                  return unit(undefined)([n].concat(stack));
              };
              var pop = (stack) => {
                  return unit(stack[0])(stack.slice(1,stack.length));
              };

              /* #@range_begin(bind_defined_by_continuation) */
              var bind = (state, continues) => {
                return continues(state);
              };
              expect(
                bind(push(2,empty), (state1) => {
                  return bind(push(3, state1.stack), (state2) =>{
                    return bind(pop(state2.stack), (state3) => {
                      return bind(pop(state3.stack), (state4) => {
                        return state4;
                      });
                    });
                  });
                })
              ).to.eql(
                {
                  value: 2,
                  stack: []
                }
              );
              /* #@range_end(bind_defined_by_continuation) */
              next();
            });
            // it('push,popのシグネチャを変更(2)', (next) =>{
            //   var bind = (state, continues) => {
            //     return continues(state);
            //   };
            //   var unit = (n) => {
            //     return (stack) => {
            //       return {
            //         value: n,
            //         stack: stack
            //       };
            //     };
            //   };
            //   var push = (n) => {
            //     return (state) => {
            //       return unit(undefined)([n].concat(state.stack));
            //     };
            //   };
            //   var pop = () => {
            //     return (state) => {
            //       return unit(state.stack[0])(state.stack.slice(1,state.stack.length));
            //     };
            //   };
            //   var empty = unit(undefined)([]);
            //   expect(
            //     bind(push(2)(empty), (state1) => {
            //       return bind(push(3)(state1), (state2) =>{
            //         return bind(pop()(state2), (state3) => {
            //           return bind(pop()(state3), (state4) => {
            //             return state4;
            //           });
            //         });
            //       });
            //     })
            //   ).to.eql(
            //     {
            //       value: 2,
            //       stack: []
            //     }
            //   );
            //   next();
            // });
            it('カリー化', (next) =>{
              /* #@range_begin(curring) */
              var bind = (operate, continues) => {
                return (stack) => {
                  var newState = operate(stack);
                  return continues(newState)(newState.stack);
                };
              };
              var unit = (n) => {
                return (stack) => {
                  return {
                    value: n,
                    stack: stack
                  };
                };
              };
              var push = (n) => {
                return (stack) => {
                  return unit(undefined)([n].concat(stack));
                };
              };
              var pop = () => {
                return (stack) => {
                  return unit(stack[0])(stack.slice(1,stack.length));
                };
              };
              expect(
                bind(push(2), (state1) => {
                  return bind(push(3), (state2) =>{
                    return bind(pop(), (state3) => {
                      return bind(pop(), (state4) => {
                        return (stack) => {
                          return unit(state4.value)(stack);
                        };
                      });
                    });
                  });
                })(empty)
              ).to.eql(
                {
                  value: 2,
                  stack: []
                }
              );
              /* #@range_end(curring) */
              next();
            });
            it('中間状態を隠蔽する', (next) =>{
              /* #@range_begin(hide_internal_state) */
              var bind = (operate, continues) => {
                return (stack) => {
                  var newState = operate(stack);
                  return continues(newState.value)(newState.stack);
                };
              };
              var computation = bind(push(2), () => {
                return bind(push(3), () =>{
                  return bind(pop(), (state3) => {
                    return bind(pop(), (state4) => {
                      return unit(state4);
                    });
                  });
                });
              });
              expect(
                computation(empty)
              ).to.eql(
                {
                  value: 2,
                  stack: []
                }
              );
              /* #@range_end(hide_internal_state) */
              next();
            });
            it('計算を合成する', (next) =>{
              /* #@range_begin(combining_monad) */
              var computation1 = bind(push(2), () => {
                return bind(push(3), () => {
                  return bind(push(4), () => {
                    return unit();
                  });
                });
              });
              var computation2 = bind(pop(), (state1) => {
                return bind(pop(), (state2) => {
                  return unit(state2);
                });
              });
              var combine = (a, b) => {
                return (stack) => {
                  var initialState = unit(undefined)(stack);
                  var newState = a(stack);
                  return b(newState.stack);
                };
              };
              expect(
                combine(computation1,computation2)(empty)
              ).to.eql(
                {
                  value: 3,
                  stack: [2]
                }
              );
              /* #@range_end(combining_monad) */
              next();
            });
            it('逆ポーランド電卓', (next) =>{
              /* #@range_begin(revserse_polish) */
              var add = bind(pop(), (state1) => {
                return bind(pop(), (state2) => {
                  return unit(state1 + state2);
                });
              });
              var subtract = bind(pop(), (state1) => {
                return bind(pop(), (state2) => {
                  return unit(state1 - state2);
                });
              });
              var multiply = bind(pop(), (state1) => {
                return bind(pop(), (state2) => {
                  return unit(state1 * state2);
                });
              });
              var divide = bind(pop(), (state1) => {
                return bind(pop(), (state2) => {
                  return unit(state1 / state2);
                });
              });
              expect(
                add([1,2])
              ).to.eql(
                {
                  value: 3,
                  stack: []
                }
              );
              /* #@range_end(revserse_polish) */
              next();
            });
          });
        });
      });
    });
  });
  describe('関数型プログラミングの利点', () => {
    describe('モジュール性とは何か', () => {
      it('名前空間としてのモジュール', (next) => {
        /* #@range_begin(module_as_namespace) */
        // 数値計算のモジュール
        var math = {
          add: (n, m) => { // 数値の足し算
            return n + m;
          },
          multiply: (n, m) => {
            return n * m;
          }
        };
        // 文字列操作のモジュール
        var string = {
          add: (strL, strR) => { // 文字列の連結
            return strL.concat(strR);
          },
          multiply: (str, nTimes) => {
            //
          }
        };
        /* #@range_end(module_as_namespace) */
        next();
      });
      describe('モジュールの独立性', () => {
        it('統一的なインターフェイス', (next) => {
          /* #@range_begin(books_as_array) */
          var books = [
            {name: "こころ", author: ["夏目漱石"], genre: "文学"},
            {name: "夢十夜", author: ["夏目漱石"], genre: "文学"},
            {name: "ソクラテスの弁明", author: ["プラトン"], genre: "哲学"},
            {name: "国家", author: ["プラトン"], genre: "哲学"},
            {name: "プログラミング言語C", author: ["カーニハン","リッチー"], genre: "コンピュータ"},

            {name: "計算機プログラムの構造と解釈", author: ["サスマン","エイベルソン"], genre: "コンピュータ"},
          ];
          /* #@range_end(books_as_array) */
          var get = (object) => {
            return (key) => {
              return object[key];
            };
          };
          /* #@range_begin(pluck) */
          var pluck = (key) => {
            return (object) => {
              return object[key];
            };
          };
          /* #@range_end(pluck) */
          /* #@range_begin(mapWith) */
          var mapWith = (func) => {
            return (array) => {
              return array.map(func);
            };
          };
          /* #@range_end(mapWith) */
          var multiplier = (n) => {
            return (m) => {
              return n * m;
            };
          };
          var square = (n) => {
            return multiplier(n)(n);
          };
          expect(
            mapWith(square)([1,2,3])
          ).to.eql(
            [1,4,9]
          );
          expect(
            /* #@range_begin(mapWith_pluck) */
            mapWith(pluck("name"))(books)
            /* #@range_end(mapWith_pluck) */
          ).to.eql(
            ['こころ','夢十夜','ソクラテスの弁明','国家','プログラミング言語C','計算機プログラムの構造と解釈']
          );
          /* #@range_begin(filterWith) */
          var filterWith = (predicate) => {
            return (array) => {
              return array.filter(predicate);
            };
          };
          /* #@range_end(filterWith) */
          var isEqual = (a,b) => {
            return a === b;
          };
          expect(
            /* #@range_begin(filterWith_pluck) */
            filterWith((book) => { 
              return pluck("genre")(book) ===  "文学";
            })(books)
            /* #@range_end(filterWith_pluck) */
          ).to.eql(
            /* #@range_begin(filterWith_pluck_result) */
            [
              {name: "こころ", author: ["夏目漱石"], genre: "文学"},
              {name: "夢十夜", author: ["夏目漱石"], genre: "文学"},
            ]
            /* #@range_end(filterWith_pluck_result) */
          );
          expect(
            /* #@range_begin(map_filter) */
            mapWith(pluck("name"))(filterWith((book) => { 
              return pluck("genre")(book) ===  "哲学";
            })(books))
            /* #@range_end(map_filter) */
          ).to.eql(
            /* #@range_begin(map_filter_result) */
            ["ソクラテスの弁明", "国家"]
            /* #@range_end(map_filter_result) */
          );
          
          /* #@range_begin(doesContain) */
          var doesContain = (value) => {
            return (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator || (item === value);
              },false);
            };
          };
          /* #@range_end(doesContain) */
          var doesMatch = (predicate) => {
            return (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator || predicate(item);
              },false);
            };
          };
          expect(
            /* #@range_begin(filterWith_doesContain) */
            filterWith((book) => { 
              return doesContain("カーニハン")(pluck("author")(book));
            })(books)
            /* #@range_end(filterWith_doesContain) */
          ).to.eql(
            /* #@range_begin(filterWith_doesContain_result) */
            [
              {name: "プログラミング言語C", author: ["カーニハン","リッチー"], genre: "コンピュータ"},
            ]
            /* #@range_end(filterWith_doesContain_result) */
          );
          /* #@range_begin(findWith) */
          var findWith = (predicate) => {
            return (array) => {
              return array.filter(predicate)[0];
            };
          };
          /* #@range_end(findWith) */
          expect(
            findWith((book) => { 
              return pluck("genre")(book) === "哲学";
            })(books)
          ).to.eql(
            {name: "ソクラテスの弁明", author: ["プラトン"], genre: "哲学"}
          );
          next();
        });
      });
      describe('部品の汎用性', () => {
        it('単純なインターフェイス', (next) => {
          /* #@range_begin(constant) */
          var constant = (any) => {
            return (_) => {
              return any;
            };
          };
          /* #@range_end(constant) */
          var alwaysOne = constant(1); 
          /* #@range_begin(add_uncurried) */
          var add = (n,m) => {
            return n + m;
          };
          /* #@range_end(add_uncurried) */
          expect(
            add(1,2)
          ).to.eql(
            3
          );
          /* #@range_begin(add_curried) */
          var adder = (n) => {
            return (m) => {
              return n + m;
            };
          };
          /* #@range_end(add_curried) */
          expect(
            adder(1)(2)
          ).to.eql(
            3
          );
          /* #@range_begin(succ_defined) */
          var succ = adder(1); 
          /* #@range_end(succ_defined) */
          expect(
            succ(1)
          ).to.eql(
            2
          );
          /* #@range_begin(multiply_uncurried) */
          var multiply = (n,m) => {
            return n * m;
          };
          /* #@range_end(multiply_uncurried) */
          expect(
            multiply(2,3)
          ).to.eql(
            6
          );
          /* #@range_begin(multiply_curried) */
          var multiplier = (n) => {
            return (m) => {
              return n * m;
            };
          };
          /* #@range_end(multiply_curried) */
          var square = (n) => {
            return multiply(n,n);
          };
          var cube = (n) => {
            return multiplier(n)(square(n));
          };
          expect(
            cube(2)
          ).to.eql(
            8
          );
          next();
        });
        describe('reduceによる反復処理の汎用化', () => {
          it('reduceによるsumの定義', (next) => {
            /* #@range_begin(sum_in_array_reduce) */
            var sum = (array) => {
              return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
                return accumulator + item;                 // 足し算を実行する
              },0); // 第2引数には、蓄積変数の初期値として0を渡す 
            };
            /* #@range_end(sum_in_array_reduce)  */
            expect(
              sum([1,2,3,4])
            ).to.eql(
              10
            );
            next();
          });
          it('productの定義', (next) => {
            /* #@range_begin(product_in_array_reduce) */
            var product = (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator * item;               // かけ算を実行する
              }, 1); // 第2引数には、蓄積変数の初期値として1を渡す 
            };
            /* #@range_end(product_in_array_reduce)  */
            /* #@range_begin(product_in_array_reduce_test) */
            expect(
              product([1,2,3,4])
            ).to.eql(
              24
            );
            /* #@range_end(product_in_array_reduce_test)  */
            next();
          });
          it('allの定義', (next) => {
            /* #@range_begin(all_in_array_reduce) */
            var all = (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator && item;
              }); // 第2引数を指定していない場合は、
                  // 配列の先頭要素が変数accumulatorの初期値になる
            };
            /* #@range_end(all_in_array_reduce)  */
            expect(
              all([true,true,true])
            ).to.eql(
              true
            );
            expect(
              /* #@range_begin(all_in_array_reduce_test) */
              all([true,false,true])
              /* #@range_end(all_in_array_reduce_test) */
            ).to.eql(
              /* #@range_begin(all_in_array_reduce_test_result) */
              false
              /* #@range_end(all_in_array_reduce_test_result) */
            );
            next();
          });
          it('maxの定義', (next) => {
            /* #@range_begin(max_in_array_reduce) */
            var max = (array) => {
              var bigger = (n,m) => {
                if(n > m) {
                  return n;
                } else {
                  return m;
                };
              };
              return array.reduce((accumulator, item) => {
                return bigger(accumulator, item);
              });
            };
            /* #@range_end(max_in_array_reduce)  */
            expect(
              max([1,2,3,4])
            ).to.eql(
              4
            );
            expect(
              max([1,-2,3,-4])
            ).to.eql(
              3
            );
            next();
          });
          it('reverseの定義', (next) => {
            /* #@range_begin(reverse_in_array_reduce) */
            var reverse = (array) => {
              return array.reduce((accumulator, currentValue) => {
                return [currentValue].concat(accumulator);
              },[]);
            };
            expect(reverse([1,2,3,4])).to.eql([4,3,2,1]);
            /* #@range_end(reverse_in_array_reduce)  */
            next();
          });
          it('reduceによるmapの定義', (next) => {
            var succ = (n) => {
              return n + 1;
            };
            /* #@range_begin(map_in_array_reduce) */
            var map = (transform) => {
              return (array) => {
                return array.reduce((accumulator, item) => {
                  return accumulator.concat(transform(item));
                },[]); // 蓄積変数の初期値として空の配列[]を指定する
              };
            };
            /* #@range_end(map_in_array_reduce)  */
            expect(
              map(succ)([1,2,3])
            ).to.eql(
              [2,3,4]
            );
            var constant = (any) => {
              return (_) => {
                return any;
              };
            };
            var alwaysOne = constant(1); 
            expect(
              /* #@range_begin(map_in_array_reduce_test) */
              map(alwaysOne)([1,2,3])
              /* #@range_end(map_in_array_reduce_test) */
            ).to.eql(
              /* #@range_begin(map_in_array_reduce_test_result) */
              [1,1,1]
              /* #@range_end(map_in_array_reduce_test_result) */
            );
            next();
          });
          it('reduceによるfilterの定義', (next) => {
            var even = (n) => {
              return n % 2 === 0;
            };
            /* #@range_begin(filter_in_array_reduce) */
            var filter = (array) => {
              return (predicate) => {
                return array.reduce((accumulator, item) => {
                  if(predicate(item)) {
                    return accumulator.concat(item);
                  } else {
                    return accumulator;
                  }
                },[]); // 蓄積変数の初期値として空の配列[]を指定する
              };
            };
            /* #@range_end(filter_in_array_reduce)  */
            expect(
              filter([1,2,3])(even)
            ).to.eql(
              [2]
            );
            next();
          });
        });
      });
      describe('部品を合成する', () => {
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
        it('length関数を作る', (next) => {
          var map = (transform) => {
            return (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator.concat(transform(item));
              },[]); // 蓄積変数の初期値として空の配列[]を指定する
            };
          };
          var sum = (array) => {
            return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
              return accumulator + item;                 // 足し算を実行する
            },0); // 第2引数には、蓄積変数の初期値として0を渡す 
          };
          var constant = (any) => {
            return (_) => {
              return any;
            };
          };
          var alwaysOne = constant(1); 
          /* #@range_begin(array_length) */
          var length = (array) => {
            return sum(map(alwaysOne)(array));
          };
          /* #@range_end(array_length)  */
          expect(
            length([1,2,3])
          ).to.eql(
            3
          );
          next();
        });
        describe('関数の合成', () => {
          /* #@range_begin(function_compose) */
          var compose = (f,g) => {
            return (arg) => {
              return f(g(arg));
            };
          };
          /* #@range_end(function_compose)  */
          it('関数合成でlength関数を作る(ポイントフリー版)', (next) => {
            var map = (transform) => {
              return (array) => {
                return array.reduce((accumulator, item) => {
                  return accumulator.concat(transform(item));
                },[]); // 蓄積変数の初期値として空の配列[]を指定する
              };
            };
            var sum = (array) => {
              return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
                return accumulator + item;                 // 足し算を実行する
              },0); // 第2引数には、蓄積変数の初期値として0を渡す 
            };
            var constant = (any) => {
              return (_) => {
                return any;
              };
            };
            var alwaysOne = constant(1); 
            /* #@range_begin(flip_definition) */
            var flip = (fun) => {
              return  (x) => {
                return (y) => {
                  return fun(y)(x);
                };
              };
            };
            /* #@range_end(flip_definition) */
            /* #@range_begin(array_length_in_composition_with_point_free_style) */
            var length = compose(sum,map(alwaysOne));
            /* #@range_end(array_length_in_composition_with_point_free_style)  */
            expect(
              length([1,2,3])
            ).to.eql(
              3
            );
            next();
          });
          it('関数合成でlength関数を作る', (next) => {
            var map = (transform) => {
              return (array) => {
                return array.reduce((accumulator, item) => {
                  return accumulator.concat(transform(item));
                },[]); // 蓄積変数の初期値として空の配列[]を指定する
              };
            };
            var sum = (array) => {
              return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
                return accumulator + item;                 // 足し算を実行する
              },0); // 第2引数には、蓄積変数の初期値として0を渡す 
            };
            var constant = (any) => {
              return (_) => {
                return any;
              };
            };
            var alwaysOne = constant(1); 
            var flip = (fun) => {
              return  (x) => {
                return (y) => {
                  return fun(y)(x);
                };
              };
            };
            /* #@range_begin(array_length_in_composition) */
            var length = (array) => { // 引数が配列であることを明示する
              return compose(sum,map(alwaysOne))(array);
            };
            /* #@range_end(array_length_in_composition)  */
            expect(
              length([1,2,3])
            ).to.eql(
              3
            );
            next();
          });
        });
      });
      describe('モジュールの独立性', () => {
        var not = (predicate) => {
          return (arg) => {
            return ! predicate(arg);
          };
        };
        var array = {
          cons: (head, tail) => {
            return [head].concat(tail);
          },
          empty: (_) => {
            return [];
          },
          head: (anArray) => {
            return anArray[0];
          },
          tail: (anArray) => {
            return anArray.slice(1,array.length(anArray));
          },
          length: (anArray) => {
            return anArray.length;
          },
          isEmpty: (anArray) => {
            return array.length(anArray) === 0;
          },
          fromString: (str) => {
            if(string.isEmpty(str)) {
              return array.empty();
            } else {
              return array.cons(string.head(str), 
                               array.fromString(string.tail(str)));
            }
          },
          takeWhile: (anArray) => {
            return (predicate) => {
              if(array.isEmpty(anArray)){
                return array.empty(); 
              } else {
                var head = array.head(anArray);
                var tail = array.tail(anArray);
                if(predicate(head) === true) {
                  return array.cons(head,
                                    array.takeWhile(tail)(predicate));
                } else {
                  return array.empty();
                }
              }
            };
          },
          dropWhile: (anArray) => {
            return (predicate) => {
              if(array.isEmpty(anArray)){
                return [];
              } else {
                var head = array.head(anArray);
                var tail = array.tail(anArray);
                if(predicate(head) === true) {
                  return array.dropWhile(tail)(predicate);
                } else {
                  return anArray;
                }
              };
            };
          },
          span: (anArray) => {
            return (predicate) => {
              if(array.isEmpty(anArray)){
                return [];
              } else {
                var head = array.head(anArray);
                var tail = array.tail(anArray);
                return [array.takeWhile(anArray)(predicate),
                        array.dropWhile(anArray)(predicate)];
              };
            };
          },
          break: (anArray) => {
            return (predicate) => {
              return array.span(anArray)(not(predicate));
            };
          },  
          lines: (xs) => {
            var isNewline = (ch) => {
              return ch === '\n';
            };
            var apair = array.break(xs)(isNewline);
            var ys = apair[0];
            var zs = apair[1];

            if(array.isEmpty(zs)){
              return [ys];
            } else {
              var head = array.head(zs);
              var tail = array.tail(zs);
              return array.cons(ys, array.lines(tail));
            };
          }
        };
        var string = {
          head: (str) => {
            return str[0];
          },
          tail: (str) => {
            return str.substring(1);
          },
          isEmpty: (str) => {
            return str.length === 0;
          },
          add: (strL, strR) => {
            return strL + strR;
          },
          toArray: (str) => {
            if(string.isEmpty(str)) {
              return [];
            } else {
              return array.cons(string.head(str),
                                string.toArray(string.tail(str)));
            }
          },
          fromArray: (anArray) => {
            return anArray.reduce((accumulator, item) => {
              return string.add(accumulator, item);
            }, "");
          },
          lines: (str) => {
            var isNewline = (ch) => {
              return ch === '\n';
            };
            var apair = array.break(array.fromString(str))(isNewline);
            var ys = apair[0];
            var zs = apair[1];

            if(array.isEmpty(zs)){
              return [string.fromArray(ys)];
            } else {
              var tail = array.tail(zs);
              return array.cons(string.fromArray(ys), 
                                string.lines(string.fromArray(tail)));
            };
          }
        };
        describe('array', () => {
          it('array#head', (next) => {
            expect(
              array.head([0,1,2])
            ).to.eql(
              0
            );
            next();
          });
          it('array#tail', (next) => {
            expect(
              array.tail([0,1,2])
            ).to.eql(
              [1,2]
            );
            next();
          });
          it('array#cons', (next) => {
            expect(
              array.cons(0,[1,2])
            ).to.eql(
              [0,1,2]
            );
            next();
          });
          it('array#fromString', (next) => {
            expect(
              array.fromString("123")
            ).to.eql(
              [1,2,3]
            );
            next();
          });
          it('array#takeWhile', (next) => {
            var theArray = [1,2,3]; 
            var even = (n) => {
              return 0 === (n % 2);
            };
            var odd = not(even);
            expect(
              array.takeWhile(theArray)(odd)
            ).to.eql(
              [1]
            );
            next();
          });
          it('array#dropWhile', (next) => {
            var theArray = [1,2,3]; 
            var even = (n) => {
              return 0 === (n % 2);
            };
            var odd = not(even);
            expect(
              array.dropWhile(theArray)(odd)
            ).to.eql(
              [2,3]
            );
            next();
          });
          it('array#span', (next) => {
            var theArray = [1,2,3]; 
            var even = (n) => {
              return 0 === (n % 2);
            };
            expect(
              array.span(theArray)(even)
            ).to.eql(
              [[],[1,2,3]]
            );
            var odd = not(even);
            expect(
              array.span(theArray)(odd)
            ).to.eql(
              [[1],[2,3]]
            );
            next();
          });
          it('array#break', (next) => {
            var theArray = [1,2,3]; 
            var even = (n) => {
              return 0 === (n % 2);
            };
            expect(
              array.break(theArray)(even)
            ).to.eql(
              [[1],[2,3]]
            );
            var odd = not(even);
            expect(
              array.break(theArray)(odd)
            ).to.eql(
              [[],[1,2,3]]
            );
            var isNewline = (ch) => {
              return ch === '\n';
            };
            expect(
              array.break(['a','b','c','\n','d','e','f'])(isNewline)
            ).to.eql(
              [['a','b','c'],['\n','d','e','f']]
            );
            next();
          });
          it('array#lines', (next) => {
            var theArray = array.fromString("abc\ndef"); 
            expect(
              array.lines(theArray)
            ).to.eql(
              [ [ 'a', 'b', 'c' ], [ 'd', 'e', 'f' ] ]
            );
            next();
          });
        });
        describe('string', () => {
          it('string#head', (next) => {
            expect(
              string.head("abc")
            ).to.eql(
              'a'
            );
            next();
          });
          it('string#tail', (next) => {
            expect(
              string.tail("abc")
            ).to.eql(
              "bc"
            );
            next();
          });
          it('string#fromArray', (next) => {
            expect(
              string.fromArray(['a','b','c'])
            ).to.eql(
              "abc"
            );
            next();
          });
          it('string#toArray', (next) => {
            expect(
              string.toArray("abc")
            ).to.eql(
              ['a','b','c']
            );
            next();
          });
          it('string#lines', (next) => {
            expect(
              string.lines("abc\ndef")
            ).to.eql(
              [ 'abc', 'def' ]
            );
            next();
          });
        });
      });
      describe('階乗の計算', () => {
        it('命令型プログラミングによる階乗の計算', (next) => {
          /* #@range_begin(imperative_factorial) */
          var factorial = (n) => {
            var result = 1;             // result変数に結果が入る
            var times = 1;              // times変数は反復の回数を数える
            while(times < n + 1) {      // while文は反復を処理する
              result = result * times;  // result変数を代入で更新する
              times = times + 1;        // times変数を代入で更新する
            }
            return result;
          };
          /* #@range_end(imperative_factorial) */
          expect(
            factorial(2)
          ).to.eql(
            2
          );
          expect(
            factorial(3)
          ).to.eql(
            6
          );
          expect(
            factorial(4)
          ).to.eql(
            24
          );
          next();
        });
        it('関数型プログラミングによる階乗の計算', (next) => {
          // var foldr = (aStream) => {
          //   return (accumulator) => {
          //     return (glue) => {
          //       if(aStream.length === 1) {
          //         return accumulator;
          //       } else {
          //         return glue(aStream[0])(foldr(aStream[1]())(accumulator)(glue));
          //       };
          //     };
          //   };
          // };
          var multiply = (m,n) => {
            return m * n;
          };
          var product = (array) => {
           return array.reduce(multiply,1);
          }; 
          var adder = (m) => {
            return (n) => {
              return m + n;
            };
          };
          var succ = adder(1);
          var iterate = (step) => {
            return (init) => {
              return [init, (_) => {
                return iterate(step)(step(init));
              }];
            };
          };
          var take = (n) => {
            return (aStream) => {
              if(n === 0) {
                return [];
              } else {
                return [aStream[0]].concat(take(n-1)(aStream[1]()));
              }
            };
          };
          var enumFrom = (from) => {
            return iterate(succ)(from);
          };
          var compose = (f,g) => {
            return (arg) => {
              return f(g(arg));
            };
          };
          /* #@range_begin(functional_factorial) */
          var enumFromTo = (from) => {
            return (to) => {
              return take(to - from + 1)(enumFrom(from));
            };
          };
          var factorial = (n) => {
            return compose(product, enumFromTo(1))(n);
          };
          /* #@range_end(functional_factorial) */
          // var enumFromTo = (from, to) => {
          //   return take(to - from + 1)(enumFrom(from));
          // };
          // var factorial = (n) => {
          //   return product(enumFromTo(1,n));
          // };
          expect(
            enumFromTo(2)(4)
          ).to.eql(
            [2,3,4]
          );
          expect(
            factorial(2)
          ).to.eql(
            2
          );
          expect(
            factorial(3)
          ).to.eql(
            6
          );
          expect(
            factorial(4)
          ).to.eql(
            24
          );
          /* #@range_begin(permutation_combination) */
          var perm = (n, r) => {
            return factorial(n) / factorial(n-r);          
          };
          var comb = (n, r) => {
            return perm(n,r) / factorial(r);
          };
          /* #@range_end(permutation_combination) */
          expect(
            perm(3,3)
          ).to.eql(
            6
          );
          expect(
            comb(10,4)
          ).to.eql(
            210
          );
          
          next();
        });
      });
      it('素数の計算', (next) => {
        var match = (data, pattern) => {
          return data(pattern);
        };
        var stream = {
          empty: (_) => {
            return (pattern) => {
              return pattern.empty();
            };
          },
          cons: (head,tailThunk) => {
            return (pattern) => {
              return pattern.cons(head,tailThunk);
            };
          },
          // head:: STREAM[T] => T
          /* ストリーム型headの定義は、リスト型headと同じ */
          head: (astream) => {
            return match(astream,{
              empty: (_) => { return null; },
              cons: (value, tailThunk) => { return value; }
            });
          },
          // tail:: STREAM[T] => STREAM[T]
          tail: (astream) => {
            return match(astream,{
              empty: (_) => { return null; },
              cons: (head, tailThunk) => {
                return tailThunk();  // ここで初めてサンクを評価する
              }
            });
          },
          take: (astream, n) => {
            return match(astream,{
              empty: (_) => {
                return list.empty();
              },
              cons: (head,tailThunk) => {
                if(n === 0) {
                  return list.empty();
                } else {
                  return list.cons(head,stream.take(tailThunk(),(n -1)));
                }
              }
            });
          }
        };
        var list = {
          empty: (_) => {
            return (pattern) => {
              return pattern.empty();
            };
          },
          cons: (value, list) => {
            return (pattern) => {
              return pattern.cons(value, list);
            };
          },
          isEmpty: (alist) => {
            return match(alist, { // match関数で分岐する
              empty: true,
              cons: (head, tail) => { // headとtailにそれぞれ先頭要素、末尾要素が入る
                return false;
              }
            });
          },
          head: (alist) => {
            return match(alist, {
              empty: null, // 空のリストには先頭要素はありません
              cons: (head, tail) => {
                return head;
              }
            });
          },
          tail: (alist) => {
            return match(alist, {
              empty: null,  // 空のリストには末尾要素はありません
              cons: (head, tail) => {
                return tail;
              }
            });
          },
          toArray: (alist) => {
            var toArrayAux = (alist,accumulator) => {
              return match(alist, {
                empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return toArrayAux(tail, accumulator.concat(head));
                }
              });
            };
            return toArrayAux(alist, []);
          }
        };
        var integersFrom = (n) => {
          return stream.cons(n, (_) => {
            return integersFrom(n + 1);
          });
        };
        var sieve = (astream) => {
          var head = stream.head(astream);
          var tail = stream.tail(astream);
          var mark = (astream, k, m) => {
            var head = stream.head(astream);
            var tail = stream.tail(astream);
            if(k === m) {
              return stream.cons(0,(_) => {
                return mark(tail, 1, m);
              });
            } else {
              return stream.cons(head, (_) => {
                return mark(tail, k+1, m);
              });
            }
          };
          if(head === 0) {
            return sieve(tail);
          } else {
            return stream.cons(head, (_) => {
              return sieve(mark(tail, 1, head));
            });
          }
        };
        expect(
          list.toArray(stream.take(sieve(integersFrom(2)), 10))
        ).to.eql(
          [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
        );
        next();
      });
    });
    describe('遅延評価', () => {
      var take = (n, astream) => {
        if(n === 1) {
          return astream[0];
        } else {
          return [astream[0]].concat(take(n-1, astream[1]()));
        }
      };
      var repeat = (n) => {
        return [n, (_) => {
          return repeat(n);
        }];
      };
      expect(
        take(3, repeat(2))
      ).to.eql(
        [2,2,2]
      );
      var ones = repeat(1);
      expect(
        take(2, ones)
      ).to.eql(
        [1,1]
      );
      var replicate = (n, x) => {
        return take(n, repeat(x));
      };
      expect(
        replicate(4, 3)
      ).to.eql(
        [3,3,3,3]
      );
      var upto = (m, n) => {
        if(m > n) {
          return [];
        } else {
          return [m, (_) => {
            return upto(m+1,n);
          }];
        };
      };
      expect(
        take(3, upto(2,10))
      ).to.eql(
        [2,3,4]
      );
      /* #@range_begin(stream_filter) */
      var filter = (predicate) => {
        return (aStream) => {
          var head = aStream[0];
          if(predicate(head) === true) {
            return [head, (_) => {
              return filter(predicate)(aStream[1]());
            }];
          } else {
            return filter(predicate)(aStream[1]());
          }
        };
      };
      /* #@range_end(stream_filter) */
      var mod = (n,m) => {
        return n % m;
      };
      var even = (n) => {
        return mod(n,2) === 0;
      };
      var adder = (m) => {
        return (n) => {
          return m + n;
        };
      };
      var succ = adder(1);
      /* #@range_begin(stream_enumFrom) */
      var iterate = (step) => {
        return (init) => {
          return [init, (_) => {
            return iterate(step)(step(init));
          }];
        };
      };
      var enumFrom = (n) => {
        return iterate(succ)(n);
      };
      /* #@range_end(stream_enumFrom) */
      expect(
        take(3,filter(even)(enumFrom(2)))
      ).to.eql(
        [2,4,6]
      );
      describe('カリー化バージョン', () => {
        var take = (n) => {
          return (aStream) => {
            if(n === 1) {
              return aStream[0];
            } else {
              return [aStream[0]].concat(take(n-1)(aStream[1]()));
            }
          };
        };
        var filter = (predicate) => {
          return (aStream) => {
            var head = aStream[0];
            if(predicate(head) === true) {
              return [head, (_) => {
                return filter(predicate)(aStream[1]());
              }];
            } else {
              return filter(predicate)(aStream[1]());
            }
          };
        };
        var even = (n) => {
          return n % 2 === 0;
        };
        var iterate = (step) => {
          return (init) => {
            return [init, (_) => {
              return iterate(step)(step(init));
            }];
          };
        };
        var adder = (m) => {
          return (n) => {
            return m + n;
          };
        };
        var succ = adder(1);
        var enumFrom = (n) => {
          return iterate(succ)(n);
        };
        expect(
          compose(take(3), filter(even))(enumFrom(2))
        ).to.eql(
          [2,4,6]
        );
      });
      describe('ストリーム', () => {
        it('簡単なストリームの例', (next) => {
          /* #@range_begin(stream_example) */
          var stream = [1, (_) => { // 末尾要素は関数で表現する
            return 2;
          }];
          /* #@range_end(stream_example) */
          expect(
            stream[0]
          ).to.eql(
            1
          );
          expect(
            stream[1]()
          ).to.eql(
            2
          );
          /* #@range_begin(stream_ones) */
          var ones = [1, (_) => {
            return ones;
          }];
          /* #@range_end(stream_ones) */
          expect(
            ones[0]
          ).to.eql(
            1
          );
          expect(
            ones[1]()[0]
          ).to.eql(
            1
          );
          next();
        });
        var series = (func) => {
          return (init) => {
            return [init, (_) => {
              return series(func)(func(init));
            }];
          };
        };
        var identity = (any) => {
          return any;
        };
        var ones = series(identity)(1);
        expect(
          take(10,ones)
        ).to.eql(
          [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
        );
        /* #@range_begin(stream_limit) */
        var limit = (predicate) => {
          return (aStream) => {
            if(predicate(aStream)) {
              return aStream[0];
            } else {
              return limit(predicate)(aStream[1]());
            };
          };
        };
        /* #@range_end(stream_limit) */
        it('エラトステネスのふるいによる素数の生成', (next) => {
          var filter = (predicate) => {
            return (aStream) => {
              var head = aStream[0];
              if(predicate(head) === true) {
                return [head, (_) => {
                  return filter(predicate)(aStream[1]());
                }];
              } else {
                return filter(predicate)(aStream[1]());
              }
            };
          };
          /* #@range_begin(stream_remove) */
          var not = (arg) => {
            return ! arg;
          };
          var remove = (predicate) => {
            return (aStream) => {
              return filter(compose(not,predicate))(aStream);
            };
          };
          /* #@range_end(stream_remove) */
          var take = (n) => {
            return (aStream) => {
              if(n === 1) {
                return aStream[0];
              } else {
                return [aStream[0]].concat(take(n-1)(aStream[1]()));
              }
            };
          };
          var even = (n) => {
            return n % 2 === 0;
          };
          expect(
            /* #@range_begin(stream_remove_test) */
            take(5)(remove(even)(enumFrom(1)))
            /* #@range_end(stream_remove_test) */
          ).to.eql(
            /* #@range_begin(stream_remove_test_result) */
            [1,3,5,7,9]
            /* #@range_end(stream_remove_test_result) */
          );
          /* #@range_begin(multipleOf) */
          var multipleOf = (n,m) => {
            if((n % m) === 0) {
              return true;
            } else {
              return false;
            }
          };
          /* #@range_end(multipleOf) */
          expect(
            multipleOf(4,2)
          ).to.eql(
            true
          );
          expect(
            multipleOf(5,2)
          ).to.eql(
            false
          );
          /* #@range_begin(eratosthenes_sieve) */
          var sieve = (aStream) => {
            var prime = aStream[0];
            return [prime, (_) => {
              return sieve(remove((item) => { // 素数の倍数を除去する
                return multipleOf(item, prime);  
              })(aStream[1]()));
            }]; 
          };
          var primes = sieve(enumFrom(2)); // 無限の素数列
          /* #@range_end(eratosthenes_sieve) */
          /* #@range_begin(stream_take) */
          var take = (n) => {
            return (aStream) => {
              if(n === 1) {
                return aStream[0];
              } else {
                return [aStream[0]].concat(take(n-1)(aStream[1]()));
              }
            };
          };
          /* #@range_end(stream_take) */
          expect(
            /* #@range_begin(eratosthenes_sieve_test) */
            take(10)(primes)
            /* #@range_end(eratosthenes_sieve_test) */
          ).to.eql(
            /* #@range_begin(eratosthenes_sieve_test_result) */
            [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]
            /* #@range_end(eratosthenes_sieve_test_result) */
          );
          expect(
            take(50)(primes)
          ).to.eql(
            [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229]
          );
          next();
        });
      });
    });
    describe('テスト', () => {
      it('参照透過性のあるコードはテストが容易である', (next) => {
        var adder = (m) => {
          return (n) => {
            return m + n;
          };
        };
        /* #@range_begin(adder_test) */
        expect(
          adder(1)(2)
        ).to.eql(
          3
        );
        /* #@range_end(adder_test) */
        next();
      });
      describe('参照透過性のないコードはテストが困難である', () => {
        it('状態を持つ銀行口座', (next) => {
          /* #@range_begin(account_with_state) */
          var mkAccount = (balance) => {
            var deposit = (amount) => {
              balance = balance + amount;
              return dispatch;
            };
            var withdraw = (amount) => {
              balance = balance - amount;
              return dispatch;
            };
            var dispatch = (method) => {
              switch(method){
              case "deposit":
                return deposit;
              case "withdraw":
                return withdraw; 
              case "balance":
                return balance;
              default:
                throw {
                  name: "そのようなメソッドはありません",
                  message: "unknown: " + method
                };
              }
            };
            return dispatch;
          };
          /* #@range_end(account_with_state) */
          var account = mkAccount(10);
          expect(
            account("balance")
          ).to.eql(
            10
          );
          expect(
            account("deposit")(20)("balance")
          ).to.eql(
            30
          );
          expect(
            account("deposit")(20)("balance")
          ).to.eql(
            50
          );
          next();
        });
        it('参照透過性のある銀行口座', (next) => {
          /* #@range_begin(account_with_explicit_state) */
          var account = {
            init: (balance) => { // 銀行口座を作る
              return balance;
            },
            deposit: (amount) => { // 口座にお金を預ける処理
              return (anAccount) => {
                return account.init(anAccount + amount);
              };
            },
            withdraw: (amount) => { // 口座からお金を引き出す処理
              return (anAccount) => {
                return account.init(anAccount - amount);
              };
            },
            commit: (anAccount) => { // 処理を実行する
              return (transactions) => {
                return transactions.reduce((accumulator, transact) => {
                  return transact(accumulator);
                },anAccount);
              };
            }
          };
          /* #@range_end(account_with_explicit_state) */
          expect(
            account.init(20)
          ).to.eql(
            20
          );
          expect(
            account.deposit(30)(account.init(20))
          ).to.eql(
            50
          );

          /* #@range_begin(account_test) */
          expect(
            account.commit(account.init(30))(
              [account.deposit(20), account.withdraw(10)] // 一連の取り引きを実行する
            )
          ).to.eql(
            40
          );
          /* #@range_end(account_test) */
          next();
        });
        describe('ゲームの勝敗の例', () => {
          it("参照透過性のないコードのテスト", (next) => {
            /* #@range_begin(winner_with_sideeffect) */
            var winner = (playerL, playerR) => {
              if(playerR.score > playerL.score) {
                console.log(playerR.name + "が勝者です");
              } else if(playerR.score < playerL.score) {
                console.log(playerL.name + "が勝者です");
              } else {
                console.log("引き分けです");
              }
            }; 
            /* #@range_end(winner_with_sideeffect) */
            var playerA = {
              name: 'a',
              score: 10
            };
            var playerB = {
              name: 'b',
              score: 20
            };
            next();
          });
          it("参照透過性のあるコードのテスト", (next) => {
            /* #@range_begin(winner_without_sideeffect) */
            var judge = (playerL, playerR) => {
              if(playerL.score > playerR.score) {
                return playerL;
              } else if(playerL.score < playerR.score) {
                return playerR;
              } else {
                return null;
              }
            }; 
            var announce = (winner) => {
              if(winner) {
                return winner.name + "が勝者です";
              } else {
                return "引き分けです";
              }
            };
            /* #@range_end(winner_without_sideeffect) */
            // expect(
            //   judge(playerA, playerB)
            // ).to.eql(
            //   playerB
            // );
            /* #@range_begin(announce_winner) */
            var playerA = {
              name: 'a',
              score: 10
            };
            var playerB = {
              name: 'b',
              score: 20
            };
            // 純粋な関数をテストする
            expect(
              announce(judge(playerA, playerB))
            ).to.eql(
              "bが勝者です"
            );
            /* #@range_end(announce_winner) */
            // var displayWinner = compose(console.log, judge);
            // displayWinner(playerA, playerB);
            next();
          });
        });
        it('数学モジュールの例', (next) => {
          var math = {
            PI: 3.14
          };
          var area = (radius) => {
            return radius * radius * math.PI;
          };
          expect(
            area(1)
          ).to.eql(
            3.14
          );
          next();
        });
        it('adderの例', (next) => {
          // var sinon = require('sinon');
          // var m = sinon.stub();
          // m.returns(1);
          var m = 1;
          var adder = (n) => {
            return m + n;
          };
          expect(
            adder(2)
          ).to.eql(
            3
          );
          next();
        });
        it('認証の例', (next) => {
          var length = (array) => {
            return array.length; 
          };
          var pluck = (key) => {
            return (object) => {
              return object[key];
            };
          };
          var filterWith = (predicate) => {
            return (array) => {
              return array.filter(predicate);
            };
          };
          var doesContain = (value) => {
            return (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator || (item === value);
              },false);
            };
          };
          /* #@range_begin(authenticate_test) */
          var database = [
            {
              name: "夏目漱石",
              password: "12345"
            },
            {
              name: "プラトン",
              password: "54321"
            }
          ];
          var authenticate = (name, password) => {
            var match = (key) => {
              return pluck(key);
            };
            return length(filterWith((record) => { 
              return (pluck("name")(record) === name) && (pluck("password")(record) === password);
            })(database)) === 1;
          };
          /* #@range_end(authenticate_test) */
          expect(
            authenticate("夏目漱石","12345")
          ).to.eql(
            true
          );
          next();
        });
        // it('認証をテスト可能にする', (next) => {
        //   var length = (array) => {
        //     return array.length; 
        //   };
        //   var pluck = (key) => {
        //     return (object) => {
        //       return object[key];
        //     };
        //   };
        //   var filterWith = (predicate) => {
        //     return (array) => {
        //       return array.filter(predicate);
        //     };
        //   };
        //   var doesContain = (value) => {
        //     return (array) => {
        //       return array.reduce((accumulator, item) => {
        //         return accumulator || (item === value);
        //       },false);
        //     };
        //   };
        //   var doesMatch = (predicate) => {
        //     return (array) => {
        //       return array.reduce((accumulator, item) => {
        //         return accumulator || predicate(item);
        //       },false);
        //     };
        //   };
        //   /* #@range_begin(authenticate_test) */
        //   var database = [
        //     {
        //       name: "夏目漱石",
        //       password: "12345"
        //     },
        //     {
        //       name: "プラトン",
        //       password: "54321"
        //     }
        //   ];
        //   var password = (name) => {
            
        //   var authenticate = (name, challengePassword, realPassword) => {
        //     return length(filterWith((record) => { 
        //       return (pluck("name")(record) === name) && (pluck("password")(record) === password);
        //     })(database)) === 1;
        //   };
        //   /* #@range_end(authenticate_test) */
        //   expect(
        //     authenticate("夏目漱石","12345")
        //   ).to.eql(
        //     true
        //   );
        //   next();
        // });
      });
      describe('参照透過性のあるコードはテストが容易である', () => {
        describe('性質テスト', () => {
          var adder = (m) => {
            return (n) => {
              return m + n;
            };
          };
          var succ = adder(1);
          var iterate = (step) => {
            return (init) => {
              return [init, (_) => {
                return iterate(step)(step(init));
              }];
            };
          };
          var take = (n) => {
            return (aStream) => {
              if(n === 0) {
                return [];
              } else {
                return [aStream[0]].concat(take(n-1)(aStream[1]()));
              }
            };
          };
          var enumFrom = (from) => {
            return iterate(succ)(from);
          };
          var filter = (predicate) => {
            return (aStream) => {
              var head = aStream[0];
              if(predicate(head) === true) {
                return [head, (_) => {
                  return filter(predicate)(aStream[1]());
                }];
              } else {
                return filter(predicate)(aStream[1]());
              }
            };
          };
          it('succ関数の性質テスト', (next) => {
            /* #@range_begin(succ_property_test) */
            // ストリームのmap関数
            var map = (transform) => {
              return (aStream) => {
                var head = aStream[0];
                return [transform(head), (_) => {
                  return map(transform)(aStream[1]());
                }];
              };
            };
            // 配列のall関数
            var all = (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator && item;
              },true);
            };
            // 検証の対象となる命題
            var proposition = (x) => {
              return succ(0) + succ(x) == succ(succ(x));
            };
            expect(
              all(take(100)(map(proposition)(enumFrom(0))))
            ).to.eql(
              true
            );
            /* #@range_end(succ_property_test) */
            // compose(all,
            //         compose(take(100),map(proposition)))(enumFrom(0))
            // all(compose(take(100),map(proposition))(enumFrom(0)))
            next();
          });
        });
      });
    });
  });
});
