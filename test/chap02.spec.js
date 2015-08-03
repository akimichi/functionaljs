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
          expect(
            aResult
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
        describe('不変なデータ', () =>  {
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
          describe('モナド', () => {
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
});
