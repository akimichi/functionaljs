"use strict";

var expect = require('expect.js');
var util = require('util');

// データの種類と特徴
// ============================
describe('データの種類と特徴', () => {
  describe('型とは何か', () => {
    it('整数の構成', (next) => {
      /* #@range_begin(integer_construction) */
      var succ = (n) => {
        return n + 1;
      };
      expect(
        succ(0)
      ).to.eql(
        1
      );
      expect(
        succ(succ(0))
      ).to.eql(
        2
      );
      /* #@range_end(integer_construction) */
      next();
    });
    /*
    describe('型決定のタイミング', () => {
      it('NaNはエラーの場所を発見しにくくする', (next) => {
         var one = 1;
         var two = 2;
         var three = "three";
         var four = 4;
         expect(
           one * two * (three + four)
         ).to.eql(
            11   // 1 * 2 + (3 + 4) = 10 を期待する
         );
         next();
       });
    })
    */
  });
  describe('基本型', () => {
    it('未定義の変数', (next) => {
      /* #@range_begin(undefined_variable) */
      var variable; // 宣言されているが値と結びついていない変数
      expect(
        variable
      ).to.be(
        undefined
      );
      /* #@range_end(undefined_variable) */
      next();
    });
    it('真理値型は不変である', (next) => {
      /* #@range_begin(truth_is_immutable) */
      var truth = true;
      expect(
        ! truth
      ).to.eql(
        false
      );
      expect(
        truth
      ).to.eql(
        truth
      );
      /* #@range_end(truth_is_immutable) */
      next();
    });
    it('数値型は不変である', (next) => {
      /* #@range_begin(number_is_immutable) */
      expect(
        1
      ).to.eql(
        1
      );
      var one = 1;
      one = one + 1;
      expect(
        one
      ).to.eql(
        2
      );
      /* #@range_end(number_is_immutable) */
      next();
    });
    it('文字列は不変である', (next) => {
      /* #@range_begin(string_is_immutable) */
      var str = "to be, or not to be";
      expect(
        str.toUpperCase()
      ).to.be(
        "TO BE, OR NOT TO BE"
      );
      expect(
        str
      ).to.be(
        "to be, or not to be"
      );
      /* #@range_end(string_is_immutable)  */
      next();
    });
    it('==比較演算子', (next) => {
      /* #@range_begin(equality_operator) */
      expect(
        null == undefined
      ).to.be(
        true
      );
      expect(
        false == ''
      ).to.be(
        true
      );
      expect(
        true == '1'
      ).to.be(
        true
      );
      expect(
        1 == '1'
      ).to.be(
        true
      );
      /* #@range_end(equality_operator)  */
      next();
    });
  });
  describe('合成型', () => {
    describe('オブジェクト型', () => {
      describe('アドレス帳の例', () => {
        it('オブジェクトのインスタンスの例', (next) => {
          /* #@range_begin(object_instance_example) */
          var addressbook = {
            No1: "Alan Turing",
            No2: "Jane Austen",
            No3: "Fyodor Dostoyevsky",
            No4: "Ada Lovelace"
          };
          /* #@range_end(object_instance_example) */
           next();
        });
        it('オブジェクト型の入れ子', (next) => {
          /* ##@range_begin(object_can_embed_object) */
          var addressbook = {
            No1: {
              name: "Alan Turing",
              gender: "male",
              birthDay: "1912/6/23"
            },
            No2: {
              name: "Jane Austen",
              gender: "female",
              birthDay: "1775/12/16"
            },
            No3: {
              name: "Fyodor Dostoyevsky",
              gender: "male",
              birthDay: "1821/11/11"
            },
            No4: {
              name: "Ada Lovelace",
              gender: "female",
              birthDay: "1815/12/10"
            }
          };
          /* ##@range_end(object_can_embed_object) */
          var addressbook_nested = {
            /* ##@range_begin(object_can_embed_object_nested) */
            No1: {
              name: "Alan Turing",
              gender: "male",
              birthDay: {
                year: 1912,
                month: 6,
                day: 23
              }
            }
            /* ##@range_end(object_can_embed_object_nested) */
          };
          // hasOwnPropertyでプロパティの有無を調べる
          /* #@range_begin(object_access) */
          expect(
            addressbook.No1.name     // オブジェクト.キー記法
          ).to.eql(
            "Alan Turing"
          );
          expect(
            addressbook["No1"]["name"]  // オブジェクト[キー]記法
          ).to.eql(
            "Alan Turing"
          );
          expect(
            addressbook.hasOwnProperty("No1")
          ).to.eql(
            true
          );
          expect(
            addressbook.hasOwnProperty("No9")
          ).to.eql(
            false
          );
          /* #@range_end(object_access) */
          // var expression = {
          //   add: {
          //     x: 1,
          //     y: {
          //       multiply: {
          //         x: 2,
          //         y: 3
          //       }
          //     }
          //   }
          // };
          // expect(
          //   expression.add.y.multiply.x
          // ).to.eql(
          //   2
          // );
          next();
        });
        it('オブジェクト型によるアドレス帳の表現', (next) => {
          /* #@range_begin(addressbook_example) */
          var addressbook = {
            BMI: (weight, height) => {
              expect(weight).to.be.a('number');
              expect(height).to.be.a('number');
              return weight / ((height / 100.0) * (height / 100.0));
            },
            No1: {
              name: "Alan Turing",
              birthDay: "1912/6/23",
              weight: 62,
              height: 175
            },
            No2: {
              name: "Haskell Curry",
              birthDay: "1900/9/12",
              weight: 62,
              height: 180
            },
            No3: {
              name: "Alonzo Church",
              birthDay: "1903/6/14",
              weight: 75,
              height: 168
            }
          };
          expect(
            addressbook.No1.name
          ).to.eql(
            "Alan Turing"
          );
          expect(
            addressbook.BMI(
              addressbook.No1.weight,
              addressbook.No1.height)
          ).to.be.within(20.0,21.0); // within(A,B) は値が AとBのあいだにはいっていることをチェックする
          /* #@range_end(addressbook_example) */
          next();
        });
      });
      it('for in で反復処理する', (next) => {
        /* ##@range_begin(for_in_object) */
        var obj = {
          one: 1,
          two: 2
        };
        var results = [];
        for (var key in obj) {
          results.push(obj[key])
        }
        expect(
          results
        ).to.eql(
          [1,2]
        );
        /* ##@range_end(for_in_object)     */
        next();
      });
    });
    describe('配列型', () => {
      it("配列の基本操作", (next) => {
        /* #@range_begin(array_access) */
        var array = [10,11,12];
        expect(
          array[0]
        ).to.eql(
          10
        );
        expect(
          array[2]
        ).to.eql(
          12
        );
        expect(
          array[100]
        ).to.eql(
          undefined
        );
        /* #@range_end(array_access) */
        next();
      });
      it("Array.forEach文", (next) => {
        /* #@range_begin(forEach_in_array) */
        var array = [1,2,3,4,5];
        var sum = 0;
        array.forEach((element) => {
          sum += element;
        });
        expect(
          sum
        ).to.eql(
          15
        );
        /* #@range_end(forEach_in_array) */
        next();
      });
      it("Array.sort文", (next) => {
        /* #@range_begin(sort_in_array) */
        var array = [5,3,4,1,2];
        expect(
          array.sort((n,m) => {
            return n > m;
          })
        ).to.eql(
          [1,2,3,4,5]
        );
        /* #@range_end(sort_in_array) */
        next();
      });
      it("アドレス帳の配列表現", (next) => {
        /* #@range_begin(addressbook_example_in_array) */
          var addressbook = [ // 配列に要素を格納する
            {
              name: "Alan Turing",
              gender: "male",
              birthDay: "1912/6/23"
            },
            {
              name: "Jane Austen",
              gender: "female",
              birthDay: "1775/12/16"
            },
            {
              name: "Fyodor Dostoyevsky",
              gender: "male",
              birthDay: "1821/11/11"
            },
            {
              name: "Ada Lovelace",
              gender: "female",
              birthDay: "1815/12/10"
            }
          ];
        /* #@range_end(addressbook_example_in_array) */
        // var addressbook = [ // 配列に要素を格納する
        //   {
        //     name: "Alan Turing",
        //     birthDay: "1912/6/23",
        //     weight: 62,
        //     height: 175
        //   },
        //   {
        //     name: "Haskell Curry",
        //     birthDay: "1900/9/12",
        //     weight: 62,
        //     height: 180
        //   },
        //   {
        //     name: "Alonzo Church",
        //     birthDay: "1903/6/14",
        //     weight: 75,
        //     height: 168
        //   }
        // ];
        /* #@range_begin(sorting_array) */
        expect(
          addressbook.sort((onePerson,anotherPerson) => {
            return onePerson.name> anotherPerson.name;
          })
        ).to.eql(
          [
            {
              name: "Ada Lovelace",
              gender: "female",
              birthDay: "1815/12/10"
            },
            {
              name: "Alan Turing",
              gender: "male",
              birthDay: "1912/6/23"
            },
            {
              name: "Fyodor Dostoyevsky",
              gender: "male",
              birthDay: "1821/11/11"
            },
            {
              name: "Jane Austen",
              gender: "female",
              birthDay: "1775/12/16"
            }
          ]
        );
        /* #@range_end(sorting_array) */
        next();
      });
    });
    describe('抽象データ型', () => {
      it('抽象データ型としてのリスト', (next) => {
        var cons = (n, list) => {
          return [n].concat(list);
        };
        var head = (list) => {
          return list[0];
        };
        var tail = (list) => {
          return list.slice(1,list.length);
        };
        var empty = [];
        var isEmpty = (list) => {
          return list.length === 0;
        };
        /* #@range_begin(list_as_abstract_type) */
        expect(
          head(tail(cons(1,cons(2,empty))))
        ).to.eql(
          2
        );
        /* #@range_end(list_as_abstract_type) */
        next();
      });
      it('抽象データ型としてのスタック', (next) => {
        var push = (n, stack) => {
          return [n].concat(stack);
        };
        var top = (stack) => {
          return stack[0];
        }
        var pop = (stack) => {
          return stack.slice(1,stack.length);
        };
        var empty = [];
        /* #@range_begin(stack_as_abstract_type) */
        expect(
          top(pop(pop(push(3,push(2,push(1,empty))))))
        ).to.eql(
          1
        );
        /* #@range_end(stack_as_abstract_type) */
        next();
      });
    });

    describe('関数型', () => {
      it('関数はオブジェクト型である', (next) => {
        /* #@range_begin(function_is_object_type) */
        var func = (n) => {
          return n;
        };
        expect(
          func.length
        ).to.be(
          1
        )
        expect(
          func.hasOwnProperty('name')
        ).to.be(
          true
        )
        /* #@range_end(function_is_object_type) */
        next();
      });
      it('引数のない関数', (next) => {
        /* #@range_begin(function_without_argument) */
        var three = () => {
          return 3;
        };
        expect(
          three()
        ).to.eql(
          3
        )
        /* #@range_end(function_without_argument) */
        next();
      });
      it('関数と変数の類似性', (next) => {
        /* #@range_begin(function_resembles_variable) */
        var three = 3;
        expect(
          three
        ).to.eql(
          3
        );
        /* #@range_end(function_resembles_variable) */
        var id = (any) => {
          return any;
        };
        expect(
          ((x) => {
            return x;
          })(3)
        ).to.be(
          3
        );
        next();
      });
      it('オブジェクトに関数をいれる', (next) => {
        /* ##@range_begin(object_can_embed_function) */
        var addressbook = [
          {
            name: "Alan Turing",
            gender: "male",
            birthDay: "1912/6/23",
            age: (now) => {
              expect(now).to.be.a('object');
              var birthDayObject = new Date(this.birthDay);
              return (now.getFullYear() - birthDayObject.getFullYear());
            }
          },
          {
            name: "Jane Austen",
            gender: "female",
            birthDay: "1775/12/16"
          },
          {
            name: "Fyodor Dostoyevsky",
            gender: "male",
            birthDay: "1821/11/11"
          },
          {
            name: "Ada Lovelace",
            gender: "female",
            birthDay: "1815/12/10"
          }
        ];
        expect(
          addressbook[0].age(new Date())
        ).to.be(103);
        /* ##@range_end(object_can_embed_function) */
        /* ##@range_begin(embeded_function_invocation) */
        // expect(
        //   addressbook.BMI(addressbook.turing.weight, addressbook.turing.height)
        // ).to.within(21.0,22,0);
        /* ##@range_end(embeded_function_invocation) */
        // it('値に関数をいれる', (next) => {
        //   var natural = {
        //     zero: 0,
        //     succ: (n) => {
        //       return n + 1;
        //     }
        //   };
        //   expect(
        //     natural.succ(natural.zero)
        //   ).to.eql(
        //     1
        //   );
        next();
      });
      it('関数はデータと類似している', (next) => {
        // trait env extends terms {
        //   abstract class Env {
        //     def apply[a](v : Var[a]): a
        //     def extend[a](v : Var[a], x : a) = new Env {
        //       def apply[b](w: Var[b]): b = w match {
        //         case _: v.type => x
        //         case _ =>  Env.this.apply(w)
        //       }}}
        //   object emptyEnv extends Env {
        //     def apply[a](x : Var[a]): a = throw new Error("not found : "+x.name)
        //   }
        // }
        /* #@range_begin(function_as_object) */
        var empty = (key) => {
          return undefined;
        };
        var get = (key, obj) => {
          return obj(key);
        };
        var set = (key, value, obj) => {
          return (key2) => {
            if(key === key2) {
              return value;
            } else {
              return get(key2,obj)
            }
          }
        };
        expect(
          get(1,set(1,"one",empty)) // {1: "one"}[1] => "one"
        ).to.eql(
          "one"
        )
        expect(
          get(2,set(2,"two",set(1,"one",empty)))  // {1: "one", 2: "two"}[2] => "two"
        ).to.eql(
          "two"
        )
        expect(
          get(1,set(2,"two",set(1,"one",empty)))  // {1: "one", 2: "two"}[1] => "one"
        ).to.eql(
          "one"
        )
        /* #@range_end(function_as_object) */
        next();
      });
    });
    describe('合成型の可変性', () => {
      it('配列は可変である', (next) => {
        /* #@range_begin(array_is_mutable) */
        var array = [0,1,2,3];
        array[0] = 7; // 配列の一部を書きかえている
        expect(
          array
        ).not.to.eql(
          [0,1,2,3] // [7,1,2,3]に変更されている
        )
        /* #@range_end(array_is_mutable) */
        next();
      });
      it('Array.reverseは破壊的操作である', (next) => {
        /* #@range_begin(destructive_reverse) */
        var array = [1,2,3,4,5];
        expect(
          array.reverse()
        ).to.eql(
          [5,4,3,2,1]
        );
        expect(
          array
        ).to.eql(
          [5,4,3,2,1] // array変数の中身が変更されている
        );
        /* #@range_end(destructive_reverse) */
        next();
      });
      it('非破壊的なreverse関数', (next) => {
        /* #@range_begin(immutable_reverse) */
        var reverse = (array) => {
          return array.reduce((accumulator, item) => {
            return [item].concat(accumulator);
          }, []);
        };
        var array = [1,2,3,4,5];
        expect(
          reverse(array)
        ).to.eql(
          [5,4,3,2,1]
        );
        expect(((_) => {
          var reversed = reverse(array);
          return array
        })()).to.eql(
          [1,2,3,4,5] // 元の配列と同じ
        );
        /* #@range_end(immutable_reverse) */
        /* #@range_begin(immutable_reverse_is_not_immutable) */
        var reversed = reverse(array);
        reversed[0] = 0;
        expect(
          reversed
        ).to.eql(
          [0,4,3,2,1]
        );
        /* #@range_end(immutable_reverse_is_not_immutable) */
        next();
      });
    });
  });
  describe('同値性', () => {
    describe('値としてのデータ', () => {
      it('基本型は値である', (next) => {
        /* #@range_begin(basic_type_is_value_type) */
        var n = 1;
        expect(
          n
        ).to.be(
          1
        )
        var s = "hello"
        expect(
          s
        ).to.be(
          "hello"
        )
        /* #@range_end(basic_type_is_value_type) */
        expect(
          n
        ).to.be(
          1
        )
        expect(
          s
        ).to.be(
          "hello"
        )
        next();
      });
      it('合成型は参照である', (next) => {
        /* #@range_begin(complex_type_is_reference) */
        var array = [0,1,2,3];
        expect(
          array
        ).not.to.be(
          [0,1,2,3]
        )
        var object = { key: 1 };
        expect(
          object
        ).not.to.be(
          {
            key: 1
          }
        )
        var func = (_) => { return 1; };
        expect(
          func
        ).not.to.be(
          ((_) => { return 1; })
        )
        /* #@range_end(complex_type_is_reference) */
        next();
      });
    });
  });
  describe('変数', () => {
    describe('スコープ', () => {
      it('関数はスコープを分ける', (next) => {
        /* #@range_begin(function_creates_scope) */
        var createScope = (_) =>  {
          var innerScope = "inner"; // innerScope変数は createScopeの中でのみ有効
          return innerScope;
        }
        expect(
          (_) => {
            innerScope // 関数内の局所スコープにある変数にアクセスを試みます
          }
        ).to.throwException((e)=> {
          expect(e).to.be.a(
            ReferenceError
          );
        });
        /* #@range_end(function_creates_scope) */
        expect(
          createScope()
        ).to.be(
          "inner"
        )
        next();
      });
      it('スコープの入れ子', (next) => {
        /* #@range_begin(nested_scope) */
        var outerScope = 1;
        var createScope = (_) =>  {
          var innerScope = 2;
          return innerScope + outerScope; // 内側のスコープから外側のスコープにある outerScope変数にアクセスしています
        }
        /* #@range_end(nested_scope) */
        expect(
          outerScope
        ).to.eql(
          1
        )
        expect(
          createScope()
        ).to.eql(
          3
        )
        next();
      });
      it('ホイスティング', (next) => {
        /* #@range_begin(hoisting) */
        var scope = "global";
        var f = (_) =>  {
          expect(
            scope
          ).to.be(
            "global"
          )
        }
        /* #@range_end(hoisting) */
        next();
      });
      it('クロージャーによるスコープの入れ子', (next) => {
        /* #@range_begin(nested_scope_in_closure) */
        var outerFunction = (outerArgument) => {
          var innerFunction = (innerArgument) => {
            return outerArgument + innerArgument;
          };
          return innerFunction;
        }
        /* #@range_end(nested_scope_in_closure) */
        // expect(
        //   adder(2)(3)
        // ).to.be(
        //   5
        // );
        next();
      });
      it('内側のスコープから外側のスコープの参照', (next) => {
        /* #@range_begin(function_scope_nesting) */
        var scope = "outer";
        var func = (_) =>  {
          return scope;
        }
        expect(
          func()
        ).to.be(
          "outer"
        )
        // var outer = 1;
        // var f = (_) => {
        //   var middle = 2;
        //   var g = (_) => {
        //  var inner = 3;
        //  return outer + middle + inner;
        //   };
        //   return g();
        // };
        // expect(
        //   f()
        // ).to.be(
        //   6
        // )
        /* #@range_end(function_scope_nesting) */
        next();
      });
    });
    describe('環境と値', () => {
      it('変数のバインディング', (next) => {
        /* #@range_begin(variable_binding_value) */
        var bound = "バインドされた値";
        expect(
          bound
        ).to.be(
          "バインドされた値"
        );
        expect(
          (_) => { // 例外をキャッチするにはexpectに関数を渡します
            unbound // ここで自由変数にアクセスします
          }
        ).to.throwException((exception)=> {
          expect(exception).to.be.a(
            ReferenceError
          );
        });
        /* #@range_end(variable_binding_value) */
        next();
      });
      it('関数本体でのバインド変数', (next) => {
        /* #@range_begin(bound_variable_in_function) */
        var add = (x,y) => {
          return x + y; // x も y もバインド変数
        };
        /* #@range_end(bound_variable_in_function) */
        expect(
          add(2,3)
        ).to.be(
          5
        );
        expect(
          add(2,3)
        ).to.be(
          ((x,y) => {
            return x + y;
          })(2,3)
        );
        next();
      });
      it('関数本体での自由変数', (next) => {
        /* #@range_begin(free_variable_in_function) */
        var add = (x) => {
          return x + y;  // x はバインド変数だが、y は自由変数
        };
        /* #@range_end(free_variable_in_function) */
        /* #@range_begin(free_variable_in_function_test) */
        expect(
          (_) => {
            add(1,2)
          }
        ).to.throwException((exception)=> {
          expect(exception).to.be.a(
            ReferenceError
          );
        });
        /* #@range_end(free_variable_in_function_test) */
        next();
      });
      it('クロージャーの変数バインディング', (next) => {
        /* #@range_begin(binding_in_closure) */
        var adder = (y) => { // 外側の関数
          var add = (x) => { // 内側の関数
            return x + y;
          };
          return add;
        };
        /* #@range_end(binding_in_closure) */
        /* #@range_begin(binding_in_closure_test) */
        expect(
          adder(2)(3)
        ).to.be(
          5
        );
        /* #@range_end(binding_in_closure_test) */
        next();
      });
      it('環境の実装', (next) => {
        /* #@range_begin(environment_implemented) */
        // 空の環境
        var emptyEnv = (variable) => {
          return undefined;
        };
        // 変数名に対応する値を環境から取りだす
        var lookupEnv = (variable, env) => {
          return env(variable);
        };
        // 環境を拡張する
        var extendEnv = (variable, value, env) => {
          return (queryVariable) => {
            if(variable === queryVariable) {
              return value;
            } else {
              return lookupEnv(queryVariable,env);
            }
          };
        };
        /* #@range_end(environment_implemented) */
        /* #@range_begin(environment_implemented_usage) */
        expect(((_) => {
          // 空の辞書を作成する
          var initEnv = emptyEnv;
          // var a = 1 を実行して、辞書を拡張する
          var firstEnv = extendEnv("a", 1, initEnv);
          // var b = 3 を実行して、辞書を拡張する
          var secondEnv = extendEnv("b",3, firstEnv);
          // 辞書から b の値を参照する
          return lookupEnv("b",secondEnv);
        })()).to.eql(
          3
        );
        /* #@range_end(environment_implemented_usage) */
        /* #@range_begin(environment_implemented_shadowing_usage) */
        expect(((_) => {
          // 空の辞書を作成する
          var initEnv = emptyEnv;
          // var x = 1 を実行して、辞書を拡張する
          var xEnv = extendEnv("x", 1, initEnv);
          // var z = 2 を実行して、辞書を拡張する
          var zEnv = extendEnv("z", 2, xEnv);
          // 内部のスコープで var x = 3 を実行して、辞書を拡張する
          var xEnvInner = extendEnv("x",3, zEnv);
          // 内部のスコープで var y = 4 を実行して、辞書を拡張する
          var innerMostEnv = extendEnv("y",4, xEnvInner);
          // 一番内側のスコープを利用して x + y + z を計算する
          return lookupEnv("x",innerMostEnv) + lookupEnv("y",innerMostEnv) + lookupEnv("z",innerMostEnv) ;
        })()).to.eql(
          3 + 4 + 2
        );
        /* #@range_end(environment_implemented_shadowing_usage) */
        next();
      });
    });
    describe('記憶域と参照', () => {
      it('変数への代入', (next) => {
        /* #@range_begin(assign_to_variable) */
        var age = 29;
        expect(
          age
        ).to.be(
          29
        );
        // この時点で誕生日を迎えました
        age = 30;
        expect(
          age
        ).to.be(
          30
        );
        /* #@range_end(assign_to_variable) */
        next();
      });
      it('変数は参照を保持する', (next) => {
        /* #@range_begin(variables_hold_reference) */
        expect(() => {
          var x = 1;
          var y = x;
          y = 2;
          return x;
        }()).to.be(
          1
        )
        expect(() => {
          var x = 1;
          var y = x;
          x = 2;
          return y;
        }()).to.be(
          1
        )
        expect(() => {
          var x = [0,1,2,3];
          var y = x;
          return x === y;
        }()).to.be(
          true
        )
        /* #@range_end(variables_hold_reference) */
        next();
      });
    });
    describe('代入の仕組み', () => {
      it('合成型への代入', (next) => {
        /* #@range_begin(assign_to_complex_value) */
        var object = {one: 1, two: 2}
        expect(
          object.one
        ).to.eql(
          1
        );
        object.one = 3;
        expect(
          object.one
        ).to.eql(
          3
        );
        /* #@range_end(assign_to_complex_value) */
        next();
      });
      it('基本型への代入', (next) => {
        /* #@range_begin(assign_to_basic_value) */
        var basic_value = 1;
        expect(
          basic_value
        ).to.eql(
          1
        );
        basic_value = 3;
        expect(
          basic_value
        ).to.eql(
          3
        );
        /* #@range_end(assign_to_basic_value) */
        next();
      });
    });
  });
  describe('参照透過性の仕組み', () => {
    it('参照透過性がある場合', (next) => {
      /* #@range_begin(referential_transparency_example) */
      var x = 1;
      var y = 2;
      var z = x + y;
      expect(
        (x + y) * (x + y)
      ).to.be(
        z * z // (x + y) を z に置きかえた
      );
      /* #@range_end(referential_transparency_example) */
      next();
    });
    it('代入によって参照透過性を失う場合', (next) => {
      /* #@range_begin(referential_transparency_counterexample_assignment) */
      var x = 1;
      var y = 2;
      var z = x + y;
      x = 3
      expect(
        (x + y) * (x + y)
      ).not.to.be(
        z * z // (x + y) を z に置きかえた
      );
      /* #@range_end(referential_transparency_counterexample_assignment) */
      next();
    });
    it('可変なデータによって参照透過性を失う場合', (next) => {
      /* #@range_begin(referential_transparency_counterexample_mutable) */
      var x = 1;
      var y = 2;
      var z = [x,y];
      expect(
        [x,y]
      ).not.to.be( // notを使っている
        z // [x,y] を z に置きかえた
      );
      /* #@range_end(referential_transparency_counterexample_mutable) */
      next();
    });
  });
});
