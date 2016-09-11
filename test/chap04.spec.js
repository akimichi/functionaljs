"use strict";

// データの種類と特徴
// ========

var expect = require('expect.js');

// ## 型とは何か
describe('型とは何か', () => {
  // 自然数の作り方
  it('自然数の作り方', (next) => {
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
});
// ### 基本型
describe('基本型', () => {
  // 未定義の変数
  it('未定義の変数', (next) => {
    /* #@range_begin(undefined_variable) */
    var variable; // 宣言されているが値と結びついていない変数
    expect(
      variable
    ).to.eql(
      undefined
    );
    /* #@range_end(undefined_variable) */
    next();
  });
  it('真理値型は不変である', (next) => {
    // ~~~
    // node> true === true
    // true
    // ~~~
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
    // ~~~
    // node> 1 === 1
    // true
    // ~~~
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
    ).to.eql(
      "TO BE, OR NOT TO BE"
    );
    expect(
      str
    ).to.eql(
      "to be, or not to be"
    );
    /* #@range_end(string_is_immutable)  */
    next();
  });
});
// ### 合成型
describe('合成型', () => {
  // #### オブジェクト型
  describe('オブジェクト型', () => {
    describe('アドレス帳の例', () => {
      // オブジェクト型の例
      it('オブジェクト型の例', (next) => {
        /* #@range_begin(object_instance_example) */
        var addressbook = {
          No1: "Alan Turing",
          No2: "Haskell Curry",
          No3: "Alonzo Church",
          No4: "Ada Lovelace"
        };
        /* #@range_end(object_instance_example) */
        next();
      });
      // オブジェクト型の入れ子
      it('オブジェクト型の入れ子', (next) => {
        /* ##@range_begin(object_can_embed_object) */
        var addressbook = {
          No1: {
            name: "Alan Turing",
            gender: "male",
            birthDay: "1912/6/23"
          },
          No2: {
            name: "Haskell Curry",
            gender: "male",
            birthDay: "1900/9/12"
          },
          No3: {
            name: "Alonzo Church",
            gender: "male",
            birthDay: "1903/6/14"
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
        /* hasOwnPropertyでプロパティの有無を調べる */
        // オブジェクト型インスタンスへのアクセス
        /* #@range_begin(object_access) */
        expect(
          addressbook.No1.name        // オブジェクト.キー記法
        ).to.eql(
          "Alan Turing"
        );
        expect(
          addressbook["No1"]["name"]  // オブジェクト[キー]記法
        ).to.eql(
          "Alan Turing"
        );
        /* #@range_end(object_access) */
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
  });
  // #### 配列型
  describe('配列型', () => {
    // 配列の基本操作
    it("配列の基本操作", (next) => {
      /* #@range_begin(array_access) */
      var array = [10,11,12];
      expect(
        array[0] // 配列は0から始まるインデックスを持つ
      ).to.eql(
        10
      );
      expect(
        array[2]
      ).to.eql(
        12
      );
      expect(
        array[100] // 存在しない要素にアクセスする
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
    // sortによる配列要素の並べかえ
    it("sortによる配列要素の並べかえ", (next) => {
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
    // 名簿の配列型表現
    it("名簿の配列型表現", (next) => {
      /* #@range_begin(addressbook_example_in_array) */
      var addressbook = [ // 配列に要素を格納する
        {
          name: "Alan Turing",
          gender: "male",
          birthDay: "1912/6/23"
        },
        {
          name: "Haskell Curry",
          gender: "male",
          birthDay: "1900/9/12"
        },
        {
          name: "Alonzo Church",
          gender: "male",
          birthDay: "1903/6/14"
        },
        {
          name: "Ada Lovelace",
          gender: "female",
          birthDay: "1815/12/10"
        }
      ];
      /* #@range_end(addressbook_example_in_array) */
      // 名簿の並べかえ
      /* #@range_begin(sorting_array) */
      expect(
        addressbook.sort((onePerson,anotherPerson) => {
          return onePerson.name > anotherPerson.name;
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
            name: "Alonzo Church",
            gender: "male",
            birthDay: "1903/6/14"
          },
          {
            name: "Haskell Curry",
            gender: "male",
            birthDay: "1900/9/12"
          }
        ]
      );
      /* #@range_end(sorting_array) */
      next();
    });
  });
  // #### 関数型
  describe('関数型', () => {
    // 関数はオブジェクト型である
    it('関数はオブジェクト型である', (next) => {
      /* #@range_begin(function_is_object_type) */
      var func = (n) => {
        return n;
      };
      expect(
        func.length
      ).to.eql(
        1
      );
      /* #@range_end(function_is_object_type) */
      expect(
        func.hasOwnProperty('name')
      ).to.eql(
        true
      );
      next();
    });
    it('引数のない関数の適用', (next) => {
      /* #@range_begin(function_without_argument) */
      var three = () => {
        return 3;
      };
      expect(
        three()
      ).to.eql(
        3
      );
      /* #@range_end(function_without_argument) */
      next();
    });
    // 関数と変数の類似
    it('関数と変数の類似', (next) => {
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
    it('関数はデータと類似している', (next) => {
      // ~~~scala
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
      // ~~~
      /* #@range_begin(function_as_object) */
      var empty = (key) => {
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
      expect(
        get(1,set(1,"one",empty)) // {1: "one"}[1] => "one"
      ).to.eql(
        "one"
      );
      expect(
        get(2,set(2,"two",set(1,"one",empty)))  // {1: "one", 2: "two"}[2] => "two"
      ).to.eql(
        "two"
      );
      expect(
        get(1,set(2,"two",set(1,"one",empty)))  // {1: "one", 2: "two"}[1] => "one"
      ).to.eql(
        "one"
      );
      /* #@range_end(function_as_object) */
      next();
    });
  });
  // #### 抽象データ型
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
      var empty = (_) => {
        return [];
      };
      var isEmpty = (list) => {
        return list.length === 0;
      };
      /* #@range_begin(list_as_abstract_type) */
      expect(
        head(tail(cons(1,cons(2,empty()))))
      ).to.eql(
        2
      );
      /* #@range_end(list_as_abstract_type) */
      next();
    });
  });
  // #### JavaScriptにおける合成型の可変性
  describe('合成型の可変性', () => {
    // 配列型の可変性
    it('配列型の可変性', (next) => {
      /* #@range_begin(array_is_mutable) */
      var array = [0,1,2,3];
      array[0] = 7; // 配列の一部を書きかえている
      expect(
        array
      ).not.to.eql(
        [0,1,2,3] // [7,1,2,3]に変更されている
      );
      /* #@range_end(array_is_mutable) */
      next();
    });
    // 配列の破壊的メソッド
    it('配列の破壊的メソッド', (next) => {
      /* #@range_begin(destructive_reverse) */
      var array = [1,2,3,4,5];
      expect(
        array.reverse()
      ).to.eql(
        [5,4,3,2,1]
      );
      expect(
        array
      ).not.to.eql(
        [1,2,3,4,5]  // 変数arrayの中身が[5,4,3,2,1]に変更されている
      );
      /* #@range_end(destructive_reverse) */
      next();
    });
    it('非破壊的なreverse関数', (next) => {
      // 非破壊的なreverse関数
      /* #@range_begin(immutable_reverse) */
      var reverse = (array) => {
        return array.reduce((accumulator, item) => {
          return [item].concat(accumulator);
        }, []);
      };
      var array = [1,2,3,4,5];
      expect(((_) => {
        var reversed = reverse(array);
        return array; // 逆転前の配列を返す
      })()).to.eql(
        [1,2,3,4,5]   // 逆転前の配列と同じ
      );
      /* #@range_end(immutable_reverse) */
      expect(
        reverse(array)
      ).to.eql(
        [5,4,3,2,1]
      );
      // 非破壊的なreverse関数は完全には不変でない
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
// ## 変数とデータの関係
describe('変数とデータの関係', () => {
  it('変数のバインド', (next) => {
    // バインド変数と自由変数
    /* #@range_begin(variable_binding_value) */
    var bound = "我思うゆえに我あり";
    expect(
      bound
    ).to.eql(
      "我思うゆえに我あり"
    );
    expect(
      (_) => { // 例外をキャッチするにはexpectに関数を渡す
        unbound // 変数unboundは自由変数
      }
    ).to.throwException((exception)=> {
      expect(exception).to.be.a(
        ReferenceError
      );
    });
    /* #@range_end(variable_binding_value) */
    next();
  });
  // #### <a name="environment_demo"> **変数バインディングと環境** </a>
  // ![変数バインディングと環境](images/environment.gif) 
  it('関数本体でのバインド変数', (next) => {
    /* #@range_begin(bound_variable_in_function) */
    var add = (x,y) => { // xとyは引数
      return x + y; // それゆえに、xもyもバインド変数
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
  describe('環境と値', () => {
    it('関数本体での自由変数', (next) => {
      // 関数本体での自由変数
      /* #@range_begin(free_variable_in_function) */
      var addWithFreeVariable = (x) => {
        return x + y;  // xはバインド変数だが、yは自由変数
      };
      /* #@range_end(free_variable_in_function) */
      // 関数本体での自由変数のテスト
      /* #@range_begin(free_variable_in_function_test) */
      expect(
        (_) => {
          return addWithFreeVariable(1);
        }
      ).to.throwException((exception)=> {
        expect(exception).to.be.a(
          ReferenceError
        );
      });
      /* #@range_end(free_variable_in_function_test) */
      next();
    });
    it('環境の実装', (next) => {
      /* #@range_begin(environment_implemented) */
      /* 空の環境 */
      var emptyEnv = (variable) => {
        return null;
      };
      /* 変数名に対応する値を環境から取りだす */
      var lookupEnv = (variable, env) => {
        return env(variable);
      };
      /* 環境を拡張する */
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
        /* 空の辞書を作成する */
        var initEnv = emptyEnv;
        /* var a = 1 を実行して、辞書を拡張する */
        var firstEnv = extendEnv("a", 1, initEnv);
        /* var b = 3 を実行して、辞書を拡張する */
        var secondEnv = extendEnv("b",3, firstEnv);
        /* 辞書から b の値を参照する */
        return lookupEnv("b",secondEnv);
      })()).to.eql(
        3
      );
      /* #@range_end(environment_implemented_usage) */
      /* #@range_begin(environment_implemented_shadowing_usage) */
      expect(((_) => {
        /* 空の辞書を作成する */
        var initEnv = emptyEnv;
        /* var x = 1 を実行して、辞書を拡張する */
        var xEnv = extendEnv("x", 1, initEnv);
        /* var z = 2 を実行して、辞書を拡張する */
        var zEnv = extendEnv("z", 2, xEnv);
        /* 内部のスコープで var x = 3 を実行して、辞書を拡張する */
        var xEnvInner = extendEnv("x",3, zEnv);
        /* 内部のスコープで var y = 4 を実行して、辞書を拡張する */
        var innerMostEnv = extendEnv("y",4, xEnvInner);
        /* 一番内側のスコープを利用して x + y + z を計算する */
        return lookupEnv("x",innerMostEnv) + lookupEnv("y",innerMostEnv) + lookupEnv("z",innerMostEnv) ;
      })()).to.eql(
        3 + 4 + 2
      );
      /* #@range_end(environment_implemented_shadowing_usage) */
      next();
    });
  });
  // #### 変数のスコープ
  describe('変数のスコープ', () => {
      // 関数とローカルスコープ
    it('関数とローカルスコープ', (next) => {
      /* #@range_begin(function_creates_scope) */
      var createScope = (_) =>  { // ローカルスコープを作る
        var innerScope = "inner"; 
        return innerScope; // 変数innerScopeはcreateScopeのなかでのみ有効
      };
      expect(
        (_) => {
          innerScope // ローカルスコープにある変数innerScopeにアクセスを試みる
        }
      ).to.throwException((e)=> {
        expect(e).to.be.a(
          ReferenceError // 参照先が見つからないという例外エラーとなる
        );
      });
      /* #@range_end(function_creates_scope) */
      expect(
        createScope()
      ).to.be(
        "inner"
      );
      next();
    });
    // 入れ子になった関数の変数バインド
    it('入れ子になった関数の変数バインド', (next) => {
      /* #@range_begin(binding_in_closure) */
      var adder = (y) => { // 外側の関数
        var addWithFreeVariable = (x) => { // 内側の関数
          return x + y; // 変数yはadder関数の引数yを参照できる
        };
        return addWithFreeVariable;
      };
      /* #@range_end(binding_in_closure) */
      // 入れ子になった関数の適用
      /* #@range_begin(binding_in_closure_test) */
      expect(
        adder(2)(3)
      ).to.eql(
        5
      );
      /* #@range_end(binding_in_closure_test) */
      next();
    });
  });
});
// ## 参照透過性の仕組み
describe('参照透過性の仕組み', () => {
  describe('記憶域と参照', () => {
    it('変数は参照を保持する', (next) => {
      /* #@range_begin(variables_hold_reference) */
      expect((() => {
        var x = 1;
        var y = x;
        y = 2;
        return x;
      })()).to.be(
        1
      );
      expect((() => {
        var x = 1;
        var y = x;
        x = 2;
        return y;
      })()).to.be(
        1
      );
      expect((() => {
        var x = [0,1,2,3];
        var y = x;
        return x === y;
      })()).to.be(
        true
      );
      /* #@range_end(variables_hold_reference) */
      next();
    });
  });
  // ### 不変なデータの仕組み 
  describe('不変なデータの仕組み', () => {
    // 基本型は値としてのデータである
    it('基本型は値としてのデータである', (next) => {
      /* #@range_begin(basic_type_is_value_type) */
      var n = 1;
      expect(
        n
      ).to.eql(
        1
      );
      var s = "hello";
      expect(
        s
      ).to.eql(
        "hello"
      );
      /* #@range_end(basic_type_is_value_type) */
      expect(
        n
      ).to.eql(
        1
      );
      expect(
        s
      ).to.eql(
        "hello"
      );
      next();
    });
  });
  // ### 代入の仕組みと効果
  describe('代入の仕組みと効果', () => {
    // 変数への代入
    it('変数への代入', (next) => {
      /* #@range_begin(assign_to_variable) */
      var age = 29;
      expect(
        age
      ).to.eql(
        29
      );
      // この時点で誕生日を迎えた
      age = 30;
      expect(
        age
      ).to.eql(
        30
      );
      /* #@range_end(assign_to_variable) */
      next();
    });
    it('代入によって参照透過性を失う場合', (next) => {
      /* #@range_begin(referential_transparency_counterexample_assignment) */
      var x = 1;
      var y = 2;
      var z = x + y;
      x = 3;
      expect(
        (x + y) * (x + y)
      ).not.to.be(
        z * z // (x + y) を z に置きかえた
      );
      /* #@range_end(referential_transparency_counterexample_assignment) */
      next();
    });
  });
});

// [目次に戻る](http://akimichi.github.io/functionaljs/) [次章に移る](http://akimichi.github.io/functionaljs/chap05.spec.html) 
