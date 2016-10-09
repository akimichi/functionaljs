"use strict";

// 第4章 データの種類と特徴
// ========

// ## 小目次
// <div class="toc">
// <ul class="toc">
//   <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#what-is-type">4.1 型とは何か</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#basic-type">4.2 基本型</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#composite-type">4.3 合成型</a>
//     <ul>
//       <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#object-type">オブジェクト型</a></li>
//       <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#array-type">配列型</a></li>
//       <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#function-type">関数型</a></li>
//       <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#abstract-datatype">抽象データ型</a></li>
//       <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#mutability-of-composite-type">合成型の可変性</a></li>
//    </ul></li>
//   <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#variable-and-data">4.4 変数とデータの関係</a>
//     <ul>
//        <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#variable-binding">変数のバインド</a></li>
//       <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#variable-scope">変数のスコープ</a></li></ul>
//   </li>
//   <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#mechanism-of-referential-transparency">4.5 参照透過性の仕組み</a>
//      <ul>
//        <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#mechanism-of-immutability">不変なデータの仕組み</a></li>
//        <li><a href="http://akimichi.github.io/functionaljs/chap04.spec.html#mechanism-of-assingment">代入の仕組みと効果</a></li></ul>
//   </li>
// </ul>
// </div>

var expect = require('expect.js');

// ## 4.1 <section id='what-is-type'>型とは何か</section>
describe('型とは何か', () => {
  // **リスト4.1** 自然数の作り方
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
// ## 4.2 <section id='basic-type'>基本型</section>
// 基本型とは、そのデータ型の定義に他のデータ型を含まないような原始的な型をいう。
describe('基本型', () => {
  // **リスト4.2** 未定義の変数
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
  // ### 基本型は不変なデータである
  describe('基本型は不変なデータである', () => {
    it('真理値型は不変である', (next) => {
      // ~~~
      // node> true === true
      // true
      // ~~~
      /* #@range_begin(truth_is_immutable) */
      var truth = true;
      expect(
        true === true
      ).to.eql(
        true
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
        1 === 1
      ).to.eql(
        true
      );
      /* #@range_end(number_is_immutable) */
      next();
    });
    it('文字列は不変である', (next) => {
      /* #@range_begin(string_is_immutable) */
      var str = "to be, or not to be";
      expect(
        str
      ).to.eql(
        "to be, or not to be"
      );
      /* #@range_end(string_is_immutable)  */
      next();
    });
  });
});

// ## 4.3 <section id='composite-type'>合成型</section>
// 合成型は、内部に構造を持つ
describe('合成型', () => {
  // ### <section id='object-type'>オブジェクト型</section>
  // > 参考資料: [オブジェクトを利用する](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Working_with_Objects)
  describe('オブジェクト型', () => {
    describe('アドレス帳の例', () => {
      // **リスト4.4** オブジェクト型の例
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
      // **リスト4.5** オブジェクト型の入れ子
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
        // **リスト4.6** オブジェクト型インスタンスへのアクセス
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
        next();
      });
    });
  });
  // ### <section id='array-type'>配列型</section>
  // > 参考資料: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Array  
  describe('配列型', () => {
    // **リスト4.7** 配列の基本操作
    it("配列の基本操作", (next) => {
      /* #@range_begin(array_access) */
      var array = [10,11,12];
      // 配列は0から始まるインデックスを持つ
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
      // 存在しない要素にアクセスするとundefinedとなる
      expect(
        array[100] 
      ).to.eql(
        undefined
      );
      /* #@range_end(array_access) */
      next();
    });
    // **リスト4.8** sortによる配列要素の並べかえ
    //
    // > [5,3,4,1,2]の配列を昇順に並べかえる
    // > 参考資料: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Array/sort
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
    // **リスト4.9** 名簿の配列型表現
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
      // **リスト4.10** 名簿の並べかえ
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
  // ### <section id='function-type'>関数型</section>
  describe('関数型', () => {
    // **リスト4.11** 関数はオブジェクト型である
    it('関数はオブジェクト型である', (next) => {
      /* #@range_begin(function_is_object_type) */
      var func = (n) => {
        return n;
      };
      /* lengthプロパティにアクセスするとアリティ（引数の数）が返る */
      expect(
        func.length
      ).to.eql(
        1
      );
      /* #@range_end(function_is_object_type) */
      next();
    });
    // **リスト4.12** 引数のない関数 
    it('引数のない関数', (next) => {
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
    // **リスト4.13** 関数と変数の類似
    it('関数と変数の類似', (next) => {
      /* #@range_begin(function_resembles_variable) */
      var three = 3;
      expect(
        three
      ).to.eql(
        3
      );
      /* #@range_end(function_resembles_variable) */
      next();
    });
  });
  // ### <section id='abstract-datatype'>抽象データ型</section>
  // > 参考資料: [抽象データ型](https://ja.wikipedia.org/wiki/%E6%8A%BD%E8%B1%A1%E3%83%87%E3%83%BC%E3%82%BF%E5%9E%8B)
  describe('抽象データ型', () => {
    // **リスト4.16** 具体的なリストの利用法
    it('具体的なリストの利用法', (next) => {
      /* 具体的なリスト型を定義しておく */
      /* ただし、引数listにはJavaScriptの配列が入る */
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
  // ### <section id='mutability-of-composite-type'>合成型の可変性</section>
  describe('合成型の可変性', () => {
    // **リスト4.17** 配列型の可変性
    // > JavaScriptの配列は、その一部を書きかえることができる
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
    // **リスト4.18** 配列の破壊的メソッド
    // > 配列のreverse関数は、元の配列を逆転させる
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
      // **リスト4.19** 非破壊的なreverse関数
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
      // **リスト4.20** 非破壊的なreverse関数は完全には不変でない
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
// ## 4.4 <section id='variable-and-data'>変数とデータの関係</section>
describe('変数とデータの関係', () => {
  // ### <section id='variable-binding'>変数のバインド</section>
  // > 変数boundはバインドされているが、変数unboundはバインドされていない
  it('変数のバインド', (next) => {
    // **リスト4.21** バインド変数と自由変数
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
  // **リスト4.22** 関数本体でのバインド変数
  it('関数本体でのバインド変数', (next) => {
    /* #@range_begin(bound_variable_in_function) */
    var add = (x,y) => { // xとyは引数
      return x + y; // それゆえに、xもyもバインド変数
    };
    /* #@range_end(bound_variable_in_function) */
    expect(
      add(2,3)
    ).to.eql(
      5
    );
    expect(
      add(2,3)
    ).to.eql(
      ((x,y) => {
        return x + y;
      })(2,3)
    );
    next();
  });
  describe('環境と値', () => {
    it('関数本体での自由変数', (next) => {
      // **リスト4.23** 関数本体での自由変数
      /* #@range_begin(free_variable_in_function) */
      var addWithFreeVariable = (x) => {
        return x + y;  // xはバインド変数だが、yは自由変数
      };
      /* #@range_end(free_variable_in_function) */
      // 関数本体での自由変数のテスト
      // 
      // 例外が発生する場合は、無名関数で包みこみ、発生した例外を以下のように捕捉する。
      // > to.throwException((exception)=> {
      // >   expect(exception).to.be.a(
      // >     ReferenceError
      // >   );
      // > })
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
  });
  // ### <section id='variable-scope'>変数のスコープ</section>
  describe('変数のスコープ', () => {
    // **リスト4.25** 関数とローカルスコープ
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
    // **リスト4.26** 入れ子になった関数の変数バインド
    it('入れ子になった関数の変数バインド', (next) => {
      /* #@range_begin(binding_in_closure) */
      var adder = (y) => { // 外側の関数
        var addWithFreeVariable = (x) => { // 内側の関数
          return x + y; // 変数yはadder関数の引数yを参照できる
        };
        return addWithFreeVariable;
      };
      /* #@range_end(binding_in_closure) */
      // **リスト4.27** 入れ子になった関数の適用 
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
// ## 4.5 <section id='mechanism-of-referential-transparency'>参照透過性の仕組み</section>
describe('参照透過性の仕組み', () => {
  // ### <section id='mechanism-of-immutability'>不変なデータの仕組み</section>
  describe('不変なデータの仕組み', () => {
    // **リスト4.28** 基本型は値としてのデータである
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
  // ### <section id='mechanism-of-assingment'>代入の仕組みと効果</section>
  describe('代入の仕組みと効果', () => {
    // **リスト4.29** 変数への代入
    it('変数への代入', (next) => {
      /* #@range_begin(assign_to_variable) */
      var age = 29;
      expect(
        age
      ).to.eql(
        29
      );
      /* この時点で誕生日を迎えた */
      age = 30;
      expect(
        age
      ).to.eql(
        30
      );
      /* #@range_end(assign_to_variable) */
      next();
    });
    // <a name="mechanism-of-assignment"> **代入の仕組み** </a>
    // ![代入の仕組み](images/mechanism-of-assignment.gif) 
  });
});

// [目次に戻る](index.html) [次章に移る](chap05.spec.html) 
