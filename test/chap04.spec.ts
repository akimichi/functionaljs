"use strict";

// 第4章 データの種類と特徴
// ========

// ## 4.1 <section id='what-is-type'>型とは何か</section>
describe('型とは何か', () => {
  // **リスト4.1** 自然数の作り方
  it('自然数の作り方', () => {
    /* #@range_begin(integer_construction) */
    const succ = (n: number) => {
      return n + 1;
    };
    expect(
      succ(0)
    ).toEqual(
      1
    );
    expect(
      succ(succ(0))
    ).toEqual(
      2
    );
    /* #@range_end(integer_construction) */
  });
});

// ## 4.2 <section id='basic-type'>基本型</section>
describe('基本型', () => {
  // **リスト4.2** 未定義の変数
  it('未定義の変数', () => {
    /* #@range_begin(undefined_variable) */
    let variable; // 宣言されているが値と結びついていない変数
    expect(
      variable
    ).toEqual(
      undefined
    );
    /* #@range_end(undefined_variable) */
  });

  // ### 基本型は不変なデータである
  describe('基本型は不変なデータである', () => {
    it('真理値型は不変である', () => {
      /* #@range_begin(truth_is_immutable) */
      const truth = true;
      expect(
        true === true
      ).toEqual(
        true
      );
      /* #@range_end(truth_is_immutable) */
    });

    it('数値型は不変である', () => {
      /* #@range_begin(number_is_immutable) */
      expect(
        1 === 1
      ).toEqual(
        true
      );
      /* #@range_end(number_is_immutable) */
    });

    it('文字列は不変である', () => {
      /* #@range_begin(string_is_immutable) */
      const str = "to be, or not to be";
      expect(
        str
      ).toEqual(
        "to be, or not to be"
      );
      /* #@range_end(string_is_immutable)  */
    });
  });
});

// ## 4.3 <section id='composite-type'>合成型</section>
describe('合成型', () => {
  // ### <section id='object-type'>オブジェクト型</section>
  describe('オブジェクト型', () => {
    describe('アドレス帳の例', () => {
      // **リスト4.4** オブジェクト型の例
      it('オブジェクト型の例', () => {
        /* #@range_begin(object_instance_example) */
        const addressbook = {
          No1: "Alan Turing",
          No2: "Haskell Curry",
          No3: "Alonzo Church",
          No4: "Ada Lovelace"
        };
        /* #@range_end(object_instance_example) */
      });

      // **リスト4.5** オブジェクト型の入れ子
      it('オブジェクト型の入れ子', () => {
        /* ##@range_begin(object_can_embed_object) */
        const addressbook = {
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
        const addressbook_nested = {
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
        ).toEqual(
          "Alan Turing"
        );
        expect(
          addressbook["No1"]["name"]  // オブジェクト[キー]記法
        ).toEqual(
          "Alan Turing"
        );
        /* #@range_end(object_access) */
      });
    });
  });

  // ### <section id='array-type'>配列型</section>
  describe('配列型', () => {
    // **リスト4.7** 配列の基本操作
    it("配列の基本操作", () => {
      /* #@range_begin(array_access) */
      const array = [10, 11, 12];
      // 配列は0から始まるインデックスを持つ
      expect(
        array[0]
      ).toEqual(
        10
      );
      expect(
        array[2]
      ).toEqual(
        12
      );
      // 存在しない要素にアクセスするとundefinedとなる
      expect(
        array[100]
      ).toEqual(
        undefined
      );
      /* #@range_end(array_access) */
    });

    // **リスト4.8** sortによる配列要素の並べかえ
    it("sortによる配列要素の並べかえ", () => {
      /* #@range_begin(sort_in_array) */
      const array = [5, 3, 4, 1, 2];
      expect(
        array.sort((n, m) => {
          return n - m;
        })
      ).toEqual(
        [1, 2, 3, 4, 5]
      );
      /* #@range_end(sort_in_array) */
    });

    // **リスト4.9** 名簿の配列型表現
    it("名簿の配列型表現", () => {
      /* #@range_begin(addressbook_example_in_array) */
      const addressbook = [ // 配列に要素を格納する
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
        addressbook.sort((onePerson, anotherPerson) => {
          return onePerson.name.localeCompare(anotherPerson.name);
        })
      ).toEqual(
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
    });
  });

  // ### <section id='function-type'>関数型</section>
  describe('関数型', () => {
    // **リスト4.11** 関数はオブジェクト型である
    it('関数はオブジェクト型である', () => {
      /* #@range_begin(function_is_object_type) */
      const func = (n: number) => {
        return n;
      };
      /* lengthプロパティにアクセスするとアリティ（引数の数）が返る */
      expect(
        func.length
      ).toEqual(
        1
      );
      /* #@range_end(function_is_object_type) */
    });

    // **リスト4.12** 引数のない関数
    it('引数のない関数', () => {
      /* #@range_begin(function_without_argument) */
      const three = () => {
        return 3;
      };
      expect(
        three()
      ).toEqual(
        3
      );
      /* #@range_end(function_without_argument) */
    });

    // **リスト4.13** 関数と変数の類似
    it('関数と変数の類似', () => {
      /* #@range_begin(function_resembles_variable) */
      const three = 3;
      expect(
        three
      ).toEqual(
        3
      );
      /* #@range_end(function_resembles_variable) */
    });
  });

  // ### <section id='abstract-datatype'>抽象データ型</section>
  describe('抽象データ型', () => {
    // **リスト4.16** 具体的なリストの利用法
    it('具体的なリストの利用法', () => {
      /* 具体的なリスト型を定義しておく */
      /* ただし、引数listにはJavaScriptの配列が入る */
      const cons = <T>(n: T, list: T[]): T[] => {
        return [n].concat(list);
      };
      const head = <T>(list: T[]): T => {
        return list[0];
      };
      const tail = <T>(list: T[]): T[] => {
        return list.slice(1, list.length);
      };
      const empty = <T>(): T[] => {
        return [];
      };
      const isEmpty = <T>(list: T[]) => {
        return list.length === 0;
      };
      /* #@range_begin(list_as_abstract_type) */
      expect(
        head(tail(cons(1, cons(2, empty()))))
      ).toEqual(
        2
      );
      /* #@range_end(list_as_abstract_type) */
    });
  });

  // ### <section id='mutability-of-composite-type'>合成型の可変性</section>
  describe('合成型の可変性', () => {
    // **リスト4.17** 配列型の可変性
    it('配列型の可変性', () => {
      /* #@range_begin(array_is_mutable) */
      const array = [0, 1, 2, 3];
      array[0] = 7; // 配列の一部を書きかえている
      expect(
        array
      ).not.toEqual(
        [0, 1, 2, 3] // [7,1,2,3]に変更されている
      );
      /* #@range_end(array_is_mutable) */
    });

    // **リスト4.18** 配列の破壊的メソッド
    it('配列の破壊的メソッド', () => {
      /* #@range_begin(destructive_reverse) */
      const array = [1, 2, 3, 4, 5];
      expect(
        array.reverse()
      ).toEqual(
        [5, 4, 3, 2, 1]
      );
      expect(
        array
      ).not.toEqual(
        [1, 2, 3, 4, 5]  // 変数arrayの中身が[5,4,3,2,1]に変更されている
      );
      /* #@range_end(destructive_reverse) */
    });

    it('非破壊的なreverse関数', () => {
      // **リスト4.19** 非破壊的なreverse関数
      /* #@range_begin(immutable_reverse) */
      const reverse = <T>(array: T[]): T[] => {
        return array.reduce((accumulator: T[], item) => {
          return [item].concat(accumulator);
        }, []);
      };
      const array = [1, 2, 3, 4, 5];
      expect((() => {
        const reversed = reverse(array);
        return array; // 逆転前の配列を返す
      })()).toEqual(
        [1, 2, 3, 4, 5]   // 逆転前の配列と同じ
      );
      /* #@range_end(immutable_reverse) */
      expect(
        reverse(array)
      ).toEqual(
        [5, 4, 3, 2, 1]
      );
      // **リスト4.20** 非破壊的なreverse関数は完全には不変でない
      /* #@range_begin(immutable_reverse_is_not_immutable) */
      const reversed = reverse(array);
      reversed[0] = 0;
      expect(
        reversed
      ).toEqual(
        [0, 4, 3, 2, 1]
      );
      /* #@range_end(immutable_reverse_is_not_immutable) */
    });
  });
});

// ## 4.4 <section id='variable-and-data'>変数とデータの関係</section>
describe('変数とデータの関係', () => {
  // ### <section id='variable-binding'>変数のバインド</section>
  it('変数のバインド', () => {
    // **リスト4.21** バインド変数と自由変数
    /* #@range_begin(variable_binding_value) */
    const bound = "我思うゆえに我あり";
    expect(
      bound
    ).toEqual(
      "我思うゆえに我あり"
    );
    expect(
      (_: any) => { // 例外をキャッチするにはexpectに関数を渡す
        (undefined as any).unbound // 変数unboundは自由変数
      }
    ).toThrow();
  });
    /* #@range_end(variable_binding_value) */

  // **リスト4.22** 関数本体でのバインド変数
  it('関数本体でのバインド変数', () => {
    /* #@range_begin(bound_variable_in_function) */
    const add = (x: number, y: number) => { // xとyは引数
      return x + y; // それゆえに、xもyもバインド変数
    };
    /* #@range_end(bound_variable_in_function) */
    expect(
      add(2, 3)
    ).toEqual(
      5
    );
    expect(
      add(2, 3)
    ).toEqual(
      ((x: number, y: number) => {
        return x + y;
      })(2, 3)
    );
  });

  describe('環境と値', () => {
    it('関数本体での自由変数', () => {
      // **リスト4.23** 関数本体での自由変数
      /* #@range_begin(free_variable_in_function) */
      const addWithFreeVariable = (x: number) => {
        return x + (undefined as any).y;  // xはバインド変数だが、yは自由変数
      };
      /* #@range_end(free_variable_in_function) */
      // 関数本体での自由変数のテスト
      /* #@range_begin(free_variable_in_function_test) */
      expect(
        (_: any) => {
          return addWithFreeVariable(1);
        }
      ).toThrow();
      /* #@range_end(free_variable_in_function_test) */
    });
  });

  // ### <section id='variable-scope'>変数のスコープ</section>
  describe('変数のスコープ', () => {
    // **リスト4.25** 関数とローカルスコープ
    it('関数とローカルスコープ', () => {
      /* #@range_begin(function_creates_scope) */
      const createScope = (_?: any) => { // ローカルスコープを作る
        const innerScope = "inner";
        return innerScope; // 変数innerScopeはcreateScopeのなかでのみ有効
      };
      expect(
        (_: any) => {
          (undefined as any).innerScope // ローカルスコープにある変数innerScopeにアクセスを試みる
        }
      ).toThrow();
      /* #@range_end(function_creates_scope) */
      expect(
        createScope()
      ).toBe(
        "inner"
      );
    });

    // **リスト4.26** 入れ子になった関数の変数バインド
    it('入れ子になった関数の変数バインド', () => {
      /* #@range_begin(binding_in_closure) */
      const adder = (y: number) => { // 外側の関数
        const addWithFreeVariable = (x: number) => { // 内側の関数
          return x + y; // 変数yはadder関数の引数yを参照できる
        };
        return addWithFreeVariable;
      };
      /* #@range_end(binding_in_closure) */
      // **リスト4.27** 入れ子になった関数の適用
      /* #@range_begin(binding_in_closure_test) */
      expect(
        adder(2)(3)
      ).toEqual(
        5
      );
      /* #@range_end(binding_in_closure_test) */
    });
  });
});

// ## 4.5 <section id='mechanism-of-referential-transparency'>参照透過性の仕組み</section>
describe('参照透過性の仕組み', () => {
  // ### <section id='mechanism-of-immutability'>不変なデータの仕組み</section>
  describe('不変なデータの仕組み', () => {
    // **リスト4.28** 基本型は値としてのデータである
    it('基本型は値としてのデータである', () => {
      /* #@range_begin(basic_type_is_value_type) */
      const n = 1;
      expect(
        n
      ).toEqual(
        1
      );
      const s = "hello";
      expect(
        s
      ).toEqual(
        "hello"
      );
      /* #@range_end(basic_type_is_value_type) */
      expect(
        n
      ).toEqual(
        1
      );
      expect(
        s
      ).toEqual(
        "hello"
      );
    });
  });

  // ### <section id='mechanism-of-assingment'>代入の仕組みと効果</section>
  describe('代入の仕組みと効果', () => {
    // **リスト4.29** 変数への代入
    it('変数への代入', () => {
      /* #@range_begin(assign_to_variable) */
      let age = 29;
      expect(
        age
      ).toEqual(
        29
      );
      /* この時点で誕生日を迎えた */
      age = 30;
      expect(
        age
      ).toEqual(
        30
      );
      /* #@range_end(assign_to_variable) */
    });
  });
});

// [目次に戻る](index.html) [次章に移る](chap05.spec.html)
