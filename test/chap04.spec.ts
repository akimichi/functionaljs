"use strict";

// 第4章 データの種類と特徴
// ========

// ## 4.1 <section id='what-is-type'>型とは何か</section>
describe('型とは何か', () => {
  // **リスト4.1** 自然数の作り方
  it('自然数の作り方', () => {
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
  });
});

// ## 4.2 <section id='basic-type'>基本型</section>
describe('基本型', () => {
  // **リスト4.2** 未定義の変数
  it('未定義の変数', () => {
    let variable; // 宣言されているが値と結びついていない変数
    expect(
      variable
    ).toEqual(
      undefined
    );
  });

  // ### 基本型は不変なデータである
  describe('基本型は不変なデータである', () => {
    it('真理値型は不変である', () => {
      const truth = true;
      expect(
        true === true
      ).toEqual(
        true
      );
    });

    it('数値型は不変である', () => {
      expect(
        1 === 1
      ).toEqual(
        true
      );
    });

    it('文字列は不変である', () => {
      const str = "to be, or not to be";
      expect(
        str
      ).toEqual(
        "to be, or not to be"
      );
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
        const addressbook = {
          No1: "Alan Turing",
          No2: "Haskell Curry",
          No3: "Alonzo Church",
          No4: "Ada Lovelace"
        };
      });

      // **リスト4.5** オブジェクト型の入れ子
      it('オブジェクト型の入れ子', () => {
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
        const addressbook_nested = {
          No1: {
            name: "Alan Turing",
            gender: "male",
            birthDay: {
              year: 1912,
              month: 6,
              day: 23
            }
          }
        };
        // **リスト4.6** オブジェクト型インスタンスへのアクセス
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
      });
    });
  });

  // ### <section id='array-type'>配列型</section>
  describe('配列型', () => {
    // **リスト4.7** 配列の基本操作
    it("配列の基本操作", () => {
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
    });

    // **リスト4.8** sortによる配列要素の並べかえ
    it("sortによる配列要素の並べかえ", () => {
      const array = [5, 3, 4, 1, 2];
      expect(
        array.sort((n, m) => {
          return n - m;
        })
      ).toEqual(
        [1, 2, 3, 4, 5]
      );
    });

    // **リスト4.9** 名簿の配列型表現
    it("名簿の配列型表現", () => {
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
      // **リスト4.10** 名簿の並べかえ
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
    });
  });

  // ### <section id='function-type'>関数型</section>
  describe('関数型', () => {
    // **リスト4.11** 関数はオブジェクト型である
    it('関数はオブジェクト型である', () => {
      const func = (n: number) => {
        return n;
      };
      /* lengthプロパティにアクセスするとアリティ（引数の数）が返る */
      expect(
        func.length
      ).toEqual(
        1
      );
    });

    // **リスト4.12** 引数のない関数
    it('引数のない関数', () => {
      const three = () => {
        return 3;
      };
      expect(
        three()
      ).toEqual(
        3
      );
    });

    // **リスト4.13** 関数と変数の類似
    it('関数と変数の類似', () => {
      const three = 3;
      expect(
        three
      ).toEqual(
        3
      );
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
      expect(
        head(tail(cons(1, cons(2, empty()))))
      ).toEqual(
        2
      );
    });
  });

  // ### <section id='mutability-of-composite-type'>合成型の可変性</section>
  describe('合成型の可変性', () => {
    // **リスト4.17** 配列型の可変性
    it('配列型の可変性', () => {
      const array = [0, 1, 2, 3];
      array[0] = 7; // 配列の一部を書きかえている
      expect(
        array
      ).not.toEqual(
        [0, 1, 2, 3] // [7,1,2,3]に変更されている
      );
    });

    // **リスト4.18** 配列の破壊的メソッド
    it('配列の破壊的メソッド', () => {
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
    });

    it('非破壊的なreverse関数', () => {
      // **リスト4.19** 非破壊的なreverse関数
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
      expect(
        reverse(array)
      ).toEqual(
        [5, 4, 3, 2, 1]
      );
      // **リスト4.20** 非破壊的なreverse関数は完全には不変でない
      const reversed = reverse(array);
      reversed[0] = 0;
      expect(
        reversed
      ).toEqual(
        [0, 4, 3, 2, 1]
      );
    });
  });
});

// ## 4.4 <section id='variable-and-data'>変数とデータの関係</section>
describe('変数とデータの関係', () => {
  // ### <section id='variable-binding'>変数のバインド</section>
  it('変数のバインド', () => {
    // **リスト4.21** バインド変数と自由変数
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

  // **リスト4.22** 関数本体でのバインド変数
  it('関数本体でのバインド変数', () => {
    const add = (x: number, y: number) => { // xとyは引数
      return x + y; // それゆえに、xもyもバインド変数
    };
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
      const addWithFreeVariable = (x: number) => {
        return x + (undefined as any).y;  // xはバインド変数だが、yは自由変数
      };
      // 関数本体での自由変数のテスト
      expect(
        (_: any) => {
          return addWithFreeVariable(1);
        }
      ).toThrow();
    });
  });

  // ### <section id='variable-scope'>変数のスコープ</section>
  describe('変数のスコープ', () => {
    // **リスト4.25** 関数とローカルスコープ
    it('関数とローカルスコープ', () => {
      const createScope = (_?: any) => { // ローカルスコープを作る
        const innerScope = "inner";
        return innerScope; // 変数innerScopeはcreateScopeのなかでのみ有効
      };
      expect(
        (_: any) => {
          (undefined as any).innerScope // ローカルスコープにある変数innerScopeにアクセスを試みる
        }
      ).toThrow();
      expect(
        createScope()
      ).toBe(
        "inner"
      );
    });

    // **リスト4.26** 入れ子になった関数の変数バインド
    it('入れ子になった関数の変数バインド', () => {
      const adder = (y: number) => { // 外側の関数
        const addWithFreeVariable = (x: number) => { // 内側の関数
          return x + y; // 変数yはadder関数の引数yを参照できる
        };
        return addWithFreeVariable;
      };
      // **リスト4.27** 入れ子になった関数の適用
      expect(
        adder(2)(3)
      ).toEqual(
        5
      );
    });
  });
});

// ## 4.5 <section id='mechanism-of-referential-transparency'>参照透過性の仕組み</section>
describe('参照透過性の仕組み', () => {
  // ### <section id='mechanism-of-immutability'>不変なデータの仕組み</section>
  describe('不変なデータの仕組み', () => {
    // **リスト4.28** 基本型は値としてのデータである
    it('基本型は値としてのデータである', () => {
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
    });
  });
});

// [目次に戻る](index.html) [次章に移る](chap05.spec.html)
