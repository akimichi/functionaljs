"use strict";

// 第5章 プログラムをコントロールする仕組み
// ============================

// ## 5.1 <section id='conditional-statements'>条文分岐の種類と特徴</section>
describe('条文分岐の種類と特徴', () => {
  // ### <section id='if-statement'>条文分岐としてのif文</section>
  describe('条件分岐としてのif文', () => {
    // **リスト5.2** 偶数かどうかを判定する
    it('偶数かどうかを判定する', () => {
      const even = (n: number): boolean => {
        if ((n % 2) === 0) { // 2で割った余りが0の場合
          return true;
        } else {            // 2で割った余りが0でない場合
          return false;
        }
      };
      expect(
        even(2)
      ).toEqual(
        true
      );
      expect(
        even(3)
      ).toEqual(
        false
      );
    });

    // **リスト5.3** ネストされたif文
    it("ネストされたif文", () => {
      const compare = (n: number, m: number): number => {
        if (n > m) {     // nがmよりも大きなケース
          return 1;
        } else {
          if (n === m) {  // ネストされたif文
            return 0;
          } else {
            return -1;
          }
        }
      };
      /* テスト */
      /* 3 は 2 よりも大きい */
      expect(
        compare(3, 2)
      ).toEqual(
        1
      );
      /* 2 は 3 よりも小さい */
      expect(
        compare(2, 3)
      ).toEqual(
        -1
      );
      /* 1 と 1 は等しい */
      expect(
        compare(1, 1)
      ).toEqual(
        0
      );
    });

    // **リスト5.4** else if文による3つ以上の条件分岐
    it("else if文による3つ以上の条件分岐", () => {
      const compare = (n: number, m: number): number => {
        if (n > m) {
          return 1;
        } else if (n === m) { // elseにif文を続ける
          return 0;
        } else {
          return -1;
        }
      };
      /* テスト */
      /* 3 は 2 よりも大きい */
      expect(
        compare(3, 2)
      ).toEqual(
        1
      );
      /* 1 と 1 は等しい */
      expect(
        compare(1, 1)
      ).toEqual(
        0
      );
      /* 2 は 3 よりも小さい */
      expect(
        compare(2, 3)
      ).toEqual(
        -1
      );
    });

    // #### if文の問題点
    describe('if文の問題点', () => {
      // **リスト5.7** returnで関数を抜ける
      it('returnで関数を抜ける', () => {
        const even = (n: number): boolean => {
          if ((n % 2) === 0) {
            /* returnでeven関数を抜けてtrueを返す */
            return true;
          } else {
            /* returnでeven関数を抜けてfalseを返す */
            return false;
          }
        };
        expect(even(2)).toBe(true);
      });
    });
  });

  // ### <section id='switch-statement'>条件分岐としてのswitch文</section>
  describe('条件分岐としてのswitch文', () => {
    // #### switch文の問題点
    it("switch文の問題点", () => {
      // **リスト5.10** 可変なデータとのマッチング
      const match_for_mutable = (array: number[]): boolean => {
        switch (array) {
          case [1, 2, 3]: // [1,2,3] とマッチさせたい
            return true;   // マッチすれば、trueを返す
          default:
            return false;  // マッチしなければ、falseを返す
        }
      };
      /* テスト */
      expect(
        match_for_mutable([1, 2, 3])
      ).toEqual(
        false  // case [1,2,3] にはマッチしない
      );
    });
  });

  // #### 代数的データ型とパターンマッチ
  describe('代数的データ型とパターンマッチ', () => {
    // リスト型の定義
    type List<T> = (pattern: ListPattern<T>) => T | List<T> | boolean | null;

    interface ListPattern<T> {
      empty: () => T | List<T> | boolean | null;
      cons: (head: T, tail: List<T>) => T | List<T> | boolean | null;
    }

    // **リスト5.12** 代数的データ構造によるリスト
    it('代数的データ構造によるリスト', () => {
      /* リストの代数的データ型 */
      const empty = <T>(): List<T> => { // 空のリスト
        return (pattern: ListPattern<T>) => {
          return pattern.empty();
        };
      };
      const cons = <T>(value: T, list: List<T>): List<T> => { // 空でないリスト
        return (pattern: ListPattern<T>) => {
          return pattern.cons(value, list);
        };
      };

      // **リスト5.13** 代数的データ構造のmatch関数
      /* 代数的データ型に対してパターンマッチを実現する関数 */
      const match = <T, R>(data: List<T>, pattern: { empty: () => R; cons: (head: T, tail: List<T>) => R }): R => {
        return data(pattern as any) as R;
      };

      // **リスト5.14** リストの関数定義
      /* isEmpty関数は、引数alistに渡されたリストが空のリストかどうかを判定する */
      const isEmpty = <T>(alist: List<T>): boolean => {
        /* match関数で分岐する */
        return match(alist, {
          /* emptyにマッチするケース */
          empty: () => {
            return true;
          },
          /* consにマッチするケース */
          cons: (head: T, tail: List<T>) => {  // headとtailにそれぞれ先頭と後尾が入る
            return false;
          }
        });
      };

      /* head関数は、引数alistに渡されたリストの先頭の要素を返す */
      const head = <T>(alist: List<T>): T | null => {
        return match(alist, {
          /* 空のリストに先頭要素はない */
          empty: () => {
            return null;
          },
          cons: (head: T, tail: List<T>) => {
            return head;
          }
        });
      };

      /* tail関数は、引数alistに渡されたリストの後尾のリストを返す */
      const tail = <T>(alist: List<T>): List<T> | null => {
        return match(alist, {
          /* 空のリストに後尾はない */
          empty: () => {
            return null;
          },
          cons: (head: T, tail: List<T>) => {
            return tail;
          }
        });
      };

      // **リスト5.15** 代数的データ構造のリストの関数のテスト
      /* emptyは空のリストか */
      expect(
        isEmpty(empty<number>())
      ).toEqual(
        true
      );
      /* cons(1,empty())は空のリストか */
      expect(
        isEmpty(cons(1, empty<number>()))
      ).toEqual(
        false
      );
      /* cons(1,empty())の先頭要素は1である */
      expect(
        head(cons(1, empty<number>()))
      ).toEqual(
        1
      );
      /* cons(1,cons(2,empty()))の2番目の要素は2である */
      expect(
        head(tail(cons(1, cons(2, empty<number>())))!)
      ).toEqual(
        2
      );
      expect(
        isEmpty(tail(cons(1, empty<number>()))!)  // [1]の末尾要素は空のリストである
      ).toBe(
        true
      );
    });
  });
});

// ## 5.2 <section id='loop-statements'>反復処理の種類と特徴</section>
describe("反復処理の種類と特徴", () => {
  // **リスト5.16** while文の例
  it("while文の例", () => {
    let counter = 0;         // 変数の初期化
    while (counter < 10) {   // 反復の条件
      counter = counter + 1; // 変数の更新
    }
    /* テスト */
    expect(
      counter
    ).toEqual(
      10
    );
  });

  // **リスト5.17** for文の例
  it("for文の例", () => {
    let counter = 0;
    for (counter = 0; counter < 10; counter += 1) {
      ;
    }
    /* テスト */
    expect(
      counter
    ).toEqual(
      10
    );
  });

  // **リスト5.18** forEachメソッドの例
  it("forEach文によるlength", () => {
    const length = (array: number[]): number => {
      let result = 0;
      array.forEach((element) => {
        result += 1;
      });
      return result;
    };
    /* テスト */
    expect(
      length([1, 2, 3, 4, 5])
    ).toEqual(
      5
    );
  });
});

// ## 5.3 <section id='recursion'>再帰による反復処理</section>
describe('再帰による反復処理', () => {
  // ### 複利法
  describe('複利法の例', () => {
    // **リスト5.19** 複利の計算
    it("複利の計算", () => {
      const compoundInterest = (a: number, r: number, n: number): number => {
        if (n === 0) { // 初年度は利率がつかないので元金がそのまま返る
          return a;
        } else {
          /* compoundInterestの再帰呼び出し */
          return compoundInterest(a, r, n - 1) * (1 + r);
        }
      };
      expect(
        compoundInterest(100000, 0.02, 1)
      ).toEqual(
        102000
      );
      expect(
        compoundInterest(100000, 0.02, 2)
      ).toEqual(
        104040  // 10万円を預けてから2年後には10万4040円が銀行口座に入っている
      );
      expect(
        compoundInterest(100000, 0.02, 25)
      ).toEqual(
        164060.59944647306
      );
    });
  });

  // ### <section id='requirements-of-recursion'>再帰呼び出しの条件</section>
  describe('再帰呼び出しの条件', () => {
    // **リスト5.20** infiniteLoop関数
    const infiniteLoop = (_: any): never => {
      return infiniteLoop(_);
    };

    // **リスト5.21** 再帰によるmap関数
    it('再帰によるmap関数', () => {
      // リスト型の定義
      type List<T> = (pattern: ListPattern<T>) => any;

      interface ListPattern<T> {
        empty: (_?: any) => any;
        cons: (x: T, xs: List<T>) => any;
      }

      /* 第5章で紹介したリスト型 */
      const match = <T, R>(exp: List<T>, pattern: { empty: (_?: any) => R; cons: (x: T, xs: List<T>) => R }): R => {
        return exp.call(pattern, pattern);
      };

      const empty = <T>(_?: any): List<T> => {
        return (pattern: ListPattern<T>) => {
          return pattern.empty(_);
        };
      };

      const cons = <T>(x: T, xs: List<T>): List<T> => {
        return (pattern: ListPattern<T>) => {
          return pattern.cons(x, xs);
        };
      };

      const map = <T, U>(alist: List<T>, transform: (item: T) => U): List<U> => {
        return match(alist, {
          empty: () => { return empty<U>(); },  // 終了条件で再帰を抜ける
          cons: (head: T, tail: List<T>) => {
            return cons(transform(head),
              map(tail, transform)); // map関数の再帰呼び出し
          }
        });
      };

      // **リスト5.22** 再帰によるtoArray関数
      const toArray = <T>(alist: List<T>): T[] => {
        /* 補助関数 toArrayHelper */
        const toArrayHelper = (alist: List<T>, accumulator: T[]): T[] => {
          return match(alist, {
            empty: () => { return accumulator; },  // 空のリストの場合は終了
            cons: (head: T, tail: List<T>) => {
              return toArrayHelper(tail, accumulator.concat(head));
            }
          });
        };
        return toArrayHelper(alist, []);
      };

      const succ = (n: number): number => {
        return n + 1;
      };
      expect(
        toArray(map(cons(1, cons(2, cons(3, empty<number>()))), succ))
      ).toEqual(
        [2, 3, 4]
      );
    });
  });

  // ### <section id='advantages-of-recursion'>再帰処理の利点</section>
  describe('再帰処理の利点', () => {
    // #### 再帰処理と再帰的データ構造
    describe('再帰処理と再帰的データ構造', () => {
      // リスト型の定義
      type List<T> = (pattern: ListPattern<T>) => any;

      interface ListPattern<T> {
        empty: (_?: any) => any;
        cons: (x: T, xs: List<T>) => any;
      }

      const match = <T, R>(exp: List<T>, pattern: { empty: (_?: any) => R; cons: (x: T, xs: List<T>) => R }): R => {
        return exp.call(pattern, pattern);
      };

      const empty = <T>(_?: any): List<T> => {
        return (pattern: ListPattern<T>) => {
          return pattern.empty(_);
        };
      };

      const cons = <T>(x: T, xs: List<T>): List<T> => {
        return (pattern: ListPattern<T>) => {
          return pattern.cons(x, xs);
        };
      };

      const isEmpty = <T>(list: List<T>): boolean => {
        return match(list, {
          empty: () => {
            return true;
          },
          cons: (head: T, tail: List<T>) => {
            return false;
          }
        });
      };

      const head = <T>(list: List<T>): T | null => {
        return match(list, {
          empty: () => {
            return null;
          },
          cons: (head: T, tail: List<T>) => {
            return head;
          }
        });
      };

      const tail = <T>(list: List<T>): List<T> | null => {
        return match(list, {
          empty: () => {
            return null;
          },
          cons: (head: T, tail: List<T>) => {
            return tail;
          }
        });
      };

      describe('再帰的データ構造としてのリスト', () => {
        // **リスト5.25** 再帰によるlength関数
        it('再帰によるlength関数', () => {
          const length = <T>(list: List<T>): number => {
            return match(list, {
              /* emptyの場合は、終了条件となる */
              empty: () => {
                return 0;
              },
              /* consの場合は、length関数を再帰的に呼び出す */
              cons: (head: T, tail: List<T>) => {
                return 1 + length(tail);
              }
            });
          };
          /************************ テスト ************************/
          expect(
            length(cons(1, cons(2, cons(3, empty<number>())))) // [1,2,3]の長さは 3
          ).toEqual(
            3
          );
        });

        // **リスト5.26** 再帰によるappend関数
        it('再帰によるappend関数', () => {
          const toArray = <T>(seq: List<T>): T[] => {
            const toArrayAux = (seq: List<T>, accumulator: T[]): T[] => {
              return match(seq, {
                empty: () => {
                  return accumulator;
                },
                cons: (head: T, tail: List<T>) => {
                  return toArrayAux(tail, accumulator.concat(head));
                }
              });
            };
            return toArrayAux(seq, []);
          };

          /* append :: (LIST[T], LIST[T]) -> LIST[T] */
          const append = <T>(xs: List<T>, ys: List<T>): List<T> => {
            return match(xs, {
              /* emptyの場合は、終了条件 */
              empty: () => {
                return ys; // xsが空の場合は、ysを返す
              },
              /* consの場合は、append関数を再帰的に呼び出す */
              cons: (head: T, tail: List<T>) => {
                /* xsとysを連結させる */
                return cons(head, append(tail, ys));
              }
            });
          };

          const xs = cons(1, cons(2, empty<number>()));
          const ys = cons(3, cons(4, empty<number>()));
          expect(
            toArray(append(xs, ys)) // toArray関数でリストを配列に変換する
          ).toEqual(
            [1, 2, 3, 4]
          );
        });

        // **リスト5.27** 再帰によるreverse関数
        it('再帰によるreverse関数', () => {
          const reverse = <T>(list: List<T>): List<T> => {
            const reverseHelper = (list: List<T>, accumulator: List<T>): List<T> => {
              return match(list, {
                empty: () => {  // emptyの場合は、終了条件
                  return accumulator;
                },
                cons: (head: T, tail: List<T>) => { // consの場合は、reverse関数を再帰的に呼び出す
                  return reverseHelper(tail, cons(head, accumulator));
                }
              });
            };
            return reverseHelper(list, empty<T>());
          };
        });
      });

      // #### 再帰的データ構造としての数式
      describe('再帰的データ構造としての数式', () => {
        // 数式の型定義
        type Exp = (pattern: ExpPattern) => number;

        interface ExpPattern {
          num: (n: number) => number;
          add: (exp1: Exp, exp2: Exp) => number;
          mul: (exp1: Exp, exp2: Exp) => number;
        }

        // **リスト5.28** 代数的データ構造による数式
        const num = (n: number): Exp => {
          return (pattern: ExpPattern) => {
            return pattern.num(n);
          };
        };

        const add = (exp1: Exp, exp2: Exp): Exp => {
          return (pattern: ExpPattern) => {
            return pattern.add(exp1, exp2);
          };
        };

        const mul = (exp1: Exp, exp2: Exp): Exp => {
          return (pattern: ExpPattern) => {
            return pattern.mul(exp1, exp2);
          };
        };

        const match = (exp: Exp, pattern: ExpPattern): number => {
          return exp(pattern);
        };

        // **リスト5.30** 数式を再帰的に計算する
        const calculate = (exp: Exp): number => {
          return match(exp, { // パターンマッチを実行する
            num: (n: number) => {
              return n;
            },
            add: (expL: Exp, expR: Exp) => {
              /* calculateを再帰的に呼び出して足し算を実行する */
              return calculate(expL) + calculate(expR);
            },
            mul: (expL: Exp, expR: Exp) => {
              /* calculateを再帰的に呼び出してかけ算を実行する */
              return calculate(expL) * calculate(expR);
            }
          });
        };

        /**** テスト ****/
        /* 1 + (2 * 3) を計算する */
        const expression = add(num(1),
          mul(num(2),
            num(3)));
        expect(
          calculate(expression)
        ).toEqual(
          7
        );
      });
    });
  });

  // #### 再帰処理と帰納法
  describe('再帰処理と帰納法', () => {
    // リスト型の定義
    type List<T> = (pattern: ListPattern<T>) => any;

    interface ListPattern<T> {
      empty: (_?: any) => any;
      cons: (head: T, tail: List<T>) => any;
    }

    const match = <T, R>(data: List<T>, pattern: { empty: (_?: any) => R; cons: (head: T, tail: List<T>) => R }): R => {
      return data.call(pattern, pattern);
    };

    const cons = <T>(head: T, tail: List<T>): List<T> => {
      return (pattern: ListPattern<T>) => {
        return pattern.cons(head, tail);
      };
    };

    const length = <T>(list: List<T>): number => {
      return match(list, {
        empty: () => {    // リストが空のときが終了条件となる
          return 0;
        },
        cons: (head: T, tail: List<T>) => {
          return 1 + length(tail);
        }
      });
    };

    const append = <T>(xs: List<T>, ys: List<T>): List<T> => {
      return match(xs, {
        empty: () => {
          return ys;
        },
        cons: (head: T, tail: List<T>) => {
          return cons(head, append(tail, ys));
        }
      });
    };

    // 命題P  length(append(xs, ys)) === length(xs) + length(ys)
    it('リストの長さに関する命題P', () => {
      const empty = <T>(_?: any): List<T> => {
        return (pattern: ListPattern<T>) => {
          return pattern.empty(_);
        };
      };

      // **リスト5.36** 命題Pの単体テスト
      const xs = cons(1, cons(2, empty<number>()));
      const ys = cons(3, cons(4, empty<number>()));
      expect(
        length(append(xs, ys))  // 命題Pの左辺
      ).toEqual(
        length(xs) + length(ys) // 命題Pの右辺
      );
    });
  });
});

// [目次に戻る](index.html) [次章に移る](chap06.spec.html)
