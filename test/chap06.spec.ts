"use strict";

// 第6章 関数を利用する
// ========

import * as fs from 'fs';

// ## 6.1 <section id='function-basics'>関数の基本</section>
describe('関数の基本', () => {
  // ### <section id='function-definition'>関数を定義する</section>
  describe('関数を定義する', () => {
    // **リスト6.1** 恒等関数
    it('恒等関数', () => {
      /* #@range_begin(identity_function_definition) */
      const identity = <T>(any: T): T => {
        return any;
      };
      /* #@range_end(identity_function_definition) */
        /* #@range_begin(multiply_lazy_evaluation_test) */
      expect(
        identity(1)
      ).toEqual(
        1
      );
      expect(
        identity("a")
      ).toEqual(
        "a"
      );
        /* #@range_end(multiply_lazy_evaluation_test) */
    });

    // **リスト6.2** succ関数
    it('succ関数', () => {
      /* #@range_begin(succ_function_definition) */
      const succ = (n: number): number => {
        return n + 1;
      };
      /* #@range_end(succ_function_definition) */
      /* テスト */
          /* #@range_begin(infinite_ones_test) */
      expect(
        succ(0)  // 0 を引数にsucc関数を適用する
      ).toEqual(
        1
      );
      expect(
        succ(1)  // 数値1にsucc関数を適用する
      ).toEqual(
        2
      );
          /* #@range_end(infinite_ones_test) */
    /* #@range_begin(log_destroys_referential_transparency) */
      expect(
        succ("abc" as any)
      ).toEqual(
        "abc1"
      );
    /* #@range_end(log_destroys_referential_transparency) */
    });

    // **リスト6.3** add関数
    it('add関数', () => {
      /* #@range_begin(add_function_definition) */
      /* add:: (NUM, NUM) => NUM */
      const add = (n: number, m: number): number => {
        return n + m;
      };
      /* #@range_end(add_function_definition) */
      /* テスト */
      expect(
        add(0, 1)
      ).toEqual(
        1
      );
    });

    // **リスト6.5** 関数の変数へのバインド
    it('関数の変数へのバインド', () => {
      /* #@range_begin(function_bound_to_variable) */
      const succ = (x: number): number => {
        return x + 1;
      };
      /* #@range_end(function_bound_to_variable) */
      expect(succ(0)).toBe(1);
    });

    it('引数を参照しない関数', () => {
      // **リスト6.6** 定数関数
      /* #@range_begin(constant_one_function) */
      const alwaysOne = (x: any): number => {
        return 1;
      };
      /* #@range_end(constant_one_function) */
      expect(
        alwaysOne(1)
      ).toEqual(
        1
      );
      expect(
        alwaysOne("a")
      ).toEqual(
        1
      );

      // **リスト6.7** left関数
      /* #@range_begin(left_function) */
      const left = <T, U>(x: T, y: U): T => {
        return x;
      };
      /* #@range_end(left_function) */
      expect(
        left(1, 2)
      ).toEqual(
        1
      );
    });
  });

  // ### <section id='function-application'>関数を適用する</section>
  describe('関数を適用する', () => {
    // **リスト6.8** succ関数のテスト
    it('succ関数のテスト', () => {
      /* #@range_begin(succ_function_test) */
      const succ = (n: number): number => { // nは仮引数
        return n + 1;
      };
      expect(
        succ(1)  // 数値1にsucc関数を適用する
      ).toEqual(
        2
      );
      /* #@range_end(succ_function_test) */
    });

    // #### 関数の評価戦略
    describe('関数の評価戦略', () => {
      it('add(succ(0), succ(2))の簡約', () => {
        const add = (n: number, m: number): number => {
          return n + m;
        };
        const succ = (n: number): number => {
          return n + 1;
        };
        expect(
          add(succ(0), succ(2))
        ).toEqual(
          4
        );
      });

      // **リスト6.9** JavaScriptにおける正格評価
      it('JavaScriptにおける正格評価', () => {
        /* #@range_begin(strict_evaluation_in_javascript) */
        const left = <T, U>(x: T, y: U): T => {
          return x;
        };
        const infiniteLoop = (_: any): never => {
          return infiniteLoop(_);
        };
        /* このテストは無限ループになるのでコメントアウトしている
         expect(
           left(1, infiniteLoop())
         ).toEqual(
           1
         )
         */
        /* #@range_end(strict_evaluation_in_javascript) */
      });

      // **リスト6.10** 条件文と遅延評価
      it('条件文と遅延評価', () => {
        /* #@range_begin(conditional_is_nonstrict) */
        const infiniteLoop = (): never => {
          return infiniteLoop();
        };

        const conditional = (n: number): boolean => {
          if (n === 1) {
            return true;
          } else {
            /* 条件文が真の場合には評価されない */
            return infiniteLoop();
          }
        };
        expect(
          conditional(1)
        ).toEqual(
          true // 無限ループに陥ることなく計算に成功する
        );
        /* #@range_end(conditional_is_nonstrict) */
      });

      it('乗算の遅延評価', () => {
        const infiniteLoop = (): never => {
          return infiniteLoop();
        };

        // **リスト6.11** 遅延評価で定義したmultiply関数
        /* #@range_begin(multiply_lazy_evaluation) */
        const lazyMultiply = (funX: () => number, funY: () => number): number => {
          const x = funX();

          if (x === 0) {
            return 0;          // xが0ならば、funYは評価しない
          } else {
            return x * funY(); // ここで初めてfunYを評価する
          }
        };
        /* #@range_end(multiply_lazy_evaluation) */

        // **リスト6.12** 遅延評価で定義したmultiply関数のテスト
          /* #@range_begin(infinite_integer_test) */
        expect(
          lazyMultiply(() => {    // 値を関数でラッピングする
            return 0;
          }, () => {
            return infiniteLoop(); // ここが評価されると無限ループに陥る
          })
        ).toEqual(
          0
        );
          /* #@range_end(infinite_integer_test) */
      });
    });

    // #### サンクで無限を表現する
    describe('サンクで無限を表現する', () => {
      // ストリームの型定義
      type Stream<T> = (pattern: StreamPattern<T>) => any;

      interface StreamPattern<T> {
        empty: () => any;
        cons: (head: T, tailThunk: () => Stream<T>) => any;
      }

      // **リスト6.14** サンクによるストリーム型の定義
      /* #@range_begin(stream_with_thunk) */
      const stream = {
        match: <T, R>(data: Stream<T>, pattern: { empty: () => R; cons: (head: T, tailThunk: () => Stream<T>) => R }): R => {
          return data(pattern as any);
        },
        empty: <T>(): Stream<T> => {
          return (pattern: StreamPattern<T>) => {
            return pattern.empty();
          };
        },
        cons: <T>(head: T, tailThunk: () => Stream<T>): Stream<T> => {
          return (pattern: StreamPattern<T>) => {
            return pattern.cons(head, tailThunk);
          };
        },
        /* head:: STREAM[T] => T */
        head: <T>(astream: Stream<T>): T | null => {
          return stream.match(astream, {
            empty: () => { return null; },
            cons: (value: T, tailThunk: () => Stream<T>) => { return value; }
          });
        },
        /* tail:: STREAM[T] => STREAM[T] */
        tail: <T>(astream: Stream<T>): Stream<T> | null => {
          return stream.match(astream, {
            empty: () => { return null; },
            cons: (head: T, tailThunk: () => Stream<T>) => {
              return tailThunk(); // ここで初めてサンクを評価する
            }
          });
        }
      };
      /* #@range_end(stream_with_thunk) */

      // **リスト6.16** ストリーム型のテスト
      it("ストリーム型のテスト", () => {
        /* #@range_begin(stream_with_thunk_test) */
        const theStream = stream.cons(1, () => { // 第2引数にサンクを渡す
          return stream.cons(2, () => {         // 第2引数にサンクを渡す
            return stream.empty<number>();
          });
        });
        expect(
          stream.head(theStream)  // ストリームの先頭要素を取り出す
        ).toEqual(
          1
        );
        /* #@range_end(stream_with_thunk_test) */
      });

      describe("無限ストリームを作る", () => {
        // リスト型の定義
        type List<T> = (pattern: ListPattern<T>) => any;

        interface ListPattern<T> {
          empty: () => any;
          cons: (value: T, alist: List<T>) => any;
        }

        const match = <T, R>(data: Stream<T>, pattern: { empty: () => R; cons: (head: T, tailThunk: () => Stream<T>) => R }): R => {
          return data(pattern as any);
        };

        const streamInternal = {
          empty: <T>(): Stream<T> => {
            return (pattern: StreamPattern<T>) => {
              return pattern.empty();
            };
          },
          cons: <T>(head: T, tailThunk: () => Stream<T>): Stream<T> => {
            return (pattern: StreamPattern<T>) => {
              return pattern.cons(head, tailThunk);
            };
          },
          head: <T>(astream: Stream<T>): T | null => {
            return match(astream, {
              empty: () => { return null; },
              cons: (value: T, tailThunk: () => Stream<T>) => { return value; }
            });
          },
          tail: <T>(astream: Stream<T>): Stream<T> | null => {
            return match(astream, {
              empty: () => { return null; },
              cons: (head: T, tailThunk: () => Stream<T>) => {
                return tailThunk();  // ここで初めてサンクを評価する
              }
            });
          }
        };

        it("無限の整数列を作る", () => {
          // **リスト6.17** 無限に1が続く数列
          /* #@range_begin(infinite_ones) */
          /* ones = 1,1,1,1,... */
          const ones: Stream<number> = streamInternal.cons(1, () => {
            return ones; // onesを再帰的に呼び出す
          });
          /* #@range_end(infinite_ones) */
          expect(
            streamInternal.head(ones) // 最初の要素を取りだす
          ).toEqual(
            1
          );
          expect(
            streamInternal.head(streamInternal.tail(ones)!)  // 2番目の要素を取りだす
          ).toEqual(
            1
          );

          // **リスト6.19** 無限に連続する整数列を生成するenumFrom関数
          /* #@range_begin(infinite_integer) */
          const enumFrom = (n: number): Stream<number> => {
            return streamInternal.cons(n, () => {
              return enumFrom(n + 1);
            });
          };
          /* #@range_end(infinite_integer) */
          expect(
            streamInternal.head(enumFrom(1)) // 最初の要素を取りだす
          ).toEqual(
            1
          );
          expect(
            streamInternal.head(streamInternal.tail(enumFrom(1))!)  // 2番目の要素を取りだす
          ).toEqual(
            2
          );
        });

        it("無限の整数列をテストする", () => {
          const enumFrom = (n: number): Stream<number> => {
            return streamInternal.cons(n, () => {
              return enumFrom(n + 1);
            });
          };

          /* take関数を定義するため、streamモジュールを再掲する */
          const list = {
            match: <T, R>(data: List<T>, pattern: { empty: () => R; cons: (value: T, alist: List<T>) => R }): R => {
              return data(pattern as any);
            },
            empty: <T>(): List<T> => {
              return (pattern: ListPattern<T>) => {
                return pattern.empty();
              };
            },
            cons: <T>(value: T, alist: List<T>): List<T> => {
              return (pattern: ListPattern<T>) => {
                return pattern.cons(value, alist);
              };
            },
            /* #@range_begin(list_toArray) */
            toArray: <T>(alist: List<T>): T[] => {
              const toArrayHelper = (alist: List<T>, accumulator: T[]): T[] => {
                return list.match(alist, {
                  empty: () => {
                    return accumulator;
                  },
                  cons: (head: T, tail: List<T>) => {
                    return toArrayHelper(tail, accumulator.concat(head));
                  }
                });
              };
              return toArrayHelper(alist, []);
            }
            /* #@range_end(list_toArray) */
          };

          const streamWithTake = {
            ...streamInternal,
            // **リスト6.21** ストリームのtake関数
            /* #@range_begin(stream_take) */
            /* take:: (STREAM[T], NUM) => LIST[T] */
            take: <T>(astream: Stream<T>, n: number): List<T> => {
              return match(astream, {
                empty: () => {              // ストリームが空のケース
                  return list.empty<T>();
                },
                cons: (head: T, tailThunk: () => Stream<T>) => {  // ストリームが空でないケース
                  if (n === 0) {
                    return list.empty<T>();
                  } else {
                    return list.cons(head,   // リストを生成する
                      streamWithTake.take(tailThunk(), (n - 1)));
                  }
                }
              });
            }
            /* #@range_end(stream_take) */
          };

          expect(
            streamWithTake.head(enumFrom(1))
          ).toEqual(
            1
          );
          expect(
            streamWithTake.head(streamWithTake.tail(enumFrom(1))!)
          ).toEqual(
            2
          );

          // **リスト6.23** 無限の整数列をテストする
          /* #@range_begin(stream_filter_test) */
          expect(
            list.toArray( // ストリームを配列に変換する
              streamWithTake.take(enumFrom(1), 4) // 無限の整数列から4個の要素を取り出す
            )
          ).toEqual(
            [1, 2, 3, 4]
          );
          /* #@range_end(stream_filter_test) */

    /* #@range_begin(succ_has_referential_transparency) */
          expect(
            /* 無限の整数列から最初の4つの要素を取り出し、それを配列に変換する */
            list.toArray(streamWithTake.take(enumFrom(1), 4))
          ).toEqual(
            [1, 2, 3, 4]
          );
    /* #@range_end(succ_has_referential_transparency) */
        }, 3000);
      });
    }); // サンクで無限を表現する
  }); // 関数の適用
}); // 関数の基本

// ## 6.2 <section id='function-and-referential-transparency'>関数と参照透過性</section>
describe('関数と参照透過性', () => {
  // ### <section id='purity-of-function'>関数の純粋性</section>
  // **リスト6.25** succ関数は参照透過性を持つ
  it('succ関数は参照透過性を持つ', () => {
    const succ = (n: number): number => {
      return n + 1;
    };
    expect(
      succ(1)
    ).toEqual(
      succ(1)
    );
  });

  // **リスト6.26** ファイル操作は参照透過性を破壊する
  it('ファイル操作は参照透過性を破壊する', () => {
    /* #@range_begin(fileio_destroys_referential_transparency) */
    /* テストの実行前にあらかじめ "This is a test."
       という文字列をファイルに書き込んでおく */
    fs.writeFileSync('test/resources/file.txt', "This is a test.");

    /* 第1回目のファイルの読み込み */
    const text = fs.readFileSync("test/resources/file.txt", 'utf8');
    expect(
      fs.readFileSync("test/resources/file.txt", 'utf8')
    ).toEqual(
      "This is a test."
    );
    /* 途中でのファイルへの書き込み */
    fs.writeFileSync('test/resources/file.txt', "This is another test.");

    /* 第2回目のファイルの読み込み */
    expect(
      fs.readFileSync("test/resources/file.txt", 'utf8')
    ).toEqual(/* 最初の readFileSync関数の結果と異なっている */
      "This is another test."
    );
    /* #@range_end(fileio_destroys_referential_transparency) */
  });

  it('画面出力が参照透過性を損なうこと', () => {
    expect(
      console.log("this is a test")
    ).toEqual(
      console.log("this is anoter test")
    );
  });

  // ### <section id='coping-sideeffect'>副作用への対処</section>
  describe('副作用への対処', () => {
    describe('tap関数', () => {
      // **リスト6.27** tap関数
      /* #@range_begin(tap_combinator) */
      const tap = <T>(target: T, sideEffect: (t: T) => void): T => {
        sideEffect(target); // 副作用を実行する
        return target;
      };
      /* #@range_end(tap_combinator) */

      // **リスト6.28** tap関数によるconsole.logのテスト
      it('tap関数によるconsole.logのテスト', () => {
        const succ = (n: number): number => {
          return n + 1;
        };
        /* #@range_begin(tap_combinator_test_in_console) */
        /* 画面出力という副作用を実行する関数 */
        const consoleSideEffect = (any: any): void => {
          console.log(any);
        };
        expect(
          tap(succ(1), consoleSideEffect)
        ).toEqual(
          tap(succ(1), consoleSideEffect)
        );
        /* #@range_end(tap_combinator_test_in_console) */
      });

      // **リスト6.29** tap関数によるファイル入出力のテスト
      it('tap関数によるファイル入出力のテスト', () => {
        /* #@range_begin(tap_combinator_test_in_fileio) */
        /* あらかじめ文字列をファイルに書き込んでおく */
        fs.writeFileSync('test/resources/file.txt', "This is a test.");

        /* ファイルからの読み込みという副作用を実行する */
        const IOSideEffect = (_: any): string => {
          const content = fs.readFileSync("test/resources/file.txt", 'utf8');
          fs.writeFileSync('test/resources/file.txt', "This is another test.");
          return content;
        };

        expect(
          tap(fs.readFileSync("test/resources/file.txt", 'utf8'),
            IOSideEffect)
        ).not.toEqual( // 同じ引数に適用しているのに両者は等しくない
          tap(fs.readFileSync("test/resources/file.txt", 'utf8'),
            IOSideEffect)
        );
        /* #@range_end(tap_combinator_test_in_fileio) */
      });
    });
  });
});

// [目次に戻る](index.html) [次章に移る](chap07.spec.html)
