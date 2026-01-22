"use strict";

// 第2章 なぜ関数型プログラミングが重要か
// ========

import * as fs from 'fs';

// ## 2.2 <section id='features-of-functional-programming'>関数型プログラミングの特徴</section>
describe('関数型プログラミングの特徴', () => {
  // ### <section id='first-class-object'>ファーストクラスオブジェクトとしての関数</section>
  describe('ファーストクラスオブジェクトとしての関数', () => {
    it('数値はファーストクラスオブジェクトである', () => {
      /*  #@range_begin(number_as_first_class_citizen) */
      // 値を変数にバインドする
      const zero = 0;
      const name = "Haskell Curry";
      // 値をデータ構造に埋めこむ
      const birthday = {
        year: 1999,
        month: 1,
        day: 12
      };
      // 関数から値を返す
      const birthYear = (birthdayObject: { year: number }) => {
        return birthdayObject.year;
      };
      /* #@range_end(number_as_first_class_citizen) */
      // 値を関数に渡す
        /* #@range_begin(product_in_array_reduce_test) */
      expect(
        Math.sqrt(2)
      ).toEqual(
        // 関数から値を返す
        1.4142135623730951
      );
        /* #@range_end(product_in_array_reduce_test)  */
    });

    it('関数は変数にバインドできる', () => {
      /* #@range_begin(function_bound_to_variable) */
      const succ = (n: number) => {
        return n + 1;
      };
      /* #@range_end(function_bound_to_variable) */
      /* #@range_begin(expect_example) */
      expect(
        succ(1) // 変数succを用いてλ式を呼びだす
      ).toEqual(
        2
      );
      /* #@range_end(expect_example) */
    });

    it('関数をオブジェクトに埋めこむ', () => {
      /* #@range_begin(function_embedded_in_object) */
      const math = {
        add: (n: number, m: number) => {
          return n + m;
        }
      };
      expect(
        math.add(1, 2)
      ).toEqual(
        3
      );
      /* #@range_end(function_embedded_in_object) */
    });

    // **リスト 2.1** forEach文によるsumの定義
    it('forEach文によるsumの定義', () => {
      /* #@range_begin(sum_forEach) */
      const sum = (array: number[]) => {
        let result = 0;
        array.forEach((item) => { // forEachに関数を渡す
          result = result + item;
        });
        return result;
      };
      /* #@range_end(sum_forEach)  */
      expect(
        sum([1, 2, 3, 4])
      ).toEqual(
        10
      );
    });

    describe('関数を返す', () => {
      // adderを定義する
      it('adderを定義する', () => {
        /* #@range_begin(adder_definition) */
        const adder = (n: number) => {
          return (m: number) => { // 関数を返す
            return n + m;
          };
        };
        /* #@range_end(adder_definition) */
        const succ = adder(1);
        expect(
          succ(0)
        ).toEqual(
          1
        );
      });
    });
  });

  // ### <section id='referential-transparency'>参照透過性</section>
  describe('参照透過性', () => {
    // #### 参照透過性が成立する場面
    describe('参照透過性が成立する場面', () => {
      // 値の参照透過性
      it('値の参照透過性', () => {
        expect(
          2 === 2
        ).toEqual(
          true
        );
      });

      // 変数の参照透過性
      describe('変数の参照透明性', () => {
        it('変数が参照透明性を持つ場合', () => {
          const x = 1;
          expect(
            x
          ).toEqual(
            x
          );
        });

        it('代入は変数の参照透明性を破壊する', () => {
          let foo = 2;
          const bar = foo;
          foo = 3;
          expect(
            foo
          ).not.toEqual(
            bar
          );
        });
      });

      // 関数の参照透過性
      describe('関数の参照透明性', () => {
        // succ関数は参照透明性を持つ
        it('succ関数は参照透明性を持つ', () => {
          const succ = (x: number) => {
            return x + 1;
          };
          expect(
            succ(1) === succ(1)
          ).toEqual(
            true
          );
          expect(
            succ(1) // 1回目の実行
          ).toEqual(
              /* #@range_begin(strict_evaluation_result) */
            2
          );
          expect(
            succ(1) // 2回目の実行
          ).toEqual(
            2
              /* #@range_end(strict_evaluation_result) */
          );
        });
      });
    });

    // #### 参照透過性を破壊するもの
    describe('参照透過性を破壊するもの', () => {
      it('可変なデータは参照透明性を破壊する', () => {
        const array = [1];
        expect(
          array
        ).toEqual(
          [1]
        );
        array.push(2);
        expect(
          array
        ).toEqual(
          [1, 2]
        );
      });

      // **リスト2.2** 代入が参照透明性を破壊する例
      it('代入が参照透明性を破壊する例', () => {
        /* #@range_begin(assignment_breaks_referential_transparency) */
        let x = 0;
        const add = (y: number) => {
          x = x + 1; // 代入で変数を更新する
          return x + y;
        };
        /* #@range_end(assignment_breaks_referential_transparency)  */
        expect(
          add(1)
        ).toEqual(
          2
        );
      });

      // **リスト2.3** 命令的な階乗関数
      it('命令的な階乗関数', () => {
        /* #@range_begin(imperative_factorial) */
        const factorial = (n: number) => {
          /* 変数resultに結果が入る */
          let result = 1;
          /* 変数timesは反復の回数を数える */
          let times = 1;
          /* while文は反復を処理する */
          while (times < n + 1) {
            /* 変数resultを代入で更新する */
            result = result * times;
            /* 変数timesを代入で更新する */
            times = times + 1;
          }
          return result;
        };
        /* #@range_end(imperative_factorial) */
        expect(
          factorial(2)
        ).toEqual(
          2
        );
        expect(
          factorial(3)
        ).toEqual(
          6
        );
        expect(
          factorial(4)
        ).toEqual(
          24
        );
      });

      // 副作用と入出力
      describe('副作用と入出力', () => {
        // Date.now関数は参照透明性を持たない
        it('Date.now関数は参照透明性を持たない', (done) => {
          const onceUponATime = Date.now();
          /* 時間を1秒進める */
          setTimeout(() => {
            expect(
              onceUponATime
            ).not.toEqual( /* 等しくないことをテストしている */
              Date.now()
            );
            done();
          }, 1000);
        });

        // ファイル入出力が参照透明性を破壊する例
        it('ファイル入出力が参照透明性を破壊する例', () => {
          const read = (path: string) => {
            const readValue = fs.readFileSync(path);
            return parseInt(readValue.toString());
          };
          const write = (path: string, n: number) => {
            fs.writeFileSync(path, String(n));
            return n;
          };
          write('test/resources/io.txt', 1);
          expect(
            read('test/resources/io.txt')
          ).toEqual(
            1
          );
          write('test/resources/io.txt', 2); // ここでファイルに値を書きこむ
          expect(
            read('test/resources/io.txt')
          ).toEqual(
            2
          );
        });
      });
    });

    // #### 参照透明性を保証する
    describe('参照透明性を保証する', () => {
      // ##### 値の参照透過性を保証する（可変なデータの排除）
      describe('値の参照透過性を保証する（可変なデータの排除）', () => {
        // ** リスト2.4** 不変なデータ構造としてのオブジェクト型の実装
        it('不変なデータ構造としてのオブジェクト型の実装', () => {
          /* #@range_begin(immutable_datatype) */
          const empty = (_?: any): null => {
            return null;
          };
          const get = (key: string, obj: (k: string) => any): any => {
            return obj(key);
          };
          const set = (key: string, value: any, obj: (k: string) => any) => {
            return (key2: string) => {
              if (key === key2) {
                return value;
              } else {
                return get(key2, obj);
              }
            };
          };
          /* #@range_end(immutable_datatype) */
        });
      });

      // ##### 変数の参照透過性を保証する(代入の排除)
      describe('変数の参照透過性を保証する(代入の排除)', () => {
        // ** リスト2.5** 代入を使った足し算の定義
        it('代入を使った足し算の定義', () => {
          /* #@range_begin(imperative_addition) */
          const add = (x: number, y: number) => {
            let times = 0;
            let result = x;

            /* while文で反復を処理する */
            while (times < y) {
              result = result + 1;
              times = times + 1;
            }
            return result;
          };
          /* #@range_end(imperative_addition) */
          expect(add(2, 1)).toEqual(3);
          expect(add(2, 2)).toEqual(4);
          expect(add(3, 2)).toEqual(5);
        });

        // ** リスト2.6** 関数型プログラミングによる足し算の定義
        it('関数型プログラミングによる足し算の定義', () => {
          /* #@range_begin(functional_addition) */
          const add = (x: number, y: number): number => {
            if (y < 1) {
              return x;
            } else {
              /* 新しい引数でadd関数を再帰的に呼び出す */
              return add(x + 1, y - 1);
            }
          };
          /* #@range_end(functional_addition) */
          expect(add(2, 1)).toEqual(3);
          expect(add(2, 2)).toEqual(4);
          expect(add(3, 2)).toEqual(5);
        });
      });

      // ##### 関数の参照透過性を保証する（副作用の分離）
      describe('関数の参照透過性を保証する（副作用の分離）', () => {
        // **リスト2.7** 副作用が分離されていないコード
        it('副作用が分離されていないコード', () => {
          /* #@range_begin(age_sideeffect) */
          const age = (birthYear: number) => {
            /* todayは現時点の日付データ */
            const today = new Date();
            /* getFullYear関数は日付データにもとづいて現時点の西暦を返す */
            const thisYear = today.getFullYear();
            return thisYear - birthYear;
          };
          /* #@range_end(age_sideeffect) */
        });

        // **リスト2.8** 副作用が分離されているコード
        it('副作用が分離されているコード', () => {
          /* #@range_begin(age_without_sideeffect) */
          const age = (birthYear: number, thisYear: number) => {
            return thisYear - birthYear;
          };
          /* #@range_end(age_without_sideeffect) */
          expect(
            age(1999, (new Date()).getFullYear())
          ).toEqual(
            27
          );
        });
      });
    });
  });
});

// ## 2.3 <section id='advantages-of-functional-programming'>関数型プログラミングの利点</section>
describe('関数型プログラミングの利点', () => {
  /* 下記テストで利用される関数をあらかじめ定義しておく */
  const compose = <T, U, V>(f: (arg: U) => V, g: (arg: T) => U) => {
    return (arg: T): V => {
      return f(g(arg));
    };
  };

  // ### <section id='modularity'>高いモジュール性</section>
  describe('モジュール性とは何か', () => {
    // #### 部品の汎用性
    describe('部品の汎用性', () => {
      // **リスト2.9** sumの定義
      describe('sum関数の定義', () => {
        // ##### forEachメソッドによるsumの定義
        it('forEachメソッドによるsumの定義', () => {
          /* #@range_begin(sum_in_array_while) */
          const sum = (array: number[]) => {
            /* 結果を格納する変数 */
            let result = 0;
            /* 反復した回数を格納する変数 */
            let index = 0;
            while (index < array.length) {
              /* 変数resultを代入で更新する */
              result = result + array[index];
              /* 反復回数を更新する */
              index = index + 1;
            }
            return result;
          };
          /* #@range_end(sum_in_array_while)  */
          expect(
            sum([1, 2, 3, 4])
          ).toEqual(
            10
          );
          expect(
            sum([1, 2, 3, 4])
          ).toEqual(
            10
          );
        });

        // ##### reduceメソッドによるsumの定義
        it('reduceメソッドによるsumの定義', () => {
          /* #@range_begin(sum_in_array_reduce) */
          const sum = (array: number[]) => {
            return array.reduce(/* 第1引数に関数を渡す */
              (accumulator, item) => {
                return accumulator + item; /* 足し算を実行する */
              }, 0); // 第2引数には、蓄積変数の初期値として0を渡す
          };
          /* #@range_end(sum_in_array_reduce)  */
          expect(
            sum([1, 2, 3, 4])
          ).toEqual(
            10
          );
        });
      });

      // **リスト2.10** product関数の定義
      it('product関数の定義', () => {
        /* #@range_begin(product_in_array_reduce) */
        const product = (array: number[]) => {
          return array.reduce((accumulator, item) => {
            return accumulator * item; /* かけ算を実行する */
          }, 1); // 第2引数には、蓄積変数の初期値として1を渡す
        };
        /* #@range_end(product_in_array_reduce)  */
        expect(
          product([1, 2, 3, 4])
        ).toEqual(
          24
        );
      });

      // **リスト2.11** map関数の定義
      it('map関数の定義', () => {
        const succ = (n: number) => {
          return n + 1;
        };
        /* #@range_begin(map_in_array_reduce) */
        const map = <T, U>(transform: (item: T) => U) => {
          return (array: T[]): U[] => {
            return array.reduce((accumulator: U[], item) => {
              return accumulator.concat(transform(item));
            }, []); // 蓄積変数の初期値として空の配列[]を指定する
          };
        };
        /* #@range_end(map_in_array_reduce)  */
        expect(
          map(succ)([1, 2, 3])
        ).toEqual(
          [2, 3, 4]
        );
        expect(
          // **リスト2.12** map関数のテスト
          /* #@range_begin(map_in_array_reduce_test) */
          map(succ)([1, 3, 5])
          /* #@range_end(map_in_array_reduce_test) */
        ).toEqual(
          /* #@range_begin(map_in_array_reduce_test_result) */
          [2, 4, 6]
          /* #@range_end(map_in_array_reduce_test_result) */
        );
      });
    });

    // #### 部品を組み合わせる
    describe('部品を組み合わせる', () => {
      // ##### 関数適用で部品を組み合わせる
      describe('関数適用で部品を組み合わせる', () => {
        const map = <T, U>(transform: (item: T) => U) => {
          return (array: T[]): U[] => {
            return array.reduce((accumulator: U[], item) => {
              return accumulator.concat(transform(item));
            }, []); // 蓄積変数の初期値として空の配列[]を指定する
          };
        };
        const sum = (array: number[]) => {
          return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
            return accumulator + item;                 // 足し算を実行する
          }, 0); // 第2引数には、蓄積変数の初期値として0を渡す
        };

        // **リスト2.13** constant関数
        /* #@range_begin(constant) */
        const constant = <T>(any: T) => {
          return (_?: any): T => {
            return any;
          };
        };
        const alwaysOne = constant(1);
        /* #@range_end(constant) */

        // **リスト2.14** map(alwaysOne)で配列の全要素を1に変える
        /* #@range_begin(map_alwaysOne) */
        expect(
          map(alwaysOne)([1, 2, 3])
        ).toEqual(
          [1, 1, 1]
        );
        /* #@range_end(map_alwaysOne) */

        const flip = <T, U, V>(fun: (x: U) => (y: T) => V) => {
          return (x: T) => {
            return (y: U): V => {
              return fun(y)(x);
            };
          };
        };

        // **リスト2.15** 関数適用によるlength関数の定義
        it('関数適用によるlength関数の定義', () => {
          /* #@range_begin(array_length) */
          const length = (array: number[]) => {
            return sum(map(alwaysOne)(array));
          };
          /* #@range_end(array_length)  */
          expect(
            length([1, 2, 3])
          ).toEqual(
            3
          );
        });

        // ##### 関数合成による処理の合成
        describe('関数合成による処理の合成', () => {
          // **リスト2.16** 関数の合成
          /* #@range_begin(function_compose) */
          const compose = <T, U, V>(f: (arg: U) => V, g: (arg: T) => U) => {
            return (arg: T): V => {
              return f(g(arg));
            };
          };
          /* #@range_end(function_compose)  */

          // **リスト2.17** 関数合成によるlength関数の定義（ポイントフリースタイル）
          it('関数合成によるlength関数の定義（ポイントフリースタイル）', () => {
            /* #@range_begin(array_length_in_composition_with_point_free_style) */
            const length = compose(sum, map(alwaysOne));
            /* #@range_end(array_length_in_composition_with_point_free_style)  */
            expect(
              length([1, 2, 3])
            ).toEqual(
              3
            );
          });

          // **リスト2.18** 関数合成によるlength関数の定義（引数を明示したスタイル）
          it('関数合成によるlength関数の定義（引数を明示したスタイル）', () => {
            /* #@range_begin(array_length_in_composition) */
            const length = (array: number[]) => { // 引数が配列であることを明示する
              return compose(sum, map(alwaysOne))(array);
            };
            /* #@range_end(array_length_in_composition)  */
            expect(
              length([1, 2, 3])
            ).toEqual(
              3
            );
          });
        });
      });

      // ##### 関数による遅延評価
      describe('関数による遅延評価', () => {
        // 評価戦略の種類
        describe('評価戦略の種類', () => {
          const map = <T, U>(transform: (item: T) => U) => {
            return (array: T[]): U[] => {
              return array.reduce((accumulator: U[], item) => {
                return accumulator.concat(transform(item));
              }, []);
            };
          };
          const sum = (array: number[]) => {
            return array.reduce((accumulator, item) => {
              return accumulator + item;
            }, 0);
          };
          const constant = <T>(any: T) => {
            return (_?: any): T => {
              return any;
            };
          };
          const alwaysOne = constant(1);
          const length = compose(sum, map(alwaysOne));

          // **リスト2.19** 正格評価の例
          it('正格評価の例', () => {
            expect(
              /* #@range_begin(strict_evaluation) */
              length([1, 1 + 1])
              /* #@range_end(strict_evaluation) */
            ).toEqual(
              2
            );
          });

          // **リスト2.20** 遅延評価の例
          it('遅延評価の例', () => {
            expect(
              /* #@range_begin(lazy_evaluation) */
              length([1, ((_: any) => {
                return 1 + 1;
              }) as any])
              /* #@range_end(lazy_evaluation) */
            ).toEqual(
              /* #@range_begin(lazy_evaluation_result) */
              2
              /* #@range_end(lazy_evaluation_result) */
            );
          });
        });

        // ##### 遅延評価によるストリーム
        describe('遅延評価によるストリーム', () => {
          type Stream<T> = [T, () => T | Stream<T>];

          // **リスト2.21** ストリームの例
          it('ストリームの例', () => {
            /* #@range_begin(stream_example) */
            const aStream: [number, () => number] = [1, () => { // 後尾は無名関数で表現する
              return 2;
            }];
            /* #@range_end(stream_example) */
            expect(
              aStream[0] // ストリームの先頭要素を取得する
            ).toEqual(
              1
            );
            expect(
              aStream[1]() // 関数適用でストリームの後尾を取り出す
            ).toEqual(
              2
            );
          });

          describe('無限ストリームを作る', () => {
            // **リスト2.22** enumFrom関数
            const succ = (n: number) => {
              return n + 1;
            };

            /* #@range_begin(enumFrom) */
            const enumFrom = (n: number): any => {
              return [n, () => { // ストリームを返す
                return enumFrom(succ(n));
              }];
            };
            /* #@range_end(enumFrom) */

            // **リスト2.23** 無限の偶数列を作る
            it('無限の偶数列を作る', () => {
              /* #@range_begin(evenStream) */
              const evenFrom = (n: number): any => {
                return [n, () => {
                  return evenFrom(n + 2);
                }];
              };
              const evenStream = evenFrom(2);
              /* #@range_end(evenStream) */
              const take = (n: number, astream: any): any => {
                if (n === 1) {
                  return astream[0];
                } else {
                  return [astream[0]].concat(take(n - 1, astream[1]()));
                }
              };
              expect(
                take(3, evenStream)
              ).toEqual(
                [2, 4, 6]
              );
            });

            // **リスト2.24** iterate関数
            /* #@range_begin(stream_iterate) */
            const iterate = (init: number) => {  // 先頭の値を渡す
              return (step: (n: number) => number): any => {       // 次の値との差を計算する関数を渡す
                return [init, () => { // ストリーム型を返す
                  return iterate(step(init))(step);
                }];
              };
            };
            /* #@range_end(stream_iterate) */

            // **リスト2.25** 無限ストリームの例
            it('無限の偶数列', () => {
              const succ = (n: number) => {
                return n + 1;
              };
              /* #@range_begin(enumFrom_by_iterate) */
              const enumFrom = (n: number) => {
                return iterate(n)(succ);
              };
              // 自然数列を定義する
              const naturals = enumFrom(1);
              // 偶数列を定義する
              const twoStep = (n: number) => {
                return n + 2;
              };
              const evenStream = iterate(2)(twoStep);
              /* #@range_end(enumFrom_by_iterate) */
            });

            it('ストリームを加工する', () => {
              // **リスト2.26** ストリームのfilter関数
              /* #@range_begin(stream_filter) */
              const filter = (predicate: (x: number) => boolean) => {
                return (aStream: any): any => {
                  /* ストリームの先頭要素を取り出す */
                  const head = aStream[0];
                  /* 先頭要素が条件に合致する場合 */
                  if (predicate(head) === true) {
                    return [head, () => {
                      return filter(predicate)(aStream[1]());
                    }];
                  } else {
                    /* 先頭要素が条件に合致しない場合 */
                    return filter(predicate)(aStream[1]());
                  }
                };
              };
              /* #@range_end(stream_filter) */

              // **リスト2.27**filter関数で無限の偶数列を作る
              /* #@range_begin(evenStream_by_filter) */
              const even = (n: number) => {
                return (n % 2) === 0;
              };
              const evenStream = filter(even)(enumFrom(1));
              /* #@range_end(evenStream_by_filter) */

              // **リスト2.28** ストリームのelemAt関数
              /* #@range_begin(array_elemAt) */
              const elemAt = (n: number) => {
                return (aStream: any): any => {
                  if (n === 1) {
                    return aStream[0];
                  } else {
                    return elemAt(n - 1)(aStream[1]());
                  }
                };
              };
              /* #@range_end(array_elemAt) */

              expect(
                /* #@range_begin(third_element_of_evenStream) */
                elemAt(1)(evenStream)
              ).toEqual(
                2
              );
              expect(
                elemAt(2)(evenStream)
                /* #@range_end(third_element_of_evenStream) */
              ).toEqual(
                4
              );
              // **リスト2.29** 3番目の偶数を求める
              expect(
                elemAt(3)(evenStream)
              ).toEqual(
                /* #@range_begin(third_element_of_evenStream_result) */
                6
                /* #@range_end(third_element_of_evenStream_result) */
              );
            });

            // **リスト2.30** 配列のelemAt関数
            it('配列のelemAt関数', () => {
              /* #@range_begin(stream_elemAt) */
              const elemAt = (n: number) => {
                return (anArray: number[]): number | undefined => {
                  if (n === 1) {
                    return anArray[0];
                  } else {
                    const tail = anArray.slice(1, anArray.length);
                    return elemAt(n - 1)(tail);
                  }
                };
              };
              /* #@range_end(stream_elemAt) */
              expect(
                // **リスト2.31** 4番目の偶数を求める
                /* #@range_begin(fourth_element_of_evenArray) */
                elemAt(4)([2, 4, 6])
                /* #@range_end(fourth_element_of_evenArray) */
              ).toEqual(
                /* #@range_begin(fourth_element_of_evenArray_result) */
                undefined
                /* #@range_end(fourth_element_of_evenArray_result) */
              );
            });
          });
        });
      });
    });
  });

  // ### <section id='testability'>テストが容易である</section>
  describe('テストが容易である', () => {
    // **リスト2.32** 単体テストの書き方の例
    it('単体テストの書き方の例', () => {
      const succ = (n: number) => {
        return n + 1;
      };
      expect(
        succ(1) // テストしたい式を書く
      ).toEqual(
        2       // 期待する結果を書く
      );
    });

    // **リスト2.33** 参照透過性のない関数の単体テスト
    it("参照透過性のない関数の単体テスト", () => {
      /* #@range_begin(winner_with_sideeffect) */
      const winner = (playerL: { name: string; score: number }, playerR: { name: string; score: number }) => {
        if (playerR.score > playerL.score) {
          console.log(playerR.name + "が勝者です");
        } else if (playerR.score < playerL.score) {
          console.log(playerL.name + "が勝者です");
        } else {
          console.log("引き分けです");
        }
      };
      /* #@range_end(winner_with_sideeffect) */
      const playerA = {
        name: 'a',
        score: 10
      };
      const playerB = {
        name: 'b',
        score: 20
      };
    });

    describe('副作用を分離する', () => {
      // **リスト2.34** winner関数の分離
      it("winner関数の分離", () => {
        /* #@range_begin(winner_without_sideeffect) */
        /* 勝者を判定する */
        const judge = (playerL: { name: string; score: number }, playerR: { name: string; score: number }) => {
          if (playerL.score > playerR.score) {
            return playerL;
          } else if (playerL.score < playerR.score) {
            return playerR;
          } else {
            return null;
          }
        };
        /* 勝者を告げる文字列を生成する */
        const announce = (winner: { name: string } | null) => {
          if (winner) {
            return winner.name + "が勝者です";
          } else {
            return "引き分けです";
          }
        };
        /* 勝者を表示する */
        const displayWinner = (winner: { name: string } | null) => {
          console.log(announce(winner));
        };
        /* #@range_end(winner_without_sideeffect) */

        // **リスト2.35** 副作用のない関数のテスト
        /* #@range_begin(announce_winner) */
        const socrates = {
          name: 'ソクラテス',
          score: 10
        };
        const plato = {
          name: 'プラトン',
          score: 20
        };
        /* 純粋な関数をテストする */
        expect(
          announce(judge(socrates, plato))
        ).toEqual(
          "プラトンが勝者です"
        );
        /* #@range_end(announce_winner) */
      });
    });
  });

  // ### <section id='provable'>コードの正しさを証明できる</section>
  describe('コードの正しさを証明できる', () => {
    // #### プロパティテストで正しさを検証する
    describe('プロパティテストで正しさを検証する', () => {
      const adder = (m: number) => {
        return (n: number) => {
          return m + n;
        };
      };
      const succ = adder(1);
      const iterate = (step: (n: number) => number) => {
        return (init: number): any => {
          return [init, () => {
            return iterate(step)(step(init));
          }];
        };
      };
      const enumFrom = (from: number) => {
        return iterate(succ)(from);
      };

      it('succ関数の性質テスト', () => {
        // **リスト2.39** プロパティテストのための関数
        /* #@range_begin(succ_property) */
        /* ストリームのmap関数 */
        const map = (transform: (x: number) => any) => {
          return (aStream: any): any => {
            const head = aStream[0];
            return [transform(head), () => {
              return map(transform)(aStream[1]());
            }];
          };
        };
        /* ストリームの先頭から引数n分だけ取り出すtake関数 */
        const take = (n: number) => {
          return (aStream: any): any => {
            if (n === 0) {
              return null;
            } else {
              return [aStream[0], () => {
                return take(n - 1)(aStream[1]());
              }];
            }
          };
        };
        /* ストリームの全ての要素がtrueであるかを判定するall関数 */
        const all = (aStream: any): boolean => {
          const allHelper = (aStream: any, accumulator: boolean): boolean => {
            const head = aStream[0];
            const newAccumulator = accumulator && head;
            if (aStream[1]() === null) {
              return newAccumulator;
            } else {
              return allHelper(aStream[1](), newAccumulator);
            }
          };
          return allHelper(aStream, true);
        };
        /* 検証の対象となる命題 */
        const proposition = (n: number) => {
          return succ(0) + succ(n) === succ(succ(n));
        };
        /* #@range_end(succ_property) */

        // **リスト2.40** succ関数のプロパティテスト
        /* #@range_begin(succ_property_test) */
        /* 100個の整数について命題が正しいかをテストする */
        expect(
          all(
            take(100)(
              map(proposition)(enumFrom(0))
            )
          )
        ).toEqual(
          true
        );
        /* #@range_end(succ_property_test) */
      });
    });
  });
});

// [目次に戻る](index.html) [次章に移る](chap03.spec.html)
