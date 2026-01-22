"use strict";

// 第7章 高階関数を活用する
// =======

import * as fs from 'fs';

// リスト型の定義
type List<T> = (pattern: ListPattern<T>) => any;

interface ListPattern<T> {
  empty: () => any;
  cons: (value: T, alist: List<T>) => any;
}

// listモジュール
const list = {
  match: <T, R>(data: List<T>, pattern: { empty: () => R; cons: (value: T, alist: List<T>) => R }): R => {
    return data.call(list, pattern);
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
  head: <T>(alist: List<T>): T | null => {
    return list.match(alist, {
      empty: () => {
        return null;
      },
      cons: (head: T, tail: List<T>) => {
        return head;
      }
    });
  },
  tail: <T>(alist: List<T>): List<T> | null => {
    return list.match(alist, {
      empty: () => {
        return null;
      },
      cons: (head: T, tail: List<T>) => {
        return tail;
      }
    });
  },
  isEmpty: <T>(alist: List<T>): boolean => {
    return list.match(alist, {
      empty: () => {
        return true;
      },
      cons: (head: T, tail: List<T>) => {
        return false;
      }
    });
  },
  /* append:: LIST[T] -> LIST[T] -> LIST[T] */
  append: <T>(xs: List<T>) => {
    return (ys: List<T>): List<T> => {
      return list.match(xs, {
        empty: () => {
          return ys;
        },
        cons: (head: T, tail: List<T>) => {
          return list.cons(head, list.append(tail)(ys));
        }
      });
    };
  },
  /* map:: LIST[T] -> FUNC[T -> T] -> LIST[T] */
  map: <T, U>(alist: List<T>) => {
    return (transform: (item: T) => U): List<U> => {
      return list.match<T, List<U>>(alist, {
        empty: () => {
          return list.empty<U>();
        },
        cons: (head: T, tail: List<T>) => {
          return list.cons(transform(head), list.map<T, U>(tail)(transform));
        }
      });
    };
  },
  reverse: <T>(alist: List<T>): List<T> => {
    const reverseAux = (alist: List<T>, accumulator: List<T>): List<T> => {
      return list.match(alist, {
        empty: () => {
          return accumulator;  // 空のリストの場合は終了
        },
        cons: (head: T, tail: List<T>) => {
          return reverseAux(tail, list.cons(head, accumulator));
        }
      });
    };
    return reverseAux(alist, list.empty<T>());
  },
  toArray: <T>(alist: List<T>): T[] => {
    const toArrayAux = (alist: List<T>, accumulator: T[]): T[] => {
      return list.match(alist, {
        empty: () => {
          return accumulator;  // 空のリストの場合は終了
        },
        cons: (head: T, tail: List<T>) => {
          return toArrayAux(tail, accumulator.concat(head));
        }
      });
    };
    return toArrayAux(alist, []);
  },
  fromArray: <T>(array: T[]): List<T> => {
    return array.reduce((accumulator: List<T>, item: T) => {
      return list.append(accumulator)(list.cons(item, list.empty<T>()));
    }, list.empty<T>());
  }
};

// ストリーム型の定義
type Stream<T> = (pattern: StreamPattern<T>) => any;

interface StreamPattern<T> {
  empty: () => any;
  cons: (head: T, tailThunk: () => Stream<T>) => any;
}

// streamモジュール
const stream = {
  match: <T, R>(data: Stream<T>, pattern: { empty: () => R; cons: (head: T, tailThunk: () => Stream<T>) => R }): R => {
    return data.call(stream, pattern);
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
  /* head:: STREAM -> MAYBE[STREAM] */
  head: <T>(lazyList: Stream<T>): T | null => {
    return stream.match(lazyList, {
      empty: () => {
        return null;
      },
      cons: (value: T, tailThunk: () => Stream<T>) => {
        return value;
      }
    });
  },
  /* tail:: STREAM -> MAYBE[STREAM] */
  tail: <T>(lazyList: Stream<T>): Stream<T> | null => {
    return stream.match(lazyList, {
      empty: () => {
        return null;
      },
      cons: (head: T, tailThunk: () => Stream<T>) => {
        return tailThunk();
      }
    });
  },
  isEmpty: <T>(lazyList: Stream<T>): boolean => {
    return stream.match(lazyList, {
      empty: () => {
        return true;
      },
      cons: (head: T, tailThunk: () => Stream<T>) => {
        return false;
      }
    });
  },
  /* take:: STREAM -> NUMBER -> STREAM */
  take: <T>(lazyList: Stream<T>) => {
    return (number: number): Stream<T> => {
      return stream.match(lazyList, {
        empty: () => {
          return stream.empty<T>();
        },
        cons: (head: T, tailThunk: () => Stream<T>) => {
          if (number === 0) {
            return stream.empty<T>();
          } else {
            return stream.cons(head, () => {
              return stream.take(tailThunk())(number - 1);
            });
          }
        }
      });
    };
  },
  enumFrom: (from: number): Stream<number> => {
    return stream.cons(from, () => {
      return stream.enumFrom(from + 1);
    });
  },
  forAll: <T>(astream: Stream<T>) => {
    return (predicate: (item: T) => boolean): boolean => {
      const forAllHelper = (astream: Stream<T>): boolean => {
        return stream.match(astream, {
          empty: () => {
            return true;
          },
          cons: (head: T, tailThunk: () => Stream<T>) => {
            return predicate(head) && forAllHelper(tailThunk());
          }
        });
      };
      return stream.match(astream, {
        empty: () => {
          return false; // 空のストリームの場合は、必ず false が返る
        },
        cons: (head: T, tailThunk: () => Stream<T>) => {
          return forAllHelper(astream);
        }
      });
    };
  }
}; // end of 'stream' module


// ## 7.2 <section id='currying'>カリー化で関数を渡す</section>
describe('カリー化で関数を渡す', () => {
  // **リスト7.1** multipleOf関数の定義
  it('multipleOf関数の定義', () => {
    /* #@range_begin(multipleOf_uncurried) */
    const multipleOf = (n: number, m: number): boolean => {
      if (m % n === 0) { /* m / n の余りが 0 かどうか */
        return true;
      } else {
        return false;
      }
    };
    /* #@range_end(multipleOf_uncurried) */
    // **リスト7.2** multipleOf関数のテスト
    /* #@range_begin(multipleOf_uncurried_test) */
    expect(
      multipleOf(2, 4)     /* 4は、2の倍数である */
    ).toEqual(
      true
    );
    /* #@range_end(multipleOf_uncurried_test) */
    expect(
      multipleOf(3, 4)     /* 4は、3の倍数ではない */
    ).toEqual(
      false
    );
  });

  // **リスト7.3** カリー化されたmultipleOf関数の定義
  it('カリー化されたmultipleOf関数', () => {
    /* #@range_begin(multipleOf_curried) */
    const multipleOf = (n: number) => { // 外側の関数定義
      return (m: number): boolean => {  // 内側の関数定義
        if (m % n === 0) {
          return true;
        } else {
          return false;
        }
      };
    };
    /* #@range_end(multipleOf_curried) */
    // **リスト7.4** カリー化されたmultipleOf関数のテスト
      /* #@range_begin(church_numeral_counter) */
    expect(
      multipleOf(2)(4)   /* 関数適用を2回実行する */
    ).toEqual(
      true
    );
    expect(
      multipleOf(3)(4)    /* 4は、3の倍数ではない */
    ).toEqual(
      false
    );
      /* #@range_end(church_numeral_counter) */
    // **リスト7.5** multipleOf関数のテスト
    /* #@range_begin(multipleOf_curried_partilly_applied) */
    const twoFold = multipleOf(2);
    expect(
      twoFold(4)    /* 4は、2の倍数である */
    ).toEqual(
      true
    );
    /* #@range_end(multipleOf_curried_partilly_applied) */
  });

  it('カリー化された指数関数', () => {
    // **リスト7.6** 指数関数の例
    /* #@range_begin(exponential_curried) */
    const exponential = (base: number) => {
      return (index: number): number => {
        if (index === 0) {
          return 1;
        } else {
          return base * exponential(base)(index - 1);
        }
      };
    };
    /****** テスト ******/
    expect(
      exponential(2)(3) // 2の3乗を求める
    ).toEqual(
      8
    );
    /* #@range_end(exponential_curried) */
        /* #@range_begin(identity_monad_laws_right_unit_law) */
    expect(
      exponential(2)(2)
    ).toEqual(
      4
    );
        /* #@range_end(identity_monad_laws_right_unit_law) */

    // **リスト7.7** flip関数の定義
    /* #@range_begin(flip_definition) */
    const flip = <A, B, C>(fun: (a: A) => (b: B) => C) => {
      return (x: B) => {
        return (y: A): C => {
          return fun(y)(x); // 適用する引数の順番を逆転させる
        };
      };
    };
    /* #@range_end(flip_definition) */

    // **リスト7.8** flip関数でexponential関数の引数の順番を変更する
    /* #@range_begin(flipped_exponential) */
    /* flipで引数を逆転させて、2乗を定義する */
    const square = flip(exponential)(2);
    /* flipで引数を逆転させて、3乗を定義する */
    const cube = flip(exponential)(3);
    /* #@range_end(flipped_exponential) */
      /* #@range_begin(flatMap_and_composition) */
    expect(
      square(2)
    ).toEqual(
      4 /* 2 * 2 = 4 */
    );
    expect(
      cube(2)
    ).toEqual(
      8 /* 2 * 2 * 2 = 8 */
    );
      /* #@range_end(flatMap_and_composition) */
  });

  // ### コラム： チャーチ数
  describe('コラム： チャーチ数', () => {
    // **リスト7.9** チャーチによる自然数の定義
    it('チャーチによる自然数の定義', () => {
      type ChurchNumeral = (f: (x?: any) => any) => (x?: any) => any;

      const zero: ChurchNumeral = (f) => {
        return (x) => {
          return x;           // 関数を0回適用する
        };
      };
      /* #@range_begin(church_one) */
      const one: ChurchNumeral = (f) => {
        return (x) => {
          return f(x);        // 関数を1回適用する
        };
      };
      /* #@range_end(church_one) */
      const two: ChurchNumeral = (f) => {
        return (x) => {
          return f(f(x));     // 関数を2回適用する
        };
      };
      const three: ChurchNumeral = (f) => {
        return (x) => {
          return f(f(f(x)));  // 関数を3回適用する
        };
      };
      const add = (m: ChurchNumeral) => {
        return (n: ChurchNumeral): ChurchNumeral => {
          return (f) => {
            return (x) => {
              return m(f)(n(f)(x));
            };
          };
        };
      };
      const succ = (n: ChurchNumeral): ChurchNumeral => {
        return (f) => {
          return (x) => {
            return f(n(f)(x));
          };
        };
      };
      const counter = (init: number) => {
        let _init = init;
        return (_?: any): number => {
          _init = _init + 1;
          return _init;
        };
      };
      expect(one(counter(0))()).toEqual(1);
      expect(two(counter(0))()).toEqual(2);
      expect(three(counter(0))()).toEqual(3);
      expect(succ(one)(counter(0))()).toEqual(2);
      expect(succ(two)(counter(0))()).toEqual(3);
      expect(add(zero)(one)(counter(0))()).toEqual(1);
      expect(add(one)(one)(counter(0))()).toEqual(2);
      expect(add(one)(two)(counter(0))()).toEqual(3);
      expect(add(two)(three)(counter(0))()).toEqual(5);
    });
  });
});

// ## 7.3 <section id='combinator'>コンビネータで関数を組み合わせる</section>
describe('コンビネータで関数を組み合わせる', () => {
  // ### <section id='combinator-creation'>コンビネータの作り方</section>
  describe('コンビネータの作り方', () => {
    // **リスト7.10** multipleOf関数の再利用
    it('multipleOf関数の再利用', () => {
      const multipleOf = (n: number) => {
        return (m: number): boolean => {
          if (m % n === 0) {
            return true;
          } else {
            return false;
          }
        };
      };
      /* #@range_begin(multipleOf_combinator) */
      const even = multipleOf(2); /* カリー化されたmultipleOf関数を使う */

      expect(
        even(2)
      ).toEqual(
        true
      );
      /* #@range_end(multipleOf_combinator) */
    });

    describe('論理コンビネータ', () => {
      const multipleOf = (n: number) => {
        return (m: number): boolean => {
          if (m % n === 0) {
            return true;
          } else {
            return false;
          }
        };
      };
      const even = multipleOf(2);

      // **リスト7.13** notコンビネータ
      it('notコンビネータ', () => {
        /* #@range_begin(not_combinator) */
        /* not:: FUN[NUM => BOOL] => FUN[NUM => BOOL] */
        const not = (predicate: (n: number) => boolean) => { // predicateの型はFUN[NUM => BOOL]
          /* 全体として、FUN[NUM => BOOL]型を返す */
          return (arg: number): boolean => { // argの型はNUM
            return !predicate(arg); // !演算子で論理を反転させて、BOOLを返す
          };
        };
        /* #@range_end(not_combinator) */
        // **リスト7.15** notコンビネータによるodd関数の定義
        /* #@range_begin(not_combinator_test) */
        const odd = not(even); // notコンビネータでodd関数を定義する
        /******** テスト ********/
        expect(
          odd(3) // 3は奇数である
        ).toEqual(
          true
        );
        expect(
          odd(2) // 2は奇数でない
        ).toEqual(
          false
        );
        /* #@range_end(not_combinator_test) */
      });

      /* 本書では割愛したが、論理和や論理積を実行するコンビネータも同様に定義できる */
      it('他の論理コンビネータ', () => {
        const not = (predicate: (n: number) => boolean) => {
          return (arg: number): boolean => {
            return !predicate(arg);
          };
        };
        /* 「もしくは」を表す論理和  */
        const or = (f: (n: number) => boolean, g: (n: number) => boolean) => {
          return (arg: number): boolean => {
            return f(arg) || g(arg);
          };
        };
        /* 「かつ」を表す論理積  */
        const and = (f: (n: number) => boolean, g: (n: number) => boolean) => {
          return (arg: number): boolean => {
            return f(arg) && g(arg);
          };
        };
        const positive = (n: number): boolean => {
          return n > 0;
        };
        const zero = (n: number): boolean => {
          return n === 0;
        };
        /* negativeは、0より小さな数値かどうかを判定する */
        const negative = and(not(positive), not(zero));
        expect(negative(-3)).toEqual(true);
        expect(negative(3)).toEqual(false);
        expect(negative(0)).toEqual(false);
      });
    });
  });

  // ### <section id='composing-function'>関数を合成する</section>
  describe('関数を合成する', () => {
    // **リスト7.16** 関数合成の定義
    /* #@range_begin(compose_definition) */
    const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B) => {
      return (arg: A): C => {
        return f(g(arg));
      };
    };
    /* #@range_end(compose_definition) */
    // **リスト7.17** 関数合成のテスト
    /* #@range_begin(compose_test) */
    const f = (x: number): number => {
      return x * x + 1;
    };
    const g = (x: number): number => {
      return x - 2;
    };
    expect(
      compose(f, g)(2) // f◦g で合成された関数
    ).toEqual(
      f(g(2))         // 合成せずに順次実行した場合
    );
    /* #@range_end(compose_test) */

    // #### 関数合成の条件
    describe('関数合成の条件', () => {
      // **リスト7.18** 反数関数の合成
      it('反数関数の合成', () => {
        /* #@range_begin(composition_example_opposite_twice) */
        /* 反数の定義 */
        const opposite = (n: number): number => {
          return -n;
        };
        expect(
          /* 反数同士の合成は成功する */
          compose(opposite, opposite)(2)
        ).toEqual(
          2 // -(-2) === 2
        );
        /* #@range_end(composition_example_opposite_twice) */
      });

      // **リスト7.20** カリー化による合成
      it('カリー化による合成', () => {
        /* #@range_begin(compose_opposite_add_successful) */
        const opposite = (x: number): number => {
          return -x;
        };
        const addCurried = (x: number) => { // カリー化されたadd関数
          return (y: number): number => {
            return x + y;
          };
        };
        expect(
          compose(opposite, addCurried(2))(3)
        ).toEqual(
          -5
        );
        /* #@range_end(compose_opposite_add_successful) */
      });
    });

    const flip = <A, B, C>(fun: (a: A) => (b: B) => C) => {
      return (f: B) => {
        return (g: A): C => {
          return fun(g)(f);
        };
      };
    };

    // #### 関数合成による抽象化
    describe('関数合成による抽象化', () => {
      // **リスト7.21** 具体的なlast関数
      it('具体的なlast関数', () => {
        /* #@range_begin(list_last_recursive) */
        const last = <T>(alist: List<T>): T | null => {
          return list.match(alist, {
            empty: () => { // alistが空の場合
              return null;
            },
            cons: (head: T, tail: List<T>) => { // alistが空でない場合
              return list.match(tail, {
                empty: () => { // alistの要素がただ1個の場合
                  return head;
                },
                cons: (_: T, __: List<T>) => {
                  return last(tail);
                }
              });
            }
          });
        };
        /* #@range_end(list_last_recursive) */
        const aList = list.cons(1,
          list.cons(2,
            list.cons(3,
              list.empty<number>())));
        /* #@range_begin(identity_monad_laws_left_unit_law) */
        expect(
          last(aList)
        ).toEqual(
          3
        );
        /* #@range_end(identity_monad_laws_left_unit_law) */
      });

      // ** リスト7.22** 抽象的なlast関数
      it('抽象的なlast関数', () => {
        const last = <T>(alist: List<T>): T | null => {
          return compose<List<T>, List<T>, T | null>(list.head, list.reverse)(alist);
        };
        const sequence = list.cons(1, list.cons(2, list.cons(3, list.cons(4, list.empty<number>()))));
        expect(
          last(sequence)
        ).toEqual(
          4
        );
      });

      // **表7.1** 関数合成による様々な関数定義
      describe('関数合成による様々な関数定義', () => {
        const alwaysOne = (x: any): number => {
          return 1;
        };
        const alist = list.cons(1,
          list.cons(2,
            list.cons(3,
              list.empty<number>())));

        // length関数の定義
        it('length関数の定義', () => {
        /* #@range_begin(foldr_map) */
          const sum = (alist: List<number>): number => {
            const sumHelper = (alist: List<number>, accumulator: number): number => {
              return list.match(alist, {
                empty: () => {
                  return accumulator;
                },
                cons: (head: number, tail: List<number>) => {
                  return sumHelper(tail, accumulator + head);
                }
              });
            };
            return sumHelper(alist, 0);
          };
          const length = (alist: List<number>): number => {
            return compose(sum, flip(list.map)(alwaysOne))(alist);
          };
          /****** テスト *******/
          expect(
            length(alist)
          ).toEqual(
            3
          );
        /* #@range_end(foldr_map) */
        });

        // last関数の定義
        it('last関数の定義', () => {
          const last = <T>(alist: List<T>): T | null => {
            return compose<List<T>, List<T>, T | null>(list.head, list.reverse)(alist);
          };
          expect(
            last(alist)
          ).toEqual(
            3
          );
        });

        // init関数の定義
        it('init関数の義', () => {
          /* init = reverse . tail . reverse  */
          const init = <T>(alist: List<T>): List<T> | null => {
            return compose<List<T>, List<T> | null, List<T> | null>(
              (x) => x ? list.reverse(x) : null,
              compose<List<T>, List<T>, List<T> | null>(list.tail, list.reverse)
            )(alist);
          };
          /****** テスト *******/
          expect(
            list.toArray(init(alist)!)
          ).toEqual(
            [1, 2]
          );
        });

        // all関数の定義
        it('all関数の定義', () => {
      /* #@range_begin(list_foldr) */
          const and = (alist: List<boolean>): boolean => {
            return list.match(alist, {
              empty: () => {
                return true;
              },
              cons: (head: boolean, tail: List<boolean>) => {
                return head && and(tail);
              }
            });
          };
          const all = (predicate: (x: number) => boolean) => {
            return (alist: List<number>): boolean => {
              return compose(and, flip(list.map)(predicate))(alist);
            };
          };
      /* #@range_end(list_foldr) */
      /* #@range_begin(identity_monad_flatMap_test) */
          expect(
            all((x) => {
              return x > 0;
            })(alist)
          ).toEqual(
            true
          );
      /* #@range_end(identity_monad_flatMap_test) */
        });

        // any関数の定義
        it('any関数の定義', () => {
        /* #@range_begin(foldr_sum) */
          const or = (alist: List<boolean>): boolean => {
            return list.match(alist, {
              empty: () => {
                return false;
              },
              cons: (head: boolean, tail: List<boolean>) => {
                return head || or(tail);
              }
            });
          };
        /* #@range_end(foldr_sum) */
          const any = (predicate: (x: number) => boolean) => {
            return (alist: List<number>): boolean => {
              return compose(or, flip(list.map)(predicate))(alist);
            };
          };
        /* #@range_begin(identity_monad_laws_associative_law) */
          expect(
            any((x) => {
              return x < 2;
            })(alist)
          ).toEqual(
            true
          );
        /* #@range_end(identity_monad_laws_associative_law) */
        /* #@range_begin(run_println_without_world) */
          expect(
            any((x) => {
              return x < 1;
            })(alist)
          ).toEqual(
            false
          );
        /* #@range_end(run_println_without_world) */
        });

        // none関数の定義
        it('none関数の定義', () => {
          const and = (alist: List<boolean>): boolean => {
            return list.match(alist, {
              empty: () => {
                return true;
              },
              cons: (head: boolean, tail: List<boolean>) => {
                return head && and(tail);
              }
            });
          };
          const not = (predicate: (n: number) => boolean) => {
            return (arg: number): boolean => {
              return !predicate(arg);
            };
          };
          const none = (predicate: (x: number) => boolean) => {
            return (alist: List<number>): boolean => {
              return compose(and, flip(list.map)(not(predicate)))(alist);
            };
          };
          expect(
            none((x) => {
              return x < 0;
            })(alist)
          ).toEqual(
            true
          );
        });
      });
    });

    // ### コラム: Yコンビネータ
    it('Y combinator', () => {
      /* #@range_begin(Y_combinator) */
      type Rec<T, R> = (f: (y: T) => R) => (y: T) => R;
      const Y = <T, R>(F: Rec<T, R>): ((y: T) => R) => {
        return ((x: any) => {
          return F((y: T) => {
            return x(x)(y);
          });
        })((x: any) => {
          return F((y: T) => {
            return x(x)(y);
          });
        });
      };
      /* #@range_end(Y_combinator)  */
      // **リスト7.24** Yコンビネータによるfactorial関数の実装
      /* #@range_begin(Y_combinator_test) */
      const factorial = Y((fact: (n: number) => number) => {
        return (n: number): number => {
          if (n === 0) {
            return 1;
          } else {
            return n * fact(n - 1);
          }
        };
      });
      expect(
        factorial(3) // 3 * 2 * 1 = 6
      ).toEqual(
        6
      );
      /* #@range_end(Y_combinator_test) */
    });
  }); // 関数を合成する
}); // コンビネータ


// ## <section id='closure'>7.4 クロージャーを使う</section>
describe('クロージャーを使う', () => {
  const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B) => {
    return (arg: A): C => {
      return f(g(arg));
    };
  };

  // ### <section id='mechanism-of-closure'>クロージャーの仕組み</section>
  describe('クロージャーの仕組み', () => {
    // **リスト7.25** 環境における変数のバインディング
    it('環境における変数のバインディング', () => {
      /* #@range_begin(variable_binding_in_environment) */
      /* 変数fooに数値1をバインドする */
      const foo = 1;
      /* 変数bar に文字列 "a string" をバインドする */
      const bar = "a string";
      /* #@range_end(variable_binding_in_environment) */

      // **リスト7.26** 環境からバインディングを参照する
      /* #@range_begin(variable_binding_in_environment_test) */
      /* 環境 <foo |-> 1, bar |-> "a string"> のもとで評価する */
      expect(
        foo  // 上記環境から変数fooの値を取り出す
      ).toEqual(
        1
      );
      /* #@range_end(variable_binding_in_environment_test) */
    });

    // **リスト7.27** 部分適用と環境
    it('部分適用と環境', () => {
      const multipleOf = (n: number) => { // 外側の関数定義
        return (m: number): boolean => {  // 内側の関数定義
          if (m % n === 0) {
            return true;
          } else {
            return false;
          }
        };
      };
      /* #@range_begin(partial_application_with_environment) */
      const twoFold = multipleOf(2);
      expect(
        twoFold(4)
      ).toEqual(
        true
      );
      /* #@range_end(partial_application_with_environment) */
    });

    // ### <section id='encapsulation-with-closure'>クロージャーで状態をカプセル化する</section>
    describe('クロージャーで状態をカプセル化する', () => {
      // **リスト7.28** クロージャーとしてのcounter関数
      it('クロージャーとしてのcounter関数', () => {
        /* #@range_begin(counter_as_closure) */
        const counter = (init: number) => {
          let countingNumber = init;
          /* countingNumberの環境を持つクロージャーを返す */
          return (): number => {
            countingNumber = countingNumber + 1;
            return countingNumber;
          };
        };
        /* #@range_end(counter_as_closure) */
        // **リスト7.29** counter関数の利用法
        /* #@range_begin(counter_as_closure_test) */
        const counterFromZero = counter(0);
        expect(
          counterFromZero() // 1回目の実行
        ).toEqual(
          1
        );
        expect(
          counterFromZero() // 2回目の実行
        ).toEqual(
          2
        );
        /* #@range_end(counter_as_closure_test) */
      });

      // #### クロージャーで不変なデータ型を作る
      describe('クロージャーで不変なデータ型を作る', () => {
        // **リスト7.31** カリー化された不変なオブジェクト型
        it('カリー化された不変なオブジェクト型', () => {
          type ImmutableObject = (key: string) => any;
          /* #@range_begin(immutable_object_type_curried) */
          const object = {  // objectモジュール
            /* empty:: STRING => Any */
            empty: (key: string): any => {
              return null;
            },
            /* set:: (STRING,Any) => (STRING => Any) => STRING => Any */
            set: (key: string, value: any) => {
              return (obj: ImmutableObject) => {
                return (queryKey: string): any => {
                  if (key === queryKey) {
                    return value;
                  } else {
                    return object.get(queryKey)(obj);
                  }
                };
              };
            },
            /* get:: (STRING) => (STRING => Any) => Any */
            get: (key: string) => {
              return (obj: ImmutableObject): any => {
                return obj(key);
              };
            }
          };
          /* #@range_end(immutable_object_type_curried) */
          // **リスト7.32** カリー化された不変なオブジェクト型のテスト
          /* #@range_begin(immutable_object_type_curried_test) */
          const robots = compose(
            object.set("C3PO", "Star Wars"),
            object.set("HAL9000", "2001: a space odessay")
          )(object.empty);

          expect(
            object.get("HAL9000")(robots)
          ).toEqual(
            "2001: a space odessay"
          );
          expect(
            object.get("C3PO")(robots)
          ).toEqual(
            "Star Wars"
          );
          // 該当するデータがなければ、nullが返る
          expect(
            object.get("鉄腕アトム")(robots)
          ).toEqual(
            null
          );
          /* #@range_end(immutable_object_type_curried_test) */
        });
      });

      // #### クロージャーでジェネレーターを作る
      describe('クロージャーでジェネレーターを作る', () => {
        // **リスト7.33** ストリームからジェネレータを作る
        describe('ストリームからジェネレータを作る', () => {
          /* #@range_begin(generator_from_stream) */
          const generate = <T>(aStream: Stream<T>) => {
            /* いったんローカル変数にストリームを格納する */
            let _stream = aStream;
            /* ジェネレータ関数が返る */
            return (): T | null => {
              return stream.match(_stream, {
                empty: () => {
                  return null;
                },
                cons: (head: T, tailThunk: () => Stream<T>) => {
                  _stream = tailThunk();  // ローカル変数を更新する
                  return head;  // ストリームの先頭要素を返す
                }
              });
            };
          };
          /* #@range_end(generator_from_stream) */

          // **リスト7.34** 整数列のジェネレータ
          it('整数列のジェネレータ', () => {
            const enumFrom = (from: number): Stream<number> => {
              return stream.cons(from, () => {
                return enumFrom(from + 1);
              });
            };
            /* #@range_begin(integer_generator) */
            /* 無限の整数列を生成する */
            const integers = enumFrom(0);
            /* 無限ストリームからジェネレータを生成する */
            const intGenerator = generate(integers);
            expect(intGenerator()).toEqual(
              0
            );
            expect(intGenerator()).toEqual(
              1
            );
            expect(intGenerator()).toEqual(
              2
            );
            /* #@range_end(integer_generator) */
          });

          it('無限の素数列を作る', () => {
            const not = (predicate: (n: number) => boolean) => {
              return (arg: number): boolean => {
                return !predicate(arg);
              };
            };

            const streamLocal = {
              match: <T, R>(data: Stream<T>, pattern: { empty: () => R; cons: (head: T, tailThunk: () => Stream<T>) => R }): R => {
                return data.call(streamLocal, pattern);
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
              head: <T>(lazyList: Stream<T>): T | null => {
                return streamLocal.match(lazyList, {
                  empty: () => {
                    return null;
                  },
                  cons: (value: T, tailThunk: () => Stream<T>) => {
                    return value;
                  }
                });
              },
              tail: <T>(lazyList: Stream<T>): Stream<T> | null => {
                return streamLocal.match(lazyList, {
                  empty: () => {
                    return null;
                  },
                  cons: (head: T, tailThunk: () => Stream<T>) => {
                    return tailThunk();
                  }
                });
              },
              toArray: <T>(lazyList: Stream<T>): T[] => {
                return streamLocal.match(lazyList, {
                  empty: () => {
                    return [];
                  },
                  cons: (head: T, tailThunk: () => Stream<T>) => {
                    return streamLocal.match(tailThunk(), {
                      empty: () => {
                        return [head];
                      },
                      cons: (head_: T, tailThunk_: () => Stream<T>) => {
                        return [head].concat(streamLocal.toArray(tailThunk()));
                      }
                    });
                  }
                });
              },
              take: <T>(lazyList: Stream<T>) => {
                return (number: number): Stream<T> => {
                  return streamLocal.match(lazyList, {
                    empty: () => {
                      return streamLocal.empty<T>();
                    },
                    cons: (head: T, tailThunk: () => Stream<T>) => {
                      if (number === 0) {
                        return streamLocal.empty<T>();
                      } else {
                        return streamLocal.cons(head, () => {
                          return streamLocal.take(tailThunk())(number - 1);
                        });
                      }
                    }
                  });
                };
              },
              // **リスト7.35** ストリームのfilter関数
              /* #@range_begin(stream_filter) */
              filter: <T>(predicate: (item: T) => boolean) => {
                return (aStream: Stream<T>): Stream<T> => {
                  return streamLocal.match(aStream, {
                    empty: () => {
                      return streamLocal.empty<T>();
                    },
                    cons: (head: T, tailThunk: () => Stream<T>) => {
                      if (predicate(head)) { // 条件に合致する場合
                        return streamLocal.cons(head, () => {
                          return streamLocal.filter(predicate)(tailThunk());
                        });
                      } else { // 条件に合致しない場合
                        return streamLocal.filter(predicate)(tailThunk());
                      }
                    }
                  });
                };
              },
              /* #@range_end(stream_filter) */
              // **リスト7.36** ストリームのremove関数
              /* #@range_begin(stream_remove) */
              remove: (predicate: (item: number) => boolean) => {
                return (aStream: Stream<number>): Stream<number> => {
                  return streamLocal.filter(not(predicate))(aStream);
                };
              },
              /* #@range_end(stream_remove) */
              enumFrom: (from: number): Stream<number> => {
                return streamLocal.cons(from, () => {
                  return streamLocal.enumFrom(from + 1);
                });
              },
              /* #@range_begin(stream_generate) */
              generate: <T>(astream: Stream<T>) => {
                let theStream = astream;
                return (): T | null => {
                  return streamLocal.match(theStream, {
                    empty: () => {
                      return null;
                    },
                    cons: (head: T, tailThunk: () => Stream<T>) => {
                      theStream = tailThunk();
                      return head;
                    }
                  });
                };
              }
              /* #@range_end(stream_generate) */
            }; // end of 'streamLocal' module

            const multipleOf = (n: number) => {
              return (m: number): boolean => {
                if (n % m === 0) {
                  return true;
                } else {
                  return false;
                }
              };
            };

            // **リスト7.37** 素数列の生成
            /* #@range_begin(eratosthenes_sieve) */
            /* エラトステネスのふるい */
            const sieve = (aStream: Stream<number>): Stream<number> => {
              return streamLocal.match(aStream, {
                empty: () => {
                  return null as any;
                },
                cons: (head: number, tailThunk: () => Stream<number>) => {
                  return streamLocal.cons(head, () => {
                    return sieve(streamLocal.remove(
                      (item: number) => {
                        return multipleOf(item)(head);
                      }
                    )(tailThunk()));
                  });
                }
              });
            };
            const primes = sieve(streamLocal.enumFrom(2)); // 無限の素数列
            /* #@range_end(eratosthenes_sieve) */
            expect(
              streamLocal.toArray(streamLocal.take(primes)(10))
            ).toEqual(
              [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
            );

            // **リスト7.39** 素数のジェネレータ
            /* #@range_begin(prime_generator) */
            const primesForGen = sieve(streamLocal.enumFrom(2)); // 無限の素数列
            const primeGenerator = generate(primesForGen);  // 素数のジェネレータ
            /******* テスト ********/
            expect(primeGenerator()).toEqual(
              2
            );
            expect(primeGenerator()).toEqual(
              3
            );
            expect(primeGenerator()).toEqual(
              5
            );
            /* #@range_end(prime_generator) */
          }, 4000);
        });

        // #### コラム：ECMAScript2015（ES6）におけるジェネレータ
        it('ECMAScript2015（ES6）におけるジェネレータ', () => {
          // **リスト7.40** ECMAScript2015のジェネレータ
          /* #@range_begin(es6_generator) */
          function* genCounter() {
            yield 1;
            yield 2;
            return 3;
          };
          const counter = genCounter();
          expect(
            counter.next().value
          ).toEqual(
            1
          );
          expect(
            counter.next().value
          ).toEqual(
            2
          );
          /* #@range_end(es6_generator) */
        });
      });
    }); // クロージャーで状態をカプセル化する
  });

  // ### <section id='pure-closure'>クロージャーの純粋性 </section>
  describe('クロージャーの純粋性', () => {
    // **リスト 7.41** multipleOf関数の参照透過性
    it('multipleOf関数の参照透過性', () => {
      const multipleOf = (n: number) => {
        return (m: number): boolean => {
          if (m % n === 0) {
            return true;
          } else {
            return false;
          }
        };
      };
      expect(
        multipleOf(2)(4)
      ).toEqual(
        multipleOf(2)(4)
      );
      expect(
        multipleOf(3)(5)
      ).toEqual(
        multipleOf(3)(5)
      );
    });

    // **リスト7.42** 参照透過性のないクロージャーの例
    it('参照透過性のないクロージャーの例', () => {
      const counter = (init: number) => {
        let _init = init;
        return (): number => {
          _init = _init + 1;
          return _init;
        };
      };
      /* #@range_begin(counter_is_not_transparent) */
      const counterFromZero = counter(0);
      expect(
        counterFromZero()
      ).not.toEqual( // notで一致しないことをテストしている
        counterFromZero()
      );
      /* #@range_end(counter_is_not_transparent) */
    });

    // **リスト7.44** カウンターをクロージャーで定義する
    it('カウンターをクロージャーで定義する', () => {
      /* チャーチ数 church numeral */
      type ChurchNumeral = (f: (x?: any) => any) => (x?: any) => any;

      /* #@range_begin(church_numeral) */
      const zero: ChurchNumeral = (f) => {
        return (x) => {
          return x;
        };
      };
      // **リスト7.45** チャーチ数のone関数
      const one: ChurchNumeral = (f) => {
        return (x) => {
          return f(x); // f関数を1回適用する
        };
      };
      const two: ChurchNumeral = (f) => {
        return (x) => {
          return f(f(x));
        };
      };
      const three: ChurchNumeral = (f) => {
        return (x) => {
          return f(f(f(x)));
        };
      };
      /*#@range_end(church_numeral) */
      const succ = (n: ChurchNumeral): ChurchNumeral => {
        return (f) => {
          return (x) => {
            return f(n(f)(x));
          };
        };
      };
      const add = (m: ChurchNumeral) => {
        return (n: ChurchNumeral): ChurchNumeral => {
          return (f) => {
            return (x) => {
              return m(f)(n(f)(x));
            };
          };
        };
      };
      /* 関数適用の回数を数えるcounterクロージャー */
      const counter = (init: number) => {
        let _init = init; // 可変な変数
        return (_?: any): number => {
          _init = _init + 1; // 代入で変数_initを更新する
          return _init;
        };
      };
      /***** counterクロージャーを用いたチャーチ数のテスト *****/
      expect(
        one(counter(0))() // oneはチャーチ数の1
      ).toEqual(
        1
      );
      expect(
        two(counter(0))() // twoはチャーチ数の2
      ).toEqual(
        2
      );
      expect(
        add(one)(two)(counter(0))()
      ).toEqual(
        3
      );
    });
  });
});

// ## 7.5 <section id='passing-function'>関数を渡す</section>
describe('関数を渡す', () => {
  const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B) => {
    return (arg: A): C => {
      return f(g(arg));
    };
  };

  // ### <section id='callback'>コールバックで処理をモジュール化する</section>
  describe('コールバックで処理をモジュール化する', () => {
    // **リスト7.47** 直接的な呼び出しの例
    it('直接的な呼び出しの例', () => {
      /* #@range_begin(direct_call) */
      const succ = (n: number): number => {
        return n + 1;
      };
      const doCall = (arg: number): number => {
        return succ(arg);  // succ関数を直接呼び出す
      };
      expect(
        doCall(2)
      ).toEqual(
        3
      );
      /* #@range_end(direct_call) */
    });

    // **リスト7.48** 単純なコールバックの例
    it('単純なコールバックの例', () => {
      const succ = (n: number): number => {
        return n + 1;
      };
      /* #@range_begin(call_callback) */
      const setupCallback = (callback: (n: number) => number) => {
        /* コールバック関数を実行する無名関数を返す */
        return (arg: number): number => {
          return callback(arg);
        };
      };
      /* コールバック関数を設定する */
      const doCallback = setupCallback(succ);
      expect(
        doCallback(2) // 設定されたコールバック関数を実行する
      ).toEqual(
        3
      );
      /* #@range_end(call_callback) */
    });

    it('リストのmap関数', () => {
      // **リスト7.49** リストのmap関数の定義
      /* #@range_begin(list_map) */
      const map = <T, U>(callback: (item: T) => U) => {
        return (alist: List<T>): List<U> => {
          return list.match(alist, {
            empty: () => {
              return list.empty<U>();
            },
            cons: (head: T, tail: List<T>) => {
              /* コールバック関数を実行する */
              return list.cons(callback(head),
                map(callback)(tail)); // 再帰呼び出し
            }
          });
        };
      };
      /* #@range_end(list_map) */

      // **リスト7.50** map関数のテスト
      /* #@range_begin(list_map_test) */
      /* map処理の対象となる数値のリスト */
      const numbers = list.cons(1,
        list.cons(2,
          list.cons(3,
            list.empty<number>())));
      /* 要素を2倍するmap処理 */
      const mapDouble = map((n: number) => {
        return n * 2;
      });
      expect(
        compose(list.toArray, mapDouble)(numbers)
      ).toEqual(
        [2, 4, 6]
      );
      /* 要素を2乗するmap処理 */
      const mapSquare = map((n: number) => {
        return n * n;
      });
      expect(
        compose(list.toArray, mapSquare)(numbers)
      ).toEqual(
        [1, 4, 9]
      );
      /* #@range_end(list_map_test) */
    });
  });

  // ### <section id='folding'>畳み込み関数に関数を渡す</section>
  describe('畳み込み関数に関数を渡す', () => {
    describe('コールバックによるリストの再帰関数', () => {
      // **リスト7.51** sum関数の定義
      it('sum関数の定義', () => {
        const listLocal = {
          match: <T, R>(data: List<T>, pattern: { empty: () => R; cons: (value: T, alist: List<T>) => R }): R => {
            return data.call(listLocal, pattern);
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
          /* #@range_begin(list_sum) */
          sum: (alist: List<number>) => {
            return (accumulator: number): number => {
              return listLocal.match(alist, {
                empty: () => {
                  return accumulator;
                },
                cons: (head: number, tail: List<number>) => {
                  return listLocal.sum(tail)(accumulator + head);
                }
              });
            };
          },
          /* #@range_end(list_sum) */
          // **リスト7.52** コールバック関数を用いたsum関数の再定義
          /* #@range_begin(list_sum_callback) */
          sumWithCallback: (alist: List<number>) => {
            return (accumulator: number) => {
              return (CALLBACK: (head: number) => (acc: number) => number): number => { // コールバック関数を受け取る
                return listLocal.match(alist, {
                  empty: () => {
                    return accumulator;
                  },
                  cons: (head: number, tail: List<number>) => {
                    return CALLBACK(head)( // コールバック関数を呼び出す
                      listLocal.sumWithCallback(tail)(accumulator)(CALLBACK)
                    );
                  }
                });
              };
            };
          }
          /* #@range_end(list_sum_callback) */
        };

        // **リスト7.53** sumWithCallback関数のテスト
        /* #@range_begin(list_sum_callback_test) */
        const numbers = listLocal.cons(1,
          listLocal.cons(2,
            listLocal.cons(3,
              listLocal.empty<number>())));
        /* sumWithCallback関数に渡すコールバック関数 */
        const callback = (n: number) => {
          return (m: number): number => {
            return n + m;
          };
        };
        expect(
          listLocal.sumWithCallback(numbers)(0)(callback)
        ).toEqual(
          6  // 1 + 2 + 3 = 6
        );
        /* #@range_end(list_sum_callback_test) */
        expect(
          listLocal.sum(numbers)(0)
        ).toEqual(
          6
        );
      });

      // **リスト7.54** length関数の定義
      it('length関数の定義', () => {
        const listLocal = {
          match: <T, R>(data: List<T>, pattern: { empty: () => R; cons: (value: T, alist: List<T>) => R }): R => {
            return data.call(listLocal, pattern);
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
          /* #@range_begin(list_length) */
          length: <T>(alist: List<T>) => {
            return (accumulator: number): number => {
              return listLocal.match(alist, {
                empty: () => {
                  return accumulator;
                },
                cons: (head: T, tail: List<T>) => {
                  return listLocal.length(tail)(accumulator + 1);
                }
              });
            };
          },
          /* #@range_end(list_length) */
          // **リスト7.55** length関数の再定義
          /* #@range_begin(list_length_callback) */
          lengthWithCallback: <T>(alist: List<T>) => {
            return (accumulator: number) => {
              return (CALLBACK: (head: T) => (acc: number) => number): number => { // コールバック関数を受け取る
                return listLocal.match(alist, {
                  empty: () => {
                    return accumulator;
                  },
                  cons: (head: T, tail: List<T>) => {
                    return CALLBACK(head)(
                      listLocal.lengthWithCallback(tail)(accumulator)(CALLBACK)
                    );
                  }
                });
              };
            };
          }
          /* #@range_end(list_length_callback) */
        };
        const numbers = listLocal.cons(1,
          listLocal.cons(2,
            listLocal.cons(3,
              listLocal.empty<number>())));
        expect(
          listLocal.length(numbers)(0)
        ).toEqual(
          3
        );
        // **リスト7.56** lengthWithCallback関数でリストの長さをテストする
        /* #@range_begin(list_length_callback_test) */
        /* lengthWithCallback関数に渡すコールバック関数 */
        const callback = (n: number) => {
          return (m: number): number => {
            return 1 + m;
          };
        };
        expect(
          listLocal.lengthWithCallback(numbers)(0)(callback)
        ).toEqual(
          3
        );
        /* #@range_end(list_length_callback_test) */
      });
    });

    describe('畳み込み関数', () => {
      // **リスト7.58** リストの畳み込み関数
      const foldr = <T, R>(alist: List<T>) => {
        return (accumulator: R) => {
          return (callback: (item: T) => (acc: R) => R): R => {
            return list.match<T, R>(alist, {
              empty: () => {
                return accumulator;
              },
              cons: (head: T, tail: List<T>) => {
                return callback(head)(foldr<T, R>(tail)(accumulator)(callback));
              }
            });
          };
        };
      };

      // ** リスト7.59** foldr関数によるsum関数とlength関数の定義
      it("foldr関数によるsum関数", () => {
        /* #@range_begin(list_last_compose) */
        const sum = (alist: List<number>): number => {
          return foldr<number, number>(alist)(0)((item: number) => {
            return (accumulator: number): number => {
              return accumulator + item;
            };
        /* #@range_end(list_last_compose) */
          });
        };
        /* list = [1,2,3,4] */
        const seq = list.cons(1, list.cons(2, list.cons(3, list.cons(4, list.empty<number>()))));
        expect(
          sum(seq)
        ).toEqual(
          10  // 1 + 2 + 3 + 4 = 10
        );
      });

      /* foldr関数によるlength関数 */
      it("foldrでlength関数を作る", () => {
        /* #@range_begin(foldr_length) */
        const length = <T>(alist: List<T>): number => {
          return foldr<T, number>(alist)(0)((item: T) => {
            return (accumulator: number): number => {
              return accumulator + 1;
            };
          });
        };
        /* #@range_end(foldr_length) */
        /* list = [1,2,3,4] */
        const seq = list.cons(1, list.cons(2, list.cons(3, list.cons(4, list.empty<number>()))));
        expect(
          length(seq)
        ).toEqual(
          4
        );
      });

      // **表7.2** 反復処理における蓄積変数の初期値とコールバック関数の関係
      describe('反復処理における蓄積変数の初期値とコールバック関数の関係', () => {
        it("foldrでproductを作る", () => {
          /* #@range_begin(foldr_product) */
          const product = (alist: List<number>): number => {
            return foldr<number, number>(alist)(1)((item: number) => {
              return (accumulator: number): number => {
                return accumulator * item;
              };
            });
          };
          /********* テスト **********/
          /* list = [1,2,3] */
          const seq = list.cons(1,
            list.cons(2,
              list.cons(3,
                list.empty<number>())));
          expect(
            product(seq)
          ).toEqual(
            6 // 1 * 2 * 3 = 6
          );
          /* #@range_end(foldr_product) */
        });

        it("foldrでallを作る", () => {
          const all = (alist: List<boolean>): boolean => {
            return foldr<boolean, boolean>(alist)(true)((item: boolean) => {
              return (accumulator: boolean): boolean => {
                return accumulator && item;
              };
            });
          };
          /********* テスト **********/
          const allTrueList = list.cons(true,
            list.cons(true,
              list.cons(true,
                list.empty<boolean>())));
          expect(
            all(allTrueList)
          ).toEqual(
            true
          );
          const someFalseList = list.cons(true,
            list.cons(false,
              list.cons(true,
                list.empty<boolean>())));
          expect(
            all(someFalseList)
          ).toEqual(
            false
          );
        });

        it("foldrでanyを作る", () => {
          const any = (alist: List<boolean>): boolean => {
            return foldr<boolean, boolean>(alist)(false)((item: boolean) => {
              return (accumulator: boolean): boolean => {
                return accumulator || item;
              };
            });
          };
          /********* テスト **********/
          const allTrueList = list.cons(true,
            list.cons(true,
              list.cons(true,
                list.empty<boolean>())));
          expect(
            any(allTrueList)
          ).toEqual(
            true
          );
          const someFalseList = list.cons(true,
            list.cons(false,
              list.cons(true,
                list.empty<boolean>())));
          expect(
            any(someFalseList)
          ).toEqual(
            true
          );
        });
      });

      // **リス7.60** foldr関数によるreverse関数の定義
      it("foldr関数によるreverse関数の定義", () => {
        const listLocal = {
          match: <T, R>(data: List<T>, pattern: { empty: () => R; cons: (value: T, alist: List<T>) => R }): R => {
            return data.call(listLocal, pattern);
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
          toArray: <T>(alist: List<T>): T[] => {
            const toArrayAux = (alist: List<T>, accumulator: T[]): T[] => {
              return listLocal.match(alist, {
                empty: () => {
                  return accumulator;
                },
                cons: (head: T, tail: List<T>) => {
                  return toArrayAux(tail, accumulator.concat(head));
                }
              });
            };
            return toArrayAux(alist, []);
          },
          /* #@range_begin(foldr_reverse) */
          /* listのappend関数は、2つのリストを連結する */
          append: <T>(xs: List<T>) => {
            return (ys: List<T>): List<T> => {
              return listLocal.match(xs, {
                empty: () => {
                  return ys;
                },
                cons: (head: T, tail: List<T>) => {
                  return listLocal.cons(head, listLocal.append(tail)(ys));
                }
              });
            };
          },
          /* list.reverse関数は、リストを逆転する */
          reverse: <T>(alist: List<T>): List<T> => {
            const foldrLocal = <U, R>(alist: List<U>) => {
              return (accumulator: R) => {
                return (callback: (item: U) => (acc: R) => R): R => {
                  return listLocal.match<U, R>(alist, {
                    empty: () => {
                      return accumulator;
                    },
                    cons: (head: U, tail: List<U>) => {
                      return callback(head)(foldrLocal<U, R>(tail)(accumulator)(callback));
                    }
          /* #@range_end(foldr_reverse) */
                  });
                };
              };
            };
            return foldrLocal<T, List<T>>(alist)(listLocal.empty<T>())((item: T) => {
              return (accumulator: List<T>): List<T> => {
                return listLocal.append(accumulator)(listLocal.cons(item, listLocal.empty<T>()));
              };
            });
          }
        };
        /* list = [1,2,3,4] */
        const seq = listLocal.cons(1,
          listLocal.cons(2,
            listLocal.cons(3,
              listLocal.cons(4,
                listLocal.empty<number>()))));
        expect(
          listLocal.toArray(listLocal.reverse(seq))
        ).toEqual(
          [4, 3, 2, 1]
        );
      });

      // **リスト7.61** foldr関数によるfind関数の定義
      it("foldr関数によるfind関数の定義", () => {
        const even = (n: number): boolean => {
          return (n % 2) === 0;
        };
        const listLocal = {
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
          /* #@range_begin(foldr_find) */
          /* list.find関数は、条件に合致した要素をリストから探す */
          find: <T>(alist: List<T>) => {
            return (predicate: (item: T) => boolean): T | null => { // 要素を判定する述語関数
              return foldr<T, T | null>(alist)(null)((item: T) => { // foldrを利用する
                return (accumulator: T | null): T | null => {
                  /* 要素が見つかった場合、その要素を返す */
                  if (predicate(item) === true) {
                    return item;
                  } else {
                    return accumulator;
                  };
                };
              });
            };
          }
          /* #@range_end(foldr_find) */
        };
        /******** テスト *********/
        const numbers = listLocal.cons(1,
          listLocal.cons(2,
            listLocal.cons(3,
              listLocal.empty<number>())));
        expect(
          listLocal.find(numbers)(even) // 最初に登場する偶数の要素を探す
        ).toEqual(
          2
        );
      });

      it("foldrで map関数を作る", () => {
        const double = (number: number): number => {
          return number * 2;
        };
        const map = <T, U>(alist: List<T>) => {
          return (callback: (item: T) => U): List<U> => { // 個々の要素を変換するコールバック関数
            return foldr<T, List<U>>(alist)(list.empty<U>())((item: T) => {
              return (accumulator: List<U>): List<U> => {
                return list.cons(callback(item), accumulator);
              };
            });
          };
        };
        /****** テスト ******/
        /* list = [1,2,3] */
        const seq = list.cons(1,
          list.cons(2,
            list.cons(3,
              list.empty<number>())));
        expect(
          list.toArray(map(seq)(double))
        ).toEqual(
          [2, 4, 6] // 2 * [1,2,3] = [2,4,6]
        );
      });
    });

    // #### コラム：配列の畳み込み関数
    describe("コラム：配列の畳み込み関数", () => {
      // **リスト7.62** reduceメソッドによるfromArray関数
      it("reduceメソッドによるfromArray関数", () => {
        /* #@range_begin(list_fromArray) */
        const fromArray = <T>(array: T[]): List<T> => {
          return array.reduce((accumulator: List<T>, item: T) => {
            return list.append(accumulator)(list.cons(item, list.empty<T>()));
          }, list.empty<T>());
        };
        /******* テスト *******/
        const theList = fromArray([0, 1, 2, 3]);
        expect(
          list.toArray(theList)
        ).toEqual(
          [0, 1, 2, 3]
        );
        /* #@range_end(list_fromArray) */
      });
    });
  });

  // ### <section id='asynchronous'>非同期処理にコールバック関数を渡す</section>
  describe('非同期処理にコールバック関数を渡す', () => {
    // **リスト7.64** tarai関数の定義
    it("tarai関数の定義", () => {
      /* #@range_begin(tarai_function) */
      /* たらいまわし関数 */
      const tarai = (x: number, y: number, z: number): number => {
        if (x > y) {
          return tarai(tarai(x - 1, y, z),
            tarai(y - 1, z, x),
            tarai(z - 1, x, y));
        } else {
          return y;
        }
      };
      expect(
        tarai(1 * 2, 1, 0)
      ).toEqual(
        2
      );
      /* #@range_end(tarai_function) */
    });
  });

  // ### <section id='continuation'>継続で未来を渡す</section>
  describe('継続で未来を渡す', () => {
    // #### 継続とは何か
    describe('継続とは何か', () => {
      // **リスト7.67** 継続渡しのsucc関数
      it("継続渡しのsucc関数", () => {
        /* #@range_begin(succ_cps) */
        /* continues関数は、succ(n)のあとに続く継続 */
        const succ = <R>(n: number, continues: (x: number) => R): R => {
          return continues(n + 1);
        };
        /* #@range_end(succ_cps) */

        // **リスト7.68** 継続渡しのsucc関数をテストする
        const identity = <T>(any: T): T => {
          return any;
        };

        /* #@range_begin(succ_cps_test) */
        /* identity関数を継続として渡すことで、
           succ(1)の結果がそのまま返る */
        expect(
          succ(1, identity)
        ).toEqual(
          2
        );
        /* #@range_end(succ_cps_test) */
      });

      // **リスト7.70** add(2, succ(3))の継続渡し
      it("add(2, succ(3))の継続渡し", () => {
        const identity = <T>(any: T): T => { // 値をそのまま返すだけの継続
          return any;
        };
        /* #@range_begin(continuation_in_arithmetic) */
        /* 継続渡しのsucc関数 */
        const succ = <R>(n: number, continues: (x: number) => R): R => {
          return continues(n + 1);
        };
        /* 継続渡しのadd関数 */
        const add = <R>(n: number, m: number, continues: (x: number) => R): R => {
          return continues(n + m);
        };
        /* 継続渡しのsucc関数とadd関数を使って
           add(2, succ(3)) を計算する */
        expect(
          succ(3, (succResult: number) => {
            return add(2, succResult, identity);
          })
        ).toEqual(
          6
        );
        /* #@range_end(continuation_in_arithmetic) */
      });
    });

    describe("継続で未来を選ぶ", () => {
      // **リスト7.71** 継続による反復処理からの脱出
      it("継続による反復処理からの脱出", () => {
        /* #@range_begin(stream_find_cps) */
        const find = <T>(
          aStream: Stream<T>,
          predicate: (item: T) => boolean,
          continuesOnFailure: (
            s: Stream<T>,
            p: (item: T) => boolean,
            cf: any,
            cs: (result: T | null) => T | null
          ) => T | null,
          continuesOnSuccess: (result: T | null) => T | null
        ): T | null => {
          return (aStream as any)({
            /* リストの最末尾に到着した場合
               成功継続で反復処理を抜ける */
            empty: () => {
              return continuesOnSuccess(null);
            },
            cons: (head: T, tailThunk: () => Stream<T>) => {
              /* 目的の要素を見つけた場合
                 成功継続で反復処理を脱出する */
              if (predicate(head) === true) {
                return continuesOnSuccess(head);
              } else {
                /* 目的の要素を見つけられなった場合、
                   失敗継続で次の反復処理を続ける */
                return continuesOnFailure(tailThunk(),
                  predicate,
                  continuesOnFailure,
                  continuesOnSuccess);
              };
            }
          });
        };
        /* #@range_end(stream_find_cps) */

        // find関数に渡す2つの継続
        const identity = <T>(any: T): T => {
          return any;
        };
        // **リスト7.72** find関数に渡す2つの継続
        /* #@range_begin(stream_find_continuations) */
        /* 成功継続では、反復処理を脱出する */
        const continuesOnSuccess = identity;

        /* 失敗継続では、反復処理を続ける */
        const continuesOnFailure = <T>(
          aStream: Stream<T>,
          predicate: (item: T) => boolean,
          continuesOnRecursion: any,
          escapesFromRecursion: (result: T | null) => T | null
        ): T | null => {
          /* find関数を再帰的に呼び出す */
          return find(
            aStream,
            predicate,
            continuesOnRecursion,
            escapesFromRecursion
          );
        };
        /* #@range_end(stream_find_continuations) */

        // **リスト7.73** find関数のテスト
        /* upto3変数は、1から3までの有限ストリーム */
        const upto3 = stream.cons(1, () => {
          return stream.cons(2, () => {
            return stream.cons(3, () => {
              return stream.empty<number>();
            });
          });
        });
        expect(
          find(upto3, (item: number) => {
            return (item === 4); // 4を探します
          }, continuesOnFailure, continuesOnSuccess)
        ).toEqual(
          null // リスト中に4の要素はないので、nullになります
        );
        /* #@range_begin(stream_find_cps_test) */
        /* 変数integersは、無限の整数ストリーム */
        const integers = stream.enumFrom(0);

        /* 無限の整数列のなかから100を探す */
        expect(
          find(integers, (item: number) => {
            return (item === 100);
          }, continuesOnFailure, continuesOnSuccess)
        ).toEqual(
          100 // 100を見つけて返ってくる
        );
        /* #@range_end(stream_find_cps_test) */
      });
    });

    // #### 非決定計算機を作る
    describe("非決定計算機を作る", () => {
      // 式の型定義
      type Exp = (pattern: ExpPattern) => any;

      interface ExpPattern {
        amb: (alist: List<Exp>) => any;
        num: (n: number) => any;
        add: (exp1: Exp, exp2: Exp) => any;
      }

      const exp = {
        match: (anExp: Exp, pattern: ExpPattern): any => {
          return anExp.call(exp, pattern);
        },
        // **リスト7.75** 非決定計算機の式
        /* #@range_begin(amb_expression) */
        amb: (alist: List<Exp>): Exp => {
          return (pattern: ExpPattern) => {
            return pattern.amb(alist);
          };
        },
        /* #@range_end(amb_expression) */
        num: (n: number): Exp => {
          return (pattern: ExpPattern) => {
            return pattern.num(n);
          };
        },
        add: (exp1: Exp, exp2: Exp): Exp => {
          return (pattern: ExpPattern) => {
            return pattern.add(exp1, exp2);
          };
        }
      };

      /* #@range_begin(amb_calculate) */
      // <section id='amb_calculate'>非決定性計算機の評価関数</section>
      const calculate = (
        anExp: Exp,
        continuesOnSuccess: (value: number, continuesOnFailure: () => number | null) => number | null,
        continuesOnFailure: () => number | null
      ): number | null => {
        /* 式に対してパターンマッチを実行する */
        return exp.match(anExp, {
          // **リスト7.79** 数値の評価
          /* #@range_begin(amb_calculate_num) */
          /* 数値を評価する */
          num: (n: number) => {
            return continuesOnSuccess(n, continuesOnFailure);
          },
          /* #@range_end(amb_calculate_num) */
          // **リスト7.80** 足し算の評価
          /* #@range_begin(amb_calculate_add) */
          /* 足し算の式を評価する */
          add: (x: Exp, y: Exp) => {
            /* まず引数xを評価する */
            return calculate(x, (resultX: number, continuesOnFailureX: () => number | null) => {
              /* 次に引数yを評価する */
              return calculate(y, (resultY: number, continuesOnFailureY: () => number | null) => {
                /* 引数xとyがともに成功すれば、両者の値で足し算を計算する */
                return continuesOnSuccess(resultX + resultY, continuesOnFailureY);
              }, continuesOnFailureX); /* y の計算に失敗すれば、xの失敗継続を渡す */
            }, continuesOnFailure);    /* x の計算に失敗すれば、おおもとの失敗継続を渡す */
          },
          /* #@range_end(amb_calculate_add) */
          // **リスト7.81** amb式の評価
          /* #@range_begin(amb_calculate_amb) */
          /* amb式を評価する */
          amb: (choices: List<Exp>) => {
            const calculateAmb = (choices: List<Exp>): number | null => {
              return list.match(choices, {
                /*
                   amb(list.empty()) の場合、
                   すなわち選択肢がなければ、失敗継続を実行する
                */
                empty: () => {
                  return continuesOnFailure();
                },
                /*
                   amb(list.cons(head, tail))の場合、
                   先頭要素を計算して、後尾は失敗継続に渡す
                */
                cons: (head: Exp, tail: List<Exp>) => {
                  return calculate(head, continuesOnSuccess, () => {
                    /* 失敗継続で後尾を計算する */
                    return calculateAmb(tail);
                  });
                }
              });
            };
            return calculateAmb(choices);
          }
          /* #@range_end(amb_calculate_amb) */
        });
      };
      /* #@range_end(amb_calculate) */

      // **リスト7.82** 非決定計算機の駆動関数
      /* #@range_begin(amb_driver) */
      const driver = (expression: Exp) => {
        /* 中断された計算を継続として保存する変数 */
        let suspendedComputation: (() => number | null) | null = null;
        /* 成功継続 */
        const continuesOnSuccess = (anyValue: number, continuesOnFailure: () => number | null): number | null => {
          /* 再開に備えて、失敗継続を保存しておく */
          suspendedComputation = continuesOnFailure;
          return anyValue;
        };
        /* 失敗継続 */
        const continuesOnFailure = (): number | null => {
          return null;
        };

        /* 内部に可変な状態suspendedComputationを持つクロージャーを返す */
        return (): number | null => {
          /* 中断された継続がなければ、最初から計算する */
          if (suspendedComputation === null) {
            return calculate(expression,
              continuesOnSuccess,
              continuesOnFailure);
          } else { /* 中断された継続があれば、その継続を実行する */
            return suspendedComputation();
          }
        };
      };
      /* #@range_end(amb_driver) */

      it("amb[1,2] + 3  = amb[4, 5]", () => {
        const ambExp = exp.add(exp.amb(list.cons(exp.num(1), list.cons(exp.num(2), list.empty<Exp>()))),
          exp.num(3));
        const calculator = driver(ambExp);
        expect(
          calculator()
        ).toEqual(
          4 // 1 + 3 = 4
        );
        expect(
          calculator()
        ).toEqual(
          5 // 2 + 3 = 5
        );
        expect(
          calculator()
        ).toEqual(
          null
        );
      });

      // **リスト7.83** 非決定計算機のテスト
      it("非決定計算機のテスト", () => {
        /* #@range_begin(amb_test) */
        /* amb[1,2] + amb[3,4] = amb[4, 5, 5, 6] */
        /* amb[1,2] + amb[3,4] = 4, 5, 5, 6 */
        const ambExp = exp.add(
          exp.amb(list.fromArray([exp.num(1), exp.num(2)])),
          exp.amb(list.fromArray([exp.num(3), exp.num(4)])));
        const calculator = driver(ambExp);
        expect(
          calculator()
        ).toEqual(
          4 // 1 + 3 = 4
        );
        expect(
          calculator()
        ).toEqual(
          5 // 2 + 3 = 5
        );
        expect(
          calculator()
        ).toEqual(
          5 // 1 + 4 = 5
        );
        expect(
          calculator()
        ).toEqual(
          6 // 2 + 4 = 6
        );
        expect(
          calculator()
        ).toEqual(
          null // これ以上の候補はないので、計算は終了
        );
        /* #@range_end(amb_test) */
      });

      it("amb[1,2,3] + amb[10,20] = amb[11,21,12,22,13,23]", () => {
        const ambExp = exp.add(
          exp.amb(list.fromArray([exp.num(1), exp.num(2), exp.num(3)])),
          exp.amb(list.fromArray([exp.num(10), exp.num(20)])));
        const calculator = driver(ambExp);
        expect(
          calculator()
        ).toEqual(
          11 // 1 + 10 = 11
        );
        expect(
          calculator()
        ).toEqual(
          21 // 1 + 20 = 21
        );
        expect(
          calculator()
        ).toEqual(
          12 // 2 + 10 = 12
        );
        expect(
          calculator()
        ).toEqual(
          22 // 2 + 20 = 22
        );
        expect(
          calculator()
        ).toEqual(
          13 // 3 + 10 = 13
        );
        expect(
          calculator()
        ).toEqual(
          23 // 3 + 20 = 23
        );
        expect(
          calculator()
        ).toEqual(
          null // これ以上の候補はないので、計算は終了
        );
      });

      it("amb[1,2] + amb[10,20,30] = amb[11,21,31,12,22,32]", () => {
        const ambExp = exp.add(
          exp.amb(list.fromArray([exp.num(1), exp.num(2)])),
          exp.amb(list.fromArray([exp.num(10), exp.num(20), exp.num(30)])));
        const calculator = driver(ambExp);
        expect(
          calculator()
        ).toEqual(
          11 // 1 + 10 = 11
        );
        expect(
          calculator()
        ).toEqual(
          21 // 1 + 20 = 21
        );
        expect(
          calculator()
        ).toEqual(
          31 // 1 + 30 = 31
        );
        expect(
          calculator()
        ).toEqual(
          12 // 2 + 10 = 12
        );
        expect(
          calculator()
        ).toEqual(
          22 // 2 + 20 = 22
        );
        expect(
          calculator()
        ).toEqual(
          32 // 2 + 30 = 32
        );
        expect(
          calculator()
        ).toEqual(
          null // これ以上の候補はないので、計算は終了
        );
      });
    });
  }); // 継続を渡す
}); // 関数を渡す

// ## 7.6 <section id='monad'>モナドを作る</section>
describe('モナドを作る', () => {
  const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B) => {
    return (arg: A): C => {
      return f(g(arg));
    };
  };

  // ### <section id='identity-monad'>恒等モナド</section>
  describe('恒等モナド', () => {
    // **リスト7.85** 恒等モナドの定義
    const ID = {
      /* #@range_begin(identity_monad) */
      /* unit:: T => ID[T] */
      unit: <T>(value: T): T => {  // 単なる identity関数と同じ
        return value;
      },
      /* flatMap:: ID[T] => FUN[T => ID[T]] => ID[T] */
      flatMap: <T, U>(instanceM: T) => {
        return (transform: (x: T) => U): U => {
          return transform(instanceM); // 単なる関数適用と同じ
        };
      },
      /* #@range_end(identity_monad) */
      compose: <A, B, C>(f: (a: A) => B, g: (b: B) => C) => {
        return (x: A): C => {
          return ID.flatMap<B, C>(f(x))(g);
        };
      }
    };

    // **リスト7.86** 恒等モナドunit関数のテスト
    it("恒等モナドunit関数のテスト", () => {
      expect(
        ID.unit(1)
      ).toEqual(
        1
      );
    });

    // **リスト7.87** 恒等モナドflatMap関数のテスト
    it("恒等モナドflatMap関数のテスト", () => {
      const succ = (n: number): number => {
        return n + 1;
      };
      expect(
        ID.flatMap(ID.unit(1))((one: number) => {
          return ID.unit(succ(one));
        })
      ).toEqual(
        succ(1)
      );
      const double = (m: number): number => {
        return m * 2;
      };
      // **リスト7.88** flatMapと関数合成の類似性
      /* #@range_begin(multipleOf_is_transparent) */
      expect(
        ID.flatMap(ID.unit(1))((one: number) => {
          /* succ関数を適用する */
          return ID.flatMap(ID.unit(succ(one)))((two: number) => {
            /* double関数を適用する */
            return ID.unit(double(two));
          });
        })
      ).toEqual(
        compose(double, succ)(1)
      );
      /* #@range_end(multipleOf_is_transparent) */
    });

    // **リスト7.89**  恒等モナドのモナド則
    describe("恒等モナドのモナド則", () => {
      /* #@range_begin(identity_monad_laws) */
      it("flatMap(instanceM)(unit) === instanceM", () => {
        /* flatMap(instanceM)(unit) === instanceM の一例 */
        const instanceM = ID.unit(1);
        // 右単位元則
        expect(
          ID.flatMap(instanceM)(ID.unit)
        ).toEqual(
          instanceM
        );
      });

      it("flatMap(unit(value))(f) == f(value)", () => {
        /* flatMap(unit(value))(f) === f(value) */
        const f = (n: number): number => {
          return ID.unit(n + 1);
        };
        // 左単位元則
        expect(
          ID.flatMap(ID.unit(1))(f)
        ).toEqual(
          f(1)
        );
      });

      it("flatMap(flatMap(instanceM)(f))(g) == flatMap(instanceM)((x) => flatMap(f(x))(g))", () => {
        /*
           flatMap(flatMap(instanceM)(f))(g)
           ===
           flatMap(instanceM)((x) => {
              return flatMap(f(x))(g); }
           }
        */
        const f = (n: number): number => {
          return ID.unit(n + 1);
        };
        const g = (n: number): number => {
          return ID.unit(-n);
        };
        const instanceM = ID.unit(1);
        // 結合法則
        expect(
          ID.flatMap<number, number>(ID.flatMap<number, number>(instanceM)(f))(g)
        ).toEqual(
          ID.flatMap<number, number>(instanceM)((x: number) => {
            return ID.flatMap<number, number>(f(x))(g);
          })
        );
        /* #@range_end(identity_monad_laws) */
      });
    });
  });

  // ### <section id='maybe-monad'>Maybeモナドでエラーを処理する</section>
  describe('Maybeモナドでエラーを処理する', () => {
    describe('Maybeモナドを作る', () => {
      // Maybe型の定義
      type Maybe<T> = (pattern: MaybePattern<T>) => any;

      interface MaybePattern<T> {
        just: (value: T) => any;
        nothing: (_?: any) => any;
      }

      // **リスト7.91** Maybeの代数的構造
      /* #@range_begin(algebraic_type_maybe) */
      const maybe = {
        match: <T, R>(exp: Maybe<T>, pattern: { just: (value: T) => R; nothing: (_?: any) => R }): R => {
          return exp.call(pattern, pattern);
        },
        just: <T>(value: T): Maybe<T> => {
          return (pattern: MaybePattern<T>) => {
            return pattern.just(value);
          };
        },
        nothing: <T>(): Maybe<T> => {
          return (pattern: MaybePattern<T>) => {
            return pattern.nothing();
          };
        }
      };
      /* #@range_end(algebraic_type_maybe) */

      // **リスト7.92** Maybeモナドの定義
      const MAYBE = {
        /* #@range_begin(maybe_monad) */
        /* unit:: T => MAYBE[T] */
        unit: <T>(value: T): Maybe<T> => {
          return maybe.just(value);
        },
        /* flatMap:: MAYBE[T] => FUN[T => MAYBE[U]] => MAYBE[U] */
        flatMap: <T, U>(instanceM: Maybe<T>) => {
          return (transform: (x: T) => Maybe<U>): Maybe<U> => {
            return maybe.match<T, Maybe<U>>(instanceM, {
              /* 正常な値の場合は、transform関数を計算する */
              just: (value: T) => {
                return transform(value);
              },
              /* エラーの場合は、何もしない */
              nothing: () => {
                return maybe.nothing<U>();
              }
            });
          };
        },
        /* ヘルパー関数  */
        getOrElse: <T>(instanceM: Maybe<T>) => {
          return (alternate: T | null): T | null => {
            return maybe.match(instanceM, {
              just: (value: T) => {
                return value;
              },
              nothing: () => {
                return alternate;
              }
            });
          };
        },
        /* #@range_end(maybe_monad) */
      };

      // **リスト7.93** Maybeモナドの利用法
      it("Maybeモナドの利用法", () => {
        /* #@range_begin(maybe_monad_add_test) */
        /* 足し算を定義する */
        const add = (maybeA: Maybe<number>, maybeB: Maybe<number>): Maybe<number> => {
          return MAYBE.flatMap<number, number>(maybeA)((a: number) => {
            return MAYBE.flatMap<number, number>(maybeB)((b: number) => {
              return MAYBE.unit(a + b);
            });
          });
        };
        const justOne = maybe.just(1);
        const justTwo = maybe.just(2);

        expect(
          MAYBE.getOrElse(add(justOne, justOne))(null)
        ).toEqual(
          2
        );
        expect(
          MAYBE.getOrElse(add(justOne, maybe.nothing<number>()))(null)
        ).toEqual(
          null
        );
        /* #@range_end(maybe_monad_add_test) */
      });
    });
  });

  // ### <section id='io-monad'>IOモナドで副作用を閉じ込める</section>
  describe('IOモナドで副作用を閉じ込める', () => {
    const match = <T, R>(data: any, pattern: any): R => {
      return data.call(pattern, pattern);
    };

    // Pair型の定義
    type Pair<L, R> = (pattern: PairPattern<L, R>) => any;

    interface PairPattern<L, R> {
      cons: (left: L, right: R) => any;
    }

    // **リスト7.94** Pair型の定義
    /* #@range_begin(pair_datatype) */
    const pair = {
      /* pair のデータ構造 */
      cons: <L, R>(left: L, right: R): Pair<L, R> => {
        return (pattern: PairPattern<L, R>) => {
          return pattern.cons(left, right);
        };
      },
      /* ペアの右側を取得する */
      right: <L, R>(tuple: Pair<L, R>): R => {
        return match(tuple, {
          cons: (left: L, right: R) => {
            return right;
          }
        });
      },
      /* ペアの左側を取得する */
      left: <L, R>(tuple: Pair<L, R>): L => {
        return match(tuple, {
          cons: (left: L, right: R) => {
            return left;
          }
        });
      }
    };
    /* #@range_end(pair_datatype) */

    // **リスト7.95** 外界を明示したIOモナドの定義
    describe('外界を明示したIOモナドの定義', () => {
      type World = any;
      type IO<T> = (world: World) => Pair<T, World>;

      const IO = {
        /* #@range_begin(io_monad_definition_with_world) */
        /* unit:: T => IO[T] */
        unit: <T>(any: T): IO<T> => {
          return (world: World) => {  // worldは現在の外界
            return pair.cons(any, world);
          };
        },
        /* flatMap:: IO[T] => FUN[T => IO[U]] => IO[U] */
        flatMap: <T, U>(instanceA: IO<T>) => {
          return (actionAB: (x: T) => IO<U>): IO<U> => { // actionAB:: FUN[T => IO[U]]
            return (world: World) => {
              const newPair = instanceA(world); // 現在の外界のなかで instanceAのIOアクションを実行する
              return match<Pair<T, World>, Pair<U, World>>(newPair, {
                cons: (value: T, newWorld: World) => {
                  return actionAB(value)(newWorld); // 新しい外界のなかで、actionAB(value)で作られたIOアクションを実行する
                }
              });
            };
          };
        },
        /* #@range_end(io_monad_definition_with_world) */
        // **リスト7.96** IOモナドの補助関数
        /* done:: T => IO[T] */
        done: <T>(any: T): IO<void> => {
          return IO.unit(undefined);
        },
        /* run:: IO[A] => A */
        run: <T>(instance: IO<T>) => {
          return (world: World): T => {
            const newPair = instance(world); // IOモナドのインスタンス(アクション)を現在の外界に適用する
            return pair.left(newPair);     // 結果だけを返す
          };
        },
        println: (message: string): IO<null> => {
          return (world: World) => { // IOモナドを返す
            console.log(message);
            return IO.unit<null>(null)(world);
          };
        }
      }; // IO monad

      // **リスト7.98** run関数の利用法
      /* #@range_begin(run_println) */
      /* 初期の外界に null をバインドする */
      const initialWorld: World = null;
      expect(
        IO.run(IO.println("我輩は猫である"))(initialWorld)
      ).toEqual(
        null
      );
      /* #@range_end(run_println) */
    });

    describe('外界を引数に持たないIOモナド', () => {
      // **リスト7.99** 外界を明示しないIOモナドの定義
      /* #@range_begin(io_monad_definition) */
      type IO<T> = () => T;

      const IO = {
        /* unit:: T => IO[T] */
        unit: <T>(any: T): IO<T> => {
          return () => {  // 外界を明示する必要はない
            return any;
          };
        },
        /* flatMap:: IO[T] => FUN[T => IO[U]] => IO[U] */
        flatMap: <T, U>(instanceA: IO<T>) => {
          return (actionAB: (x: T) => IO<U>): IO<U> => { // actionAB:: a -> IO[b]
            return () => {
              return IO.run(actionAB(IO.run(instanceA)));
            }
          };
        },
        /* done:: T => IO[T] */
        done: <T>(any: T): IO<void> => {
          return IO.unit(undefined);
        },
        /* run:: IO[A] => A */
        run: <T>(instance: IO<T>): T => {
          return instance();
        },
        /* readFile:: STRING => IO[STRING] */
        readFile: (path: string): IO<string> => {
          return () => {
            const content = fs.readFileSync(path, 'utf8');
            return IO.unit(content)();
          };
        },
        /* println:: STRING => IO[null] */
        println: (message: string): IO<null> => {
          return () => {
            console.log(message);
            return IO.unit<null>(null)();
          };
        },
        writeFile: (path: string) => {
          return (content: string): IO<null> => {
            return () => {
              fs.writeFileSync(path, content);
              return IO.unit<null>(null)();
            };
          };
        }
      }; // IO monad
      /* #@range_end(io_monad_definition) */

      // **リスト7.100** run関数の利用法
      it('run関数の利用法', () => {
    /* #@range_begin(multipleOf_curried_test) */
        expect(
          /* 外界を指定する必要はありません */
          IO.run(IO.println("名前はまだない"))
        ).toEqual(
          null
        );
    /* #@range_end(multipleOf_curried_test) */
      });

      // #### IOアクションを合成する
      describe('IOアクションを合成する', () => {
        /* #@range_begin(io_monad_is_composable) */
        // **リスト7.102** seq関数の定義
        /* IO.seq:: IO[a] => IO[b] => IO[b] */
        const seq = <A, B>(instanceA: IO<A>) => {
          return (instanceB: IO<B>): IO<B> => {
            return IO.flatMap<A, B>(instanceA)((a: A) => {
              return instanceB;
            });
          };
        };

        /* IO.putc:: CHAR => IO[] */
        const putc = (character: string): IO<null> => {
          return () => {
            process.stdout.write(character);
            return null;
          };
        };

        /* IO.puts:: LIST[CHAR] => IO[] */
        const puts = (alist: List<string>): IO<void> => {
          return match(alist, {
            empty: () => {
              return IO.done(null);
            },
            cons: (head: string, tail: List<string>) => {
              return seq(putc(head))(puts(tail));
            }
          });
        };
        /* #@range_end(io_monad_is_composable) */

        // **リスト7.103** stringモジュール
        /* #@range_begin(string_module) */
        const string = {
          /* 先頭文字を取得する */
          head: (str: string): string => {
            return str[0];
          },
          /* 後尾文字列を取得する */
          tail: (str: string): string => {
            return str.substring(1);
          },
          /* 空の文字列かどうかを判定する */
          isEmpty: (str: string): boolean => {
            return str.length === 0;
          },
          /* 文字列を文字のリストに変換する */
          toList: (str: string): List<string> => {
            if (string.isEmpty(str)) {
              return list.empty<string>();
            } else {
              return list.cons(string.head(str),
                string.toList(string.tail(str)));
            }
          }
        };
        /* #@range_end(string_module) */

        it('stringのテスト', () => {
          expect(
            string.head("abc")
          ).toEqual(
            'a'
          );
          expect(
            string.tail("abc")
          ).toEqual(
            'bc'
          );
        });
      });
    });
  }); // IOモナドで副作用を閉じ込める
}); // モナド

// [目次に戻る](index.html) [次章に移る](chap08.spec.html)
