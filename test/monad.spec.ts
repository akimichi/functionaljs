"use strict";

// さまざまなモナド
// =============
// ここでは、本書で紹介できなかったさまざまなモナドを紹介します。
// 参考までに、Haskellでの定義も随時併記しています。

import * as fs from 'fs';
// Pair型の読込
import * as Pair from '../lib/pair';
// String型の読込
import * as String from '../lib/string';

// ## <section id='id_monad'>恒等モナド</section>
// ### 恒等モナドの定義
const ID = {
  // **ID#unit**
  /* unit:: T => ID[T] */
  unit: <T>(value: T): T => {  // 単なる identity関数と同じ
    return value;
  },
  // **ID#flatMap**
  /* flatMap:: ID[T] => FUN[T => ID[T]] => ID[T] */
  flatMap: <T, U>(instanceM: T) => {
    return (transform: (value: T) => U): U => {
      return transform(instanceM); // 単なる関数適用と同じ
    };
  }
};

// ### 恒等モナドのテスト
describe("恒等モナドをテストする", () => {
  // **ID#unit**をテストする
  it("ID#unitをテストする", () => {
    expect(
      ID.unit(1)
    ).toEqual(
      1
    );
  });
});

// ## <section id='maybe_monad'>Maybeモナド</section>
// ### Maybeモナドの定義
// ~~~haskell
// instance Monad Maybe where
//   Nothing  >>= _ = Nothing
//   (Just x) >>= f = f x
// ~~~

interface MaybePattern<T, R> {
  just: (value: T) => R;
  nothing: (value?: any) => R;
}

type MaybeType<T> = <R>(pattern: MaybePattern<T, R>) => R;

const Maybe = {
  match: <T, R>(data: MaybeType<T>, pattern: MaybePattern<T, R>): R => {
    return data(pattern);
  },
  just: <T>(value: T): MaybeType<T> => {
    return <R>(pattern: MaybePattern<T, R>): R => {
      return pattern.just(value);
    };
  },
  nothing: <T>(_?: any): MaybeType<T> => {
    return <R>(pattern: MaybePattern<T, R>): R => {
      return pattern.nothing(_);
    };
  },
  // **Maybe#unit**
  unit: <T>(value: T): MaybeType<T> => {
    return Maybe.just(value);
  },
  // **Maybe#flatMap**
  flatMap: <T, U>(maybeInstance: MaybeType<T>) => {
    return (transform: (value: T) => MaybeType<U>): MaybeType<U> => {
      expect(typeof transform).toBe('function');
      return Maybe.match(maybeInstance, {
        just: (value: T) => {
          return transform(value);
        },
        nothing: (_) => {
          return Maybe.nothing<U>(_);
        }
      });
    };
  },
  // **Maybe#map**
  // ~~~haskell
  // instance Functor Maybe where
  //    fmap _ Nothing = Nothing
  //    fmap f (Just x) = Just (f x)
  // ~~~
  map: <T, U>(maybeInstance: MaybeType<T>) => {
    return (transform: (value: T) => U): MaybeType<U> => {
      expect(typeof transform).toBe('function');
      return Maybe.match(maybeInstance, {
        nothing: (_) => {
          return Maybe.nothing<U>(_);
        },
        just: (value: T) => {
          return Maybe.just(transform(value));
        }
      });
    };
  },
  // ~~~haskell
  // -- | Promote a function to a monad.
  // liftM :: (Monad m) => (a -> b) -> m a -> m b
  // liftM f m  = do { x <- m1; return (f x) }
  // ~~~
  liftM: <T, U>(f: (value: T) => U) => {
    return (ma: MaybeType<T>): MaybeType<U> => {
      return Maybe.flatMap<T, U>(ma)((x: T) => {
        return Maybe.unit(f(x));
      });
    };
  },
  // ~~~haskell
  // (<*>) :: (Monad m) => m (a -> b) -> m a -> m b
  // ~~~
  apply: <T, U>(mf: MaybeType<(value: T) => U>) => {
    return (ma: MaybeType<T>): MaybeType<U> => {
      return Maybe.flatMap<(value: T) => U, U>(mf)((f: (value: T) => U) => {
        return Maybe.flatMap<T, U>(ma)((a: T) => {
          return Maybe.unit(f(a));
        });
      });
    };
  },
  get: <T>(maybe: MaybeType<T>): T | null => {
    return Maybe.getOrElse(maybe)(null);
  },
  getOrElse: <T>(instance: MaybeType<T>) => {
    return (alternate: T | null): T | null => {
      return Maybe.match(instance, {
        just: (value: T) => {
          return value;
        },
        nothing: (_) => {
          return alternate;
        }
      });
    };
  },
  isEqual: <T>(maybeA: MaybeType<T>) => {
    return (maybeB: MaybeType<T>): boolean => {
      return Maybe.match(maybeA, {
        just: (valueA: T) => {
          return Maybe.match(maybeB, {
            just: (valueB: T) => {
              return (valueA === valueB);
            },
            nothing: (_) => {
              return false;
            }
          });
        },
        nothing: (_) => {
          return Maybe.match(maybeB, {
            just: (_) => {
              return false;
            },
            nothing: (_) => {
              return true;
            }
          });
        }
      });
    };
  }
};

// ### Maybeモナドのテスト
describe("Maybeモナドをテストする", () => {
  // **Maybe#flatMap**をテストする
  it("Maybe#flatMapをテストする", () => {
    Maybe.match(Maybe.flatMap(Maybe.just(1))((a) => {
      return Maybe.unit(a);
    }), {
      just: (value) => {
        expect(
          value
        ).toEqual(
          1
        );
      },
      nothing: (_) => {
        fail('Expected just, got nothing');
      }
    });
    Maybe.match(Maybe.flatMap(Maybe.nothing<number>())((a) => {
      return Maybe.unit(a);
    }), {
      just: (value) => {
        fail('Expected nothing, got just');
      },
      nothing: (_) => {
        expect(true).toBeTruthy();
      }
    });
  });
  // **Maybe#map**をテストする
  it("Maybe#mapをテストする", () => {
    const succ = (n: number) => { return n + 1; };
    // ~~~haskell
    // > fmap (+1) nothing
    // Nothing
    // ~~~
    expect(
      Maybe.isEqual(
        Maybe.map(Maybe.nothing<number>())(succ)
      )(
        Maybe.nothing<number>()
      )
    ).toEqual(
      true
    );
    // ~~~haskell
    // > fmap (succ) (Just 1)
    // Just 2
    // ~~~
    expect(
      Maybe.isEqual(
        Maybe.map(Maybe.just(1))(succ)
      )(
        Maybe.just(2)
      )
    ).toEqual(
      true
    );
  });
  // **Maybe#liftM**をテストする
  it("Maybe#liftMをテストする", () => {
    // ~~~haskell
    // > liftM (+3) (Just 2)
    // Just 5
    // ~~~
    const add3 = (n: number) => {
      return n + 3;
    };
    const justTwo = Maybe.just(2);
    const justFive = Maybe.just(5);
    expect(
      Maybe.isEqual(
        Maybe.liftM(add3)(Maybe.unit(2))
      )(
        justFive
      )
    ).toEqual(
      true
    );
  });
  // **Maybe#apply**をテストする
  it("Maybe#applyをテストする", () => {
    // ~~~haskell
    // > Just (+3) <*> (Just 2)
    // Just 5
    // ~~~
    const add3 = (n: number) => {
      return n + 3;
    };
    const justTwo = Maybe.just(2);
    const justFive = Maybe.just(5);
    expect(
      Maybe.isEqual(
        Maybe.apply(Maybe.just(add3))(Maybe.unit(2))
      )(
        justFive
      )
    ).toEqual(
      true
    );
  });
  // add関数でMaybeインスンスを足しあわせる
  it("add関数でMaybeインスンスを足しあわせる", () => {
    const add = (maybeA: MaybeType<number>, maybeB: MaybeType<number>) => {
      return Maybe.flatMap(maybeA)((a) => {
        return Maybe.flatMap(maybeB)((b) => {
          return Maybe.unit(a + b);
        });
      });
    };
    const justOne = Maybe.just(1);
    const justTwo = Maybe.just(2);
    const justThree = Maybe.just(3);
    expect(
      Maybe.isEqual(
        add(justOne, justTwo)
      )(
        justThree
      )
    ).toEqual(
      true
    );
    expect(
      Maybe.isEqual(
        add(justOne, Maybe.nothing<number>())
      )(
        Maybe.nothing<number>()
      )
    ).toEqual(
      true
    );
  });
});

// ## <section id='either_monad'>Eitherモナド</section>
// ### Eitherモナドの定義

interface EitherPattern<L, R, Result> {
  left: (value: L) => Result;
  right: (value: R) => Result;
}

type EitherType<L, R> = <Result>(pattern: EitherPattern<L, R, Result>) => Result;

const Either = {
  // ~~~haskell
  // data  Either a b  =  Left a | Right b
  // ~~~
  match: <L, R, Result>(data: EitherType<L, R>, pattern: EitherPattern<L, R, Result>): Result => {
    return data.call(data, pattern) as Result;
  },
  left: <L, R>(value: L): EitherType<L, R> => {
    return <Result>(pattern: EitherPattern<L, R, Result>): Result => {
      return pattern.left(value);
    };
  },
  right: <L, R>(value: R): EitherType<L, R> => {
    return <Result>(pattern: EitherPattern<L, R, Result>): Result => {
      return pattern.right(value);
    };
  },
  // ~~~haskell
  // instance Monad (Either a b) where
  //   return x = Right x
  //   Right x >>= f = f x
  //   Left x >>= Left x
  // ~~~
  // **Either#unit**
  unit: <L, R>(value: R): EitherType<L, R> => {
    return Either.right<L, R>(value);
  },
  // **Either#flatMap**
  flatMap: <L, R, R2>(instanceM: EitherType<L, R>) => {
    return (transform: (value: R) => EitherType<L, R2>): EitherType<L, R2> => {
      expect(typeof transform).toBe('function');
      return Either.match(instanceM, {
        right: (value: R) => {
          return transform(value);
        },
        left: (value: L) => {
          return Either.left<L, R2>(value);
        }
      });
    };
  },
  // **Either#map**
  // ~~~haskell
  // instance Functor (Either a) where
  //   fmap f (Right x) = Right (f x)
  //   fmap f (Left x) = Left x
  // ~~~
  map: <L, R, R2>(instanceM: EitherType<L, R>) => {
    return (transform: (value: R) => R2): EitherType<L, R2> => {
      return Either.match(instanceM, {
        right: (value: R) => {
          return Either.right<L, R2>(transform(value));
        },
        left: (value: L) => {
          return Either.left<L, R2>(value);
        }
      });
    };
  }
};

// ### Eitherモナドのテスト
describe("Eitherモナドをテストする", () => {
  // 数値のときだけ計算が成功するテスト
  it("数値のときだけ計算が成功するテスト", () => {
    Either.match(Either.flatMap(Either.left<string, number>("wrong"))((n) => {
      return Either.unit<string, number>(n + 1);
    }), {
      right: (value) => {
        fail('Expected left, got right');
      },
      left: (value) => {
        expect(
          value
        ).toEqual(
          "wrong"
        );
      }
    });
    Either.match(Either.flatMap(Either.unit<string, number>(2))((n) => {
      return Either.unit<string, number>(n + 1);
    }), {
      right: (value) => {
        expect(
          value
        ).toEqual(
          3
        );
      },
      left: (value) => {
        fail('Expected right, got left');
      }
    });
  });
});

// ## <section id='list_monad'>Listモナド</section>
// ### Listモナドの定義
// ~~~haskell
// instance Monad [] where
//   xs >>= f = concat (map f xs)
//   return x = [x]
//   fail s   = []
// ~~~

interface ListPattern<T, R> {
  empty: () => R;
  cons: (head: T, tail: ListType<T>) => R;
}

type ListType<T> = <R>(pattern: ListPattern<T, R>) => R;

const List = {
  match: <T, R>(data: ListType<T>, pattern: ListPattern<T, R>): R => {
    return data.call(List, pattern) as R;
  },
  empty: <T>(_?: any): ListType<T> => {
    return <R>(pattern: ListPattern<T, R>): R => {
      return pattern.empty();
    };
  },
  cons: <T>(value: T, alist: ListType<T>): ListType<T> => {
    return <R>(pattern: ListPattern<T, R>): R => {
      return pattern.cons(value, alist);
    };
  },
  head: <T>(alist: ListType<T>): T | undefined => {
    return List.match(alist, {
      empty: () => {
        return undefined;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: <T>(alist: ListType<T>): ListType<T> | undefined => {
    return List.match(alist, {
      empty: () => {
        return undefined;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  isEmpty: <T>(alist: ListType<T>): boolean => {
    return List.match(alist, {
      empty: () => {
        return true;
      },
      cons: (head, tail) => {
        return false;
      }
    });
  },
  // **List#append**
  // ~~~haskell
  // append [] ys = ys
  // append (x:xs) ys = x : (xs ++ ys)
  // ~~~
  /* append:: LIST[T] -> LIST[T] -> LIST[T] */
  append: <T>(xs: ListType<T>) => {
    return (ys: ListType<T>): ListType<T> => {
      return List.match(xs, {
        empty: () => {
          return ys;
        },
        cons: (head, tail) => {
          return List.cons(head, List.append(tail)(ys));
        }
      });
    };
  },
  // **List#concat**
  // ~~~haskell
  // concat [] = []
  // concat (xs:xss) = xs ++ concat xss
  // ~~~
  /* concat:: LIST[LIST[T]] -> LIST[T] */
  concat: <T>(xss: ListType<ListType<T>>): ListType<T> => {
    return List.foldr<ListType<T>, ListType<T>>(xss)(List.empty<T>())(List.append);
  },
  join: <T>(xss: ListType<ListType<T>>): ListType<T> => {
    return List.concat(xss);
  },
  // **List#flatten**
  //
  // ~~~haskell
  // flatten :: [[a]] -> [a]
  // flatten =  foldr (++) []
  // ~~~
  flatten: <T>(instanceMM: ListType<ListType<T>>): ListType<T> => {
    return List.concat(instanceMM);
  },
  // **List#map**
  // ~~~haskell
  // map [] _ = []
  // map (x:xs) f = f x : map xs f
  // ~~~
  /* map:: LIST[T] -> FUN[T->U] -> LIST[U] */
  map: <T, U>(instanceM: ListType<T>) => {
    return (transform: (value: T) => U): ListType<U> => {
      return List.match<T, ListType<U>>(instanceM, {
        empty: () => {
          return List.empty<U>();
        },
        cons: (head, tail) => {
          return List.cons(transform(head),
            List.map<T, U>(tail)(transform));
        }
      });
    };
  },
  unit: <T>(value: T): ListType<T> => {
    return List.cons(value, List.empty<T>());
  },
  // **List#flatMap**
  // ~~~haskell
  // xs >>= f = concat (map f xs)
  // ~~~
  flatMap: <T, U>(instanceM: ListType<T>) => {
    return (transform: (value: T) => ListType<U>): ListType<U> => { // FUN[T->LIST[T]]
      expect(typeof transform).toBe('function');
      return List.join(List.map<T, ListType<U>>(instanceM)(transform));
    };
  },
  /* 1段階のリストしか配列に変更できない */
  toArray: <T>(alist: ListType<T>): T[] => {
    return List.foldr<T, T[]>(alist)([] as T[])((item: T) => {
      return (accumulator: T[]): T[] => {
        return [item].concat(accumulator);
      };
    });
  },
  fromArray: <T>(array: T[]): ListType<T> => {
    return array.reduce((accumulator, item) => {
      return List.append(accumulator)(List.cons(item, List.empty<T>()));
    }, List.empty<T>());
  },
  // **List#foldr**
  // ~~~haskell
  // foldr []     z _ = z
  // foldr (x:xs) z f = f x (foldr xs z f)
  // ~~~
  /* foldr:: LIST[T] -> T -> FUN[T -> U -> U] -> T */
  foldr: <T, U>(alist: ListType<T>) => {         // alist:: LIST[T]
    return (accumulator: U) => { // accumulator:: T
      return (glue: (item: T) => (acc: U) => U): U => {      // glue:: FUN[T -> U -> U]
        expect(typeof glue).toBe('function');
        return List.match<T, U>(alist, {
          empty: () => {
            return accumulator;
          },
          cons: (head, tail) => {
            return glue(head)(List.foldr<T, U>(tail)(accumulator)(glue));
          }
        });
      };
    };
  }
}; // end of list monad

// ### Listモナドのテスト
describe('Listモナドのテスト', () => {
  // **List#match**でリストをパターンマッチする
  describe('List.matchでリストをパターンマッチする', () => {
    it("matchでList#emptyをマッチさせる", () => {
      List.match(List.empty(), {
        empty: () => {
          expect(true).toBeTruthy();
        },
        cons: (x, xs) => {
          fail('Expected empty');
        }
      });
    });
    it("matchでList#consをマッチさせる", () => {
      List.match(List.cons(1, List.empty()), {
        empty: () => {
          fail('Expected cons');
        },
        cons: (x, xs) => {
          expect(x).toEqual(1);
        }
      });
    });
  });
  // **List#isEmpty**は、リストが空かどうかを判定する
  it("List#isEmptyは、リストが空かどうかを判定する", () => {
    expect(
      List.isEmpty(List.empty())
    ).toEqual(
      true
    );
    expect(
      List.isEmpty(List.cons(1, List.empty()))
    ).toEqual(
      false
    );
  });
  it("'List#head'", () => {
    expect(
      List.head(List.cons(1, List.empty()))
    ).toEqual(
      1
    );
  });
  it("'List#tail'", () => {
    expect(
      List.head(List.tail(List.cons(1, List.cons(2, List.empty())))!)
    ).toEqual(
      2
    );
  });
  it("'List#append'", () => {
    const theList = List.append(List.cons(1, List.empty()))(List.cons(2, List.empty()));
    expect(
      List.head(theList)
    ).toEqual(
      1
    );
    expect(
      List.head(List.tail(theList)!)
    ).toEqual(
      2
    );
    expect(
      List.isEmpty(List.tail(List.tail(theList)!)!)
    ).toEqual(
      true
    );
  });
  // **List#concat**で２つのリストを連結する
  it("List#concatで２つのリストを連結する", () => {
    const one_two = List.cons(1, List.cons(2, List.empty()));
    const three_four = List.cons(3, List.cons(4, List.empty()));

    /* list_of_list = [[1,2],[3,4]] */
    const list_of_list = List.cons(one_two,
      List.cons(three_four, List.empty()));
    /* concated_list = [1,2,3,4] */
    const concated_list = List.concat(list_of_list);
    expect(
      List.toArray(concated_list)
    ).toEqual(
      [1, 2, 3, 4]
    );
    expect(
      List.head(concated_list)
    ).toEqual(
      1
    );
    expect(
      List.head(List.tail(concated_list)!)
    ).toEqual(
      2
    );
    expect(
      List.isEmpty(List.tail(List.tail(concated_list)!)!)
    ).toEqual(
      false
    );
  });
  // **List#foldr**をテストする
  it("List#foldrをテストする", () => {
    /* list = [1,2,3,4] */
    const theList = List.cons(1, List.cons(2, List.cons(3, List.cons(4, List.empty()))));
    expect(
      List.foldr<number, number>(theList)(0)((item) => {
        return (accumulator) => {
          return accumulator + item;
        };
      })
    ).toEqual(
      10
    );
  });
  // **List#map**をテストする
  it("List#mapをテストする", () => {
    /* list = [1,2,3,4] */
    const theList = List.cons(1, List.cons(2, List.cons(3, List.cons(4, List.empty()))));
    expect(
      List.toArray(List.map(theList)((item) => {
        return item * 2;
      }))
    ).toEqual(
      [2, 4, 6, 8]
    );
  });
  it("List#unit", () => {
    /* list = [1] */
    expect(
      List.toArray(List.unit(1))
    ).toEqual(
      [1]
    );
    expect(
      List.toArray(List.unit(null))
    ).toEqual(
      [null]
    );
  });
  // **List#flatMap**をテストする
  describe("List#flatMapをテストする", () => {
    it("'List#flatMap'", () => {
      /* theList = [1,2,3] */
      const theList = List.cons(1, List.cons(2, List.cons(3, List.empty())));
      expect(
        List.toArray(List.flatMap(theList)((item) => {
          return List.append(List.unit(item))(List.unit(-item));
        }))
      ).toEqual(
        [1, -1, 2, -2, 3, -3]
      );
    });
    it("[1]の要素の2倍は、[2]", () => {
      // ~~~haskell
      // Prelude> [1] >>= \x -> [x * 2]
      // [2]
      // ~~~
      const theList = List.cons(1, List.empty());
      expect(
        List.toArray(List.flatMap(theList)((item) => {
          return List.unit(item * 2);
        }))
      ).toEqual(
        [2]
      );
    });
    it("[]の要素の2倍は、[]", () => {
      // ~~~haskell
      // Prelude> [] >>= \x -> [x * 2]
      // []
      // ~~~
      const emptyList = List.empty<number>();
      expect(
        List.toArray(List.flatMap(emptyList)((item) => {
          return List.unit(item * 2);
        }))
      ).toEqual(
        []
      );
    });
    it("[1,2]と[1,2]のそれぞれの要素を足して3になる組み合わせを求める", () => {
      const list1 = List.cons(1, List.cons(2,
        List.empty()));
      const list2 = List.cons(1, List.cons(2,
        List.empty()));
      expect(
        List.toArray(List.flatMap(list1)((item1) => {
          return List.flatMap(list2)((item2) => {
            if (item1 + item2 === 3) {
              return List.unit([item1, item2]);
            } else {
              return List.empty<number[]>();
            }
          });
        }))
      ).toEqual(
        [[1, 2], [2, 1]]
      );
    });
  });
  // **List#toArray**でリストを配列に変換する
  describe("List#toArrayでリストを配列に変換する", () => {
    it("1段階のリストを配列に変換する", () => {
      /* theList = [1,2,3,4] */
      const theList = List.cons(1, List.cons(2, List.cons(3, List.cons(4, List.empty()))));
      expect(
        List.toArray(theList)
      ).toEqual(
        [1, 2, 3, 4]
      );
    });
    it("2段階のリストを配列に変換する", () => {
      /* nestedList = [[1],[2]] */
      const nestedList = List.cons(List.cons(1, List.empty()),
        List.cons(List.cons(2, List.empty()),
          List.empty()));
      expect(
        List.toArray(List.flatMap(nestedList)((alist) => {
          return List.flatMap(alist)((item) => {
            return List.unit(item);
          });
        }))
      ).toEqual(
        [1, 2]
      );
    });
  });
  // **List#fromArray**で配列をリストに変換する
  describe("List#toArrayで配列をリストに変換する", () => {
    it("1段階のリストを配列に変換する", () => {
      expect(
        List.toArray(
          List.fromArray([1, 2, 3])
        )
      ).toEqual(
        [1, 2, 3]
      );
    });
  });
  describe("Listモナドを活用する", () => {
    // 素数を判定するisPrimeを定義する
    it("素数を判定するisPrimeを定義する", () => {
      const enumFromTo = (x: number, y: number): ListType<number> => {
        if (x > y) {
          return List.empty();
        } else {
          return List.cons(x, enumFromTo(x + 1, y));
        }
      };
      // ~~~haskell
      // factors :: Int -> [Int]
      // factors n = [x | x <- [1..n], n `mod` x == 0]
      // ~~~
      const factors = (n: number) => {
        return List.flatMap(enumFromTo(1, n))((x) => {
          if ((n % x) === 0) {
            return List.unit(x);
          } else {
            return List.empty();
          }
        });
      };
      expect(
        List.toArray(factors(15))
      ).toEqual(
        [1, 3, 5, 15]
      );
      expect(
        List.toArray(factors(7))
      ).toEqual(
        [1, 7]
      );
      // isPrime関数
      // > isPrime(n) で n が素数かどうかを判定します。
      // ~~~haskell
      // isPrime :: Int -> Bool
      // isPrime n = factors n == [1,n]
      // ~~~
      const isPrime = (n: number) => {
        return List.toArray(factors(n)) === List.toArray(enumFromTo(1, n));
      };
      expect(
        isPrime(15)
      ).toEqual(
        false
      );
      expect(
        isPrime(13)
      ).toEqual(
        false
      );
    });
    it("フィルターとして使う", () => {
      const even = (n: number) => {
        if (n % 2 === 0) {
          return true;
        } else {
          return false;
        }
      };
      const theList = List.cons(1, List.cons(2, List.cons(3, List.cons(4, List.empty()))));
      expect(
        List.toArray(List.flatMap(theList)((item) => {
          if (even(item)) {
            return List.unit(item);
          } else {
            return List.empty<number>();
          }
        }))
      ).toEqual(
        [2, 4]
      );
    });
    it("2段階のflatMap", () => {
      const theNumberList = List.cons(1, List.cons(2, List.empty()));
      const theStringList = List.cons("one", List.cons("two", List.empty()));
      expect(
        List.toArray(List.flatMap(theNumberList)((n) => {
          return List.flatMap(theStringList)((s) => {
            return List.unit([n, s]);
          });
        }))
      ).toEqual(
        [[1, "one"], [1, "two"], [2, "one"], [2, "two"]]
      );
    });
    describe("Maybeと一緒に使う", () => {
      it("[just(1)]", () => {
        const theList = List.cons(Maybe.just(1),
          List.empty<MaybeType<number>>());
        const justList = List.flatMap(theList)((maybeItem) => {
          return (Maybe.flatMap(maybeItem) as any)((value: number) => {
            return List.unit(value);
          });
        });
        expect(
          List.toArray(justList)
        ).toEqual(
          [1]
        );
      });
      it("[just(1),just(2)]", () => {
        const theList = List.cons(Maybe.just(1),
          List.cons(Maybe.just(2), List.empty<MaybeType<number>>()));
        const justList = List.flatMap(theList)((listItem) => {
          return (Maybe.flatMap(listItem) as any)((value: number) => {
            return List.unit(value);
          });
        });
        expect(
          List.toArray(justList)
        ).toEqual(
          [1, 2]
        );
      });
    });
  });
});

// ## <section id='stream_monad'>Streamモナド</section>
// ### Streamモナドの定義

interface StreamPattern<T, R> {
  empty: () => R;
  cons: (head: T, tailThunk: () => StreamType<T>) => R;
}

type StreamType<T> = <R>(pattern: StreamPattern<T, R>) => R;

const Stream = {
  match: <T, R>(data: StreamType<T>, pattern: StreamPattern<T, R>): R => {
    return data.call(Stream, pattern) as R;
  },
  // **Stream#unit**
  /* unit:: ANY -> STREAM */
  unit: <T>(value: T | null | undefined): StreamType<T> => {
    if (value != null) {
      return Stream.cons(value as T, () => {
        return Stream.empty<T>();
      });
    } else {
      return Stream.empty<T>();
    }
  },
  empty: <T>(_?: any): StreamType<T> => {
    return <R>(pattern: StreamPattern<T, R>): R => {
      expect(typeof pattern).toBe('object');
      return pattern.empty();
    };
  },
  cons: <T>(head: T, tailThunk: () => StreamType<T>): StreamType<T> => {
    expect(typeof tailThunk).toBe('function');
    return <R>(pattern: StreamPattern<T, R>): R => {
      expect(typeof pattern).toBe('object');
      return pattern.cons(head, tailThunk);
    };
  },
  /* head:: STREAM -> MAYBE[STREAM] */
  head: <T>(lazyList: StreamType<T>): MaybeType<T> => {
    return Stream.match(lazyList, {
      empty: () => {
        return Maybe.nothing<T>();
      },
      cons: (value, tailThunk) => {
        return Maybe.just(value);
      }
    });
  },
  /* tail:: STREAM -> MAYBE[STREAM] */
  tail: <T>(lazyList: StreamType<T>): MaybeType<StreamType<T>> => {
    return Stream.match(lazyList, {
      empty: () => {
        return Maybe.nothing<StreamType<T>>();
      },
      cons: (head, tailThunk) => {
        return Maybe.just(tailThunk());
      }
    });
  },
  isEmpty: <T>(lazyList: StreamType<T>): boolean => {
    return Stream.match(lazyList, {
      empty: () => {
        return true;
      },
      cons: (head, tailThunk) => {
        return false;
      }
    });
  },
  // **Stream#toArray**
  toArray: <T>(lazyList: StreamType<T>): T[] => {
    return Stream.match(lazyList, {
      empty: () => {
        return [];
      },
      cons: (head, tailThunk) => {
        if (Stream.isEmpty(tailThunk())) {
          return [head];
        } else {
          return [head].concat(Stream.toArray(tailThunk()));
        }
      }
    });
  },
  // **Stream#map**
  map: <T, U>(lazyList: StreamType<T>) => {
    return (transform: (value: T) => U): StreamType<U> => {
      return Stream.match(lazyList, {
        empty: () => {
          return Stream.empty<U>();
        },
        cons: (head, tailThunk) => {
          return Stream.cons(transform(head), () => {
            return Stream.map(tailThunk())(transform);
          });
        }
      });
    };
  },
  // **Stream#append**
  append: <T>(xs: StreamType<T>) => {
    return (ysThunk: () => StreamType<T>): StreamType<T> => {
      return Stream.match(xs, {
        empty: () => {
          return ysThunk();
        },
        cons: (head, tailThunk) => {
          return Stream.cons(head, () => {
            return Stream.append(tailThunk())(ysThunk);
          });
        }
      });
    };
  },
  // **Stream#concat**
  /* concat:: STREAM[STREAM[T]] -> STREAM[T] */
  concat: <T>(astream: StreamType<StreamType<T>>): StreamType<T> => {
    return Stream.match(astream, {
      empty: () => {
        return Stream.empty<T>();
      },
      cons: (head, tailThunk) => {
        return Stream.append(head)(() => tailThunk() as any);
      }
    });
  },
  // **Stream#flatten**
  /* flatten :: STREAM[STREAM[T]] => STREAM[T] */
  flatten: <T>(lazyList: StreamType<StreamType<T>>): StreamType<T> => {
    return Stream.match(lazyList, {
      empty: () => {
        return Stream.empty<T>();
      },
      cons: (head, tailThunk) => {
        return Stream.append(head)(() => {
          return Stream.flatten(tailThunk());
        });
      }
    });
  },
  // **Stream#flatMap**
  // ~~~haskell
  // flatMap xs f = flatten (map f xs)
  //~~~
  /* flatMap:: STREAM[T] -> FUNC[T->STREAM[T]] -> STREAM[T] */
  flatMap: <T, U>(lazyList: StreamType<T>) => {
    return (transform: (value: T) => StreamType<U>): StreamType<U> => {
      return Stream.flatten(Stream.map(lazyList)(transform));
    };
  },
  // **Stream#foldr**
  foldr: <T, U>(instanceM: StreamType<T>) => {
    return (accumulator: U) => {
      return (glue: (head: T) => (acc: U) => U): U => {
        expect(typeof glue).toBe('function');
        return Stream.match<T, U>(instanceM, {
          empty: () => {
            return accumulator;
          },
          cons: (head, tailThunk) => {
            return glue(head)(Stream.foldr<T, U>(tailThunk())(accumulator)(glue));
          }
        });
      };
    };
  }
};

// ### Streamモナドのテスト
describe('Streamモナドのテスト', () => {
  it("Stream#unit", () => {
    Stream.match(Maybe.nothing(null) as any, {
      nothing: (_: any) => {
        return expect(
          _
        ).toEqual(
          null
        );
      },
      just: (value: any) => {
        return fail('Expected nothing');
      }
    } as any);
    const lazyList = Stream.unit(1);
    expect(
      Maybe.get(Stream.head(lazyList))
    ).toEqual(
      1
    );
    expect(
      Maybe.get(Stream.head(Stream.unit(1)))
    ).toEqual(
      1
    );
    expect(
      Maybe.get(Stream.head(Stream.unit(0)))
    ).toEqual(
      0
    );
  });
  it("Stream#cons", () => {
    const lazyList = Stream.cons(1, () => {
      return Stream.cons(2, () => {
        return Stream.empty();
      });
    });
    expect(
      Maybe.get(Stream.head(lazyList))
    ).toEqual(
      1
    );
  });
  it("Stream#tail", () => {
    /* lazyList = [1,2] */
    const lazyList = Stream.cons(1, () => {
      return Stream.cons(2, () => {
        return Stream.empty();
      });
    });
    expect(
      typeof Stream.tail(lazyList)
    ).toBe("function");

    (Stream.tail(lazyList) as any)({
      nothing: (_: any) => {
        fail('Expected just');
      },
      just: (tail: StreamType<number>) => {
        Stream.match(tail, {
          empty: () => {
            fail('Expected cons');
          },
          cons: (head, tailThunk) => {
            expect(head).toEqual(2);
          }
        });
      }
    });
    expect(
      Maybe.get(Stream.head(Maybe.get(Stream.tail(lazyList)) as StreamType<number>))
    ).toEqual(
      2
    );
  });
  it("Stream#toArray", () => {
    expect(
      Stream.toArray(Stream.empty())
    ).toEqual(
      []
    );
    expect(
      Stream.toArray(Stream.unit(1))
    ).toEqual(
      [1]
    );
  });
  it("Stream#append", () => {
    const xs = Stream.cons(1, () => {
      return Stream.empty();
    });
    const ysThunk = () => {
      return Stream.cons(2, () => {
        return Stream.empty();
      });
    };
    const theStream = Stream.append(xs)(ysThunk);
    expect(
      Maybe.get(Stream.head(theStream))
    ).toEqual(
      1
    );
    expect(
      Maybe.get(Stream.head(Maybe.get(Stream.tail(theStream)) as StreamType<number>))
    ).toEqual(
      2
    );
  });
  it("Stream#flatten", () => {
    /* innerStream = [1,2] */
    const innerStream = Stream.cons(1, () => {
      return Stream.cons(2, () => {
        return Stream.empty();
      });
    });
    /* outerStream = [[1,2]] */
    const outerStream = Stream.unit(innerStream);
    const flattenedStream = Stream.flatten(outerStream as StreamType<StreamType<number>>);
    Stream.match(flattenedStream, {
      empty: () => {
        fail('Expected cons');
      },
      cons: (head, tailThunk) => {
        expect(head).toEqual(1);
      }
    });
    expect(
      Maybe.get(Stream.head(flattenedStream))
    ).toEqual(
      1
    );
    expect(
      Maybe.get(Stream.head(Maybe.get(Stream.tail(flattenedStream)) as StreamType<number>))
    ).toEqual(
      2
    );
  });
  describe("Stream#map", () => {
    it("mapで要素を2倍にする", () => {
      /* lazyList = [1,2] */
      const lazyList = Stream.cons(1, () => {
        return Stream.cons(2, () => {
          return Stream.empty();
        });
      });
      const doubledLazyList = Stream.map(lazyList)((item) => {
        return item * 2;
      });
      expect(
        Maybe.get(Stream.head(doubledLazyList))
      ).toEqual(
        2
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(doubledLazyList)) as StreamType<number>))
      ).toEqual(
        4
      );
      expect(
        Stream.toArray(doubledLazyList)
      ).toEqual(
        [2, 4]
      );
    });
    // Streamモナドで無限の整数列を作る
    it("無限の整数列を作る", () => {
      const ones: StreamType<number> = Stream.cons(1, () => {
        return ones;
      });
      expect(
        Maybe.get(Stream.head(ones))
      ).toEqual(
        1
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(ones)) as StreamType<number>))
      ).toEqual(
        1
      );
      const twoes = Stream.map(ones)((item) => {
        return item * 2;
      });
      expect(
        Maybe.get(Stream.head(twoes))
      ).toEqual(
        2
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(twoes)) as StreamType<number>))
      ).toEqual(
        2
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(Maybe.get(Stream.tail(twoes)) as StreamType<number>)) as StreamType<number>))
      ).toEqual(
        2
      );
    });
    it("整数列を作る", () => {
      const integersFrom = (from: number): StreamType<number> => {
        return Stream.cons(from, () => {
          return integersFrom(from + 1);
        });
      };
      expect(
        Maybe.get(Stream.head(integersFrom(0)))
      ).toEqual(
        0
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(integersFrom(0))) as StreamType<number>))
      ).toEqual(
        1
      );
      const doubledIntergerMapped = Stream.map(integersFrom(0))((integer) => {
        return integer * 2;
      });
      expect(
        Maybe.get(Stream.head(doubledIntergerMapped))
      ).toEqual(
        0
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(doubledIntergerMapped)) as StreamType<number>))
      ).toEqual(
        2
      );
      const doubledInterger = Stream.flatMap(integersFrom(0))((integer) => {
        return Stream.unit(integer * 2);
      });
      expect(
        Maybe.get(Stream.head(doubledInterger))
      ).toEqual(
        0
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(doubledInterger)) as StreamType<number>))
      ).toEqual(
        2
      );
    });
    it("一段階のflatMap", () => {
      const ones: StreamType<number> = Stream.cons(1, () => {
        return ones;
      });
      const twoes = Stream.flatMap(ones)((one) => {
        expect(typeof one).toBe('number');
        return Stream.unit(one * 2);
      });
      expect(
        Maybe.get(Stream.head(twoes))
      ).toEqual(
        2
      );
    });
    it("二段階のflatMap", () => {
      // ~~~scala
      // scala> val nestedNumbers = List(List(1, 2), List(3, 4))
      // scala> nestedNumbers.flatMap(x => x.map(_ * 2))
      // res0: List[Int] = List(2, 4, 6, 8)
      // ~~~
      const innerStream12 = Stream.cons(1, () => {
        return Stream.cons(2, () => {
          return Stream.empty();
        });
      });
      const innerStream34 = Stream.cons(3, () => {
        return Stream.cons(4, () => {
          return Stream.empty();
        });
      });
      /* nestedStream = [[1,2],[3,4]] */
      const nestedStream = Stream.cons(innerStream12, () => {
        return Stream.cons(innerStream34, () => {
          return Stream.empty();
        });
      });
      const flattenedStream = Stream.flatMap(nestedStream)((innerStream) => {
        return Stream.flatMap(innerStream)((n) => {
          expect(typeof n).toBe('number');
          return Stream.unit(n * 2);
        });
      });
      expect(
        Maybe.get(Stream.head(flattenedStream))
      ).toEqual(
        2
      );
      expect(
        Stream.toArray(flattenedStream)
      ).toEqual(
        [2, 4, 6, 8]
      );
    });

  });
}); // Streamモナド

// ## <section id='reader_monad'>Readerモナド</section>

interface ReaderType<E, A> {
  run: (env: E) => A;
}

const Reader = {
  unit: <E, A>(a: A): ReaderType<E, A> => {
    return {
      run: (_: E): A => { // runReader :: Reader r a -> r -> a
        return a;
      }
    };
  },
  flatMap: <E, A, B>(reader: ReaderType<E, A>) => {
    return (f: (a: A) => ReaderType<E, B>): ReaderType<E, B> => { // transform:: a -> a
      return {
        run: (env: E): B => {
          return f(reader.run(env)).run(env);
        }
      };
    };
  },
  // **Reader#ask**
  // ~~~haskell
  // ask :: Reader r r
  // ask = Reader id
  // ~~~
  ask: {
    run: <E>(env: E): E => {
      return env;
    }
  } as ReaderType<any, any>,
  // **Reader#local**
  // ~~~haskell
  // local f c = Reader $ \e -> runReader c (f e)
  // ~~~
  local: <E>(f: (env: E) => E) => {
    return <A>(reader: ReaderType<E, A>): ReaderType<E, A> => {
      return {
        run: (env: E): A => {
          return reader.run(f(env));
        }
      };
    };
  }
};

// ### Readerモナドのテスト
describe("Readerモナドをテストする", () => {
  // localのテスト
  // c.f. "Real World Haskell",p.432
  it("localのテスト", () => {
    const myName = (step: string): ReaderType<string, string> => {
      return Reader.flatMap<string, string, string>(Reader.ask as ReaderType<string, string>)((name: string) => {
        return Reader.unit(step + ", I am " + name);
      });
    };
    const localExample = Reader.flatMap(myName("First"))((a: string) => {
      const appendDy = (env: string) => {
        return env + "dy";
      };
      return Reader.flatMap(Reader.local(appendDy)(myName("Second")))((b: string) => {
        return Reader.flatMap(myName("Third"))((c: string) => {
          return Reader.unit(a + ", " + b + ", " + c);
        });
      });
    });
    expect(
      localExample.run("Fred")
    ).toEqual(
      "First, I am Fred, Second, I am Freddy, Third, I am Fred"
    );
  });
  it("データベースのコネクションを模倣する", () => {
    const config = {
      host: "127.0.0.1",
      port: "27017"
    };
    const connect = Reader.flatMap(Reader.ask as ReaderType<typeof config, typeof config>)((config) => {
      return Reader.unit(config.host + ":" + config.port);
    });
    expect(
      connect.run(config)
    ).toEqual(
      "127.0.0.1:27017"
    );
  });
});

// ## <section id='writer_monad'>Writerモナド</section>

interface WriterType<W, A> {
  run: () => ReturnType<typeof Pair.cons>;
}

const Writer = {
  unit: <A>(a: A): WriterType<ListType<any>, A> => {
    return {
      run: () => {
        return Pair.cons(List.empty(), a);
      }
    };
  },
  flatMap: <A, B>(writer: WriterType<ListType<any>, A>) => {
    const writerPair = writer.run();
    const v = Pair.left(writerPair) as ListType<any>;
    const a = Pair.right(writerPair) as A;
    return (f: (a: A) => WriterType<ListType<any>, B>): WriterType<ListType<any>, B> => { // transform:: a -> a
      const newPair = f(a).run();
      const v_ = Pair.left(newPair) as ListType<any>;
      const b = Pair.right(newPair) as B;
      return {
        run: () => {
          return Pair.cons(List.append(v)(v_), b);
        }
      };
    };
  },
  // ~~~haskell
  // tell :: Monoid w => w -> Writer w ()  -- tell関数は、値wをもとにログを作成する
  // tell s = Writer ((), s)
  // ~~~
  tell: <T>(s: ListType<T>): WriterType<ListType<T>, ListType<never>> => {
    return {
      run: () => {
        return Pair.cons(s, List.empty());
      }
    };
  }
};

// ### Writerモナドのテスト
describe("Writerモナドをテストする", () => {
  it("Writerモナドを用いたfactorialの例", () => {
    const pred = (n: number) => {
      return n - 1;
    };
    const factorial = (n: number): WriterType<ListType<number>, number> => {
      if (n === 0) {
        return Writer.flatMap(Writer.tell(List.unit(0)))((_) => {
          return Writer.unit(1);
        });
      } else {
        return Writer.flatMap(Writer.tell(List.unit(n)))((_) => {
          return Writer.flatMap(factorial(pred(n)))((n1) => {
            return Writer.unit(n * n1);
          });
        });
      }
    };
    (factorial(0).run() as any).match({
      cons: (left: ListType<number>, right: number) => {
        expect(
          List.toArray(left)
        ).toEqual(
          [0]
        );
        expect(
          right
        ).toEqual(
          1
        );
      }
    });
    (factorial(5).run() as any).match({
      cons: (left: ListType<number>, right: number) => {
        expect(
          List.toArray(left)
        ).toEqual(
          [5, 4, 3, 2, 1, 0]
        );
        expect(
          right
        ).toEqual(
          120
        );
      }
    });
  });
});

// ## <section id='io_monad'>IOモナド</section>
// ### IOモナドの定義
const IO = {
  /* unit:: T => IO[T] */
  unit: <T>(any: T) => {
    return (_?: any): T => {  // 外界は明示する必要はありません
      return any;
    };
  },
  /* flatMap:: IO[T] => FUN[T => IO[U]] => IO[U] */
  flatMap: <T, U>(instanceA: () => T) => {
    return (actionAB: (a: T) => () => U): () => U => { // actionAB:: a -> IO[b]
      return IO.unit(IO.run(actionAB(IO.run(instanceA))));
    };
  },
  // **IO#done**関数
  //
  // > IOアクションを何も実行しない
  /* done:: T => IO[T] */
  done: <T>(any?: T) => {
    return IO.unit(undefined);
  },
  // **IO#run**関数
  //
  // > IOアクションを実行する
  /* run:: IO[A] => A */
  run: <T>(instanceM: () => T): T => {
    return instanceM();
  },
  // **IO#readFile**
  /* readFile:: STRING => IO[STRING] */
  readFile: (path: string) => {
    return (_?: any) => {
      const content = fs.readFileSync(path, 'utf8');
      return IO.unit(content)(_);
    };
  },
  // **IO#println**
  /* println:: STRING => IO[null] */
  println: (message: string) => {
    return (_?: any) => {
      console.log(message);
      return IO.unit(null)(_);
    };
  },
  // **IO#writeFile**
  writeFile: (path: string) => {
    return (content: string) => {
      return (_?: any) => {
        fs.writeFileSync(path, content);
        return IO.unit(null)(_);
      };
    };
  },
  // **IO#seq**
  /* IO.seq:: IO[a] => IO[b] => IO[b] */
  seq: <T, U>(instanceA: () => T) => {
    return (instanceB: () => U): () => U => {
      return IO.flatMap<T, U>(instanceA)((a) => {
        return instanceB;
      });
    };
  },
  // ~~~haskell
  // sequence_        :: [IO ()] -> IO ()
  // sequence_ []     =  return ()
  // sequence_ (a:as) =  do a
  //                        sequence as
  // ~~~
  seqs: function (alist: any): any {
    const self = this;
    return alist.match({
      empty: () => {
        return self.done();
      },
      cons: (head: any, tail: any) => {
        return self.flatMap(head)((_: any) => {
          return self.seqs(tail);
        });
      }
    });
  },
  // **IO#putc**
  /* IO.putc:: CHAR => IO[] */
  putc: (character: string) => {
    return (io?: any) => {
      process.stdout.write(character);
      return null;
    };
  },
  // **IO#puts**
  // ~~~haskell
  // puts list = seqs (map putc list)
  // ~~~
  /* IO.puts:: LIST[CHAR] => IO[] */
  puts: (alist: any): any => {
    return List.match(alist, {
      empty: () => {
        return IO.done();
      },
      cons: (head: any, tail: any) => {
        return IO.seq(IO.putc(head))(IO.puts(tail));
      }
    });
  },
  // **IO#getc**
  /* IO.getc :: IO[CHAR] */
  getc: () => {
    const continuation = () => {
      const chunk = (process.stdin as any).read();
      return chunk;
    };
    process.stdin.setEncoding('utf8');
    return process.stdin.on('readable', continuation);
  }
};

// ### IOモナドをテストする
describe("IOモナドをテストする", () => {
  // IOモナドで参照透過性を確保する
  it.todo('IOモナドで参照透過性を確保する');
});



describe("Maybeと一緒に使う", () => {
  it("[just(1),just(2)]", () => {
    const theList = List.cons(Maybe.just(1),
      List.cons(Maybe.just(2), List.empty<MaybeType<number>>()));
    const justList = List.flatMap(theList)((listItem) => {
      return (Maybe.flatMap(listItem) as any)((value: number) => {
        return List.unit(value);
      });
    });
    expect(
      List.toArray(justList)
    ).toEqual(
      [1, 2]
    );
  });
  it("[just(1)]", () => {
    const theList = List.cons(Maybe.just(1),
      List.empty<MaybeType<number>>());
    const justList = List.flatMap(theList)((maybeItem) => {
      return (Maybe.flatMap(maybeItem) as any)((value: number) => {
        return List.unit(value);
      });
    });
    expect(
      List.toArray(justList)
    ).toEqual(
      [1]
    );
  });
});

// ## <section id='st_monad'>STモナド</section>

type STType<S, A> = (state: S) => ReturnType<typeof Pair.cons>;

const ST = {
  // **ST#unit**
  //
  // > 第1引数valueの値に第2引数stateという状態を付加する。
  unit: <S, A>(value: A): STType<S, A> => {
    return (state: S) => {
      return Pair.cons(value, state);
    };
  },
  // **ST#flatMap**
  //
  // > 第1引数instanceMを実行して、新しい状態state_と計算結果xを得る。
  flatMap: <S, A, B>(instanceM: STType<S, A>) => { // instanceM:: ST a
    return (f: (a: A) => STType<S, B>): STType<S, B> => { // f:: a -> ST b
      expect(typeof f).toBe('function');
      return (state: S) => {
        const newState = ST.app(instanceM)(state); // instanceM(state)
        return (newState as any).match({
          cons: (x: A, state_: S) => {
            return ST.app(f(x))(state_);
          }
        });
      };
    };
  },
  app: <S, A>(st: STType<S, A>) => {
    return (state: S) => {
      return st(state);
    };
  },
  // **ST#get**
  // > 現在の状態を取ってきて、それを結果として提示する
  get: <S>(_?: any): STType<S, S> => {
    return (state: S) => {
      return Pair.cons(state, state);
    };
  },
  // **ST#put**
  // > 現在の状態stateを新しい状態newStateに更新する
  put: <S>(newState: S): STType<S, ReturnType<typeof Pair.empty>> => {
    return (state: S) => {
      return Pair.cons(Pair.empty(), newState);
    };
  }
};

// ### STモナドのテスト
describe("STモナドをテストする", () => {
  // #### スタック操作を実現する
  describe("Stackを作る", () => {
    // **pop関数**の定義
    /* pop:: State Stack Int */
    const pop = <T>(_?: any): STType<ListType<T>, T> => {
      return ST.flatMap(ST.get<ListType<T>>())((alist) => {
        return List.match(alist, {
          empty: () => {
            throw new Error("Cannot pop empty stack");
          },
          cons: (x, xs) => {
            return ST.flatMap(ST.put(xs))((_) => {
              return ST.unit(x);
            });
          }
        });
      });
    };
    // **push関数**の定義
    /* push:: Int -> State Stack () */
    const push = <T>(x: T): STType<ListType<T>, ReturnType<typeof Pair.empty>> => {
      return ST.flatMap(ST.get<ListType<T>>())((xs) => {
        return ST.put(List.cons(x, xs));
      });
    };
    it('スタックを操作する', () => {
      const stackManip = ST.flatMap(push(3))((_) => {
        return ST.flatMap(pop())((_a) => {
          return pop();
        });
      });
      expect(
        Pair.left(
          ST.app(
            stackManip
          )(
            List.fromArray([5, 8, 2, 1])
          )
        )
      ).toEqual(
        5
      );
      expect(
        List.toArray(Pair.right(
          ST.app(
            stackManip
          )(
            List.fromArray([5, 8, 2, 1])
          )
        ) as ListType<number>)
      ).toEqual(
        [8, 2, 1]
      );
    });
  });
  describe("Treeの例", () => {
    // ~~~haskell
    // data Tree a = Leaf a | Node (Tree a) (Tree a)
    // ~~~
    interface TreePattern<T, R> {
      leaf: (value: T) => R;
      node: (left: TreeType<T>, right: TreeType<T>) => R;
    }
    type TreeType<T> = <R>(pattern: TreePattern<T, R>) => R;

    const Tree = {
      match: <T, R>(data: TreeType<T>, pattern: TreePattern<T, R>): R => {
        return data.call(data, pattern) as R;
      },
      leaf: <T>(value: T): TreeType<T> => {
        return <R>(pattern: TreePattern<T, R>): R => {
          return pattern.leaf(value);
        };
      },
      node: <T>(left: TreeType<T>, right: TreeType<T>): TreeType<T> => {
        return <R>(pattern: TreePattern<T, R>): R => {
          return pattern.node(left, right);
        };
      },
      toArray: <T>(tree: TreeType<T>): T | [any, any] => {
        return Tree.match<T, T | [any, any]>(tree, {
          leaf: (value) => {
            return value;
          },
          node: (left, right) => {
            return [Tree.toArray(left), Tree.toArray(right)];
          }
        });
      },
      // ~~~haskell
      // fmap f (Leaf x) = Leaf (f x)
      // fmap f (Node left right) = Node (fmap f left) (fmap f right)
      // ~~~
      map: <T, U>(f: (value: T) => U) => {
        return (tree: TreeType<T>): TreeType<U> => {
          return Tree.match(tree, {
            leaf: (value) => {
              return Tree.leaf(f(value));
            },
            node: (left, right) => {
              return Tree.node(Tree.map(f)(left), Tree.map(f)(right));
            }
          });
        };
      }
    };
    it('Tree.toArray', () => {
      expect(
        Tree.toArray(Tree.leaf(1))
      ).toEqual(
        1
      );
      expect(
        Tree.toArray(Tree.node(Tree.leaf(1), Tree.leaf(2)))
      ).toEqual(
        [1, 2]
      );
      expect(
        Tree.toArray(Tree.node(Tree.leaf(1),
          Tree.node(Tree.leaf(2), Tree.leaf(3))))
      ).toEqual(
        [1, [2, 3]]
      );
    });
    it('rlabel', () => {
      // ~~~haskell
      // rlabel :: (TREE, STATE) -> (TREE,STATE)
      // ~~~
      const rlabel = <T>(tree: TreeType<T>, state: number): ReturnType<typeof Pair.cons> => {
        return Tree.match(tree, {
          leaf: (value) => {
            return Pair.cons(Tree.leaf(state), state + 1);
          },
          node: (left, right) => {
            const leftNode = rlabel(left, state);
            const rightNode = rlabel(right, Pair.right(leftNode) as number);
            return Pair.cons(Tree.node(Pair.left(leftNode) as TreeType<number>,
              Pair.left(rightNode) as TreeType<number>),
              Pair.right(rightNode));
          }
        });
      };
      expect(
        Tree.toArray(Pair.left(rlabel(Tree.leaf(1), 0)) as TreeType<number>)
      ).toEqual(
        0
      );
      expect(
        Tree.toArray(Pair.left(rlabel(Tree.node(Tree.leaf("a"), Tree.leaf("b")), 0)) as TreeType<number>)
      ).toEqual(
        [0, 1]
      );
    });
    it('mlabel', () => {
      const fresh: STType<number, number> = (state: number) => {
        return Pair.cons(state, state + 1);
      };
      // ~~~haskell
      // mlabel :: Tree a -> ST(Tree Int)
      // mlabel (Leaf _) = do n <- fresh
      //                      return (Leaf n)
      // mlabel (Node left right) = do left' <- mlabel left
      //                               right' <- mlabel right
      //                               return (Node left' right')
      // ~~~
      const mlabel = <T>(tree: TreeType<T>): STType<number, TreeType<number>> => {
        return Tree.match(tree, {
          leaf: (_) => {
            return ST.flatMap(fresh)((n: number) => {
              return ST.unit(Tree.leaf(n));
            });
          },
          node: (left, right) => {
            return ST.flatMap(mlabel(left))((left_: TreeType<number>) => {
              return ST.flatMap(mlabel(right))((right_: TreeType<number>) => {
                return ST.unit(Tree.node(left_, right_));
              });
            });
          }
        });
      };
      expect(
        Tree.toArray(
          Pair.left(
            ST.app(
              mlabel(Tree.leaf(1))
            )(0)) as TreeType<number>
        )
      ).toEqual(
        0
      );
      expect(
        Tree.toArray(
          Pair.left(ST.app(mlabel(Tree.node(Tree.leaf("a"), Tree.leaf("b"))))(0)) as TreeType<number>
        )
      ).toEqual(
        [0, 1]
      );
    });
  });
});

// ## <section id='cont_monad'>Contモナド</section>

type ContType<R, A> = (k: (a: A) => R) => R;

const Cont = {
  // ~~~haskell
  // return a       = Cont $ \k -> k a
  // ~~~
  unit: <R, A>(a: A): ContType<R, A> => {
    return (k: (a: A) => R): R => {
      return k(a);
    };
  },
  flatMap: <R, A, B>(m: ContType<R, A>) => {
    return (f: (a: A) => ContType<R, B>): ContType<R, B> => { // f:: a -> Cont r a
      expect(typeof f).toBe('function');
      return (k: (b: B) => R): R => {
        return m((a: A) => {
          return f(a)(k);
        });
      };
    };
  },
  // ~~~haskell
  // class Monad m => MonadCont m where
  //   callCC :: ((a -> m a) -> m a) -> m a
  // instance MonadCont (Cont r) where
  //   callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
  //   -- i.e.  callCC f = \k -> ((f (\a -> \_ -> k a)) k)
  // ~~~
  callCC: <R, A>(f: (k: (a: A) => ContType<R, any>) => ContType<R, A>): ContType<R, A> => {
    return (k: (a: A) => R): R => {
      return f((a: A): ContType<R, any> => {
        return (_: any): R => {
          return k(a);
        };
      })(k);
    };
  }
};

// ### Contモナドのテスト
describe("Contモナドをテストする", () => {
  const identity = <T>(x: T): T => {
    return x;
  };
  // ~~~haskell
  // *Main> let s3 = Cont (square 3)
  // *Main> print =: runCont s3
  // 9
  // ~~~
  it('square', () => {
    // ~~~haskell
    // square :: Int -> ((Int -> r) -> r)
    // square x = \k -> k (x * x)
    // ~~~
    const square = (n: number) => {
      return n * n;
    };
    const square3 = Cont.unit(square(3));
    expect(
      square3(identity)
    ).toEqual(
      9
    );
  });
  // **Cont.flatMap**で算術演算を組み合わせる例
  it('Cont.flatMapで算術演算を組み合わせる例', () => {
    const addCPS = (n: number, m: number) => {
      const add = (n: number, m: number) => {
        return n + m;
      };
      return Cont.unit(add(n, m));
    };
    expect(
      addCPS(2, 3)(identity)
    ).toEqual(
      5
    );
    const multiplyCPS = (n: number, m: number) => {
      const multiply = (n: number, m: number) => {
        return n * m;
      };
      return Cont.unit(multiply(n, m));
    };
    const subtractCPS = (n: number, m: number) => {
      const subtract = (n: number, m: number) => {
        return n - m;
      };
      return Cont.unit(subtract(n, m));
    };
    /* ((2 + 3) * 4) - 5 = 15 */
    expect(
      Cont.flatMap(addCPS(2, 3))((addResult) => {
        return Cont.flatMap(multiplyCPS(addResult, 4))((multiplyResult) => {
          return Cont.flatMap(subtractCPS(multiplyResult, 5))((result) => {
            return Cont.unit(result);
          });
        });
      })(identity)
    ).toEqual(
      15
    );
  });
  describe("callCCを利用する", () => {
    it('square using callCC', () => {
      // ~~~haskell
      // -- Without callCC
      // square :: Int -> Cont r Int
      // square n = return (n ˆ 2)
      // -- With callCC
      // squareCCC :: Int -> Cont r Int
      // squareCCC n = callCC $ \k -> k (n ˆ 2)
      // ~~~
      const squareCPS = (n: number) => {
        return Cont.unit(n * n);
      };
      expect(
        squareCPS(2)(identity)
      ).toEqual(
        4
      );
      const safeDivideCC = (n: number, m: number) => {
        return Cont.callCC((k) => {
          if (m !== 0) {
            return k(n / m);
          }
          return k(null);
        });
      };
      expect(
        safeDivideCC(4, 2)(identity)
      ).toEqual(
        2
      );
      expect(
        safeDivideCC(4, 0)(identity)
      ).toBe(
        null
      );
    });
    it('even', () => {
      const even = (n: number) => {
        return (n % 2) === 0;
      };
      expect(
        even(3 * Cont.callCC<number, number>((k) => {
          return k(1 + 2);
        })(identity))
      ).toEqual(
        false
      );
    });
  });
});
// [目次に戻る](index.html)
