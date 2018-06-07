"use strict";

// さまざまなモナド
// =============
// ここでは、本書で紹介できなかったさまざまなモナドを紹介します。
// 参考までに、Haskellでの定義も随時併記しています。
// 
// なお、本ページのコードは、書籍で採用された node.js 0.12版では動作しません。
// できるだけ新しいバージョンのnode.jsで実行してください。
// 当方の環境では、v8.11.1でテストが成功することを確認しています。


// ## 小目次
// <div class="toc">
// <ul class="toc">
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#id_monad"> 恒等モナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#maybe_monad"> Maybeモナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#either_monad"> Eitherモナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#list_monad"> Listモナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#stream_monad"> Streamモナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#reader_monad"> Readerモナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#writer_monad"> Writerモナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#io_monad"> IOモナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#st_monad"> STモナド</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/monad.spec.html#cont_monad"> Contモナド</a></li>
// </ul>
// </div>


var fs = require('fs');
var expect = require('expect.js');
// Pair型の読込
var Pair = require('../lib/pair.js');
// String型の読込
var String = require('../lib/string.js');

// ## <section id='id_monad'>恒等モナド</section>
// ### 恒等モナドの定義
var ID = {
  // **ID#unit**
  /* unit:: T => ID[T] */
  unit: (value) => {  // 単なる identity関数と同じ
    return value;
  },
  // **ID#flatMap**
  /* flatMap:: ID[T] => FUN[T => ID[T]] => ID[T] */
  flatMap: (instanceM) => {
    return (transform) => {
      return transform(instanceM); // 単なる関数適用と同じ
    };
  }
};

// ### 恒等モナドのテスト
describe("恒等モナドをテストする",() => {
  // **ID#unit**をテストする
  it("ID#unitをテストする", (next) => {
    expect(
      ID.unit(1)
    ).to.eql(
      1
    );
    next();
  });
});

// ## <section id='maybe_monad'>Maybeモナド</section>
// ### Maybeモナドの定義
// ~~~haskell
// instance Monad Maybe where
//   Nothing  >>= _ = Nothing
//   (Just x) >>= f = f x
// ~~~
var Maybe = {
  match: (data, pattern) => {
     return data(pattern);
  },
  just : (value) => {
    return (pattern) => {
      return pattern.just(value);
    };
  },
  nothing : (_) => {
    return (pattern) => {
      return pattern.nothing(_);
    };
  },
  // **Maybe#unit**
  unit : (value) => {
    return Maybe.just(value);
  },
  // **Maybe#flatMap**
  flatMap : (maybeInstance) => {
    return (transform) => {
      expect(transform).to.a('function');
      return Maybe.match(maybeInstance,{
        just: (value) => {
          return transform(value);
        },
        nothing: (_) => {
          return Maybe.nothing(_);
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
  map : (maybeInstance) => {
    return (transform) => {
      expect(transform).to.a('function');
      return Maybe.match(maybeInstance,{
        nothing: (_) => {
          return Maybe.nothing(_);
        },
        just: (value) => {
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
  liftM: (f) => {
    return (ma) => {
      return Maybe.flatMap(ma)((x) => {
        return Maybe.unit(f(x));
      });
    };
  },
  // ~~~haskell
  // (<*>) :: (Monad m) => m (a -> b) -> m a -> m b
  // ~~~
  apply: (mf) => {
    return (ma) => {
      return Maybe.flatMap(mf)((f) => {
        return Maybe.flatMap(ma)((a) => {
          return Maybe.unit(f(a));
        });
      });
    }; 
  },
  get: (maybe) => {
    return Maybe.getOrElse(maybe)(null);
  },
  getOrElse: (instance) => {
    return (alternate) => {
      return Maybe.match(instance,{
        just: (value) => {
          return value;
        },
        nothing: (_) => {
          return alternate;
        }
      });
    };
  },
  isEqual : (maybeA) => {
    return (maybeB) => {
      return Maybe.match(maybeA,{
        just: (valueA) => {
          return Maybe.match(maybeB,{
            just: (valueB) => {
              return (valueA === valueB);
            },
            nothing: (_) => {
              return false;
            }
          });
        },
        nothing: (_) => {
          return Maybe.match(maybeB,{
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
describe("Maybeモナドをテストする",() => {
  // **Maybe#flatMap**をテストする
  it("Maybe#flatMapをテストする", (next) => {
    Maybe.match(Maybe.flatMap(Maybe.just(1))((a) => {
      return Maybe.unit(a);
    }),{
      just: (value) => {
        expect(
          value
        ).to.eql(
          1
        );
      },
      nothing: (_) => {
       expect().fail();
      }
    });
    Maybe.match(Maybe.flatMap(Maybe.nothing())((a) => {
      return Maybe.unit(a);
    }),{
      just: (value) => {
       expect().fail();
      },
      nothing: (_) => {
       expect(true).to.be.ok();
      }
    });
    next();
  });
  // **Maybe#map**をテストする
  it("Maybe#mapをテストする", (next) => {
    var succ = (n) => { return n + 1;};
    // ~~~haskell
    // > fmap (+1) nothing
    // Nothing
    // ~~~
    expect(
      Maybe.isEqual(
        Maybe.map(Maybe.nothing())(succ)
      )(
        Maybe.nothing()
      )
    ).to.eql(
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
    ).to.eql(
      true
    );
    next();
  });
  // **Maybe#liftM**をテストする
  it("Maybe#liftMをテストする", (next) => {
    // ~~~haskell
    // > liftM (+3) (Just 2)
    // Just 5
    // ~~~
    var add3 = (n) => {
      return n + 3;
    };
    var justTwo = Maybe.just(2);
    var justFive = Maybe.just(5);
    expect(
      Maybe.isEqual(
        Maybe.liftM(add3)(Maybe.unit(2)) 
      )(
        justFive
      )
    ).to.eql(
      true
    );
    next();
  });
  // **Maybe#apply**をテストする
  it("Maybe#applyをテストする", (next) => {
    // ~~~haskell
    // > Just (+3) <*> (Just 2)
    // Just 5
    // ~~~
    var add3 = (n) => {
      return n + 3;
    };
    var justTwo = Maybe.just(2);
    var justFive = Maybe.just(5);
    expect(
      Maybe.isEqual(
        Maybe.apply(Maybe.just(add3))(Maybe.unit(2)) 
      )(
        justFive
      )
    ).to.eql(
      true
    );
    next();
  });
  // add関数でMaybeインスンスを足しあわせる
  it("add関数でMaybeインスンスを足しあわせる", (next) => {
    var add = (maybeA,maybeB) => {
      return Maybe.flatMap(maybeA)((a) => {
        return Maybe.flatMap(maybeB)((b) => {
          return Maybe.unit(a + b);
        });
      });
    };
    var justOne = Maybe.just(1);
    var justTwo = Maybe.just(2);
    var justThree = Maybe.just(3);
    expect(
      Maybe.isEqual(
        add(justOne,justTwo)
      )(
        justThree
      )
    ).to.eql(
      true
    );
    expect(
      Maybe.isEqual(
        add(justOne,Maybe.nothing())
      )(
        Maybe.nothing()
      )
    ).to.eql(
      true
    );
    next();
  });
});

// ## <section id='either_monad'>Eitherモナド</section>
// ### Eitherモナドの定義
var Either  = {
  // ~~~haskell
  // data  Either a b  =  Left a | Right b
  // ~~~
  match: (data, pattern) => {
     return data.call(data,pattern);
  },
  left : (value) => {
    return (pattern) => {
      return pattern.left(value);
    };
  },
  right : (value) => {
    return (pattern) => {
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
  unit : (value) => {
    return Either.right(value);
  },
  // **Either#flatMap**
  flatMap : (instanceM) => {
    return (transform) => {
      expect(transform).to.a('function');
      return Either.match(instanceM,{
        right: (value) => {
          return transform(value);
        },
        left: (value) => {
          return Either.left(value);
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
  map: (instanceM) => {
    return (transform) => {
      return Either.match(instanceM,{
        right: (value) => {
          return Either.right(transform(value));
        },
        left: (value) => {
          return Either.left(value);
        }
      });
    };
  }
};
// ### Eitherモナドのテスト
describe("Eitherモナドをテストする",() => {
  // 数値のときだけ計算が成功するテスト
  it("数値のときだけ計算が成功するテスト", (next) => {
    Either.match(Either.flatMap(Either.left("wrong"))((n) => {
      return Either.unit(n + 1);
    }),{
      right: (value) => {
        expect().fail();
      },
      left: (value) => {
        expect(
          value
        ).to.eql(
          "wrong"
        );
      }
    });
    Either.match(Either.flatMap(Either.unit(2))((n) => {
      return Either.unit(n + 1);
    }),{
      right: (value) => {
        expect(
          value
        ).to.eql(
          3
        );
      },
      left: (value) => {
        expect().fail();
      }
    });
    next();
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
var List  = {
  match: (data, pattern) => {
    return data.call(List,pattern);
  },
  empty: (_) => {
    return (pattern) => {
      return pattern.empty();
    };
  },
  cons: (value, alist) => {
    return (pattern) => {
      return pattern.cons(value, alist);
    };
  },
  head: (alist) => {
    return List.match(alist, {
      empty: (_) => {
        return undefined;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: (alist) => {
    return List.match(alist, {
      empty: (_) => {
        return undefined;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  isEmpty: (alist) => {
    return List.match(alist, {
      empty: (_) => {
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
  append: (xs) => {
    return (ys) => {
      return List.match(xs, {
        empty: (_) => {
          return ys;
        },
        cons: (head, tail) => {
          return List.cons(head,List.append(tail)(ys)); 
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
  concat: (xss) => {
    return List.foldr(xss)(List.empty())(List.append);
  },
  join: (xss) => {
    return List.concat(xss);
  },
  // **List#flatten**
  //
  // ~~~haskell
  // flatten :: [[a]] -> [a]
  // flatten =  foldr (++) []
  // ~~~
  flatten: (instanceMM) => {
    return List.concat(instanceMM);
  },
  // **List#map**
  // ~~~haskell
  // map [] _ = []
  // map (x:xs) f = f x : map xs f
  // ~~~
  /* map:: LIST[T] -> FUN[T->U] -> LIST[U] */
  map: (instanceM) => {
    return (transform) => {
      return List.match(instanceM,{
        empty: (_) => {
          return List.empty();
        },
        cons: (head,tail) => {
          return List.cons(transform(head),
                           List.map(tail)(transform));
        }
      });
    };
  },
  unit: (value) => {
    return List.cons(value, List.empty());
  },
  // **List#flatMap**
  // ~~~haskell
  // xs >>= f = concat (map f xs)
  // ~~~
  flatMap: (instanceM) => {
    return (transform) => { // FUN[T->LIST[T]]
      expect(transform).to.a('function');
      return List.join(List.map(instanceM)(transform));
    };
  },
  /* 1段階のリストしか配列に変更できない */
  toArray: (alist) => {
    return List.foldr(alist)([])((item) => {
      return (accumulator) => {
        return [item].concat(accumulator);
      };
    });
  },
  fromArray: (array) => {
    return array.reduce((accumulator, item) => {
      return List.append(accumulator)(List.cons(item, List.empty()));
    }, List.empty());
  },
  // **List#foldr**
  // ~~~haskell
  // foldr []     z _ = z
  // foldr (x:xs) z f = f x (foldr xs z f) 
  // ~~~
  /* foldr:: LIST[T] -> T -> FUN[T -> U -> U] -> T */
  foldr: (alist) => {         // alist:: LIST[T]
    return (accumulator) => { // accumulator:: T
      return (glue) => {      // glue:: FUN[T -> U -> U] 
        expect(glue).to.a('function');
        return List.match(alist, {
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            return glue(head)(List.foldr(tail)(accumulator)(glue));;
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
    it("matchでList#emptyをマッチさせる", (next) => {
      List.match(List.empty,{
        empty: (_) => {
          expect(true).ok();
        },
        cons: (x,xs) => {
          expect().fail();
        }
      });
      next();
    });
    it("matchでList#consをマッチさせる", (next) => {
      List.match(List.cons(1,List.empty()),{
        empty: (_) => {
          expect().fail();
        },
        cons: (x,xs) => {
          expect(x).to.eql(1);
        }
      });
      next();
    });
  });
  // **List#isEmpty**は、リストが空かどうかを判定する
  it("List#isEmptyは、リストが空かどうかを判定する", (next) => {
    expect(
      List.isEmpty(List.empty())
    ).to.eql(
      true
    );
    expect(
      List.isEmpty(List.cons(1,List.empty()))
    ).to.eql(
      false
    );
    next();
  });
  it("'List#head'", (next) => {
    expect(
      List.head(List.cons(1,List.empty()))
    ).to.eql(
      1
    );
    next();
  });
  it("'List#tail'", (next) => {
    expect(
      List.head(List.tail(List.cons(1,List.cons(2,List.empty()))))
    ).to.eql(
      2
    );
    next();
  });
  it("'List#append'", (next) => {
    var theList = List.append(List.cons(1,List.empty()))(List.cons(2,List.empty()));
    expect(
      List.head(theList)
    ).to.eql(
      1
    );
    expect(
      List.head(List.tail(theList))
    ).to.eql(
      2
    );
    expect(
      List.isEmpty(List.tail(List.tail(theList)))
    ).to.eql(
      true
    );
    next();
  });
  // **List#concat**で２つのリストを連結する
  it("List#concatで２つのリストを連結する", (next) => {
    var one_two = List.cons(1,List.cons(2,List.empty()));
    var three_four = List.cons(3,List.cons(4,List.empty()));

    /* list_of_list = [[1,2],[3,4]] */
    var list_of_list = List.cons(one_two,
                                 List.cons(three_four, List.empty()));
    /* concated_list = [1,2,3,4] */
    var concated_list = List.concat(list_of_list);
    expect(
      List.toArray(concated_list)
    ).to.eql(
      [1,2,3,4]
    );
    expect(
      List.head(concated_list)
    ).to.eql(
      1
    );
    expect(
      List.head(List.tail(concated_list))
    ).to.eql(
      2
    );
    expect(
      List.isEmpty(List.tail(List.tail(concated_list)))
    ).to.eql(
      false
    );
    next();
  });
  // **List#foldr**をテストする
  it("List#foldrをテストする", (next) => {
    /* list = [1,2,3,4] */
    var theList = List.cons(1,List.cons(2,List.cons(3,List.cons(4,List.empty()),List.empty)));
    expect(
      List.foldr(theList)(0)((item) => {
        return (accumulator) => {
          return accumulator + item;
        };
      })
    ).to.eql(
      10
    );
    next();
  });
  // **List#map**をテストする
  it("List#mapをテストする", (next) => {
    /* list = [1,2,3,4] */
    var theList = List.cons(1,List.cons(2,List.cons(3,List.cons(4,List.empty()),List.empty)));
    expect(
      List.toArray(List.map(theList)((item) => {
        return item * 2;
      }))
    ).to.eql(
      [2,4,6,8]
    );
    next();
  });
  it("List#unit", (next) => {
    /* list = [1] */
    expect(
      List.toArray(List.unit(1))
    ).to.eql(
      [1]
    );
    expect(
      List.toArray(List.unit(null))
    ).to.eql(
      [null]
    );
    next();
  });
  // **List#flatMap**をテストする
  describe("List#flatMapをテストする", () => {
    it("'List#flatMap'", (next) => {
      /* theList = [1,2,3] */
      var theList = List.cons(1,List.cons(2, List.cons(3, List.empty())));
      expect(
        List.toArray(List.flatMap(theList)((item) => {
          return List.append(List.unit(item))(List.unit(- item));
        }))
      ).to.eql(
        [1,-1,2,-2,3,-3]
      );
      next();
    });
    it("[1]の要素の2倍は、[2]", (next) => {
      // ~~~haskell
      // Prelude> [1] >>= \x -> [x * 2]
      // [2]
      // ~~~
      var theList = List.cons(1, List.empty());
      expect(
        List.toArray(List.flatMap(theList)((item) => {
          return List.unit(item * 2); 
        }))
      ).to.eql(
        [2]
      );
      next();
    });
    it("[]の要素の2倍は、[]", (next) => {
      // ~~~haskell
      // Prelude> [] >>= \x -> [x * 2]
      // []
      // ~~~
      var emptyList = List.empty();
      expect(
        List.toArray(List.flatMap(emptyList)((item) => {
          return List.unit(item * 2); 
        }))
      ).to.eql(
        []
      );
      next();
    });
    it("[1,2]と[1,2]のそれぞれの要素を足して3になる組み合わせを求める", (next) => {
      var list1 = List.cons(1, List.cons(2,
                                         List.empty()));
      var list2 = List.cons(1, List.cons(2,
                                         List.empty()));
      expect(
        List.toArray(List.flatMap(list1)((item1) => {
          return List.flatMap(list2)((item2) => {
            if(item1 + item2 === 3) {
              return List.unit([item1, item2]);
            } else {
              return List.empty();
            }
          });
        }))
      ).to.eql(
        [[1,2],[2,1]]
      );
      next();
    });
  });
  // **List#toArray**でリストを配列に変換する
  describe("List#toArrayでリストを配列に変換する", () => {
    it("1段階のリストを配列に変換する", (next) => {
      /* theList = [1,2,3,4] */
      var theList = List.cons(1,List.cons(2,List.cons(3,List.cons(4,List.empty()),List.empty)));
      expect(
        List.toArray(theList)
      ).to.eql(
        [1,2,3,4]
      );
      next();
    });
    it("2段階のリストを配列に変換する", (next) => {
      /* nestedList = [[1],[2]] */
      var nestedList = List.cons(List.cons(1,List.empty()),
                                 List.cons(List.cons(2,List.empty()),
                                           List.empty()));
      expect(
        List.toArray(List.flatMap(nestedList)((alist) => {
          return List.flatMap(alist)((item) => {
            return List.unit(item);
          });
        }))
      ).to.eql(
        [1,2]
      );
      next();
    });
  });
  // **List#fromArray**で配列をリストに変換する
  describe("List#toArrayで配列をリストに変換する", () => {
    it("1段階のリストを配列に変換する", (next) => {
      expect(
        List.toArray(
          List.fromArray([1,2,3])
        )
      ).to.eql(
        [1,2,3]
      );
      next();
    });
  });
  describe("Listモナドを活用する",() => {
    // 素数を判定するisPrimeを定義する
    it("素数を判定するisPrimeを定義する", (next) => {
      var enumFromTo = (x,y) => {
        if(x > y) {
          return List.empty();
        } else {
          return List.cons(x, enumFromTo(x+1,y));
        }
      };
      // ~~~haskell
      // factors :: Int -> [Int]
      // factors n = [x | x <- [1..n], n `mod` x == 0]
      // ~~~
      var factors = (n) => {
        return List.flatMap(enumFromTo(1,n))((x) => {
          if((n % x) === 0){
            return List.unit(x);
          } else {
            return List.empty(); 
          }
        });
      };
      expect(
        List.toArray(factors(15))
      ).to.eql(
        [1,3,5,15]
      );
      expect(
        List.toArray(factors(7))
      ).to.eql(
        [1,7]
      );
      // isPrime関数
      // > isPrime(n) で n が素数かどうかを判定します。
      // ~~~haskell
      // isPrime :: Int -> Bool
      // isPrime n = factors n == [1,n]
      // ~~~
      var isPrime = (n) => {
        return List.toArray(factors(n)) === List.toArray(enumFromTo(1,n));
      };
      expect(
        isPrime(15)
      ).to.eql(
        false
      );
      expect(
        isPrime(13)
      ).to.eql(
        false
      );
      next();
    });
    it("フィルターとして使う", (next) => {
      var even = (n) => {
        if(n % 2 === 0) {
          return true;
        } else {
          return false;
        }
      };
      var theList = List.cons(1,List.cons(2,List.cons(3,List.cons(4,List.empty()))));
      expect(
        List.toArray(List.flatMap(theList)((item) => {
          if(even(item)) {
            return List.unit(item);
          } else {
            return List.empty();
          }
        }))
      ).to.eql(
        [2,4]
      );
      next();
    });
    it("2段階のflatMap", (next) => {
      var theNumberList = List.cons(1,List.cons(2,List.empty()));
      var theStringList = List.cons("one",List.cons("two",List.empty()));
      expect(
        List.toArray(List.flatMap(theNumberList)((n) => {
          return List.flatMap(theStringList)((s) => {
            return List.unit([n,s]);
          });
        }))
      ).to.eql(
        [[1,"one"],[1,"two"],[2,"one"],[2,"two"]]
      );
      next();
    });
    describe("Maybeと一緒に使う", () => {
      it("[just(1)]", (next) => {
        var theList = List.cons(Maybe.just(1),
                                List.empty());
        var justList = List.flatMap(theList)((maybeItem) => {
          return Maybe.flatMap(maybeItem)((value) => {
            return List.unit(value);
          });
        });
        expect(
          List.toArray(justList)
        ).to.eql(
          [1]
        );
        next();
      });
      it("[just(1),just(2)]", (next) => {
        var theList = List.cons(Maybe.just(1),
                                List.cons(Maybe.just(2),List.empty()));
        var justList = List.flatMap(theList)((listItem) => {
          return Maybe.flatMap(listItem)((value) => {
            return List.unit(value);
          });
        });
        expect(
          List.toArray(justList)
        ).to.eql(
          [1,2]
        );
        next();
      });
    });
  });
});

// ## <section id='stream_monad'>Streamモナド</section>
// ### Streamモナドの定義
var Stream = {
  match: (data, pattern) => {
     return data.call(Stream,pattern);
  },
  // **Stream#unit**
  /* unit:: ANY -> STREAM */
  unit: (value) => {
    if(value != null){
      return Stream.cons(value, (_) => {
        return Stream.empty();
      });
    } else {
      return Stream.empty();
    }
  },
  empty: (_) => {
    return (pattern) => {
      expect(pattern).to.an('object');
      return pattern.empty();
    };
  },
  cons: (head,tailThunk) => {
    expect(tailThunk).to.a('function');
    return (pattern) => {
      expect(pattern).to.an('object');
      return pattern.cons(head,tailThunk);
    };
  },
  /* head:: STREAM -> MAYBE[STREAM] */
  head: (lazyList) => {
    return Stream.match(lazyList,{
      empty: (_) => {
        return Maybe.nothing();
      },
      cons: (value, tailThunk) => {
        return Maybe.just(value);
      }
    });
  },
  /* tail:: STREAM -> MAYBE[STREAM] */
  tail: (lazyList) => {
    return Stream.match(lazyList,{
      empty: (_) => {
        return Maybe.nothing();
      },
      cons: (head, tailThunk) => {
        return Maybe.just(tailThunk());
      }
    });
  },
  isEmpty: (lazyList) => {
    return Stream.match(lazyList,{
      empty: (_) => {
        return true;
      },
      cons: (head,tailThunk) => {
        return false;
      }
    });
  },
  // **Stream#toArray**
  toArray: (lazyList) => {
    return Stream.match(lazyList,{
      empty: (_) => {
        return [];
      },
      cons: (head,tailThunk) => {
        if(Stream.isEmpty(tailThunk())){
          return [head];
        } else {
          return [head].concat(Stream.toArray(tailThunk()));
        }
      }
    });
  },
  // **Stream#map**
  map: (lazyList) => {
    return (transform) => {
      return Stream.match(lazyList,{
        empty: (_) => {
          return Stream.empty();
        },
        cons: (head,tailThunk) => {
          return Stream.cons(transform(head),(_) => {
            return Stream.map(tailThunk())(transform)});
        }
      });
    };
  },
  // **Stream#append**
  append: (xs) => {
    return (ysThunk) => {
      return Stream.match(xs,{
        empty: (_) => {
          return ysThunk();
        },
        cons: (head,tailThunk) => {
          return Stream.cons(head,(_) => {
            return Stream.append(tailThunk())(ysThunk);
          });
        }
      });
    };
  },
  // **Stream#concat**
  /* concat:: STREAM[STREAM[T]] -> STREAM[T] */
  concat: (astream) => {
    return Stream.match(astream,{
      empty: (_) => {
        return Stream.empty();
      },
      cons: (head,tailThunk) => {
        return Stream.append(head,tailThunk());
      }
    });
  },
  // **Stream#flatten**
  /* flatten :: STREAM[STREAM[T]] => STREAM[T] */
  flatten: (lazyList) => {
    return Stream.match(lazyList,{
      empty: (_) => {
        return Stream.empty();
      },
      cons: (head,tailThunk) => {
        return Stream.append(head)((_) => {
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
  flatMap: (lazyList) => {
    return (transform) => {
      return Stream.flatten(Stream.map(lazyList)(transform));
    };
  },
  // **Stream#foldr**
  foldr: (instanceM) => {
    return (accumulator) => {
      return (glue) => {
        expect(glue).to.a('function');
        return Stream.match(instanceM,{
          empty: (_) => {
            return accumulator;
          },
          cons: (head,tailThunk) => {
            return glue(head)(Stream.foldr(tailThunk())(accumulator)(glue));
          }
        });
      };
    };
  }
};

// ### Streamモナドのテスト
describe('Streamモナドのテスト', () => {
  it("Stream#unit", (next) => {
    Stream.match(Maybe.nothing(null),{
      nothing: (_) => {
        return expect(
          _
        ).to.eql(
          null
        );
      },
      just: (value) => {
        return expect().fail();
      }
    });
    var lazyList = Stream.unit(1);
    expect(
      Maybe.get(Stream.head(lazyList))
    ).to.eql(
      1
    );
    expect(
      Maybe.get(Stream.head(Stream.unit(1)))
    ).to.eql(
      1
    );
    expect(
      Maybe.get(Stream.head(Stream.unit(0)))
    ).to.eql(
      0
    );
    next();
  });
  it("Stream#cons", (next) => {
    var lazyList = Stream.cons(1, (_) => {
      return Stream.cons(2,(_) => {
        return Stream.empty();
      });
    });
    expect(
      Maybe.get(Stream.head(lazyList))
    ).to.eql(
      1
    );
    next();
  });
  it("Stream#tail", (next) => {
    /* lazyList = [1,2] */
    var lazyList = Stream.cons(1, (_) => {
      return Stream.cons(2,(_) => {
        return Stream.empty();
      });
    });
    expect(
      Stream.tail(lazyList)
    ).to.a("function");

    Stream.match(Stream.tail(lazyList),{
      nothing: (_) => {
        expect().fail();
      },
      just: (tail) => {
        Stream.match(tail,{
          empty: (_) => {
            expect().fail();
          },
          cons: (head, tailThunk) => {
            expect(head).to.eql(2);
          }
        });
      }
    });
    expect(
      Maybe.get(Stream.head(Maybe.get(Stream.tail(lazyList))))
    ).to.eql(
      2
    );
    next();
  });
  it("Stream#toArray", (next) => {
    expect(
      Stream.toArray(Stream.empty())
    ).to.eql(
      []
    );
    expect(
      Stream.toArray(Stream.unit(1))
    ).to.eql(
      [1]
    );
    next();
  });
  it("Stream#append", (next) => {
    var xs = Stream.cons(1, (_) => {
      return Stream.empty();
    });
    var ysThunk = (_) => {
      return Stream.cons(2, (_) => {
        return Stream.empty();
      });
    };
    var theStream = Stream.append(xs)(ysThunk);
    expect(
      Maybe.get(Stream.head(theStream))
    ).to.eql(
      1
    );
    expect(
      Maybe.get(Stream.head(Maybe.get(Stream.tail(theStream))))
    ).to.eql(
      2
    );
    next();
  });
  it("Stream#flatten", (next) => {
    /* innerStream = [1,2] */
    var innerStream = Stream.cons(1, (_) => {
      return Stream.cons(2,(_) => {
        return Stream.empty();
      });
    });
    /* outerStream = [[1,2]] */
    var outerStream = Stream.unit(innerStream);
    var flattenedStream = Stream.flatten(outerStream);
    Stream.match(flattenedStream,{
      empty: (_) => {
        expect().fail()
      },
      cons: (head,tailThunk) => {
        expect(head).to.eql(1)
      }
    });
    expect(
      Maybe.get(Stream.head(flattenedStream))
    ).to.eql(
      1
    );
    expect(
      Maybe.get(Stream.head(Maybe.get(Stream.tail(flattenedStream))))
    ).to.eql(
      2
    );
    next();
  });
  describe("Stream#map", () => {
    it("mapで要素を2倍にする", (next) => {
      /* lazyList = [1,2] */
      var lazyList = Stream.cons(1, (_) => {
        return Stream.cons(2,(_) => {
          return Stream.empty();
        });
      });
      var doubledLazyList = Stream.map(lazyList)((item) => {
        return item * 2;
      });
      expect(
        Maybe.get(Stream.head(doubledLazyList))
      ).to.eql(
        2
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(doubledLazyList))))
      ).to.eql(
        4
      );
      expect(
        Stream.toArray(doubledLazyList)
      ).to.eql(
        [2,4]
      );
      next();
    });
    // Streamモナドで無限の整数列を作る
    it("無限の整数列を作る", (next) => {
      var ones = Stream.cons(1, (_) => {
        return ones;
      });
      expect(
        Maybe.get(Stream.head(ones))
      ).to.eql(
        1
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(ones))))
      ).to.eql(
        1
      );
      var twoes = Stream.map(ones)((item) => {
        return item * 2;
      });
      expect(
        Maybe.get(Stream.head(twoes))
      ).to.eql(
        2
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(twoes))))
      ).to.eql(
        2
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(Maybe.get(Stream.tail(twoes))))))
      ).to.eql(
        2
      );
      next();
    });
    it("整数列を作る", (next) => {
      var integersFrom = (from) => {
        return Stream.cons(from, (_) => {
          return integersFrom(from + 1);
        });
      };
      expect(
        Maybe.get(Stream.head(integersFrom(0)))
      ).to.eql(
        0
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(integersFrom(0)))))
      ).to.eql(
        1
      );
      var doubledIntergerMapped = Stream.map(integersFrom(0))((integer) => {
        return integer * 2;
      });
      expect(
        Maybe.get(Stream.head(doubledIntergerMapped))
      ).to.eql(
        0
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(doubledIntergerMapped))))
      ).to.eql(
        2
      );
      var doubledInterger = Stream.flatMap(integersFrom(0))((integer) => {
        return Stream.unit(integer * 2);
      });
      expect(
        Maybe.get(Stream.head(doubledInterger))
      ).to.eql(
        0
      );
      expect(
        Maybe.get(Stream.head(Maybe.get(Stream.tail(doubledInterger))))
      ).to.eql(
        2
      );
      next();
    });
    it("一段階のflatMap", (next) => {
      var ones = Stream.cons(1, (_) => {
        return ones;
      });
      var twoes = Stream.flatMap(ones)((one) => {
        expect(one).to.a('number');
        return Stream.unit(one * 2);
      });
      expect(
        Maybe.get(Stream.head(twoes))
      ).to.eql(
        2
      );
      next();
    });
    it("二段階のflatMap", (next) => {
      // ~~~scala
      // scala> val nestedNumbers = List(List(1, 2), List(3, 4))
      // scala> nestedNumbers.flatMap(x => x.map(_ * 2))
      // res0: List[Int] = List(2, 4, 6, 8)
      // ~~~
      var innerStream12 = Stream.cons(1, (_) => {
        return Stream.cons(2,(_) => {
          return Stream.empty();
        });
      });
      var innerStream34 = Stream.cons(3, (_) => {
        return Stream.cons(4,(_) => {
          return Stream.empty();
        });
      });
      /* nestedStream = [[1,2],[3,4]] */
      var nestedStream = Stream.cons(innerStream12, (_) => {
        return Stream.cons(innerStream34,(_) => {
          return Stream.empty();
        });
      });
      var flattenedStream = Stream.flatMap(nestedStream)((innerStream) => {
        return Stream.flatMap(innerStream)((n) => {
          expect(n).to.a('number');
          return Stream.unit(n * 2);
        });
      });
      expect(
        Maybe.get(Stream.head(flattenedStream))
      ).to.eql(
        2
      );
      expect(
        Stream.toArray(flattenedStream)
      ).to.eql(
        [2,4,6,8]
      );
      next();
    });

  });
}); // Streamモナド

// ## <section id='reader_monad'>Readerモナド</section>
//   c.f. [グローバル変数の代わりに使えるReaderモナドとWriterモナド](http://itpro.nikkeibp.co.jp/article/COLUMN/20090303/325807/?rt=nocnt)
//
// ### Readerモナドの定義
// ~~~haskell
// newtype Reader e a = Reader { runReader :: e -> a }
//
// class MonadReader e m | m -> e where 
//     ask   :: m e
//     local :: (e -> e) -> m a -> m a 
// instance MonadReader (Reader e) where 
//     ask       = Reader id 
//     local f c = Reader $ \e -> runReader c (f e) 
// instance Monad (Reader env) where
//     return a = Reader $ \_ -> a
//     m >>= f  = Reader $ \env -> runReader (f (runReader m env)) env
// ~~~
var Reader = {
  unit: (a) => {
    return {
      run: (_) => { // runReader :: Reader r a -> r -> a
        return a;
      }
    };
  },
  flatMap: (reader) => {
    return (f) => { // transform:: a -> a
      return {
        run: (env) => {
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
    run: (env) => {
      return env;
    }
  },
  // **Reader#local**
  // ~~~haskell
  // local f c = Reader $ \e -> runReader c (f e) 
  // ~~~
  local: (f) => {
    return (reader) => {
      return {
        run: (env) => {
          return reader.run(f(env));
        }
      };
    };
  }
};

// ### Readerモナドのテスト
describe("Readerモナドをテストする",() => {
  // localのテスト
  // c.f. "Real World Haskell",p.432
  it("localのテスト", (next) => {
    var myName = (step) => {
      return Reader.flatMap(Reader.ask)((name) => {
        return Reader.unit(step + ", I am " + name);
      });
    };
    var localExample = Reader.flatMap(myName("First"))((a) => {
      var appendDy = (env) => {
        return env + "dy";
      };
      return Reader.flatMap(Reader.local(appendDy)(myName("Second")))((b) => {
        return Reader.flatMap(myName("Third"))((c) => {
          return Reader.unit(a + ", " + b + ", " + c);
        });
      });
    });
    expect(
      localExample.run("Fred")
    ).to.eql(
      "First, I am Fred, Second, I am Freddy, Third, I am Fred"
    );
    next();
  });
  it("データベースのコネクションを模倣する", (next) => {
    var config = {
      host: "127.0.0.1",
      port: "27017"
    }; 
    var connect = Reader.flatMap(Reader.ask)((config) => {
      return Reader.unit(config.host + ":" + config.port);
    });
    expect(
      connect.run(config)
    ).to.eql(
      "127.0.0.1:27017"
    );
    next();
  });
});

// ## <section id='writer_monad'>Writerモナド</section>
// c.f. [グローバル変数の代わりに使えるReaderモナドとWriterモナド（3/4）](http://itpro.nikkeibp.co.jp/article/COLUMN/20090303/325807/?P=3&rt=nocnt)
// ### Writerモナドの定義
// ~~~haskell
// newtype Writer w a = Writer { run :: (a,w) }
//
// instance (Monoid w) => Monad (Writer w) where
//     return a = Writer (a, empty)
//     (Writer (a, v)) >>= f  = let (Writer (b, v')) = f a
//                              in Writer (b, v `append`v')
// ~~~
var Writer = {
  unit: (a) => {
    return {
      run: (_) => {
        return Pair.cons(List.empty(),a);
      }
    };
  },
  flatMap: (writer) => {
    var writerPair = writer.run();
    var v = Pair.left(writerPair);
    var a = Pair.right(writerPair);
    return (f) => { // transform:: a -> a
      var newPair = f(a).run();
      var v_ = Pair.left(newPair);
      var b = Pair.right(newPair);
      return {
        run: () => {
          return Pair.cons(List.append(v)(v_),b);
        }
      };
    };
  },
  // ~~~haskell
  // tell :: Monoid w => w -> Writer w ()  -- tell関数は、値wをもとにログを作成する
  // tell s = Writer ((), s)
  // ~~~
  tell: (s) => {
    return {
      run: (_) => {
        return Pair.cons(s, List.empty());
      }
    };
  }
};

// ### Writerモナドのテスト
describe("Writerモナドをテストする",() => {
  // Writerモナドを用いたfactorialの例
  //
  // ~~~haskell
  // factW :: Int -> Writer [Int] Int
  // factW 0 = tell [0] >> return 1
  // factW n = do
  //   tell [n]
  //   n1 <- factW (pred n)
  //   return $ n * n1
  // ~~~
  // 実行する場合はrunWriter
  // ~~~haskell
  // *Main> runWriter $ factW 5
  // (120,[5,4,3,2,1,0])
  // ~~~
  it("Writerモナドを用いたfactorialの例", (next) => {
    var pred = (n) => {
      return n - 1;
    };
    var factorial = (n) => {
      if(n === 0) {
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
    factorial(0).run().match({
      cons: (left, right) => {
        expect(
          List.toArray(left)
        ).to.eql(
          [0]
        );
        expect(
          right
        ).to.eql(
          1
        );
      }
    });
    factorial(5).run().match({
      cons: (left, right) => {
        expect(
          List.toArray(left)
        ).to.eql(
          [5,4,3,2,1,0]
        );
        expect(
          right
        ).to.eql(
          120
        );
      }
    });
    next();
  });
});

// ## <section id='io_monad'>IOモナド</section>
// ### IOモナドの定義
var IO = {
  /* unit:: T => IO[T] */
  unit : (any) => {
    return (_) =>  { // 外界は明示する必要はありません
      return any;
    };
  },
  /* flatMap:: IO[T] => FUN[T => IO[U]] => IO[U] */
  flatMap : (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO[b]
      return IO.unit(IO.run(actionAB(IO.run(instanceA))));
    };
  },
  // **IO#done**関数
  // 
  // > IOアクションを何も実行しない
  /* done:: T => IO[T] */
  done : (any) => {
    return IO.unit();
  },
  // **IO#run**関数
  //
  // > IOアクションを実行する
  /* run:: IO[A] => A */
  run : (instanceM) => {
    return instanceM();
  },
  // **IO#readFile**
  /* readFile:: STRING => IO[STRING] */
  readFile : (path) => {
    return (_) => {
      var content = fs.readFileSync(path, 'utf8');
      return IO.unit(content)(_);
    };
  },
  // **IO#println**
  /* println:: STRING => IO[null] */
  println : (message) => {
    return (_) => {
      console.log(message);
      return IO.unit(null)(_);
    };
  },
  // **IO#writeFile**
  writeFile : (path) => {
    return (content) => {
      return (_) => {
        fs.writeFileSync(path,content);
        return IO.unit(null)(_);
      };
    };
  },
  // **IO#seq**
  /* IO.seq:: IO[a] => IO[b] => IO[b] */
  seq: (instanceA) => {
    return (instanceB) => {
      return IO.flatMap(instanceA)((a) => {
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
  seqs: (alist) => {
    var self = this;
    return alist.match({
      empty: () => {
        return self.done();
      },
      cons: (head, tail) => {
        return self.flatMap(head)((_) => {
          return self.seqs(tail); 
        }); 
      }
    });
  },
  // **IO#putc**
  /* IO.putc:: CHAR => IO[] */
  putc: (character) => {
    return (io) => {
      process.stdout.write(character);
      return null;
    };
  },
  // **IO#puts**
  // ~~~haskell
  // puts list = seqs (map putc list)
  // ~~~
  /* IO.puts:: LIST[CHAR] => IO[] */
  puts: (alist) => {
    return List.match(alist, {
      empty: () => {
        return IO.done();
      },
      cons: (head, tail) => {
        return IO.seq(IO.putc(head))(IO.puts(tail));
      }
    });
  },
  // **IO#getc**
  /* IO.getc :: IO[CHAR] */
  getc: () => {
    var continuation = () => {
      var chunk = process.stdin.read();
      return chunk;
    }; 
    process.stdin.setEncoding('utf8');
    return process.stdin.on('readable', continuation);
  }
};

// ### IOモナドをテストする
describe("IOモナドをテストする",() => {
  // IOモナドで参照透過性を確保する
  it('IOモナドで参照透過性を確保する', (next) => {
    // 本文では割愛しましたが、IOモナドが入出力に対して参照透過性を確保していることを単体テストで示します。
    expect(
      IO.flatMap(IO.readFile("./test/resources/file.txt"))((content) => {
        return IO.flatMap(IO.println(content))((_) => {
          return IO.done(_);
        });
      })()
    ).to.eql(
      IO.flatMap(IO.readFile("./test/resources/file.txt"))((content) => {
        return IO.flatMap(IO.println(content))((_) => {
          return IO.done(_);
        });
      })()
    );
    next();
  });
});



describe("Maybeと一緒に使う", () => {
  it("[just(1),just(2)]", (next) => {
    var theList = List.cons(Maybe.just(1),
                            List.cons(Maybe.just(2),List.empty()));
    var justList = List.flatMap(theList)((listItem) => {
      return Maybe.flatMap(listItem)((value) => {
        return List.unit(value);
      });
    });
    expect(
      List.toArray(justList)
    ).to.eql(
      [1,2]
    );
    next();
  });
  it("[just(1)]", (next) => {
    var theList = List.cons(Maybe.just(1),
                            List.empty());
    var justList = List.flatMap(theList)((maybeItem) => {
      return Maybe.flatMap(maybeItem)((value) => {
        return List.unit(value);
      });
    });
    expect(
      List.toArray(justList)
    ).to.eql(
      [1]
    );
    next();
  });
});

// ## <section id='st_monad'>STモナド</section>
//   
// STモナドとは、状態の変化を取りこんだモナドのこと。
// 
// ### STモナドの定義
// 
// [Programming in Haskell(2版)](https://www.amazon.co.jp/Programming-Haskell-Graham-Hutton/dp/1316626229/ref=pd_sim_14_26?_encoding=UTF8&psc=1&refRID=ZT8DRRF420JN97H9H4DW),p.168〜p.172 を参照してください。
//
// ~~~haskell
// newtype ST a = S(State -> (a, State))
//
// app :: ST a -> State -> (a,State)
// app (S st) x = st x
//
// instance Monad ST where
//   -- (>>=) :: ST a -> (a -> ST b) -> ST b
//   st >>= f = S(\state -> 
//                   let (x, state') = app st state 
//                   in app (f x) state'
//               )
//   unit :: a -> ST a
//   unit x = S(\s -> (x,s))
// 
//   get = S(\s -> (s,s))
//   put newState = S(\s -> ((), newState))
// ~~~
//
var ST = {
  // **ST#unit**
  // 
  // > 第1引数valueの値に第2引数stateという状態を付加する。
  unit: (value) => { 
    return (state) => { 
      return Pair.cons(value,state);
    };
  },
  // **ST#flatMap**
  // 
  // > 第1引数instanceMを実行して、新しい状態state_と計算結果xを得る。
  // 計算結果xに関数fを適用すると、f(x)というSTモナドインスタンスが得られる。
  // 最後に、そのf(x)に新しい状態state_を適用して、最終結果を得る。
  flatMap: (instanceM) => { // instanceM:: ST a
    return (f) => { // f:: a -> ST b
      expect(f).to.a('function');
      return (state) => {
        var newState = ST.app(instanceM)(state); // instanceM(state)
        return newState.match({
          cons:(x, state_) => {
            return ST.app(f(x))(state_);
          }
        });
      };
    };
  },
  app: (st) => {
    return (state) => {
      return st(state);
    };
  },
  // **ST#get**
  // > 現在の状態を取ってきて、それを結果として提示する
  get: (_) => {
    return (state) => {
      return Pair.cons(state,state);
    };
  },
  // **ST#put**
  // > 現在の状態stateを新しい状態newStateに更新する
  put: (newState) => { 
    return (state) => { 
      return Pair.cons(Pair.empty(),newState);
    };
  }
};

// ### STモナドのテスト
describe("STモナドをテストする",() => {
  // #### スタック操作を実現する 
  describe("Stackを作る",() => {
    // **pop関数**の定義
    /* pop:: State Stack Int */
    var pop = (_) => {
      return ST.flatMap(ST.get())((alist) => {
        return List.match(alist,{
          cons: (x,xs) => {
            return ST.flatMap(ST.put(xs))((_) => {
              return ST.unit(x);
            });
          }
        });
      });
    };
    // **push関数**の定義
    /* push:: Int -> State Stack () */
    var push = (x) => {
      return ST.flatMap(ST.get())((xs) => {
        return ST.put(List.cons(x,xs));
      }); 
    };
    it('スタックを操作する', (next) => {
      var stackManip = ST.flatMap(push(3))((_) => {
        return ST.flatMap(pop())((a) => {
          return pop();
        });
      });
      expect(
          Pair.left(
              ST.app(
                  stackManip
                  )(
                      List.fromArray([5,8,2,1])
                   )
              )
          ).to.eql(
              5
          );
    expect(
        List.toArray(Pair.right(
                ST.app(
                    stackManip
                    )(
                        List.fromArray([5,8,2,1])
                     )
                )
            )).to.eql(
               [8,2,1]
            );
      next();
    });
  });
  describe("Treeの例",() => {
    // ~~~haskell
    // data Tree a = Leaf a | Node (Tree a) (Tree a)
    // ~~~
    var Tree = {
      match: (data, pattern) => {
       return data.call(data, pattern);
      },
      leaf: (value) => {
         return (pattern) => {
           return pattern.leaf(value);
         };
      },
      node: (left, right) => {
         return (pattern) => {
           return pattern.node(left, right);
         };
      },
      toArray: (tree) => {
        return Tree.match(tree,{
          leaf:(value) => {
            return value;
          },
          node:(left, right) => {
            return [Tree.toArray(left), Tree.toArray(right)];
          }
        });
      },
      // ~~~haskell
      // fmap f (Leaf x) = Leaf (f x)
      // fmap f (Node left right) = Node (fmap f left) (fmap f right)
      // ~~~
      map: (f) => {
        return (tree) => {
          return Tree.match(tree,{
            leaf:(value) => {
              return Tree.leaf(f(value));
            },
            node:(left, right) => {
              return Tree.node(Tree.map(f)(left),Tree.map(f)(right) );
            }
          });
        };
      }
    };
    it('Tree.toArray', (next) => {
      expect(
        Tree.toArray(Tree.leaf(1))
      ).to.eql(
        1
      );
      expect(
        Tree.toArray(Tree.node(Tree.leaf(1),Tree.leaf(2)))
      ).to.eql(
        [1,2]
      );
      expect(
        Tree.toArray(Tree.node(Tree.leaf(1),
                               Tree.node(Tree.leaf(2),Tree.leaf(3))))
      ).to.eql(
        [1,[2,3]]
      );
      next();
    });
    it('rlabel', (next) => {
      // ~~~haskell
      // rlabel :: (TREE, STATE) -> (TREE,STATE)
      // ~~~
      var rlabel = (tree, state) => {
        return Tree.match(tree,{
          leaf:(value) => {
            return Pair.cons(Tree.leaf(state), state + 1);
          },
          node:(left, right) => {
            var leftNode = rlabel(left, state);
            var rightNode = rlabel(right, Pair.right(leftNode));
            return Pair.cons(Tree.node(Pair.left(leftNode), 
                                       Pair.left(rightNode)), 
                             Pair.right(rightNode));
          }
        });
      };
      expect(
        Tree.toArray(Pair.left(rlabel(Tree.leaf(1),0)))
      ).to.eql(
        0
      );
      expect(
        Tree.toArray(Pair.left(rlabel(Tree.node(Tree.leaf("a"),Tree.leaf("b")),0)))
      ).to.eql(
        [0,1]
      );
      next();
    });
    it('mlabel', (next) => {
      var fresh = (state) => {
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
      var mlabel = (tree) => {
        return Tree.match(tree,{
          leaf:(_) => {
            return ST.flatMap(fresh)((n) => {
              return ST.unit(Tree.leaf(n));
            });
          },
          node:(left, right) => {
            return ST.flatMap(mlabel(left))((left_) => {
              return ST.flatMap(mlabel(right))((right_) => {
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
                  )(0))
          )
      ).to.eql(
        0
      );
      expect(
        Tree.toArray(
          Pair.left(ST.app(mlabel(Tree.node(Tree.leaf("a"),Tree.leaf("b"))))(0))
        )
      ).to.eql(
        [0,1]
      );
      next();
    });
  });
});

// ## <section id='cont_monad'>Contモナド</section>
// Contモナド(継続モナド)は、中断や再開が可能な計算をモデル化します。
// 
// ### Contモナドの定義
// 
//
// ~~~haskell
// newtype Cont r a = Cont { runCont :: ((a -> r) -> r) } -- r は計算全体の最終の型
// instance Monad (Cont r) where 
//     return a       = Cont $ \k -> k a                       
//     -- i.e. return a = \k -> k a 
//     (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k) 
//     -- i.e. m >>= f = \k -> m (\a -> f a k) 
// ~~~

var Cont = {
  // ~~~haskell
  // return a       = Cont $ \k -> k a
  // ~~~
  unit: (a) => {
    return (k) => {
      return k(a);
    };
  },
  flatMap: (m) => {
    return (f) => { // f:: a -> Cont r a
      expect(f).to.a('function');
      return (k) => {
        return m((a) => {
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
  callCC: (f) => {
    return (k) =>{ 
      return f((a) => {
        return (_) => {
          return k(a);
        }; 
      })(k);
    };
  }
};

// ### Contモナドのテスト
describe("Contモナドをテストする",() => {
  var identity = (x) => {
    return x;
  };
  // ~~~haskell
  // *Main> let s3 = Cont (square 3)
  // *Main> print =: runCont s3
  // 9 
  // ~~~
  it('square', (next) => {
    // ~~~haskell
    // square :: Int -> ((Int -> r) -> r)
    // square x = \k -> k (x * x)
    // ~~~
    var square = (n) => {
      return n * n;
    };
    var square3 = Cont.unit(square(3)); 
    expect(
      square3(identity)
    ).to.eql(
      9
    );
    next();
  });
  // **Cont.flatMap**で算術演算を組み合わせる例
  it('Cont.flatMapで算術演算を組み合わせる例', (next) => {
    var addCPS = (n,m) => {
      var add = (n,m) => {
        return n + m;
      };
      return Cont.unit(add(n,m)); 
    };
    expect(
      addCPS(2,3)(identity)
    ).to.eql(
      5
    );
    var multiplyCPS = (n,m) => {
      var multiply = (n,m) => {
        return n * m;
      };
      return Cont.unit(multiply(n,m)); 
    };
    var subtractCPS = (n,m) => {
      var subtract = (n,m) => {
        return n - m;
      };
      return Cont.unit(subtract(n,m)); 
    };
    /* ((2 + 3) * 4) - 5 = 15 */
    expect(
      Cont.flatMap(addCPS(2,3))((addResult) => {
        return Cont.flatMap(multiplyCPS(addResult,4))((multiplyResult) => {
          return Cont.flatMap(subtractCPS(multiplyResult,5))((result) => {
            return Cont.unit(result);
          });
        });
      })(identity)
    ).to.eql(
      15
    );
    next();
  });
  describe("callCCを利用する",() => {
    it('square using callCC', (next) => {
      // ~~~haskell
      // -- Without callCC
      // square :: Int -> Cont r Int
      // square n = return (n ˆ 2)
      // -- With callCC
      // squareCCC :: Int -> Cont r Int
      // squareCCC n = callCC $ \k -> k (n ˆ 2) 
      // ~~~
      var squareCPS = (n) => {
        return Cont.unit(n * n);
      };
      expect(
        squareCPS(2)(identity)
      ).to.eql(
        4
      );
      var safeDivideCC = (n,m) => {
        return Cont.callCC((k) => {
          if(m !== 0) {
            return k(n / m);
          }
          return k(null);
        });
      };
      expect(
        safeDivideCC(4,2)(identity)
      ).to.eql(
        2
      );
      expect(
        safeDivideCC(4,0)(identity)
      ).to.be(
        null
      );
      next();
    });
    it('even', (next) => {
      var even = (n) => {
        return (n % 2) === 0;
      };
      expect(
        even(3 * Cont.callCC((k) => {
          return k(1 + 2);
        }))
      ).to.eql(
        false
      );
      next();
    });
  });
});
// [目次に戻る](index.html) 
