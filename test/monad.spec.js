"use strict";

// モナド
// =====

var fs = require('fs');
var expect = require('expect.js');


var pair = {
  match : (data, pattern) => {
    return data.call(pair, pattern);
  },
  cons: (left, right) => {
    return (pattern) => {
      return pattern.cons(left, right);
    };
  },
  right: (tuple) => {
    return pair.match(tuple, {
      cons: (left, right) => {
        return right;
      }
    });
  },
  left: (tuple) => {
    return pair.match(tuple, {
      cons: (left, right) => {
        return left;
      }
    });
  }
};

// ## IDモナド
var ID = {
  /* unit:: T => ID[T] */
  unit: (value) => {  // 単なる identity関数と同じ
    return value;
  },
  /* flatMap:: ID[T] => FUN[T => ID[T]] => ID[T] */
  flatMap: (instanceM) => {
    return (transform) => {
      return transform(instanceM); // 単なる関数適用と同じ
    };
  }
};


// ## Maybeモナド
var Maybe = {
  match: (data, pattern) => {
     return data.call(Maybe,pattern);
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
  unit : (value) => {
    return Maybe.just(value);
  },
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
  get: (maybe) => {
    return Maybe.getOrElse(maybe)(null);
    // return Maybe.match(maybe,{
    //   just: (value) => {
    //     return value;
    //   },
    //   nothing: (_) => {
    //     return null;
    //   }
    // });
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
  },
  map : (maybeInstance) => {
    return (transform) => {
      expect(transform).to.a('function');
      return Maybe.match(maybeInstance,{
        just: (value) => {
          return Maybe.unit(transform(value));
        },
        nothing: (_) => {
          return Maybe.nothing(_);
        }
      });
    };
  }
};

// ### Maybeモナドのテスト
describe("Maybeモナドをテストする",() => {
  it("Maybe#flatMap", (next) => {
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
  it("add(maybe, maybe)", (next) => {
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
      Maybe.isEqual(add(justOne,justTwo))(justThree)
    ).to.eql(
      true
    );
    expect(
      Maybe.isEqual(add(justOne,Maybe.nothing()))(Maybe.nothing())
    ).to.eql(
      true
    );
    next();
  });
});

// ## Listモナド
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
  unit: (value) => {
    return List.cons(value, List.empty());
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
  // append:: LIST[T] -> LIST[T] -> LIST[T]
  // ~~~haskell
  // append [] ys = ys
  // append (x:xs) ys = x : (xs ++ ys)
  // ~~~
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
  // concat:: LIST[LIST[T]] -> LIST[T]
  // ~~~haskell
  // concat [] = []
  // concat (xs:xss) = xs ++ concat xss
  // ~~~
  concat: (list_of_list) => {
    return List.foldr(list_of_list)(List.empty())(List.append);
  },
  // ~~~haskell
  // flatten :: [[a]] -> [a]
  // flatten =  foldr (++) []
  // ~~~
  flatten: (instanceMM) => {
    return List.concat(instanceMM);
  },
  // map:: LIST[T] -> FUN[T->U] -> LIST[U]
  // ~~~haskell
  // map [] _ = []
  // map (x:xs) f = f x : map xs f
  // ~~~
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
  // ~~~haskell
  // xs >>= f = concat (map f xs)
  // ~~~
  flatMap: (instanceM) => {
    return (transform) => { // FUN[T->LIST[T]]
      expect(transform).to.a('function');
      return List.concat(List.map(instanceM)(transform));
    };
  },
  /* #@range_end(list_monad_definition) */
  // 1段階のリストしか配列に変更できない
  toArray: (alist) => {
    return List.foldr(alist)([])((item) => {
      return (accumulator) => {
        return [item].concat(accumulator);
      };
    });
  },
  // foldr:: LIST[T] -> T -> FUN[T -> U -> U] -> T
  // ~~~haskell
  // foldr []     z _ = z
  // foldr (x:xs) z f = f x (foldr xs z f) 
  // ~~~
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
  it("'List#empty'", (next) => {
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
  it("'List#isEmpty'", (next) => {
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
  it("'List#cons'", (next) => {
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
  it("'List#concat'", (next) => {
    /* list = [[1,2],[3,4]] */
    var one_two = List.cons(1,List.cons(2,List.empty()));
    var three_four = List.cons(3,List.cons(4,List.empty()));

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
  it("'List#foldr'", (next) => {
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
  it("'List#toArray'", (next) => {
    /* list = [1,2,3,4] */
    var theList = List.cons(1,List.cons(2,List.cons(3,List.cons(4,List.empty()),List.empty)));
    expect(
      List.toArray(theList)
    ).to.eql(
      [1,2,3,4]
    );
    next();
  });
  describe("'List#toArray'", () => {
    it("1段階のリストを配列に変換する", (next) => {
      // list = [1,2,3,4]
      var theList = List.cons(1,List.cons(2,List.cons(3,List.cons(4,List.empty()),List.empty)));
      expect(
        List.toArray(theList)
      ).to.eql(
        [1,2,3,4]
      );
      next();
    });
    it("2段階のリストを配列に変換する", (next) => {
      // list = [[1],[2]] 
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
  it("'List#map'", (next) => {
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
  describe("List#flatMap", () => {
    it("条件でフィルターする", (next) => {
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
    it("'List#flatMap'", (next) => {
      // list = [1]
      var theList = List.cons(1, List.empty());
      expect(
        List.toArray(List.flatMap(theList)((item) => {
          return List.unit(item * 2); 
        }))
      ).to.eql(
        [2]
      );
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
    it("'List#flatMap'", (next) => {
      // list = [1,2,3]
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
  });
  it("'List#unit'", (next) => {
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
  it("'List#flatMap'", (next) => {
    /* list = [1,2,3] */
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
  describe("Listモナドを活用する",() => {
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
    /* #@range_begin(list_maybe) */
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
      // it("[nothing()]", (next) => {
      //   // var theList = list.unit(maybe.nothing());
      //   var theList = list.cons(maybe.nothing(null),
      //                           list.empty());
      //   var justList = list.flatMap(theList)((listItem) => {
      //     return maybe.flatMap(listItem)((value) => {
      //       return list.unit(value);
      //       // return list.unit(value);
      //     });
      //   });
      //   expect(
      //     list.toArray(justList)
      //   ).to.eql(
      //     []
      //   );
      //   next();
      // });
      // it("[just(1),nothing()]", (next) => {
      //   // theList:: LIST[MAYBE[NUM]]
      //   //           [just(1), nothing()]
      //   var theList = list.cons(maybe.just(1),
      //                           list.cons(maybe.nothing(),list.empty()));
      //   var justList = list.flatMap(theList)((maybeItem) => {
      //     return maybe.flatMap(maybeItem)((value) => {
      //       return list.unit(value);
      //     });
      //   });
      //   expect(
      //     list.toArray(justList)
      //   ).to.eql(
      //     [2,4]
      //   );
      //   // expect(
      //   //   list.toArray(list.flatMap.call(list,theList)((maybeItem) => {
      //   //     return maybe.flatMap.call(maybe,maybeItem)((value) => {
      //   //       return list.unit.call(list,value);
      //   //     });
      //   //   }))
      //   // ).to.eql(
      //   //   [2,4]
      //   // );
      //   next();
      // });
      /* #@range_end(list_maybe) */
    });
  });
});

// ## Streamモナド
var Stream = {
  match: (data, pattern) => {
     return data.call(Stream,pattern);
  },
  // ### Stream#unit
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
  // ### Stream#toArray
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
  // ### Stream#map
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
  // ## Stream#append
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
  // ## Stream#concat
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
  // ## Stream#flatten
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
  // ### Stream#flatMap
  // flatMap:: STREAM[T] -> FUNC[T->STREAM[T]] -> STREAM[T]
  // ~~~haskell
  // flatMap xs f = flatten (map f xs)
  //~~~
  flatMap: (lazyList) => {
    return (transform) => {
      return Stream.flatten(Stream.map(lazyList)(transform));
    };
  },
  // ### monad.stream#foldr
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
    it("無限の整数列を作る", (next) => {
      /* #@range_begin(ones_infinite_sequence) */
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
      /* #@range_end(ones_infinite_sequence) */
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
      /*
        scala> val nestedNumbers = List(List(1, 2), List(3, 4))
        scala> nestedNumbers.flatMap(x => x.map(_ * 2))
        res0: List[Int] = List(2, 4, 6, 8)
      */
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

// ## Readerモナド
// ~~~haskell
// newtype Reader e a = Reader { runReader :: e -> a }
//
// instance Monad (Reader r) where
//     return a = Reader $ \_ -> a
//     m >>= f  = Reader $ \r -> runReader (f (runReader m r)) r
// ~~~

var Reader = {
  unit: (x) => {
    return (_) => {
      return x;
    };
  },
  flatMap: (instanceM) => {
    return (transform) => {
      return (w) => {
        return transform(instanceM(w))(w);
      };
    };
  }
};
describe("Readerモナドをテストする",() => {
});

// ## IOモナド
var IO = {
  // unit:: T => IO[T]
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
  // ### IO.done関数
  // 
  // IOアクションを何も実行しない
  /* done:: T => IO[T] */
  done : (any) => {
    return IO.unit();
  },
  // ### IO.run関数
  //
  // IOアクションを実行する
  /* run:: IO[A] => A */
  run : (instanceM) => {
    return instanceM();
  },
  // readFile:: STRING => IO[STRING]
  readFile : (path) => {
    return (_) => {
      var content = fs.readFileSync(path, 'utf8');
      return IO.unit(content)(_);
    };
  },
  // println:: STRING => IO[null]
  println : (message) => {
    return (_) => {
      console.log(message);
      return IO.unit(null)(_);
    };
  },
  writeFile : (path) => {
    return (content) => {
      return (_) => {
        fs.writeFileSync(path,content);
        return IO.unit(null)(_);
      };
    };
  },
  // IO.seq:: IO[a] => IO[b] => IO[b]
  seq: (instanceA) => {
    return (instanceB) => {
      return IO.flatMap(instanceA)((a) => {
        return instanceB;
      });
    };
  },
  seqs: (alist) => {
    return List.foldr(alist)(List.empty())(IO.done());
  },
  // IO.putc:: CHAR => IO[]
  putc: (character) => {
    return (io) => {
      process.stdout.write(character);
      return null;
    };
  },
  // IO.puts:: LIST[CHAR] => IO[]
  // ~~~haskell
  // puts list = seqs (map putc list)
  // ~~~
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
  // IO.getc :: IO[CHAR]
  getc: () => {
    var continuation = () => {
      var chunk = process.stdin.read();
      return chunk;
    }; 
    process.stdin.setEncoding('utf8');
    return process.stdin.on('readable', continuation);
  }
};

describe("IOモナドをテストする",() => {
});


describe('Treeモナド', () => {
  var tree  = {
    match: (data, pattern) => {
      return data.call(tree,pattern);
    },
    leaf: (value) => {
      return (pattern) => {
        return pattern.leaf(value);
      };
    },
    node: (treeL, treeR) => {
      return (pattern) => {
        return pattern.node(treeL, treeR);
      };
    },
    unit: (value) => {
      return tree.leaf(value);
    },
    flatMap: (instanceM) => {
      return (transform) => {
        return tree.match(instanceM,{
          leaf: (value) => {
            return transform(value);
          },
          node: (treeL, treeR) => {
            return tree.node(tree.flatMap(treeL)(transform),
                             tree.flatMap(treeL)(transform));
          }
        }); 
      };
    },
    map: (instanceM) => {
      return (transform) => {
        return tree.match(instanceM,{
          leaf: (value) => {
            return tree.leaf(transform(value));
          },
          node: (treeL, treeR) => {
            return tree.node(tree.map(treeL)(transform),
                             tree.map(treeL)(transform));
          }
        }); 
      };
    }
  };
});

describe('Consoleモナド', () => {
  /* unit:: A -> IO A */
  var unit = (any) => {
    return (_) => {
      return any;
    };
  };
  /* flatMap:: IO a -> (a -> IO b) -> IO b */
  var flatMap = (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return unit(run(actionAB(run(instanceA))));
    };
  };

  /* run:: IO A -> A */
  var run = (instance) => {
    return instance();
  };
  expect(run(unit(1))).to.eql(1);
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
  // it("[nothing()]", (next) => {
  //   // var theList = list.unit(maybe.nothing());
  //   var theList = list.cons(maybe.nothing(1),
  //                           list.empty());
  //   // var justList = list.map(theList)(maybe.unit);
  //   var justList = list.flatMap(theList)((maybeItem) => {
  //     return maybe.flatMap(maybeItem)((value) => {
  //       return list.unit(value);
  //     });
  //   });
  //   expect(
  //     list.toArray(justList)
  //   ).to.eql(
  //     []
  //   );
  //   next();
  // });
  // it("[just(1),nothing()]", (next) => {
  //   // theList:: LIST[MAYBE[NUM]]
  //   //           [just(1), nothing()]
  //   var theList = list.cons(maybe.just(1),
  //                           list.cons(maybe.nothing(),list.empty()));
  //   var justList = list.flatMap(theList)((maybeItem) => {
  //     return maybe.flatMap(maybeItem)((value) => {
  //       return list.unit(value);
  //     });
  //   });
  //   expect(
  //     list.toArray(justList)
  //   ).to.eql(
  //     [2,4]
  //   );
  //   // expect(
  //   //   list.toArray(list.flatMap.call(list,theList)((maybeItem) => {
  //   //     return maybe.flatMap.call(maybe,maybeItem)((value) => {
  //   //       return list.unit.call(list,value);
  //   //     });
  //   //   }))
  //   // ).to.eql(
  //   //   [2,4]
  //   // );
  //   next();
  // });
  /* #@range_end(list_maybe) */
});

// ## Variantモナド
var Variant = {
  unit: (value, key) => {
    return (pattern) => {
      return pattern[key](value);
    };
  },
  flatMap: (instance) => {
    return (transform) => {
      return instance(transform);
    };
  },
  // match: (instance, pattern) => {
  //   return flip(Variant.flatMap)(pattern)(instance);
  // }
};
// describe("Variantモナド", () => {
//   it("Maybe", (next) => {
//     var just = (value) => {
//       return variant.unit(value, 'just');
//     }; 
//     var nothing = (_) => {
//       return variant.unit(null, 'nothing');
//     }; 
//     var isEqual = (maybeA) => {
//       return (maybeB) => {
//         return variant.match(maybeA,{
//           just: (valueA) => {
//             return variant.match(maybeB,{
//               just: (valueB) => {
//                 return (valueA === valueB);
//               },
//               nothing: (_) => {
//                 return false;
//               }
//             });
//           },
//           nothing: (_) => {
//             return variant.match(maybeB,{
//               just: (_) => {
//                 return false;
//               },
//               nothing: (_) => {
//                 return true;
//               }
//             });
//           }
//         });
//       };
//     };
//     var add = (maybeA,maybeB) => {
//       return variant.flatMap(maybeA)((a) => {
//         return variant.flatMap(maybeB)((b) => {
//           return variant.unit(a + b);
//         });
//       });
//     };
//     var justOne = just(1);
//     var justTwo = just(2);
//     var justThree = just(3);
//     expect(
//       isEqual(add(justOne,justTwo))(justThree)
//     ).to.eql(
//       true
//     );
//     next();
//   });
// });

// ## Stateモナド
// Stateモナドとは、状態付き計算を包んだモナドである。
// 
// ~~~haskell
// instance Monad (State s) where                        
//   return x = State $ \s -> (x, s)
//   (State h) >>= f = State $ \s -> let (a, newState) = h s
//     (State g) = f a 
//       in g newState  
// ~~~
var State = {
  // ### State#unit
  //
  // unit :: a -> State s a
  // unit x s = (x,s)
  // ~~~scheme
  //   (unitM (x)   (lambda (s) (values x s)))
  //   (define (unit value)
  //      (lambda ()
  //         value)
  // ~~~
  unit: (value) => { 
    return (state) => { // run::STATE => PAIR[VALUE, STATE]
      return pair.cons(value,state);
    };
  },
  // ### State#evalState
  /* run:: State[A] => A */
  // ~~~haskell
  evalState: (instanceM) => {
    return (state) => {
      return pair.match(instanceM(state),{
        cons:(value, state) => {
          return value;
        }
      });
    };
  },
  // ### State#runState
  /* runState:: State[A] => A */
  // ~~~haskell
  runState: (instanceM) => {
    return (state) => {
      return instanceM(state); // Stateモナドのインスタンス(アクション)を現在の状態に適用する
    };
  },
  // ### State#flatMap
  // flatMap:: State[T,S] => FUNC[S => STATE[T,U]] => STATE[T,U]
  // ~~~haskell
  //   (State h) >>= f = State $ \s -> let (a, newState) = h s
  //                                       (State g) = f a
  //                                   in g newState
  // ~~~
  // ~~~haskell
  // newtype State s a = State { runState :: s -> (a, s) }
  //
  // instance Monad (State s) where
  //     return a = State $ \s -> (a, s)
  //     m >>= k  = State $ \s -> let
  //             (a, s') = runState m s
  //             in runState (k a) s'
  // ~~~
  flatMap: (h) => {
    expect(h).to.a('function');
    return (transform) => { // transform:: S => STATE[T,U]
      return (state) => {
        expect(transform).to.a('function');
        return pair.match(State.runState(h)(state),{
          cons:(value, newstate) => {
            return State.runState(transform(value))(newstate);
          }
        });
      };
    };
  },
  // ### monad.state#mkState
  // ~~~haskell
  // mkState :: (s -> (a,s)) -> State s a
  // mkState f = do {
  //             s <- get;
  //             let (a, s') = f s;
  //             put s';
  //             return a
  //           }
  // ~~~
  mkState: (f) => {  // f :: (s -> (a,s)) 
    return State.flatMap(f);
  },
  // get :: State s s
  // get s = (s,s)
  // ~~~
  // ~~~scala
  // def get[S]: State[S,S] = State(s => (s,s))
  // ~~~
  get: (state) => {
    return pair.cons(state,state);
  },
  //
  // put :: s -> State s ()
  // put x s = ((),x)
  put: (value) => {
    return () => {
      return  null;
    };
  }
};

// ### Stateモナドのテスト
describe("Stateモナドをテストする",() => {
  describe("スタックの例",() => {
    var Stack = {
      pop: (alist) => {
        return List.match(alist, {
          cons:(head, tail) => {
            return pair.cons(head, tail);
          }
        });
      },
      push: (n) => {
        return (alist) => {
          return List.match(alist, {
            cons:(head, tail) => {
              return pair.cons(null, List.cons(n,tail));
            }
          });
        };
      }
    };
    // it("スタッフの操作",(next) => {
    //   var stackManipulation = (stack) => {
    //     return State.flatMap(stack)(Stack.push(3));
    //   };
    //   // var stackManipulation = (stack) => {
    //   //   return State.flatMap(State.flatMap(stack)(Stack.push(3)))((_) => {
    //   //     return Stack.pop(3);
    //   //   });
    //   // };
    //   expect(
    //     State.evalState(stackManipulation(List.cons(1,List.cons(2,List.empty()))))
    //   ).to.eql(
    //     [0]
    //   );
    //   next();
    // });
  });
});

// ## STモナド

// ## Contモナド
// ~~~haskell
// newtype Cont r a = Cont { runCont :: ((a -> r) -> r) } -- r は計算全体の最終の型
//
// instance Monad (Cont r) where 
//     return a       = Cont $ \k -> k a                       -- i.e. return a = \k -> k a 
//     (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k) -- i.e. c >>= f = \k -> c (\a -> f a k) 
// class (Monad m) => MonadCont m where 
//     callCC :: ((a -> m b) -> m a) -> m a 
//
// instance MonadCont (Cont r) where 
//     callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
// ~~~
