"use strict";

// モナド
// =====

var fs = require('fs');
var expect = require('expect.js');

// ## Maybeモナド
var maybe = {
  match: (data, pattern) => {
     return data.call(maybe,pattern);
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
    return maybe.just(value);
  },
  flatMap : (maybeInstance) => {
    return (transform) => {
      expect(transform).to.a('function');
      return maybe.match(maybeInstance,{
        just: (value) => {
          return transform(value);
        },
        nothing: (_) => {
          return maybe.nothing(_);
        }
      });
    };
  },
  isEqual : (maybeA) => {
    return (maybeB) => {
      return maybe.match(maybeA,{
        just: (valueA) => {
          return maybe.match(maybeB,{
            just: (valueB) => {
              return (valueA === valueB);
            },
            nothing: (_) => {
              return false;
            }
          });
        },
        nothing: (_) => {
          return maybe.match(maybeB,{
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
      return maybe.match(maybeInstance,{
        just: (value) => {
          return maybe.unit(transform(value));
        },
        nothing: (_) => {
          return maybe.nothing(_);
        }
      });
    };
  }
};

// ### Maybeモナドのテスト
describe("maybeモナドをテストする",() => {
  it("maybe#flatMap", (next) => {
    maybe.match(maybe.flatMap(maybe.just(1))((a) => {
      return maybe.unit(a);
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
    maybe.match(maybe.flatMap(maybe.nothing())((a) => {
      return maybe.unit(a);
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
      return maybe.flatMap(maybeA)((a) => {
        return maybe.flatMap(maybeB)((b) => {
          return maybe.unit(a + b);
        });
      });
    };
    var justOne = maybe.just(1);
    var justTwo = maybe.just(2);
    var justThree = maybe.just(3);
    expect(
      maybe.isEqual(add(justOne,justTwo))(justThree)
    ).to.eql(
      true
    );
    expect(
      maybe.isEqual(add(justOne,maybe.nothing()))(maybe.nothing())
    ).to.eql(
      true
    );
    next();
  });
});

// ## Listモナド
var list  = {
  match: (data, pattern) => {
    return data.call(list,pattern);
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
    return list.cons(value, list.empty());
  },
  head: (alist) => {
    return list.match(alist, {
      empty: (_) => {
        return undefined;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: (alist) => {
    return list.match(alist, {
      empty: (_) => {
        return undefined;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  isEmpty: (alist) => {
    return list.match(alist, {
      empty: (_) => {
        return true;
      },
      cons: (head, tail) => {
        return false;
      }
    });
  },
  // append:: LIST[T] -> LIST[T] -> LIST[T]
  // append [] ys = ys
  // append (x:xs) ys = x : (xs ++ ys)
  append: (xs) => {
    return (ys) => {
      return list.match(xs, {
        empty: (_) => {
          return ys;
        },
        cons: (head, tail) => {
          return list.cons(head,list.append(tail)(ys)); 
        }
      });
    };
  },
  // concat:: LIST[LIST[T]] -> LIST[T]
  // concat [] = []
  // concat (xs:xss) = xs ++ concat xss
  concat: (list_of_list) => {
    return list.foldr(list_of_list)(list.empty())(list.append);
  },
  // ~~~haskell
  // flatten :: [[a]] -> [a]
  // flatten =  foldr (++) []
  // ~~~
  flatten: (instanceMM) => {
    return list.concat(instanceMM);
  },
  // map:: LIST[T] -> FUN[T->U] -> LIST[U]
  // map [] _ = []
  // map (x:xs) f = f x : map xs f
  map: (instanceM) => {
    return (transform) => {
      return list.match(instanceM,{
        empty: (_) => {
          return list.empty();
        },
        cons: (head,tail) => {
          return list.cons(transform(head),
                           list.map(tail)(transform));
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
      return list.concat(list.map(instanceM)(transform));
    };
  },
  /* #@range_end(list_monad_definition) */
  // 1段階のリストしか配列に変更できない
  toArray: (alist) => {
    return list.foldr(alist)([])((item) => {
      return (accumulator) => {
        return [item].concat(accumulator);
      };
    });
  },
  // foldr:: LIST[T] -> T -> FUN[T -> U -> U] -> T
  // foldr []     z _ = z
  // foldr (x:xs) z f = f x (foldr xs z f) 
  foldr: (alist) => {         // alist:: LIST[T]
    return (accumulator) => { // accumulator:: T
      return (glue) => {      // glue:: FUN[T -> U -> U] 
        expect(glue).to.a('function');
        return list.match(alist, {
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            return glue(head)(list.foldr(tail)(accumulator)(glue));;
          }
        });
      };
    };
  }
}; // end of list monad

// ### Listモナドのテスト
describe('Listモナド', () => {
  var list  = {
    match: (data, pattern) => {
      return data.call(list,pattern);
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
      return list.match(alist, {
        empty: (_) => {
          return null;
        },
        cons: (head, tail) => {
          return head;
        }
      });
    },
    tail: (alist) => {
      return list.match(alist, {
        empty: (_) => {
          return null;
        },
        cons: (head, tail) => {
          return tail;
        }
      });
    },
    isEmpty: (alist) => {
      return list.match(alist, {
        empty: (_) => {
          return true;
        },
        cons: (head, tail) => {
          return false;
        }
      });
    },
    /* #@range_begin(list_monad_definition) */
    /* ### list#unit */
    unit: (value) => {
      return list.cons(value, list.empty());
    },
    /* append:: LIST[T] -> LIST[T] -> LIST[T] */
    /* append [] ys = ys */
    /* append (x:xs) ys = x : (xs ++ ys) */
    append: (xs) => {
      return (ys) => {
        return list.match(xs, {
          empty: (_) => {
            return ys;
          },
          cons: (head, tail) => {
            return list.cons(head,list.append(tail)(ys)); 
          }
        });
      };
    },
    /* concat:: LIST[LIST[T]] -> LIST[T] */
    /* concat [] = [] */
    /* concat (xs:xss) = xs ++ concat xss */
    concat: (list_of_list) => {
      return list.match(list_of_list, {
        empty: (_) => {
          return list.empty();
        },
        cons: (head, tail) => {
          return list.append(head)(list.concat(tail));
        }
      });
    },
    /* map:: LIST[T] -> FUN[T->U] -> LIST[U] */
    /* map [] _ = [] */
    /* map (x:xs) f = f x : map xs f */
    map: (instanceM) => {
      return (transform) => {
        return list.match(instanceM,{
          empty: (_) => {
            return list.empty();
          },
          cons: (head,tail) => {
            return list.cons(transform(head),
                             list.map(tail)(transform));
          }
        });
      };
    },
    // ~~~haskell
    // flatten :: [[a]] -> [a]
    // flatten =  foldr (++) []
    // ~~~
    flatten: (instanceMM) => {
      return list.match(instanceMM,{
        empty: (_) => {
          return list.empty();
        },
        cons: (head,tail) => {
          return list.append(head)(list.flatten(tail));
        }
      });
    },
    // ~~~haskell
    // xs >>= f = concat (map f xs)
    // ~~~
    flatMap: (instanceM) => {
      return (transform) => { // FUN[T->LIST[T]]
        expect(transform).to.a('function');
        return list.concat(list.map(instanceM)(transform));
      };
    },
    /* #@range_end(list_monad_definition) */
    toArray: (alist) => {
      return list.foldr(alist)([])((item) => {
        return (accumulator) => {
          return [item].concat(accumulator);
        };
      });
    },
    /* foldr:: LIST[T] -> T -> FUN[T -> U -> U] -> T */
    /* foldr []     z _ = z */
    /* foldr (x:xs) z f = f x (foldr xs z f)  */
    foldr: (alist) => {         // alist:: LIST[T]
      return (accumulator) => { // accumulator:: T
        return (glue) => {      // glue:: FUN[T -> U -> U] 
          expect(glue).to.a('function');
          return list.match(alist, {
            empty: (_) => {
              return accumulator;
            },
            cons: (head, tail) => {
              return glue(head)(list.foldr(tail)(accumulator)(glue));;
            }
          });
        };
      };
    },
    /* #@range_begin(list_monad_map) */
    /* map:: LIST[T] -> FUNC[T -> T] -> LIST[T] */
    /* #@range_end(list_monad_map) */
  }; // end of list
  it("'list#empty'", (next) => {
    list.match(list.empty,{
      empty: (_) => {
        expect(true).ok();
      },
      cons: (x,xs) => {
        expect().fail();
      }
    });
    next();
  });
  it("'list#isEmpty'", (next) => {
    expect(
      list.isEmpty(list.empty())
    ).to.eql(
      true
    );
    expect(
      list.isEmpty(list.cons(1,list.empty()))
    ).to.eql(
      false
    );
    next();
  });
  it("'list#cons'", (next) => {
    list.match(list.cons(1,list.empty()),{
      empty: (_) => {
        expect().fail()
      },
      cons: (x,xs) => {
        expect(x).to.eql(1)
      }
    });
    next();
  });
  it("'list#head'", (next) => {
    expect(
      list.head(list.cons(1,list.empty()))
    ).to.eql(
      1
    );
    next();
  });
  it("'list#tail'", (next) => {
    expect(
      list.head(list.tail(list.cons(1,list.cons(2,list.empty()))))
    ).to.eql(
      2
    );
    next();
  });
  it("'list#append'", (next) => {
    var theList = list.append(list.cons(1,list.empty()))(list.cons(2,list.empty()));
    expect(
      list.head(theList)
    ).to.eql(
      1
    );
    expect(
      list.head(list.tail(theList))
    ).to.eql(
      2
    );
    expect(
      list.isEmpty(list.tail(list.tail(theList)))
    ).to.eql(
      true
    );
    next();
  });
  it("'list#concat'", (next) => {
    /* list = [[1,2],[3,4]] */
    var one_two = list.cons(1,list.cons(2,list.empty()));
    var three_four = list.cons(3,list.cons(4,list.empty()));

    var list_of_list = list.cons(one_two,
                                 list.cons(three_four, list.empty()));
    /* concated_list = [1,2,3,4] */
    var concated_list = list.concat(list_of_list);
    expect(
      list.toArray(concated_list)
    ).to.eql(
      [1,2,3,4]
    );
    expect(
      list.head(concated_list)
    ).to.eql(
      1
    );
    expect(
      list.head(list.tail(concated_list))
    ).to.eql(
      2
    );
    expect(
      list.isEmpty(list.tail(list.tail(concated_list)))
    ).to.eql(
      false
    );
    next();
  });
  it("'list#foldr'", (next) => {
    /* list = [1,2,3,4] */
    var theList = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()),list.empty)))
    expect(
      list.foldr(theList)(0)(function (item){
        return (accumulator) => {
          return accumulator + item;
        };
      })
    ).to.eql(
      10
    )
    next();
  })
  it("'list#toArray'", (next) => {
    /* list = [1,2,3,4] */
    var theList = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()),list.empty)))
    expect(
      list.toArray(theList)
    ).to.eql(
      [1,2,3,4]
    );
    next();
  });
  it("'list#map'", (next) => {
    /* list = [1,2,3,4] */
    var theList = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()),list.empty)));
    expect(
      list.toArray(list.map(theList)(function (item){
        return item * 2;
      }))
    ).to.eql(
      [2,4,6,8]
    );
    next();
  });
  it("'list#unit'", (next) => {
    /* list = [1] */
    expect(
      list.toArray(list.unit(1))
    ).to.eql(
      [1]
    );
    expect(
      list.toArray(list.unit(null))
    ).to.eql(
      [null]
    );
    next();
  });
  it("'list#flatMap'", (next) => {
    /* list = [1,2,3] */
    var theList = list.cons(1,list.cons(2, list.cons(3, list.empty())));
    expect(
      list.toArray(list.flatMap(theList)((item) => {
        return list.append(list.unit(item))(list.unit(- item));
      }))
    ).to.eql(
      [1,-1,2,-2,3,-3]
    );
    next();
  });
  describe("listモナドを活用する",() => {
    it("フィルターとして使う", (next) => {
      var even = (n) => {
        if(n % 2 === 0) {
          return true;
        } else {
          return false;
        }
      };
      var theList = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
      expect(
        list.toArray(list.flatMap(theList)((item) => {
          if(even(item)) {
            return list.unit(item);
          } else {
            return list.empty();
          }
        }))
      ).to.eql(
        [2,4]
      );
      next();
    });
    it("2段階のflatMap", (next) => {
      var theNumberList = list.cons(1,list.cons(2,list.empty()));
      var theStringList = list.cons("one",list.cons("two",list.empty()));
      expect(
        list.toArray(list.flatMap(theNumberList)((n) => {
          return list.flatMap(theStringList)((s) => {
            return list.unit([n,s]);
          });
        }))
      ).to.eql(
        [[1,"one"],[1,"two"],[2,"one"],[2,"two"]]
      );
      next();
    });
    /* #@range_begin(list_maybe) */
    describe("maybeと一緒に使う", () => {
      // var maybe = {
      //   match: (data, pattern) => {
      //     return data.call(maybe,pattern);
      //   },
      //   just : (value) => {
      //     return (pattern) => {
      //       return pattern.just(value);
      //     };
      //   },
      //   nothing : (_) => {
      //     return (pattern) => {
      //       expect(pattern).not.to.be.a('null');
      //       // console.log(pattern);
      //       return pattern.nothing(null);
      //     };
      //   },
      //   unit : (value) => {
      //     return maybe.just(value);
      //    // if(value !== false && value != null){
      //    //    return maybe.just(value);
      //    //  } else {
      //    //    return maybe.nothing(null);
      //    //  }
      //   },
      //   flatMap : (maybeInstance) => {
      //     return (transform) => {
      //       expect(transform).to.a('function');
      //       return maybe.match(maybeInstance,{
      //         just: (value) => {
      //           return transform(value);
      //         },
      //         nothing: (_) => {
      //           return maybe.nothing(null);
      //         }
      //       });
      //     };
      //   },
      //   // map : (maybeInstance) => {
      //   //   return (transform) => {
      //   //     expect(transform).to.a('function');
      //   //     return maybe.match(maybeInstance,{
      //   //       just: (value) => {
      //   //         return maybe.unit(transform(value));
      //   //         // return maybe.unit(transform(value));
      //   //       },
      //   //       nothing: (_) => {
      //   //         return maybe.nothing(null);
      //   //       }
      //   //     });
      //   //   };
      //   // }
      // };
      it("[just(1)]", (next) => {
        var theList = list.cons(maybe.just(1),
                                list.empty());
        var justList = list.flatMap(theList)((maybeItem) => {
          return maybe.flatMap(maybeItem)((value) => {
            return list.unit(value);
          });
        });
        expect(
          list.toArray(justList)
        ).to.eql(
          [1]
        );
        next();
      });
      it("[just(1),just(2)]", (next) => {
        var theList = list.cons(maybe.just(1),
                                list.cons(maybe.just(2),list.empty()));
        var justList = list.flatMap(theList)((listItem) => {
          return maybe.flatMap(listItem)((value) => {
            return list.unit(value);
          });
        });
        expect(
          list.toArray(justList)
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
describe("listモナドを活用する",() => {
  it("'list#empty'", (next) => {
    list.match(list.empty,{
      empty: (_) => {
        expect(true).ok();
      },
      cons: (x,xs) => {
        expect().fail();
      }
    });
    next();
  });
  it("'list#isEmpty'", (next) => {
    expect(
      list.isEmpty(list.empty())
    ).to.eql(
      true
    );
    expect(
      list.isEmpty(list.cons(1,list.empty()))
    ).to.eql(
      false
    );
    next();
  });
  it("'list#cons'", (next) => {
    list.match(list.cons(1,list.empty()),{
      empty: (_) => {
        expect().fail();
      },
      cons: (x,xs) => {
        expect(x).to.eql(1);
      }
    });
    next();
  });
  it("'list#head'", (next) => {
    expect(
      list.head(list.cons(1,list.empty()))
    ).to.eql(
      1
    );
    next();
  });
  it("'list#tail'", (next) => {
    expect(
      list.head(list.tail(list.cons(1,list.cons(2,list.empty()))))
    ).to.eql(
      2
    );
    next();
  });
  it("'list#append'", (next) => {
    var theList = list.append(list.cons(1,list.empty()))(list.cons(2,list.empty()));
    expect(
      list.head(theList)
    ).to.eql(
      1
    );
    expect(
      list.head(list.tail(theList))
    ).to.eql(
      2
    );
    expect(
      list.isEmpty(list.tail(list.tail(theList)))
    ).to.eql(
      true
    );
    next();
  });
  it("'list#concat'", (next) => {
    // list = [[1,2],[3,4]]
    var one_two = list.cons(1,list.cons(2,list.empty()));
    var three_four = list.cons(3,list.cons(4,list.empty()));

    var list_of_list = list.cons(one_two,
                                 list.cons(three_four, list.empty()));
    // concated_list = [1,2,3,4]
    var concated_list = list.concat(list_of_list);
    expect(
      list.toArray(concated_list)
    ).to.eql(
      [1,2,3,4]
    );
    expect(
      list.head(concated_list)
    ).to.eql(
      1
    );
    expect(
      list.head(list.tail(concated_list))
    ).to.eql(
      2
    );
    expect(
      list.isEmpty(list.tail(list.tail(concated_list)))
    ).to.eql(
      false
    );
    next();
  });
  it("'list#foldr'", (next) => {
    // list = [1,2,3,4]
    var theList = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()),list.empty)))
    expect(
      list.foldr(theList)(0)(function (item){
        return (accumulator) => {
          return accumulator + item;
        };
      })
    ).to.eql(
      10
    )
    next();
  });
  describe("'list#toArray'", () => {
    it("1段階のリストを配列に変換する", (next) => {
      // list = [1,2,3,4]
      var theList = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()),list.empty)));
      expect(
        list.toArray(theList)
      ).to.eql(
        [1,2,3,4]
      );
      next();
    });
    it("2段階のリストを配列に変換する", (next) => {
      // list = [[1],[2]] 
      var nestedList = list.cons(list.cons(1,list.empty()),
                                 list.cons(list.cons(2,list.empty()),
                                           list.empty()));
      expect(
        list.toArray(list.flatMap(nestedList)((alist) => {
          return list.flatMap(alist)((item) => {
            return list.unit(item);
          });
        }))
      ).to.eql(
        [1,2]
      );
      next();
    });
  });
  it("'list#map'", (next) => {
    // list = [1,2,3,4]
    var theList = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()),list.empty)));
    expect(
      list.toArray(list.map(theList)(function (item){
        return item * 2;
      }))
    ).to.eql(
      [2,4,6,8]
    );
    next();
  });
  it("'list#unit'", (next) => {
    // list = [1]
    expect(
      list.toArray(list.unit(1))
    ).to.eql(
      [1]
    );
    expect(
      list.toArray(list.unit(null))
    ).to.eql(
      [null]
    );
    next();
  });
  describe("list#flatMap", () => {
    it("条件でフィルターする", (next) => {
      var list1 = list.cons(1, list.cons(2,
                                         list.empty()));
      var list2 = list.cons(1, list.cons(2,
                                         list.empty()));
      expect(
        list.toArray(list.flatMap(list1)((item1) => {
          return list.flatMap(list2)((item2) => {
            if(item1 + item2 === 3) {
              return list.unit([item1, item2]);
            } else {
              return list.empty();
            }
          });
        }))
      ).to.eql(
        [[1,2],[2,1]]
      );
      next();
    });
    it("'list#flatMap'", (next) => {
      // list = [1]
      var theList = list.cons(1, list.empty());
      expect(
        list.toArray(list.flatMap(theList)((item) => {
          return list.unit(item * 2); 
        }))
      ).to.eql(
        [2]
      );
      var emptyList = list.empty();
      expect(
        list.toArray(list.flatMap(emptyList)((item) => {
          return list.unit(item * 2); 
        }))
      ).to.eql(
        []
      );
      next();
    });
    it("'list#flatMap'", (next) => {
      // list = [1,2,3]
      var theList = list.cons(1,list.cons(2, list.cons(3, list.empty())));
      expect(
        list.toArray(list.flatMap(theList)((item) => {
          return list.append(list.unit(item))(list.unit(- item));
        }))
      ).to.eql(
        [1,-1,2,-2,3,-3]
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
      var theList = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
      expect(
        list.toArray(list.flatMap(theList)((item) => {
          if(even(item)) {
            return list.unit(item);
          } else {
            return list.empty();
          }
        }))
      ).to.eql(
        [2,4]
      );
      next();
    });
    it("2段階のflatMap", (next) => {
      var theNumberList = list.cons(1,list.cons(2,list.empty()));
      var theStringList = list.cons("one",list.cons("two",list.empty()));
      expect(
        list.toArray(list.flatMap(theNumberList)((n) => {
          return list.flatMap(theStringList)((s) => {
            return list.unit([n,s]);
          });
        }))
      ).to.eql(
        [[1,"one"],[1,"two"],[2,"one"],[2,"two"]]
      );
      next();
    });
  });
});
describe("Readerモナドをテストする",() => {
  var READER = {
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
    return list.foldr(alist)(list.empty())(IO.done());
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
    return list.match(alist, {
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
describe('Streamモナド', () => {
  var match = (data, pattern) => {
    return data(pattern);
  };
  var maybe = {
    just: (value) => {
      return (pattern) => {
        return pattern.just(value);
      };
    },
    nothing: (_) => {
      return (pattern) => {
        return pattern.nothing(_);
      };
    },
    unit: (value) => {
      if(value){
        return self.maybe.just(value);
      } else {
        return self.maybe.nothing(null);
      }
    },
    get: (maybe) => {
      return match(maybe,{
        just: (value) => {
          return value;
        },
        nothing: (_) => {
          return null;
        }
      });
    },
    isEqual: (maybeA) => {
      return (maybeB) => {
        return match(maybeA,{
          just: (valueA) => {
            return match(maybeB,{
              just: (valueB) => {
                return (valueA === valueB);
              },
              nothing: (_) => {
                return false;
              }
            });
          },
          nothing: (_) => {
            return match(maybeB,{
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
    map: (maybe) => {
      return (transform) => {
        expect(transform).to.a('function');
        return match(maybe,{
          just: (value) => {
            return unit(transform(value));
          },
          nothing: (_) => {
            return nothing();
          }
        });
      };
    },
    flatMap: (maybe) => {
      return (transform) => {
        expect(transform).to.a('function');
        return match(maybe,{
          just: (value) => {
            return transform(value);
          },
          nothing: (_) => {
            return nothing();
          }
        });
      };
    }
  }; // end of maybe
  /* #@range_begin(stream_monad_definition) */
  var stream = {
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
      return match(lazyList,{
        empty: (_) => {
          return maybe.nothing();
        },
        cons: (value, tailThunk) => {
          return maybe.just(value);
        }
      });
    },
    /* tail:: STREAM -> MAYBE[STREAM] */
    tail: (lazyList) => {
      return match(lazyList,{
        empty: (_) => {
          return maybe.nothing();
        },
        cons: (head, tailThunk) => {
          return maybe.just(tailThunk());
        }
      });
    },
    isEmpty: (lazyList) => {
      return match(lazyList,{
        empty: (_) => {
          return true;
        },
        cons: (head,tailThunk) => {
          return false;
        }
      });
    },
    // ### stream#toArray
    toArray: (lazyList) => {
      return match(lazyList,{
        empty: (_) => {
          return [];
        },
        cons: (head,tailThunk) => {
          if(stream.isEmpty(tailThunk())){
            return [head];
          } else {
            return [head].concat(stream.toArray(tailThunk()));
          }
        }
      });
    },
    // ### stream#unit
    /* unit:: ANY -> STREAM */
    unit: (value) => {
      if(value != null){
        return stream.cons(value, (_) => {
          return stream.empty();
        });
      } else {
        return stream.empty();
      }
    },
    // ### stream#map
    map: (lazyList) => {
      return (transform) => {
        return match(lazyList,{
          empty: (_) => {
            return stream.empty();
          },
          cons: (head,tailThunk) => {
            return stream.cons(transform(head),(_) => {
              return stream.map(tailThunk())(transform)});
          }
        });
      };
    },
    // ## stream#append
    append: (xs) => {
      return (ysThunk) => {
        return match(xs,{
          empty: (_) => {
            return ysThunk();
          },
          cons: (head,tailThunk) => {
            return stream.cons(head,(_) => {
              return stream.append(tailThunk())(ysThunk);
            });
          }
        });
      };
    },
    // ## stream#concat
    /* concat:: STREAM[STREAM[T]] -> STREAM[T] */
    concat: (astream) => {
      return match(astream,{
        empty: (_) => {
          return stream.empty();
        },
        cons: (head,tailThunk) => {
          return stream.append(head,tailThunk());
        }
      });
    },
    // ## stream#flatten
    /* flatten :: STREAM[STREAM[T]] => STREAM[T] */
    flatten: (lazyList) => {
      return match(lazyList,{
        empty: (_) => {
          return stream.empty();
        },
        cons: (head,tailThunk) => {
          return stream.append(head)((_) => {
            return stream.flatten(tailThunk());
          });
        }
      });
    },
    // ### stream#flatMap
    // ~~~haskell
    // flatMap xs f = flatten (map f xs)
    //~~~
    // flatMap:: STREAM[T] -> FUNC[T->STREAM[T]] -> STREAM[T]
    flatMap: (lazyList) => {
      return (transform) => {
        return stream.flatten(stream.map(lazyList)(transform));
      };
    }
  };
  /* #@range_end(stream_monad_definition) */
  it("stream#unit", (next) => {
    match(maybe.nothing(null),{
      nothing: (_) => {
        return expect(
          _
        ).to.eql(
          null
        );
      },
      just: (value) => {
        return expect().fail()
      }
    });
    var lazyList = stream.unit(1);
    expect(
      maybe.get(stream.head(lazyList))
    ).to.eql(
      1
    );
    expect(
      maybe.get(stream.head(stream.unit(1)))
    ).to.eql(
      1
    );
    expect(
      maybe.get(stream.head(stream.unit(0)))
    ).to.eql(
      0
    );
    next();
  });
  it("stream#cons", (next) => {
    var lazyList = stream.cons(1, (_) => {
      return stream.cons(2,(_) => {
        return stream.empty();
      });
    });
    expect(
      maybe.get(stream.head(lazyList))
    ).to.eql(
      1
    );
    next();
  });
  it("stream#tail", (next) => {
    /* lazyList = [1,2] */
    var lazyList = stream.cons(1, (_) => {
      return stream.cons(2,(_) => {
        return stream.empty();
      });
    });
    expect(
      stream.tail(lazyList)
    ).to.a("function");

    match(stream.tail(lazyList),{
      nothing: (_) => {
        expect().fail();
      },
      just: (tail) => {
        match(tail,{
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
      maybe.get(stream.head(maybe.get(stream.tail(lazyList))))
    ).to.eql(
      2
    );
    next();
  });
  it("stream#toArray", (next) => {
    expect(
      stream.toArray(stream.empty())
    ).to.eql(
      []
    );
    expect(
      stream.toArray(stream.unit(1))
    ).to.eql(
      [1]
    );
    next();
  });
  it("stream#append", (next) => {
    var xs = stream.cons(1, (_) => {
      return stream.empty();
    });
    var ysThunk = (_) => {
      return stream.cons(2, (_) => {
        return stream.empty();
      });
    };
    var theStream = stream.append(xs)(ysThunk);
    expect(
      maybe.get(stream.head(theStream))
    ).to.eql(
      1
    );
    expect(
      maybe.get(stream.head(maybe.get(stream.tail(theStream))))
    ).to.eql(
      2
    );
    next();
  });
  it("stream#flatten", (next) => {
    /* innerStream = [1,2] */
    var innerStream = stream.cons(1, (_) => {
      return stream.cons(2,(_) => {
        return stream.empty();
      });
    });
    /* outerStream = [[1,2]] */
    var outerStream = stream.unit(innerStream);
    var flattenedStream = stream.flatten(outerStream);
    match(flattenedStream,{
      empty: (_) => {
        expect().fail()
      },
      cons: (head,tailThunk) => {
        expect(head).to.eql(1)
      }
    });
    expect(
      maybe.get(stream.head(flattenedStream))
    ).to.eql(
      1
    );
    expect(
      maybe.get(stream.head(maybe.get(stream.tail(flattenedStream))))
    ).to.eql(
      2
    );
    next();
  });
  describe("stream#map", () => {
    it("mapで要素を2倍にする", (next) => {
      /* lazyList = [1,2] */
      var lazyList = stream.cons(1, (_) => {
        return stream.cons(2,(_) => {
          return stream.empty();
        });
      });
      var doubledLazyList = stream.map(lazyList)((item) => {
        return item * 2;
      });
      expect(
        maybe.get(stream.head(doubledLazyList))
      ).to.eql(
        2
      );
      expect(
        maybe.get(stream.head(maybe.get(stream.tail(doubledLazyList))))
      ).to.eql(
        4
      );
      expect(
        stream.toArray(doubledLazyList)
      ).to.eql(
        [2,4]
      );
      next();
    });
    it("無限の整数列を作る", (next) => {
      /* #@range_begin(ones_infinite_sequence) */
      var ones = stream.cons(1, (_) => {
        return ones;
      });
      expect(
        maybe.get(stream.head(ones))
      ).to.eql(
        1
      );
      expect(
        maybe.get(stream.head(maybe.get(stream.tail(ones))))
      ).to.eql(
        1
      );
      /* #@range_end(ones_infinite_sequence) */
      var twoes = stream.map(ones)((item) => {
        return item * 2;
      });
      expect(
        maybe.get(stream.head(twoes))
      ).to.eql(
        2
      );
      expect(
        maybe.get(stream.head(maybe.get(stream.tail(twoes))))
      ).to.eql(
        2
      );
      expect(
        maybe.get(stream.head(maybe.get(stream.tail(maybe.get(stream.tail(twoes))))))
      ).to.eql(
        2
      );
      next();
    });
    it("整数列を作る", (next) => {
      var integersFrom = (from) => {
        return stream.cons(from, (_) => {
          return integersFrom(from + 1);
        });
      };
      expect(
        maybe.get(stream.head(integersFrom(0)))
      ).to.eql(
        0
      );
      expect(
        maybe.get(stream.head(maybe.get(stream.tail(integersFrom(0)))))
      ).to.eql(
        1
      );
      var doubledIntergerMapped = stream.map(integersFrom(0))((integer) => {
        return integer * 2;
      });
      expect(
        maybe.get(stream.head(doubledIntergerMapped))
      ).to.eql(
        0
      );
      expect(
        maybe.get(stream.head(maybe.get(stream.tail(doubledIntergerMapped))))
      ).to.eql(
        2
      );
      var doubledInterger = stream.flatMap(integersFrom(0))((integer) => {
        return stream.unit(integer * 2);
      });
      expect(
        maybe.get(stream.head(doubledInterger))
      ).to.eql(
        0
      );
      expect(
        maybe.get(stream.head(maybe.get(stream.tail(doubledInterger))))
      ).to.eql(
        2
      );
      next();
    });
    it("一段階のflatMap", (next) => {
      var ones = stream.cons(1, (_) => {
        return ones;
      });
      var twoes = stream.flatMap(ones)((one) => {
        expect(one).to.a('number');
        return stream.unit(one * 2);
      });
      expect(
        maybe.get(stream.head(twoes))
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
      var innerStream12 = stream.cons(1, (_) => {
        return stream.cons(2,(_) => {
          return stream.empty();
        });
      });
      var innerStream34 = stream.cons(3, (_) => {
        return stream.cons(4,(_) => {
          return stream.empty();
        });
      });
      /* nestedStream = [[1,2],[3,4]] */
      var nestedStream = stream.cons(innerStream12, (_) => {
        return stream.cons(innerStream34,(_) => {
          return stream.empty();
        });
      });
      var flattenedStream = stream.flatMap(nestedStream)((innerStream) => {
        return stream.flatMap(innerStream)((n) => {
          expect(n).to.a('number');
          return stream.unit(n * 2);
        });
      });
      expect(
        maybe.get(stream.head(flattenedStream))
      ).to.eql(
        2
      );
      expect(
        stream.toArray(flattenedStream)
      ).to.eql(
        [2,4,6,8]
      );
      next();
    });

  });
}); // streamモナド

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

describe("maybeと一緒に使う", () => {
  it("[just(1),just(2)]", (next) => {
    var theList = list.cons(maybe.just(1),
                            list.cons(maybe.just(2),list.empty()));
    var justList = list.flatMap(theList)((listItem) => {
      return maybe.flatMap(listItem)((value) => {
        return list.unit(value);
      });
    });
    expect(
      list.toArray(justList)
    ).to.eql(
      [1,2]
    );
    next();
  });
  it("[just(1)]", (next) => {
    var theList = list.cons(maybe.just(1),
                            list.empty());
    var justList = list.flatMap(theList)((maybeItem) => {
      return maybe.flatMap(maybeItem)((value) => {
        return list.unit(value);
      });
    });
    expect(
      list.toArray(justList)
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

// describe("Variantモナド", () => {
//   var variant = {
//     unit: (value, key) => {
//       return (pattern) => {
//         return pattern[key](value);
//       };
//     },
//     flatMap: (instance) => {
//       return (transform) => {
//         return instance(transform);
//       };
//     },
//     match: (instance, pattern) => {
//       return flip(variant.flatMap)(pattern)(instance);
//     }
//   };
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
