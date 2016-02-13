"use strict";

var fs = require('fs');
var expect = require('expect.js');

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
    // return list.match(list_of_list, {
    //   empty: (_) => {
    //     return list.empty();
    //   },
    //   cons: (head, tail) => {
    //     return list.append(head)(list.concat(tail));
    //   }
    // });
  },
  // ~~~haskell
  // flatten :: [[a]] -> [a]
  // flatten =  foldr (++) []
  // ~~~
  flatten: (instanceMM) => {
    return list.concat(instanceMM);
    // return list.foldr(list.join)(list.empty());
    // return list.match(instanceMM,{
    //   empty: (_) => {
    //     return list.empty();
    //   },
    //   cons: (head,tail) => {
    //     return list.append(head)(list.flatten(tail));
    //   }
    // });
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
        // return list.match(item,{
        //   empty: () => {
        //     return [].concat(accumulator);
        //   },
        //   cons: (head, tail) => {
        //     return [list.toArray(item)].concat(accumulator);
        //   },
        //   otherwise: (arg) => {
        //     return [item].concat(accumulator);
        //   }
        // });
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
}; // end of list


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

// ## 'IO' monad module
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
