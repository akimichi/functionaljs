"use strict";

// 高階関数を活用する
// =======

var expect = require('expect.js');

var truthy = (any) => {
  return any !== false && any != null;
};


var identity = (any) => {
  return any;
};


var double = (number) => {
  return number * 2;
};


var not = (predicate) => {
  return (arg) => {
    return ! predicate(arg);
  };
};

var compose = (f,g) => {
  return (arg) => {
    return f(g(arg));
  };
};
var pipe = (fun) => {
  expect(fun).to.a('function');
  return flip(compose)(fun);
};


var flip = (fun) => {
  return  (f) => {
    return (g) => {
      return fun(g)(f);
    };
  };
};

var match = (data, pattern) => {
  return data.call(pattern, pattern);
};

// 'string' module
// ==============
/* #@range_begin(string_module) */
// 文字列用モジュール
var string = {
  /* 先頭文字を取得する */
  head: (str) => {
    return str[0];
  },
  /* 後尾文字列を取得する */
  tail: (str) => {
    return str.substring(1);
  },
  /* 空の文字列かどうかを判定する */
  isEmpty: (str) => {
    return str.length === 0;
  },
  /* 文字列を文字のリストに変換する */
  toList: (str) => {
    if(string.isEmpty(str)) {
      return list.empty();
    } else {
      return list.cons(string.head(str), 
                       string.toList(string.tail(str)));
    }
  }
};
/* #@range_end(string_module) */
  // toArray: (str) => {
  //   expect(str).to.a('string');
  //   var glue = (item) => {
  //     return (rest) => {
  //       return [item].concat(rest);
  //     };
  //   };
  //   if(string.isEmpty(str)) {
  //     return [];
  //   } else {
  //     return [string.head(str)].concat(string.toArray(string.tail(str)));
  //   }
  // },

it('stringのテスト', (next) => {
  expect(
    string.head("abc")
  ).to.eql(
    'a'
  );
  expect(
    string.tail("abc")
  ).to.eql(
    'bc'
  );
  // expect(
  //   string.toArray("abc")
  // ).to.eql(
  //   ['a','b','c']
  // );
  next();
});

var list  = {
  match : (data, pattern) => {
    return data.call(list, pattern);
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
    // var self = this;
    return match(alist, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: (alist) => {
    // var self = this;
    return match(alist, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  isEmpty: (alist) => {
    // var self = this;
    return match(alist, {
      empty: (_) => {
        return true;
      },
      cons: (head, tail) => {
        return false;
      }
    });
  },
  /* #@range_begin(list_fromString) */
  fromString: (str) => {
    // expect(str).to.a('string');
    if(string.isEmpty(str)) {
      return list.empty();
    } else {
      return list.cons(string.head(str), list.fromString(string.tail(str)));
    }
  },
  /* #@range_end(list_fromString) */
  /* append:: LIST[T] -> LIST[T] -> LIST[T] */
  // ~~~haskell
  // append [] ys = ys
  // append (x:xs) ys = x : (xs ++ ys)
  // ~~~
  append: (xs) => {
    return (ys) => {
      return match(xs, {
        empty: (_) => {
          return ys;
        },
        cons: (head, tail) => {
          return list.cons(head, list.append(tail)(ys)); 
        }
      });
    };
  },
  /* list#concat */
  /* concat:: LIST[LIST[T]] -> LIST[T] */
  // ~~~haskell
  // concat [] = []
  // concat (xs:xss) = append(xs, xss)
  // or,
  // concat xss = foldr xss [] append
  // ~~~
  concat: (xss) => {
    return match(xss,{
      empty: (_) => {
        return list.empty();
      },
      cons: (xs,xss) => {
        return list.append(xs,xss);
      }
    });
  },
  last: (alist) => {
    var self = this;
    return match(alist, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return match(tail, {
          empty: (_) => {
            return head;
          },
          cons: (head, _) => {
            return list.last(tail);
          }
        });
      }
    });
  },
  /* join:: LIST[LIST[T]] -> LIST[T] */
  join: (list_of_list) => {
    return list.concat(list_of_list);
  },
  /* foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T */
  foldr: (alist) => {
    return (accumulator) => {
      return (glue) => {
        expect(glue).to.a('function');
        return match(alist,{
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            return glue(head)(list.foldr(tail)(accumulator)(glue));
          }
        });
      };
    };
  },
  /* map:: LIST[T] -> FUNC[T -> T] -> LIST[T] */
  map: (alist) => {
    var self = this;
    return (transform) => {
      return match(alist,{
        empty: (_) => {
          return list.empty();
        },
        cons: (head,tail) => {
          return list.cons(transform(head),list.map(tail)(transform));
        }
      });
    };
  },
  /* #@range_begin(list_reverse) */
  reverse: (alist) => {
    var self = this;
    var reverseAux = (alist, accumulator) => {
      return match(alist, {
        empty: (_) => {
          return accumulator;  // 空のリストの場合は終了
        },
        cons: (head, tail) => {
          return reverseAux(tail, list.cons(head, accumulator));
        }
      });
    };
    return reverseAux(alist, list.empty());
  },
  /* #@range_end(list_reverse) */
  /* ## list.filter */
  /* #@range_begin(list_filter) */
  filter: (alist) => {
    return (predicate) => {
      return match(alist,{
        empty: (_) => {
          return list.empty();
        },
        cons: (head,tail) => {
          if(predicate(head)){
            return list.cons(head,(_) => {
              return list.filter(tail)(predicate);
            });
          } else {
            return list.filter(tail)(predicate);
          }
        }
      });
    };
  },
  /* list#length */
  length: (alist) => {
    return match(alist,{
      empty: (_) => {
        return 0;
      },
      cons: (head,tail) => {
        return list.foldr(alist)(0)((item) => {
          return (accumulator) => {
            return 1 + accumulator;
          };
        });
      }
    });
  },
  any: (alist) => {
    return (predicate) => {
      expect(predicate).to.a('function');
      return match(alist,{
        empty: (_) => {
          return false;
        },
        cons: (head,tail) => {
          if(truthy(predicate(head))) {
            return true;
          } else {
            return list.any(tail)(predicate);
          }
        }
      });
    };
  },
  /* #@range_end(list_filter) */
  toArray: (alist) => {
    var toArrayAux = (alist,accumulator) => {
      return match(alist, {
        empty: (_) => {
          return accumulator;  // 空のリストの場合は終了
        },
        cons: (head, tail) => {
          return toArrayAux(tail, accumulator.concat(head));
        }
      });
    };
    return toArrayAux(alist, []);
  },
  fromArray: (array) => {
    expect(array).to.an('array');
    return array.reduce((accumulator, item) => {
      return list.append(accumulator)(list.cons(item, list.empty()));
    }, list.empty());
  },
  at: (alist) => {
    return (index) => {
      expect(index).to.a('number');
      expect(index).to.be.greaterThan(-1);
      if (index === 0) {
        return list.head(alist);
      } else {
          return list.at(list.tail(alist))(index - 1);
      }
    };
  },
  take: (alist) => {
    return (n) => {
      expect(n).to.a('number');
      expect(n).to.be.greaterThan(-1);
      if (n === 0) {
        return list.empty();
      } else {
        return list.cons(list.head)(list.take(list.tail)(n-1));
      }
    };
  },
  /* ## list#drop */
  /* drop:: List => List */
  drop: function(list){
    var self = this;
    self.list.censor(list);
    return function(n){
      expect(n).to.be.a('number');
      expect(n).to.be.greaterThan(-1);
      if (n === 0)
        return list;
      else {
        if(self.list.isEmpty.bind(self)(list))
          return self.list.empty;
        else {
          var tail = list.tail;
          return self.list.drop.bind(self)(tail)(n-1);
        }
      }
    };
  },
  /* #@range_begin(list_generate) */
  generate: (alist) => {
    var theList = alist;
    return (_) => {
      return match(theList,{
        empty: (_) => {
          return null; 
        },
        cons: (head,tail) => {
          theList = tail;
          return head;
        }
      });
    };
  }
  /* #@range_end(list_generate) */
};

it('listのテスト', (next) => {
  var sequence = list.cons(1,
                           list.cons(2,
                                     list.cons(3,
                                               list.cons(4,
                                                         list.empty()))));
  expect(
    list.length(sequence)
  ).to.eql(
    4
  );
  expect(
    list.head(sequence)
  ).to.eql(
    1
  );
  expect(
    list.toArray(sequence)
  ).to.eql(
    [1,2,3,4]
  );
  expect(
    list.toArray(list.reverse(sequence))
  ).to.eql(
    [4,3,2,1]
  );
  expect(
    list.head(list.reverse(sequence))
  ).to.eql(
    4
  );

  expect(
    list.last(sequence)
  ).to.eql(
    4
  );
  /* init = reverse . tail . reverse */
  var init = (alist) => {
    var self = this;
    return compose(list.reverse,
                   compose(list.tail,list.reverse))(alist);
  };
  var seq = list.cons(1, list.cons(2,list.cons(3,list.empty())));
  expect(
    list.toArray(init(seq))
  ).to.eql(
    [1,2]
  );
  expect(
    list.toArray(list.fromString("abc"))
  ).to.eql(
    ['a','b','c']
  );
  expect(
    list.at(seq)(0)
  ).to.eql(
    1
  );
  expect(
    list.at(seq)(1)
  ).to.eql(
    2
  );
  next();
});



var stream = {
  match: (data, pattern) => {
    return data.call(stream, pattern);
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
    return match(lazyList,{
      empty: (_) => {
        return null;
      },
      cons: (value, tailThunk) => {
        return value;
      }
    });
  },
  /* tail:: STREAM -> MAYBE[STREAM] */
  tail: (lazyList) => {
    return match(lazyList,{
      empty: (_) => {
        return null;
      },
      cons: (head, tailThunk) => {
        return tailThunk();
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
  /* ### stream#map */
  map: (lazyList) => {
    return (transform) => {
      return match(lazyList,{
        empty: (_) => {
          return stream.empty();
        },
        cons: (head,tailThunk) => {
          return stream.cons(transform(head),(_) => {
            return stream.map(tailThunk())(transform);
          });
        }
      });
    };
  },
  // ### stream#concat
  /* concat:: STREAM[STREAM[T]] -> STREAM[T] */
  concat: (astream) => {
    var self = this;
    return match(astream,{
      empty: (_) => {
        return stream.empty();
      },
      cons: (head,tailThunk) => {
        return stream.append(head,tailThunk());
      }
    });
  },
  // ### stream#flatten
  /* flatten:: STREAM[STREAM[T]] => STREAM[T] */
  flatten: (astream) => {
    return list.concat(astream);
  },
  toArray: (lazyList) => {
    return match(lazyList,{
      empty: (_) => {
        return [];
      },
      cons: (head,tailThunk) => {
        return match(tailThunk(),{
          empty: (_) => {
            return [head];
          },
          cons: (head_,tailThunk_) => {
            return [head].concat(stream.toArray(tailThunk()));
          }
        });
      }
    });
  },
  // ### stream#take
  /* take:: STREAM -> NUMBER -> STREAM */
  take: (lazyList) => {
    return (number) => {
      expect(number).to.a('number');
      expect(number).to.be.greaterThan(-1);
      return match(lazyList,{
        empty: (_) => {
          return stream.empty();
        },
        cons: (head,tailThunk) => {
          if(number === 0) {
            return stream.empty();
          } else {
            return stream.cons(head,(_) => {
              return stream.take(tailThunk())(number -1);
            });
          }
        }
      });
    };
  },
  // ### stream#filter
  /* #@range_begin(stream_filter) */
  /* filter:: FUN[T => BOOL] => STREAM[T] => STREAM[T] */
  filter: (predicate) => {
    return (aStream) => {
      return stream.match(aStream,{
        empty: (_) => {
          return stream.empty();
        },
        cons: (head,tailThunk) => {
          if(predicate(head)){ // 条件に合致する場合
            return stream.cons(head,(_) => {
              return stream.filter(predicate)(tailThunk());
            });
          } else { // 条件に合致しない場合
            return stream.filter(predicate)(tailThunk());
          }
        }
      });
    };
  },
  /* #@range_end(stream_filter) */
  // ### stream#remove
  /* #@range_begin(stream_remove) */
  /* remove:: FUN[T => BOOL] => STREAM[T] => STREAM[T] */
  remove: (predicate) => {
    return (aStream) => {
      return stream.filter(not(predicate))(aStream);
    };
  },
  /* #@range_end(stream_remove) */
  foldr: (astream) => {
    return (accumulator) => {
      return (glue) => {
        expect(glue).to.a('function');
        return match(astream,{
          empty: (_) => {
            return accumulator;
          },
          cons: (head,tailThunk) => {
            return glue(head)(stream.foldr(tailThunk())(accumulator)(glue));
          }
        });
      };
    };
  },
  enumFrom: (from) => {
    return stream.cons(from, (_) => {
      return stream.enumFrom(from + 1);
    });
  },
  /* #@range_begin(stream_generate) */
  generate: (astream) => {
    var theStream = astream;
    return (_) => {
      return match(theStream,{
        empty: (_) => {
          return null; 
        },
        cons: (head,tailThunk) => {
          theStream = tailThunk();
          return head;
        }
      });
    };
  },
  /* #@range_end(stream_generate) */
  forAll: (astream) => {
    return (predicate) => {
      var forAllHelper = (astream) => { // 補助関数
        return match(astream,{
          empty: (_) => {
            return true; 
          },
          cons: (head,tailThunk) => {
            return predicate(head) && forAllHelper(tailThunk());
          }
        });
      };
      return match(astream,{
        empty: (_) => {
          return false; // 空のストリームの場合は、必ず false が返る
        },
        cons: (head,tailThunk) => {
          return forAllHelper(astream);   
        }
      });
    };
  },
  /* #@range_begin(stream_forEach) */
  forEach: (astream) => {
    return (callback) => {
      return match(astream,{
        empty: (_) => {
          return null; 
        },
        cons: (head,tailThunk) => {
          callback(head);
          return stream.forEach(tailThunk())(callback);
        }
      });
    };
  }
  /* #@range_end(stream_forEach) */

}; // end of 'stream' module

describe('streamのテスト', () => {
  it('stream#map', (next) => {
    var lazyList = stream.cons(1, (_) => {
      return stream.cons(2,(_) => {
        return stream.empty();
      });
    });
    var doubled_stream = stream.map(lazyList)((item) => {
      return item * 2;
    });
    expect(
      stream.head(doubled_stream)
    ).to.eql(
      2
    );
    expect(
      stream.head(stream.tail(doubled_stream))
    ).to.eql(
      4
    );
    next();
  });
  it('stream#forAll', (next) => {
    var evens = stream.cons(2, (_) => {
      return stream.cons(4,(_) => {
        return stream.empty();
      });
    });
    
    expect(
      stream.forAll(evens)((n) => { return n % 2 === 0; })
    ).to.eql(
      true
    );
    var integers = stream.cons(1, (_) => {
      return stream.cons(2,(_) => {
        return stream.empty();
      });
    });

    expect(
      stream.forAll(integers)((n) => { return n % 2 === 0; })
    ).to.eql(
      false
    );
    next();
  });
});

var maybe = {
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

var pair = {
  cons: (left, right) => {
    return (pattern) => {
      return pattern.cons(left, right);
    };
  },
  match : (data, pattern) => {
    return data.call(pair, pattern);
  },
  right: (tuple) => {
    return match(tuple, {
      cons: (left, right) => {
        return right;
      }
    });
  },
  left: (tuple) => {
    return match(tuple, {
      cons: (left, right) => {
        return left;
      }
    });
  }
};

var object = {
  empty: (_) => {
    return null;
  },
  get: (key, obj) => {
    expect(obj).to.a('function');
    return obj(key);
  },
  set: (key, value, obj) => {
    return (key2) => {
      if(key === key2) {
        return value;
      } else {
        return object.get(key2,obj);
      }
    };
  }
};

// ## カリー化で関数を渡す
describe('カリー化で関数を渡す', () => {
  it('カリー化されていない指数関数', (next) => {
    /* #@range_begin(exponential_uncurried) */
    var exponential = (base,index) => {
      if(index === 0){
        return 1;
      } else {
        return base * exponential(base,index - 1);
      }
    };
    expect(
      exponential(2,2) // 2 * 2 = 4
    ).to.eql(
      4
    );
    expect(
      exponential(2,3) // 2 * 2 * 2 = 8
    ).to.eql(
      8
    );
    /* #@range_end(exponential_uncurried) */
    next();
  });
  it('カリー化された指数関数', (next) => {
    /* #@range_begin(exponential_curried) */
    var exponential = (base) => {
      return (index) => {
        if(index === 0){
          return 1;
        } else {
          return base * exponential(base)(index - 1);
        }
      };
    };
    /****** テスト ******/
    expect(
      exponential(2)(3) // 2の3乗を求める 
    ).to.eql(
      8
    );
    /* #@range_end(exponential_curried) */
    expect(
      exponential(2)(2)
    ).to.eql(
      4
    );
    /* #@range_begin(flip_definition) */
    var flip = (fun) => {
      return  (x) => {
        return (y) => {
          return fun(y)(x); // 適用する引数の順番を逆転させる
        };
      };
    };
    /* #@range_end(flip_definition) */
    /* #@range_begin(flipped_exponential) */
    /* flipで引数を逆転させて、2乗を定義する */
    var square = flip(exponential)(2); 
    /* flipで引数を逆転させて、3乗を定義する */
    var cube = flip(exponential)(3);   
    /* #@range_end(flipped_exponential) */
    /* #@range_begin(flipped_exponential_test) */
    expect(
      square(2)
    ).to.eql(
      4 /* 2 * 2 = 4 */
    );
    expect(
      cube(2)
    ).to.eql(
      8 /* 2 * 2 * 2 = 8 */
    );
    /* #@range_end(flipped_exponential_test) */
    next();
  });
  it('カリー化された関数の単純な例', (next) => {
    /* #@range_begin(simple_curried_function) */
    var add = function (x,y) {
      return x + y ;
    };
    var addCurried =  (x) => {
      return (y) => {
        return x + y ;
      };
    };
    expect(
      add(1,2)
    ).to.eql(
      addCurried(1)(2)
    );
    /* #@range_end(simple_curried_function) */
    next();
  });
  it('カリー化されていない multipleOf関数', (next) => {
    /* #@range_begin(multipleOf_uncurried) */
    var multipleOf = (n,m) => {
      if(m % n === 0) { /* m / n の余りが 0 かどうか */
        return true;
      } else {
        return false;
      }
    };
    /* #@range_end(multipleOf_uncurried) */
    /* #@range_begin(multipleOf_uncurried_test) */
    expect(
      multipleOf(2,4)     /* 4は、2の倍数である */
    ).to.eql(
      true
    );
    /* #@range_end(multipleOf_uncurried_test) */
    expect(
      multipleOf(3,4)     /* 4は、3の倍数ではない */
    ).to.eql(
      false
    );
    next();
  });
  it('カリー化による関数の部品化', (next) => {
    /* #@range_begin(multipleOf_curried) */
    var multipleOf = (n) => { // 外側の関数定義
      return (m) => {         // 内側の関数定義
        if(m % n === 0) {
          return true;
        } else {
          return false;
        }
      };
    };
    /* #@range_end(multipleOf_curried) */
    /* #@range_begin(multipleOf_curried_test) */
    expect(
      multipleOf(2)(4)   /* 関数適用を2回実行する */ 
    ).to.eql(
      true
    );
    /* #@range_end(multipleOf_curried_test) */
    expect(
      multipleOf(3)(4)    /* 4は、3の倍数ではない */
    ).to.eql(
      false
    );
    /* #@range_begin(multipleOf_curried_partilly_applied) */
    var twoFold = multipleOf(2);
    expect(
      twoFold(4)    /* 4は、2の倍数である */
    ).to.eql(
      true
    );
    /* #@range_end(multipleOf_curried_partilly_applied) */
    next();
  });
  describe('高階関数によるベクトル演算', () => {
    var mkVector =  (alist) => {
      return (index) => {
        return list.at(alist)(index);
      };
    };
    var zero = mkVector(list.cons(0, list.cons(0, list.empty())));
    var add = (vs) => {
      return (ws) => {
        return (index) => {
          return vs(index) + ws(index);
        };
      };
    };
    var innerProduct = (vs) => {
      return (ws) => {
        var product = (index) => {
          return vs(index) * ws(index);
        };
        var innerProductHelper = (indexes, accumulator) => {
          var index = stream.head(indexes);
          if(truthy(vs(index)) && truthy(ws(index))) {
            return innerProductHelper(stream.tail(indexes), accumulator + product(index));
          } else {
            return accumulator;
          }
        };
        var naturals = stream.enumFrom(0);
        return innerProductHelper(naturals, 0);
      };
    };
    it('ベクトルの内積を innerProduct で計算する', (next) => {
      var vs = mkVector(list.cons(1, list.cons(0, list.empty())));
      var ws = mkVector(list.cons(0, list.cons(1, list.empty())));
      expect(
        innerProduct(vs)(ws)
      ).to.eql(
        0
      );
      expect(
        innerProduct(mkVector(list.cons(-1, list.cons(-2, 
                                                      list.cons(1,
                                                                list.empty())))))(
          mkVector(list.cons(1, list.cons(-1, list.cons(2,list.empty()))))
        )
      ).to.eql(
        3
      );
      next();
    });
  });
  describe('通常の関数とカリー化関数の相互変換', () => {
    it('通常の関数をカリー化する', (next) => {
      /* #@range_begin(curry_function_definition) */
      /* カリー化する関数 */
      var curry = (fun) => {
        return (x,optionalY) => {
          if(arguments.length > 1){ // 引数が2個以上の場合
            return fun(x, optionalY);
          } else {                  // 引数が1個の場合
            return (y) =>  {
              return fun(x, y);
            };
          }
        };
      };
      var add = (x,y) => {
        return x + y;
      };
      expect(
        curry(add)(1)(2)  // add関数がカリー化されている
      ).to.eql(
        3
      );
      /* #@range_end(curry_function_definition) */
      next();
    });
    it('カリー化を通常の関数に変換する', (next) => {
      /* #@range_begin(uncurry_function_definition) */
      var uncurry = (fun) => {
        return function() {
          var result = fun;
          for (var i = 0; i < arguments.length; i++)
            result = result(arguments[i]);
          return result;
        };
      };
      var addCurried = (x) => {
        return (y) => {
          return x + y;
        };
      };
      var add = uncurry(addCurried);
      expect(
        add(1,2)
      ).to.eql(
        3
      );
      /* #@range_end(uncurry_function_definition) */
      next();
    });
  });
  // ### 関数を合成する
  describe('関数を合成する', () => {
    /* #@range_begin(compose_definition) */
    var compose = (f,g) => {
      return (arg) => {
        return f(g(arg));
      };
    };
    /* #@range_end(compose_definition) */
    var flip = (fun) => {
      return  (f) => {
        return (g) => {
          return fun(g)(f);
        };
      };
    };

    /* #@range_begin(compose_test) */
    var f = (x) => {
      return x * x + 1; 
    };
    var g = (x) => {
      return x - 2;
    };
    expect(
      compose(f,g)(2) // f . g で合成された関数
    ).to.eql(
      f(g(2))         // 合成せずに順次実行した場合
    );
    /* #@range_end(compose_test) */
    var subtract = (n) => {
      return (m) => {
        return n - m;
      };
    };
    expect(
      subtract(2)(1)
    ).to.eql(
      1
    );
    expect(
      subtract(1)(2)
    ).to.eql(
        -1
    );
    var and = (alist) => {
      return list.match(alist, {
        empty: (_) => {
          return false;
        },
        cons: (head, tail) => {
          return head && and(tail);
        }
      });
    };
    var or = (alist) => {
      return list.match(alist, {
        empty: (_) => {
          return false;
        },
        cons: (head, tail) => {
          return head || or(tail);
        }
      });
    };
    // it('pipe関数で適用の順番を逆転する', (next) => {
    //   /* #@range_begin(pipe_definition) */
    //   var pipe = (fun) => {
    //     return flip(compose)(fun);
    //   };
    //   /* #@range_end(pipe_definition) */
    //   /* #@range_begin(pipe_test) */
    //   var gf = pipe(g)(f);
      
    //   expect(
    //     gf(2) // 2^2  -4 * 2 + 5 
    //   ).to.eql(
    //     1
    //   );
    //   /* #@range_end(pipe_test) */
    //   next();
    // });
    it('カリー化の合成で加算と反数の合成は成功する', (next) => {
      /* #@range_begin(compose_opposite_add_successful) */
      var opposite = (x) => {
        return - x;
      };
      var addCurried = (x) => { // カリー化されたadd関数
        return (y) => {
          return x + y;
        };
      };
      expect(
        compose(opposite,
                addCurried(2))(3)
      ).to.eql(
          -5
      );
      /* #@range_end(compose_opposite_add_successful) */
      next();
    });
    describe('カリー化による関数の合成', () => {
      it('カリー化と関数合成', (next) => {
        var compose = (f) => {
          return (g) => {
            return (_) => {
              return f(g.apply(this, arguments));
            };
          };
        };
        var curry = (uncurriedFun) => {
          return (x,optionalY) => {
            if(arguments.length > 1){
              // return uncurriedFun(x, optionalY);
              return curry(uncurriedFun)(x)(optionalY);
            } else {
              return (y) =>  {
                return uncurriedFun(x, y);
              };
            }
          };
        };
        // var curry = (f) => {
        //   var _curry = (xs) => {
        //     if(xs.length < f.length){
        //       return (x) => { 
        //         return _curry(xs.concat([x])); 
        //       };
        //     } else {
        //       f.apply(undefined, xs);
        //     }
        //     // return xs.length < f.length ? function(x){ 
        //     //   return _curry(xs.concat([x])); } : f.apply(undefined, xs);
        //   };
        //   return _curry([]);
        // };
        /* #@range_begin(compose_and_curry) */
        var opposite = (x) => {
          return -x;
        };
        var add = (x,y) => {
          return x + y;
        };
        expect(
          compose(opposite)(curry(add)(2))(3)
        ).to.eql(
            -5
        );
        /* #@range_end(compose_and_curry) */
        // var funWithThreeArgs = (x,y,z) => {
        //   return x + y + z;
        // };
        // expect(
        //   (curry(funWithThreeArgs))(1)(2)(3)
        // ).to.eql(
        //     -5
        // );
        next();
      });
      it('脱カリー化と関数合成', (next) => {
        /* #@range_begin(compose_and_uncurry) */
        var compose = (f) => {
          return (g) => {
            return (_) => {
              return f(g.apply(this, arguments));
            };
          };
        };
        var uncurry = (fun) => {
          return () => {
            var result = fun;
            for (var i = 0; i < arguments.length; i++)
              result = result(arguments[i]);
            return result;
          };
        };
        var opposite = (x) => {
          return -x;
        };
        var multiply = (x) => {
          return (y) => {
            return x * y;
          };
        };
        expect(
          compose(opposite)(uncurry(multiply))(2,3)
        ).to.eql(
            -6
        );
        /* #@range_end(compose_and_uncurry) */
        next();
      });
      it('マイナスのマイナスはプラス', (next) => {
        /* #@range_begin(composition_example_opposite_twice) */
        /* 反数の定義 */
        var opposite = (n) => { 
          return - n;
        };
        expect(
          /* 反数同士の合成は成功する */
          compose(opposite,opposite)(2) 
        ).to.eql(
          2 // -(-2) === 2
        );
        /* #@range_end(composition_example_opposite_twice) */
        next();
      });
    });
    it("1個の引数の関数を合成する", (next) => {
      var increment = function(n){
        return n + 1;
      };
      var decrement = function(n){
        return n - 1;
      };
      var double = function(n){
        return 2 * n;
      };
      expect(
        compose(increment,decrement)(5)
      ).to.eql(
        5
      );
      next();
    });
    it("composeでlastを定義する", (next) => {
      var compose = (f) => {
        var self = this;
        return (g) => {
          return (arg) => {
            return f.call(self,
                          g.call(self,arg));
          };
        };
      };
      var last = (alist) => {
        return compose(list.head)(list.reverse)(alist);
      };
      var sequence = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
      expect(
        last(sequence)
      ).to.eql(
        4
      );
      next();
    });
    it("pipeでlastを定義する", (next) => {
      var compose = (f) => {
        var self = this;
        return (g) => {
          return (arg) => {
            return f.call(self,
                          g.call(self,arg));
          };
        };
      };
      /* #@range_begin(last_with_pipe) */
      var flip = (fun) => {
        var self = this;
        return  (f) => {
          return (g) => {
            return fun.call(self, g)(f); // return fun(g)(f);
          };
        };
      };
      var pipe = (fun) => {
        var self = this;
        expect(fun).to.a('function');
        return flip.call(self,
                         compose)(fun);
      };
      var last = (alist) => {
        return pipe(list.reverse)(list.head)(alist);
      };
      var sequence = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
      expect(
        last(sequence)
      ).to.eql(
        4
      );
      /* #@range_end(last_with_pipe) */
      next();
    });
    it('再帰によるlast', (next) => {
      /* #@range_begin(list_last_recursive) */
      var last = (alist) => {
        return match(alist, {
          empty: (_) => { // alistが空の場合
            return null;
          },
          cons: (head, tail) => { // alistが空でない場合
            return match(tail, {
              empty: (_) => { // alistの要素がただ1個の場合
                return head;
              },
              cons: (_, __) => {
                return last(tail);
              }
            });
          }
        });
      };
      /* #@range_end(list_last_recursive) */
      /* #@range_begin(list_last_test) */
      var numbers = list.cons(1,
                              list.cons(2,
                                        list.cons(3,
                                                  list.empty())));
      expect(
        last(numbers)
      ).to.eql(
        3
      );
      /* #@range_end(list_last_test) */
      next();
    });
    it('合成によるlast', (next) => {
      /* #@range_begin(list_last_compose) */
      var last = (alist) => {
        return compose(list.head,
                       list.reverse)(alist);
      };
      /* #@range_end(list_last_compose) */
      var sequence = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
      expect(
        last(sequence)
      ).to.eql(
        4
      );
      next();
    });
    it('length関数の抽象的な定義', (next) => {
      var alwaysOne = (x) => {
        return 1;
      };
      var sum = (alist) => {
        var sumHelper = (alist, accumulator) => {
          return match(alist,{
            empty: (_) => {
              return accumulator;
            },
            cons: (head, tail) => {
              return sumHelper(tail, accumulator + head);
            }
          });
        };
        return sumHelper(alist,0);
      };
      /* #@range_begin(abstract_length) */
      var length = (alist) => {
        return compose(sum,
                       flip(list.map)(alwaysOne))(alist);
      };
      /****** テスト *******/
      var alist = list.cons(1,
                            list.cons(2,
                                      list.cons(3,
                                                list.empty())));
      expect(
        length(alist)
      ).to.eql(
        3
      );
      /* #@range_end(abstract_length) */
      next();
    });
    it('init関数の抽象的な定義', (next) => {
      /* #@range_begin(abstract_init) */
      /* init = reverse . tail . reverse  */
      var init = (alist) => {
        return compose(list.reverse,
                       compose(list.tail,list.reverse)
        )(alist);
      };
      /****** テスト *******/
      var sequence = list.cons(1,
                               list.cons(2,
                                         list.cons(3,
                                                   list.empty())));
      expect(
        list.toArray(init(sequence))
      ).to.eql(
        [1,2]
      );
      /* #@range_end(abstract_init) */
      next();
    });
    it('関数合成による様々な関数の定義', (next) => {
      // var multiply = (n) => {
      //   return (m) => {
      //     return n * m;
      //   };
      // };
      // var square = (n) => {
      //   return multiply(n)(n);
      // };
      // expect(
      //   square(3)
      // ).to.eql(
      //   9
      // );
      var alist = list.cons(1,
                            list.cons(2,
                                      list.cons(3,
                                                list.empty())));
      var alwaysOne = (x) => {
        return 1;
      };
      var sum = (alist) => {
        var sumHelper = (alist, accumulator) => {
          return match(alist,{
            empty: (_) => {
              return accumulator;
            },
            cons: (head, tail) => {
              return sumHelper(tail, accumulator + head);
            }
          });
        };
        return sumHelper(alist,0);
      };
      var length = (alist) => {
        return compose(sum,
                       flip(list.map)(alwaysOne))(alist);
      };
      /****** テスト *******/
      expect(
        length(alist)
      ).to.eql(
        3
      );
      var last = (alist) => {
        return compose(list.head,
                       list.reverse)(alist);
      };
      expect(
        last(alist)
      ).to.eql(
        3
      );
      /* init = reverse . tail . reverse  */
      var init = (alist) => {
        return compose(list.reverse,
                       compose(list.tail,
                               list.reverse)
        )(alist);
      };
      /****** テスト *******/
      expect(
        list.toArray(init(alist))
      ).to.eql(
        [1,2]
      );
      var not = (predicate) => { // predicate::FUN[NUM => BOOL]
        return (arg) => { // FUN[NUM => BOOL]型を返す
          return ! predicate(arg); // !演算子で論理を反転させる
        };
      };
      var multipleOf = (n) => {
        return (m) => {
          if(m % n === 0) {
            return true;
          } else {
            return false;
          }
        };
      };
      var even = multipleOf(2);
      var odd = not(even);
      expect(
        odd(2)
      ).to.eql(
        false
      );
      expect(
        odd(3)
      ).to.eql(
        true
      );
      var map = (alist) => {
        return (transform) => {
          return match(alist,{
            empty: (_) => {
              return list.empty();
            },
            cons: (head,tail) => {
              return list.cons(transform(head),
                               list.map(tail)(transform));
            }
          });
        };
      };
      var allTrueList = list.cons(true,
                            list.cons(true,
                                      list.cons(true,
                                                list.empty())));
      var and = (alist) => {
        return list.match(alist, {
          empty: (_) => {
            return true;
          },
          cons: (head, tail) => {
            return head && and(tail);
          }
        });
      };
      expect(
        and(allTrueList)
      ).to.eql(
        true
      );
      var or = (alist) => {
        return list.match(alist, {
          empty: (_) => {
            return false;
          },
          cons: (head, tail) => {
            return head || or(tail);
          }
        });
      };
      var someTrueList = list.cons(true,
                            list.cons(false,
                                      list.cons(true,
                                                list.empty())));
      var allFalseList = list.cons(false,
                            list.cons(false,
                                      list.cons(false,
                                                list.empty())));
      expect(
        or(someTrueList)
      ).to.eql(
        true
      );
      expect(
        or(allFalseList)
      ).to.eql(
        false
      );
      var all = (predicate) => {
        return (alist) => {
          return compose(and,
                         flip(map)(predicate))(alist);
        };
      };
      expect(
        all((x) => {
          return x > 0;
        })(alist)
      ).to.eql(
        true
      );
      var any = (predicate) => {
        return (alist) => {
          return compose(or,
                         flip(map)(predicate))(alist);
        };
      };
      expect(
        any((x) => {
          return x < 2;
        })(alist)
      ).to.eql(
        true
      );
      expect(
        any((x) => {
          return x < 1;
        })(alist)
      ).to.eql(
        false
      );
      var none = (predicate) => {
        return (alist) => {
          return compose(and,
                         flip(map)(not(predicate)))(alist);
        };
      };
      expect(
        none((x) => {
          return x < 0;
        })(alist)
      ).to.eql(
        true
      );
      next();
    });
  }); // 関数合成のカリー化
  it('リストの逆順を求める', (next) => {
    var list = {
      match: (data, pattern) => {
        return data.call(list, pattern);
      },
      empty: (pattern) => {
        return pattern.empty();
      },
      cons: (value, list) => {
        return (pattern) => {
          return pattern.cons(value, list);
        };
      },
      isEmpty: (list) => {
        return match(list, { // match関数で分岐する
          empty: true,
          cons: (head, tail) => { // headとtailにそれぞれ先頭要素、末尾要素が入る
            return false;
          }
        });
      },
      head: (list) => {
        return match(list, {
          empty: null, // 空のリストには先頭要素はありません
          cons: (head, tail) => {
            return head;
          }
        });
      },
      tail: (list) => {
        return match(list, {
          empty: null,  // 空のリストには末尾要素はありません
          cons: (head, tail) => {
            return tail;
          }
        });
      }
    };
    /* #@range_begin(list_reverse) */
    var reverse = (alist) => {
      return (accumulator) => {
        return list.match(alist, {
          empty: (_) => {
            return accumulator;  // 空のリストの場合は終了
          },
          cons: (head, tail) => {
            return reverse(tail)(list.cons(head, accumulator));
          }
        });
      };
    };
    /* toArray:: LIST -> ARRAY -> ARRAY */
    var toArray = (alist) => {
      var toArrayAux = (alist) => {
        return (accumulator) => {
          return list.match(alist, {
            empty: (_) => {
              return accumulator;  // 空のリストの場合は終了
            },
            cons: (head, tail) => {
              return toArrayAux(tail)(accumulator.concat(head));
            }
          });
        };
      };
      return toArrayAux(alist)([]);
    };
    /**************** テスト ****************/
    expect(
      toArray(reverse(list.cons(1, list.cons(2,list.empty)))(list.empty))
    ).to.eql(
      [2,1]
    );
    /* #@range_end(list_reverse) */
    next();
  });
});
// ## コンビネータで関数を組み合わせる
describe('コンビネータで関数を組み合わせる', () => {
  it('multipleOfコンビネータ', (next) => {
    var multipleOf = (n) => {
      return (m) => {
        if(m % n === 0) {
          return true;
        } else {
          return false;
        }
      };
    };
    /* #@range_begin(multipleOf_combinator) */
    var even = multipleOf(2); /* カリー化されたmultipleOf関数を使う */
    
    expect(
      even(2)
    ).to.eql(
      true
    );
    /* #@range_end(multipleOf_combinator) */
    next();
  }); 
  it('論理コンビネータ', (next) => {
    var multipleOf = (n) => {
      return (m) => {
        if(m % n === 0) {
          return true;
        } else {
          return false;
        }
      };
    };
    var even = multipleOf(2);
    /* #@range_begin(not_combinator) */
    /* not:: FUN[NUM => BOOL] => FUN[NUM => BOOL] */
    var not = (predicate) => { // predicateの型はFUN[NUM => BOOL]
      /* 全体として、FUN[NUM => BOOL]型を返す */
      return (arg) => { // argの型はNUM
        return ! predicate(arg); // !演算子で論理を反転させて、BOOLを返す
      };
    };
    /* #@range_end(not_combinator) */
    /* #@range_begin(not_combinator_test) */
    var odd = not(even); // notコンビネータでodd関数を定義する
    /******** テスト ********/
    expect(
      odd(3) // 3は奇数である
    ).to.eql(
      true
    );
    expect(
      odd(2) // 2は奇数でない 
    ).to.eql(
      false
    );
    /* #@range_end(not_combinator_test) */
    /* #@range_begin(and_or_combinator) */
    /* 「もしくは」を表す論理和  */
    /* or:: (NUMBER->BOOL, NUMBER->BOOL) -> (NUMBER->BOOL) */
    var or = (f,g) => {
      return (arg) => {
        return f(arg) || g(arg);
      };
    };
    /* 「かつ」を表す論理積  */
    /* and:: (NUMBER->BOOL, NUMBER->BOOL) -> (NUMBER->BOOL) */
    var and = (f,g) => {
      return (arg) => {
        return f(arg) && g(arg);
      };
    };
    /* #@range_end(and_or_combinator) */
    var positive = (n) => {
      return n > 0;
    };
    var zero = (n) => {
      return n === 0;
    };
    var negative = or(positive, not(zero));
    expect(
      negative(-3)
    ).to.eql(
      true
    );
    expect(
      negative(0)
    ).to.eql(
      false
    );
    next();
  });
  it('パイプライン', (next) => {
    var pipe = (f, g) => {
      return (input) => {
        return compose(f,g)(input);
      };
    };
    var pipelines = (combinators) => {
      return (input) => {
        return combinators.reduce((accumulator, combinator) => {
          return compose(combinator,accumulator);
        });
      };
    };
    next();
  });
  describe('コンビネーター・ライブラリー', () => {
    describe('数値型検証コンビネータ', () => {
      /* #@range_begin(number_combinator) */
      /* is:: FUNC[ANY -> BOOL] -> ANY -> BOOL A*/
      var is = (predicate) => {
        expect(predicate).to.a('function');
        return (target) => {
          return truthy(predicate(target));
        };
      };
      var not = (predicate) => {
        expect(predicate).to.a('function');
        return (target) => {
          return ! is(predicate)(target);
        };
      };
      /* eq:: ANY -> ANY -> BOOL */
      var eq = (x) => {
        return (y) => {
          return x === y;
        };
      };
      var remainder = (n) => {
        return (m) => {
          return n % m;
        };
      };
      it('remainder', (next) => {
        expect(
          remainder(10)(3)
        ).to.eql(
          1
        );
        next();
      });
      /* multipleOf:: NUM -> NUM -> BOOL */
      var multipleOf = (n) => {
        expect(n).to.a('number');
        return (m) => {
          expect(n).to.a('number');
          return eq(remainder(m)(n))(0);
        };
      };
      /* even:: NUM -> BOOL */
      var even = multipleOf(2);
      /* odd:: NUM -> BOOL */
      var odd = not(even);
      var or = (f,g) => {
        return (arg) => {
          return f(arg) || g(arg);
        };
      };
      var and = (f,g) => {
        return (arg) => {
          return f(arg) && g(arg);
        };
      };
      var zero = (n) => {
        return n === 0;
      };
      var isZero = is(zero);
      /* positive:: NUM -> BOOL */
      var positive = (n) => {
        return n > 0;
      };
      var isNegative = or(is(positive),not(isZero));
      it('isNegative', (next) => {
        expect(
          isNegative(0)
        ).to.eql(
          false
        );
        next();
      });
      var greater = (n) => {
        return (m) => {
          return n < m;
        };
      };
      /* #@range_end(number_combinator) */
      /* #@range_begin(number_combinator_greater) */
      var greater = (n) => {
        return (m) => {
          return n < m;
        };
      };
      it('greater', (next) => {
        expect(
          is(greater(0))(1)
        ).to.eql(
          true
        );
        expect(
          is(greater(0))(0)
        ).to.eql(
          false
        );
        expect(
          is(greater(0))(-1)
        ).to.eql(
          false
        );
        next();
      });
      /* #@range_end(number_combinator_greater) */
      /* #@range_begin(number_combinator_smaller) */
      var smaller = flip(greater);
      it('smaller', (next) => {
        expect(
          is(smaller(0))(1)
        ).to.eql(
          false
        );
        expect(
          is(smaller(0))(0)
        ).to.eql(
          false
        );
        expect(
          is(smaller(0))(-1)
        ).to.eql(
          true
        );
        next();
      });
      /* #@range_end(number_combinator_smaller) */
      
      var gcd = (x) => {
        return (y) => {
          if(is(zero)(y)){
            return x;
          } else {
            if(is(zero)(x)){
              return new Error("gcd(0)(0) is not defined");
            } else {
              return gcd(y)(remainder(x)(y));
            }            
          }
        };
      };
      it('gcd', (next) => {
        expect(
          gcd(36)(12)
        ).to.eql(
          12
        );
        next();
      });
      var leastDivisor = (n) => {
        expect(n).to.a('number');
        var leastDivisorHelper = (k, n) => {
          expect(k).to.a('number');
          expect(n).to.a('number');
          if(multipleOf(k)(n)) {
            return k;
          } else {
            if(is(greater(n))(k * k)) {
              return n;
            } else {
              return leastDivisorHelper(k+1, n);
            }
          };
        };
        return leastDivisorHelper(2,n);
      };
      /*
        c.f. Haskell Road, p.19
        ~~~haskell
        factors:: Integer -> [Integer]
        factors n | n < 1 = error "argument not positive"
        | n == 1 = []
        | otherwise = p : factors (div n p) where p = ld n
        ~~~
      */
      var factors = (n) => {
        if(n < 1) {
          return new Error("argument not positive");
        }
        if(n === 1) {
          return list.empty();
        } else {
          var leastDivisorOfN = leastDivisor(n);
          return list.cons(leastDivisorOfN, factors(n / leastDivisorOfN));
        }
      };
      it('factorsで素因数分解を求める', (next) => {
        expect(
          list.toArray(factors(84))
        ).to.eql(
          [2,2,3,7]
        );
        next();
      });
    });
    describe('文字列検証コンビネータ', () => {
      /* is:: FUNC[ANY -> BOOL] -> ANY -> BOOL */
      var is = (predicate) => {
        expect(predicate).to.a('function');
        return (target) => {
          return truthy(predicate(target));
        };
      };
      var not = (predicate) => {
        expect(predicate).to.a('function');
        return (target) => {
          return ! is(predicate)(target);
        };
      };
      /* eq:: ANY -> ANY -> BOOL */
      var eq = (x) => {
        return (y) => {
          return x === y;
        };
      };
      var greater = (n) => {
        return (m) => {
          return n < m;
        };
      };
      var smaller = flip(greater);
      var or = (f,g) => {
        return (arg) => {
          return f(arg) || g(arg);
        };
      };
      var and = (f,g) => {
        return (arg) => {
          return f(arg) && g(arg);
        };
      };
      it('文字列の長さをチェックする', (next) => {
        /* #@range_begin(list_length_check) */
        /* 文字列をリスト型に変換する */
        var stringAsList = list.fromString("abcd");
        /* 文字列の長さが6より長いかどうかを判定する */
        expect(
          is(greater(6))(list.length(stringAsList))
        ).to.be(
          false
        );
        /* 文字列の長さが3より長く6より短いかどうかを判定する */
        expect(
          and(greater(3),
              smaller(6))(list.length(stringAsList))
        ).to.be(
          true
        );
        /* #@range_end(list_length_check) */
        expect(
          is(greater(3))(list.length(stringAsList))
        ).to.be(
          true
        );
        next();
      });
      it('ある文字があるかどうかを list.any でチェックする', (next) => {
        var stringAsList = list.fromString("abXd");
        expect(
          list.any(stringAsList)((ch) => {
            return ch === 'X';
          })
        ).to.be(
          true
        );
        next();
      });
    });
    describe('オブジェクト型検証コンビネータ', () => {
      var hasOwnProperty = Object.prototype.hasOwnProperty;
      var isEmpty = (obj) => {
        /* null and null are "empty" */
        if (obj == null) return true;
        /* Assume if it has a length property with a non-zero value
           that that property is correct. */
        if (obj.length > 0)    return false;
        if (obj.length === 0)  return true;
        /* Otherwise, does it have any properties of its own?
         Note that this doesn't handle
         toString and valueOf enumeration bugs in IE < 9 */
        for (var key in obj) {
          if (hasOwnProperty.call(obj, key)) return false;
        }
        return true;
      };

      /* パース結果の代数的データ型 */
      var result = {
        failed: (message) => {
          return (pattern) => {
            return pattern.failed(message);
          };
        },
        successful: (value, inputObject) => {
          return (pattern) => {
            return pattern.successful(value, inputObject);
          };
        }
      };
      /* validate: PARSER -> JSON -> PARSERESULT */
      var validate = (validator, continues, continuesInFailure) => {
        return (inputObject) => {
          return validator(inputObject, continues, continuesInFailure);
        };
      };
      /* 基本パーサー */
      /* succeed:: ANY => LIST => RESULT */
      var succeed = (value, continues, continuesInFailure) => {
        return (inputObject) => {
          return continues(result.successful(value, inputObject));
        };
      };
      /* fail:: (ANY) => LIST => PARSERESULT */
      var fail = (message, continues, continuesInFailure) => {
        return (inputObject) => {
          return continuesInFailure(result.failed(message));
        };
      };
      var continues = {
        normally: (result) => {
          return result;
        },
        abnormally: (exception) => {
          return exception;
        }
      };
      it('succeed', (next) => {
        var object = {
          key: 1
        };
        match(validate(succeed(true, continues.normally, continues.abnormally)(object)),{
          failed: () => {
            expect().fail();
          },
          successful: (value, theObject) => {
            expect(
              value
            ).to.eql(
              1
            );
            expect(
              theObject
            ).to.eql(
              object
            );
          }
        });
        next();
      });
      it('fail', (next) => {
        var object = {
          key: 1
        };
        match(validate(fail("failed", continues.normally, continues.abnormally)(object)),{
          failed: (message) => {
            expect(
              message
            ).to.eql(
              "failed"
            );
          },
          successful: (value, theObject) => {
            expect().fail();
          }
        });
        next();
      });
      var is = (predicate, continues, continuesInFailure) => {
        return (target) => {
          if(predicate(target)){
            return continues(succeed(true, continues, continuesInFailure)(target));
          } else {
            return continuesInFailure(fail("is not", continues, continuesInFailure)(target));
          }
        };
      };
      var typeOf = (target) => {
        if(target === undefined || target === null)
          return String(target);
        var classToType = {
          '[object Boolean]': 'boolean',
          '[object Number]': 'number',
          '[object String]': 'string',
          '[object Function]': 'function',
          '[object Array]': 'array',
          '[object Date]': 'date',
          '[object RegExp]': 'regexp',
          '[object Object]': 'object'
        };
        return classToType[Object.prototype.toString.call(target)];
      };
      var bool = (value) => {
        return typeOf(value) === 'boolean';
      };
      var string = (value) => {
        return typeOf(value) === 'string';
      };
      var number = (value) => {
        return typeOf(value) === 'number';
      };
      it('is', (next) => {
        /* #@range_begin(is_number_test) */
        var target = 1;
        match(validate(is(number, continues.normally, continues.abnormally)(target)),{
          failed: (message) => {
            expect().fail();
          },
          successful: (value, target) => {
            expect(
              value
            ).to.eql(
              true
            );
          }
        });
        /* #@range_end(is_number_test) */
        next();
      });
      var hasKey = (key, continues, continuesInFailure) => {
        return (inputObject) => {
          if(isEmpty(inputObject)) {
            return continuesInFailure(fail("empty", continues, continuesInFailure)(inputObject));
          } else {
            for (var theKey in inputObject) {
              if (hasOwnProperty.call(inputObject, key)) {
                return continues(succeed(true, continues, continuesInFailure)(inputObject));
              }
            }
            return continuesInFailure(fail(key + " is not found", continues, continuesInFailure)(inputObject));
          }
        };
      };
      it('hasKey', (next) => {
        var inputObject = {
          "key": 1
        };
        match(validate(hasKey("key", continues.normally, continues.abnormally)(inputObject)),{
          failed: (message) => {
            expect().fail();
          },
          successful: (value, inputObject) => {
            expect(
              value
            ).to.eql(
              true
            );
          }
        });
        match(validate(hasKey("nokey", continues.normally, continues.abnormally)({})),{
          failed: (message) => {
            expect(
              message
            ).to.eql(
              'empty'
            );
          },
          successful: (value, inputObject) => {
            expect().fail();
          }
        });
        match(validate(hasKey("nokey", continues.normally, continues.abnormally)(inputObject)),{
          failed: (message) => {
            expect(
              message
            ).to.eql(
              'nokey is not found'
            );
          },
          successful: (value, inputObject) => {
            expect().fail();
          }
        });
        next();
      });
    });
    it('ケージ監視', (next) => {
      /* #@range_begin(combinator_library) */
      var id = (any) => {
        return any;
      };
      var get = (key) => {
        return (obj) => {
          return obj[key];
        };
      };
      var isEqual = (n1) => {
        return (n2) => {
          return n2 === n1;
        };
      };
      var isLessThan = (n1) => {
        return (n2) => {
          return n1 > n2;
        };
      };
      var isMoreThan = (n1) => {
        return (n2) => {
          return n1 < n2;
        };
      };
      var not = (predicate) => {
        return (data) => {
          return ! predicate(data);
        }
      };
      var within = (lower) => {
        return (upper) => {
          return (data) => {
            return (extractor) => {
              return and(extractor, isMoreThan(lower))(extractor, isLessThan(upper))(data);
            };
          };
        };
      };
      var and = (firstExtractor, firstPredicate) => {
        return (nextExtractor, nextPredicate) => {
          return (data) => {
            var firstResult = firstPredicate(firstExtractor(data))
            if(! firstResult) {
              return false;
            } else {
              return nextPredicate(nextExtractor(data));
            }
          }
        };
      };
      var or = (firstExtractor, firstPredicate) => {
        return (nextExtractor, nextPredicate) => {
          return (data) => {
            var firstResult = firstPredicate(firstExtractor(data))
            if(firstResult) {
              return true;
            } else {
              return nextPredicate(nextExtractor(data));
            }
          }
        };
      };
      /* #@range_end(combinator_library) */
      /* #@range_begin(combinator_library_test) */
      var data = {
        temp: 24,
        time: new Date("2013/2/15 17:57:27")
      };
      expect(((_) => {
        var getTemp = (data) => {
          return get('temp')(data);
        };
        var getHour = (data) => {
          return get('time')(data).getHours();
        };
        return and(getTemp, isMoreThan(20))(getHour, isEqual(17))(data)
      })()).to.eql(
        true
      );
      expect(((_) => {
        var getTemp = (data) => {
          return get('temp')(data);
        };
        var getHour = (data) => {
          return get('time')(data).getHours();
        };
        return or(getTemp, isMoreThan(30))(getHour, isEqual(17))(data)
      })()).to.eql(
        true
      );
      expect(
        within(20)(30)(get('temp')(data))(id)
      ).to.eql(
        true
      );
      expect(
        within(20)(30)(data)(get('temp'))
      ).to.eql(
        true
      );
      expect(
        within(20)(30)(data)((data) => {
          return get('temp')(data)
        })
      ).to.eql(
        true
      );
      /* #@range_end(combinator_library_test) */
      next();
    });
  });
  it('Y combinator', (next) => {
    // Y = λF. (λx. F (λy. x x y))(λx. F (λy. x x y)) に
    /* #@range_begin(Y_combinator) */
    var Y = (F) => {
      return ((x) => {
        return F((y) => {
          return x(x)(y);
        }) ;
      })((x) =>  {
        return F((y) => {
          return x(x)(y);
        }) ;
      });
    };
    /* #@range_end(Y_combinator)  */
    // var Y = (F) => {
    //   return ((g) => {
    //     return (x) =>  {
    //       return F(g(g))(x);
    //     };
    //   })((g) =>  {
    //     return (x) => {
    //       return F(g(g))(x);
    //     };
    //   });
    // };
    /* #@range_begin(Y_combinator_test) */
    var factorial = Y((fact) => {
      return (n) => {
        if (n == 0) {
          return 1;
        } else {
          return n * fact(n - 1);
        }
      };
    });
    expect(
      factorial(3) // 3 * 2 * 1 = 6
    ).to.eql(
      6
    );
    /* #@range_end(Y_combinator_test) */
    next();
  });
  
}); // コンビネータ
// ### クロージャーを使う
describe('クロージャーを使う', () => {
  it('環境と変数のバインディング', (next) => {
    /* #@range_begin(variable_binding_in_environment) */
    /* 変数fooに数値1をバインドする */
    var foo = 1;
    /* 変数bar に文字列 "a string" をバインドする */
    var bar = "a string"; 
    /* #@range_end(variable_binding_in_environment) */
    /* #@range_begin(variable_binding_in_environment_test) */
    /* 環境 <foo |-> 1, bar |-> "a string"> のもとで評価する */
    expect(
      foo  // 上記環境から変数fooの値を取り出す
    ).to.eql(
      1
    );
    /* #@range_end(variable_binding_in_environment_test) */
    next();
  });
  it('部分適用と環境', (next) => {
    var multipleOf = (n) => { // 外側の関数定義
      return (m) => {         // 内側の関数定義
        if(m % n === 0) {
          return true;
        } else {
          return false;
        }
      };
    };
    /* #@range_begin(partial_application_with_environment) */
    var twoFold = multipleOf(2);
    expect(
      twoFold(4) 
    ).to.eql(
      true
    );
    /* #@range_end(partial_application_with_environment) */
    next();
  });
  // ### クロージャーで状態をカプセル化する
  describe('クロージャーで状態をカプセル化する', () => {
    describe('単純なクロージャーの例', (next) => {
      it('counter関数の例', (next) => {
        /* #@range_begin(counter_as_closure) */
        var counter = (init) => {
          var countingNumber =  init;
          /* countingNumberの環境を持つクロージャーを返す */
          return (_) => {  
            countingNumber = countingNumber + 1;
            return countingNumber ;
          };
        };
        /* #@range_end(counter_as_closure) */
        /* #@range_begin(counter_as_closure_test) */
        var counterFromZero = counter(0);
        expect(
          counterFromZero() // 1回目の実行
        ).to.eql( 
          1
        );
        expect(
          counterFromZero() // 2回目の実行
        ).to.eql( 
          2
        );
        /* #@range_end(counter_as_closure_test) */
        /* #@range_begin(another_counter) */
        var anoterCounterFromZero = counter(0);
        expect(
          anoterCounterFromZero()
        ).to.eql( 
          1
        );
        /* #@range_end(another_counter) */
        next();
      });
      it('counter関数における、外側のスコープからのクロージャーによる自由変数の捕捉', (next) => {
        var _init = 0;
        var counter = (_) => {  // クロージャーを返す
          _init = _init + 1;
          return _init;
        };
        expect(
          counter()
        ).to.eql( 
          1
        );
        _init = 100;
        expect(
          counter()
        ).to.eql( 
          101
        );
        next();
      });
      it('関数の外側にあるスコープによる自由変数の捕捉', (next) => {
        var sleep = require('sleep-async')();
        /* #@range_begin(captures_free_variable_outside_function) */
        var startUpTime = Date.now();
        
        var application = {
          timeLapse: () => {
            var now = Date.now();
            /* 外側のスコープにある startUpTime変数を捕捉している */
            return now - startUpTime; 
          }
        };
        /* #@range_end(captures_free_variable_outside_function) */
        sleep.sleep(5000, () => {
          expect(
            application.timeLapse()
          ).to.be.greaterThan(
            1
          );
        });
        next();
      });
    });
    describe('関数とデータの類似性', (next) => {
      it('チャーチ数', (next) => {
        /* #@range_begin(church_numeral) */
        var zero = (f) => {
          return (x) => {
            return x;           // 関数を0回適用する
          };
        };
        var one = (f) => {
          return (x) => {
            return f(x);        // 関数を1回適用する
          };
        };
        var two = (f) => {
          return (x) => {
            return f(f(x));     // 関数を2回適用する
          };
        };
        var three = (f) => {
          return (x) => {
            return f(f(f(x)));  // 関数を3回適用する
          };
        };
        /*#@range_end(church_numeral) */
        var add = (m) => {
          return (n) => {
            return (f) => {
              return (x) => {
                return m(f)(n(f)(x));
              };
            };
          };
        };
        var succ = (n) => {
          return (f) => {
            return (x) => {
              return f(n(f)(x));
            };
          };
        };
        var counter = (init) => {
          var _init = init;
          return (dummy) => {
            _init = _init + 1;
            return _init;
          };
        };
        expect(one(counter(0))()).to.eql(1);
        expect(two(counter(0))()).to.eql(2);
        expect(three(counter(0))()).to.eql(3);
        expect(succ(one)(counter(0))()).to.eql(2);
        expect(succ(two)(counter(0))()).to.eql(3);
        expect(add(zero)(one)(counter(0))()).to.eql(1);
        expect(add(one)(one)(counter(0))()).to.eql(2);
        expect(add(one)(two)(counter(0))()).to.eql(3);
        expect(add(two)(three)(counter(0))()).to.eql(5);
        next();
      });
      it('関数とリストの類似性', (next) => {
        var match = (data, pattern) => {
          return data(pattern);
        };
        var empty = (pattern) => {
          return pattern.empty;
        };
        var cons = (value, list) => {
          return (pattern) => {
            return pattern.cons(value, list);
          };
        };
        var isEmpty = (list) => {
          return match(list, { // match関数で分岐する
            empty: true,
            cons: (head, tail) => { // headとtailにそれぞれ先頭要素、末尾要素が入る
              return false;
            },
          });
        };
        var head = (list) => {
          return match(list, {
            empty: null, // 空のリストには先頭要素はありません
            cons: (head, tail) => {
              return head;
            },
          });
        };
        var tail = (list) => {
          return match(list, {
            empty: null,  // 空のリストには末尾要素はありません
            cons: (head, tail) => {
              return tail;
            },
          });
        };
        /*
          ~~~haskell
          list2fct :: Eq a => [(a,b)] -> a -> b
          list2fct [] _ = error "function not total"
          list2fct ((u,v):uvs) x | x == u = v
          | otherwise = list2fct uvs x
          fct2list :: (a -> b) -> [a] -> [(a,b)]
          fct2list f xs = [ (x, f x) | x <- xs ]
          ~~~
        */
        next();
      });
      describe('クロージャーによる「環境」の実装', () => {
        /* #@range_begin(environment_in_closure) */
        /* 空の環境  */
        var emptyEnv = (variable) => {
          return null;
        };
        /* 変数名に対応する値を環境から取りだす */
        var lookupEnv = (identifier, env) => {
          return env(identifier);
        };
        /* 環境を拡張する */
        var extendEnv = (identifier, value, env) => {
          return (queryIdentifier) => {
            if(identifier === queryIdentifier) {
              return value;
            } else {
              return lookupEnv(queryIdentifier,env);
            }
          };
        };
        /* #@range_end(environment_in_closure) */
        it('extendEnvで環境を作り、 lookupEnv で環境を探る', (next) => {
          /* #@range_begin(environment_in_closure_test) */
          expect(
            lookupEnv("a", emptyEnv)
          ).to.be(
            null
          );
          var newEnv = extendEnv('a',1, emptyEnv);
          expect(
            lookupEnv("a", newEnv)
          ).to.be(
            1
          );
          expect(((_) => {
            /* 空の辞書を作成する */
            var initEnv = emptyEnv;
            /* var a = 1 を実行して、辞書を拡張する */
            var firstEnv = extendEnv("a", 1, initEnv);
            /* var b = 3 を実行して、辞書を拡張する */
            var secondEnv = extendEnv("b",3, firstEnv);
            /* 辞書から b の値を参照する */
            return lookupEnv("b",secondEnv);
          })()).to.eql(
            3
          );
          expect(((_) => {
            /* 空の辞書を作成する */
            var initEnv = emptyEnv;
            /* var x = 1 を実行して、辞書を拡張する */
            var xEnv = extendEnv("x", 1, initEnv);
            /* var z = 2 を実行して、辞書を拡張する */
            var zEnv = extendEnv("z", 2, xEnv);
            /* 内部のスコープで var x = 3 を実行して、辞書を拡張する */
            var xEnvInner = extendEnv("x",3, zEnv);
            /* 内部のスコープで var y = 4 を実行して、辞書を拡張する */
            var innerMostEnv = extendEnv("y",4, xEnvInner);
            /* 一番内側のスコープを利用して x + y + z を計算する */
            return lookupEnv("x",innerMostEnv) + lookupEnv("y",innerMostEnv) + lookupEnv("z",innerMostEnv) ;
          })()).to.eql(
            3 + 4 + 2
          );
          /* #@range_end(environment_in_closure_test) */
          next();
        });
      });
    });
    it('クロージャーの変数バインディング', (next) => {
      /* #@range_begin(free_variable_in_closure) */
      var outerFunction = (outerArgument) => {
        var innerFunction = (innerArgument) => {
          return outerArgument + innerArgument;
        };
        return innerFunction;
      };
      /* #@range_end(free_variable_in_closure) */
      next();
    });
    describe('クロージャーと参照透過性', () => {
      it('multipleOf関数は参照透過である', (next) => {
        var multipleOf = (n) => {
          return (m) => {
            if(m % n === 0) {
              return true;
            } else {
              return false;
            }
          };
        };
        /* #@range_begin(multipleOf_is_transparent) */
        expect(
          multipleOf(2)(4)
        ).to.eql(
          multipleOf(2)(4)
        );
        expect(
          multipleOf(3)(5)
        ).to.eql(
          multipleOf(3)(5)
        );
        /* #@range_end(multipleOf_is_transparent) */
        next();
      });
      it('参照透過でないクロジャーの例', (next) => {
        var counter = (init) => {
          var _init = init;
          return (_) => {
            _init = _init + 1;
            return _init;
          };
        };
        /* #@range_begin(counter_is_not_transparent) */
        var counterFromZero = counter(0);
        expect(
          counterFromZero()
        ).not.to.eql( // notで一致しないことをテストしている
          counterFromZero()
        );
        /* #@range_end(counter_is_not_transparent) */
        next();
      });
      it('参照透過でないクロジャーの利用法', (next) => {
        /* チャーチ数 church numeral */
        var zero = (f) => {
          return (x) => {
            return x;
          };
        };
        var one = (f) => {
          return (x) => {
            return f(x);
          };
        };
        var two = (f) => {
          return (x) => {
            return f(f(x));
          };
        };
        var three = (f) => {
          return (x) => {
            return f(f(f(x)));
          };
        };
        var succ = (n) => {
          return (f) => {
            return (x) => {
              return f(n(f)(x));
            };
          };
        };
        var add = (m) => {
          return (n) => {
            return (f) => {
              return (x) =>{
                return m(f)(n(f)(x));
              };
            };
          };
        };
        /* 関数適用の回数を数えるcounterクロージャー */
        var counter = (init) => {
          var _init = init; // 可変な変数
          return (_) => {
            _init = _init + 1; // 代入で_init変数を更新する
            return _init;
          };
        };
        /***** counterクロージャーを用いたチャーチ数のテスト *****/
        /* #@range_begin(church_numeral_counter) */
        expect(
          one(counter(0))() // oneはチャーチ数（@<list>{church_numeral}）の1
        ).to.eql(
          1
        );
        expect(
          two(counter(0))() // twoはチャーチ数（@<list>{church_numeral}）の2
        ).to.eql(
          2
        );
        /* #@range_end(church_numeral_counter) */
        expect(
          add(one)(two)(counter(0))()
        ).to.eql(
          3
        );
        next();
      });
    });
    // #### 不変なデータ型を作る
    describe('不変なデータ型を作る', () => {
      it('オブジェクト型は不変ではない', (next) => {
        /* #@range_begin(object_is_not_immutable) */
        var object = {
          a: 1
        };
        expect(
          object.a
        ).to.eql(
          1
        );
        object.a = 2;
        expect(
          object.a
        ).to.eql(
          2 // 1だった値が2に変更されています
        );
        /* #@range_end(object_is_not_immutable) */
        next();
      });
      it('不変なオブジェクト型を作る', (next) => {
        /* #@range_begin(immutable_object_type) */
        var object = {
          empty: (_) => {
            return null;
          },
          get: (key, obj) => {
            return obj(key);
          },
          set: (key, value, obj) => {
            return (key2) => {
              if(key === key2) {
                return value;
              } else {
                return object.get(key2,obj);
              }
            };
          }
        };
        /* #@range_end(immutable_object_type) */
        /* #@range_begin(immutable_object_type_test) */
        expect(
          object.get("R2D2", 
                     object.set("R2D2", "Star Wars", 
                                object.set("HAL9000","2001: a space odessay",
                                           object.empty)))
        ).to.eql(
          "Star Wars"
        );
        expect(
          object.get("R2D2", 
                     object.set("HAL9000","2001: a space odessay",
                                object.empty))
        ).to.eql(
          null
        );
        expect(
          object.get("HAL9000", 
                     object.set("C3PO", "Star Wars", 
                                object.set("R2D2", "Star Wars", 
                                           object.set("HAL9000","2001: a space odessay",
                                                      object.empty))))
        ).to.eql(
          "2001: a space odessay"
        );
        /* #@range_end(immutable_object_type_test) */
        next();
      });
      it('カリー化された不変なオブジェクト型', (next) => {
        /* #@range_begin(immutable_object_type_curried) */
        var object = {  // objectモジュール
          empty: (_) => {
            return null;
          },
          set: (key, value) => {
            return (obj) => {
              return (queryKey) => {
                if(key === queryKey) {
                  return value;
                } else {
                  return object.get(queryKey)(obj);
                }
              };
            };
          },
          get: (key) => {
            return (obj) => {
              return obj(key);
            };
          }
        };
        /* #@range_end(immutable_object_type_curried) */
        /* #@range_begin(immutable_object_type_curried_test) */
        var robots = compose( // object.set関数を合成する
          object.set("C3PO", "Star Wars"),
          object.set("HAL9000","2001: a space odessay")
        )(object.empty());

        expect(
          object.get("HAL9000")(robots)
        ).to.eql(
          "2001: a space odessay"
        );
        /* #@range_end(immutable_object_type_curried_test) */
        next();
      });
      it('不変なオブジェクト型を作る(改良版)', (next) => {
        /* #@range_begin(immutable_object_type_improved) */
        var objects = {
          empty: (_) => {
            return null;
          },
          get: (key, obj) => {
            return obj(key);
          },
          set: (key, value, obj) => {
            var self = this;
            return (key2) => {
              if(key === key2) {
                return value;
              } else {
                return self.get(key2,obj)
              }
            };
          },
          fromObject: (obj) => {
            var self = this;
            var keys = (o) => {
              var result = [];
              for(var prop in o) {
                if(o.hasOwnProperty(prop))
                  result.push(prop);
              }
              return result;
            };
            return keys(obj).reduce((accumulator, key) => {
              return self.set.call(self,key, obj[key], accumulator);
            }, self.empty);
          }
        };
        /* #@range_end(immutable_object_type_improved) */
        /* #@range_begin(immutable_object_type_improved_test) */
        expect(
          objects.get("R2D2", objects.set("HAL9000","2001: a space odessay",objects.empty))
        ).to.eql(
          null
        );
        expect(
          objects.get("HAL9000", objects.fromObject.call(objects,{"HAL9000" : "2001: a space odessay", "R2D2": "Star Wars"}))
        ).to.eql(
          "2001: a space odessay"
        );
        expect(
          objects.get("R2D2", objects.fromObject.call(objects,{"HAL9000" : "2001: a space odessay", "R2D2": "Star Wars"}))
        ).to.eql(
          "Star Wars"
        );
        /* #@range_end(immutable_object_type_improved_test) */
        next();
      });
      it('不変なリスト型', (next) => {
        // ~~~scheme
        // (define (cons x y)
        //   (lambda (m) (m x y)))
        // (define (car z)
        //   (z (lambda (p q) p)))
        // (define (cdr z)
        //   (z (lambda (p q) q)))
        // ~~~
        /* #@range_begin(immutable_list) */
        var list = {
          empty: (index) => {
            return true;
          },
          cons: (head,tail) => {
            return (f) => {
              return f(head,tail);
            };
          },
          head: (array) => {
            return array((head,tail) => {
              return head;
            });
          },
          tail: (array) => {
            return array((head,tail) => {
              return tail;
            });
          },
          at: (index,array) => {
            if(index === 0){
              return this.head(array);
            } else {
              return this.at(index -1, this.tail(array));
            }
          }
        };
        var theList = list.cons(1,list.cons(2,list.cons(3,list.empty)));
        expect(
          list.head(theList)
        ).to.eql(
          1
        );
        expect(
          list.head(list.tail(theList))
        ).to.eql(
          2
        )
        expect(
          list.at(0,theList)
        ).to.eql(
          1
        )
        expect(
          list.at(1,theList)
        ).to.eql(
          2
        )
        expect(
          list.at(2,theList)
        ).to.eql(
          3
        );
        /* #@range_end(immutable_list) */
        next();
      });
      describe('代数的ストリーム型', () => {
        /* #@range_begin(algebraic_stream) */
        var empty = (_) => {
          return (pattern) => {
            expect(pattern).to.an('object');
            return pattern.empty(_);
          };
        };
        var cons = (head,tailThunk) => {
          expect(tailThunk).to.a('function');
          return (pattern) => {
            expect(pattern).to.an('object');
            return pattern.cons(head,tailThunk);
          };
        };
        /* head:: STREAM -> MAYBE[STREAM] */
        var head = (lazyList) => {
          return match(lazyList,{
            empty: (_) => {
              return null;
            },
            cons: (value, tailThunk) => {
              return value;
            }
          });
        };
        /* tail:: STREAM -> MAYBE[STREAM] */
        var tail = (lazyList) => {
          return match(lazyList,{
            empty: (_) => {
              return null;
            },
            cons: (head, tailThunk) => {
              return tailThunk();
            }
          });
        };
        var isEmpty = (lazyList) => {
          return match(lazyList,{
            empty: (_) => {
              return true;
            },
            cons: (head1,tailThunk1) => {
              return false;
            }
          });
        };
        /* #@range_end(algebraic_stream) */
        /* #@range_begin(algebraic_stream_helpers) */
        // ### stream#toArray
        var toArray = (lazyList) => {
          return match(lazyList,{
            empty: (_) => {
              return [];
            },
            cons: (head,tailThunk) => {
              return match(tailThunk(),{
                empty: (_) => {
                  return [head];
                },
                cons: (head_,tailThunk_) => {
                  return [head].concat(toArray(tailThunk()));
                }
              });
            }
          });
        };
        // ### stream#take
        /* take:: STREAM -> NUMBER -> STREAM */
        var take = (lazyList) => {
          return (number) => {
            expect(number).to.a('number');
            expect(number).to.be.greaterThan(-1);
            return match(lazyList,{
              empty: (_) => {
                return empty();
              },
              cons: (head,tailThunk) => {
                if(number === 0) {
                  return empty();
                } else {
                  return cons(head,(_) => {
                    return take(tailThunk())(number -1);
                  });
                }
              }
            });
          };
        };
        /* #@range_end(algebraic_stream_helpers) */
        it("stream#cons", (next) => {
          var stream = cons(1, (_) => {
            return cons(2,(_) => {
              return empty();
            });
          });
          expect(
            head(stream)
          ).to.eql(
            1
          );
          next();
        });
        it("stream#tail", (next) => {
          /* stream = [1,2] */
          var stream = cons(1, (_) => {
            return cons(2,(_) => {
              return empty();
            });
          });
          expect(
            tail(stream)
          ).to.a("function");
          expect(
            head(tail(stream))
          ).to.eql(
            2
          );
          next();
        });
        describe("無限ストリーム", () => {
          /* #@range_begin(infinite_stream) */
          /* ones = [1,1,1,1,...] */
          var ones = cons(1, (_) => {
            return ones;
          });
          expect(
            head(ones)
          ).to.eql(
            1
          );
          expect(
            head(tail(ones))
          ).to.eql(
            1
          );
          /* #@range_end(infinite_stream) */
          /* #@range_begin(infinite_integer) */
          /* ones = [1,2,3,4,...] */
          var enumFrom = (n) => {
            return cons(n, (_) => {
              return enumFrom(n + 1);
            });
          };
          it("無限の整数列をテストする", (next) => {
            expect(
              head(enumFrom(1))
            ).to.eql(
              1
            );
            expect(
              head(tail(enumFrom(1)))
            ).to.eql(
              2
            );
            expect(
              toArray(take(enumFrom(1))(10))
            ).to.eql(
              [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] 
            );
            /* #@range_end(infinite_integer) */
            next();
          });
          it("filterで無限の偶数列を作る", (next) => {
            var even = (n) => {
              return 0 === (n % 2);
            };
            /* #@range_begin(infinite_even_integer) */
            var evenIntegers = stream.filter(even)(enumFrom(1));
            expect(
              head(evenIntegers)
            ).to.eql(
              2
            );
            expect(
              toArray(stream.take(evenIntegers)(4))
            ).to.eql(
              [ 2, 4, 6, 8 ]
            );
            /* #@range_end(infinite_even_integer) */
            next();
          });
          it("filterで無限の素数列を作る", (next) => {
            this.timeout(7000);
            var multipleOf = (n) => {
              return (m) => {
                if(m % n === 0) {
                  return true;
                } else {
                  return false;
                }
              };
            };
            /* #@range_begin(infinite_primes) */
            var leastDivisor = (n) => {
              expect(n).to.a('number');
              var leastDivisorHelper = (k, n) => {
                expect(k).to.a('number');
                expect(n).to.a('number');
                if(multipleOf(k)(n)) {
                  return k;
                } else {
                  if(n < (k * k)) {
                    return n;
                  } else {
                    return leastDivisorHelper(k+1, n);
                  }
                };
              };
              return leastDivisorHelper(2,n);
            };
            var isPrime = (n) => {
              if(n < 1) {
                return new Error("argument not positive");
              }
              if(n === 1) {
                return false;
              } else {
                return leastDivisor(n)  === n ;
              }
            };
            
            var primes = stream.filter(isPrime)(enumFrom(1));
            expect(
              toArray(stream.take(primes)(10))
            ).to.eql(
              [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]
            );
            /* #@range_end(infinite_primes) */
            next();
          });
        });
        it("代数的ストリーム型は不変ではない", (next) => {
          var stream = cons({key: 1}, (_) => {
            return cons(2,(_) => {
              return empty();
            });
          });
          expect(
            head(stream).key
          ).to.eql(
            1
          );
          var object = head(stream);
          object.key = 2;
          expect(
            head(stream).key
          ).to.eql(
            2 // 1が2に変更されている
          );
          next();
        });
      });
      describe('不変なストリーム型', () => {
        /* #@range_begin(immutable_stream) */
        var empty = (index) => {
          return true;
        };
        var cons = (head,tailThunk) => {
          return (f) => {
            return f(head,tailThunk);
          };
        };
        var head = (lazyList) => {
          return lazyList((head,tailThunk) => {
            return head;
          });
        };
        var tail = (lazyList) => {
          return lazyList((head,tailThunk) => {
            return tailThunk();
          });
        };
        /* #@range_end(immutable_stream) */
        it("関数的ストリーム型は不変である", (next) => {
          var stream = cons({key: 1}, (_) => {
            return cons(2,(_) => {
              return empty();
            });
          });
          expect(
            head(stream).key
          ).to.eql(
            1
          );
          var object = head(stream);
          object.key = 2;
          expect(
            head(stream).key
          ).to.eql(
            2 // 1が2に変更されている
          );
          next();
        });
      });
    });
    // #### クロージャーでジェネレーターを作る
    describe('クロージャーでジェネレーターを作る', () => {
      it('ECMAScript6 generator', (next) => {
        /* #@range_begin(es6_generator) */
        function* genCounter(){
          yield 1;
          yield 2;
          return 3;
        };
        var counter = genCounter();
        expect(
          counter.next().value
        ).to.eql(
          1
        );
        expect(
          counter.next().value
        ).to.eql(
          2
        );
        /* #@range_end(es6_generator) */
        next();
      });
      it('リスト・ジェレネータ', (next) => {
        /* #@range_begin(list_generator_test) */
        var generator = list.generate(list.cons(1, list.cons(2, list.empty())));
        expect(
          generator()
        ).to.eql(
          1
        );
        expect(
          generator()
        ).to.eql(
          2
        );
        /* #@range_end(list_generator_test) */
        next();
      });
      it('ストリーム・ジェレネータ', (next) => {
        /* #@range_begin(stream_generator_test) */
        var generator = stream.generate(stream.cons(1, (_) => {
          return stream.cons(2, (_) => {
            return stream.empty();
          });
        }));
        expect(
          generator()
        ).to.eql(
          1
        );
        expect(
          generator()
        ).to.eql(
          2
        );
        /* #@range_end(stream_generator_test) */
        next();
      });
      describe('streamからジェネレータを作る', () => {
        /* #@range_begin(generator_from_stream) */
        var generate = (aStream) => {
          /* いったんローカル変数にストリームを格納する */
          var _stream = aStream; 
          /* ジェネレータ関数が返る */
          return (_) => {
            return stream.match(_stream, {
              empty: () => {
                return null;
              },
              cons: (head, tailThunk) => {
                _stream = tailThunk();  // ローカル変数を更新する
                return head;  // ストリームの先頭要素を返す
              }
            });
          };
        };
        /* #@range_end(generator_from_stream) */
        it('整数列のジェネレータ',(next) => {
          var enumFrom = (from) => {
            return stream.cons(from, (_) => {
              return enumFrom(from + 1);
            });
          };
          /* #@range_begin(integer_generator) */
          /* 無限の整数列を生成する */
          var integers = enumFrom(0);            
          /* 無限ストリームからジェネレータを生成する */
          var intGenerator = generate(integers); 
          expect(intGenerator()).to.eql(
            0
          );
          expect(intGenerator()).to.eql(
            1
          );
          expect(intGenerator()).to.eql(
            2
          );
          /* #@range_end(integer_generator) */
          next();
        });
        it('無限の素数列',(next) => {
          this.timeout(4000);
          var multipleOf = (n) => {
            return (m) => {
              if(n % m === 0) {
                return true;
              } else {
                return false;
              }
            };
          };
          // #### エラトステネスのふるいによる素数の生成 
          // [![IMAGE ALT TEXT](http://img.youtube.com/vi/1NzrrU8BawA/0.jpg)](http://www.youtube.com/watch?v=1NzrrU8BawA "エラトステネスのふるいの動画")
          /* #@range_begin(eratosthenes_sieve) */
          /* エラトステネスのふるい */
          var sieve = (aStream) => {
            return stream.match(aStream, {
              empty: () => { 
                return null;
              },
              cons: (head, tailThunk) => {
                return stream.cons(head, (_) => {
                  return sieve(stream.remove( // 後尾を素数の倍数でふるいにかける
                    (item) => { 
                      return multipleOf(item)(head);  
                    }
                  )(tailThunk()));
                }); 
              }
            });
          };
          var primes = sieve(stream.enumFrom(2)); // 無限の素数列
          /* #@range_end(eratosthenes_sieve) */
          /* #@range_begin(eratosthenes_sieve_test) */
          expect(
            stream.toArray(stream.take(primes)(10))
          ).to.eql(
            [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]
          );
          /* #@range_end(eratosthenes_sieve_test) */
          next();
        });
        it('素数のジェネレータ',(next) => {
          this.timeout(7000);

          var multipleOf = (n) => {
            return (m) => {
              if(n % m === 0) {
                return true;
              } else {
                return false;
              }
            };
          };
          var leastDivisor = (n) => {
            expect(n).to.a('number');
            var leastDivisorHelper = (k, n) => {
              expect(k).to.a('number');
              expect(n).to.a('number');
              if(multipleOf(n)(k)) {
                return k;
              } else {
                if(n < (k * k)) {
                  return n;
                } else {
                  return leastDivisorHelper(k+1, n);
                }
              };
            };
            return leastDivisorHelper(2,n);
          };
          var isPrime = (n) => {
            if(n < 1) {
              return new Error("argument not positive");
            }
            if(n === 1) {
              return false;
            } else {
              return leastDivisor(n)  === n ;
            }
          };
          var integers = stream.enumFrom(1);
          /* 素数のストリーム */
          var sieve = (aStream) => {
            return stream.match(aStream, {
              empty: () => { return null; },
              cons: (head, tailThunk) => {
                return stream.cons(head, (_) => {
                  return sieve(stream.filter(
                    (item) => { 
                      return ! multipleOf(item)(head);  
                    }
                  )(tailThunk()));
                }); 
              }
            });
          };
          /* #@range_begin(prime_generator) */
          var primes = sieve(stream.enumFrom(2)); // 無限の素数列
          var primeGenerator = generate(primes);  // 素数のジェネレータ
          /******* テスト ********/
          expect(primeGenerator()).to.eql(
            2
          );
          expect(primeGenerator()).to.eql(
            3
          );
          expect(primeGenerator()).to.eql(
            5
          );
          /* #@range_end(prime_generator) */
          next();
        });
      });
      describe('ジェネレータ・コンビネータ', () => {
        var identity = (any) => { return any; };
        var succ = (n) => { return n + 1; };
        /* #@range_begin(generator_in_closure) */
        var generator = (init) => {
          return (current) => {
            return (step) => {
              return stream.cons(current(init),
                                 (_) => { 
                                   return generator(step(init))(current)(step);
                                 });
            };
          };
        };
        var integers = generator(0)(identity)(succ);
        it('integers をテストする', (next) =>{
          expect(
            stream.head(integers)
          ).to.eql(
            0
          );
          expect(
            stream.head(stream.tail(integers))
          ).to.eql(
            1
          );
          expect(
            stream.head(stream.tail(stream.tail(integers)))
          ).to.eql(
            2
          );
          /* #@range_end(generator_in_closure) */
          next();
        });
        it('ジェレネータによる単体テストの自動生成', (next) => {
          /* #@range_begin(generate_unit_tests) */
          var even = (n) => {
            return 0 === (n % 2);
          };
          var evenIntegers = stream.filter(even)(integers);
          stream.forEach(stream.take(evenIntegers)(10))((n) => {
            return expect(
              even(n)
            ).to.eql(
              true
            );
          });
          /* #@range_end(generate_unit_tests) */
          next();
        });
      });
    });
  }); // クロージャーで状態をカプセル化する
});

// ### 関数を渡す
describe('関数を渡す', () => {
  // #### コールバックで処理をモジュール化する
  describe('コールバックで処理をモジュール化する', () => {
    it('直接コールする', (next) => {
      /* #@range_begin(direct_call) */
      var succ = (n) => {
        return n + 1;
      };
      var doCall = (arg) => {
        return succ(arg);  // succ関数を直接呼び出す
      };
      expect(
        doCall(2)
      ).to.eql(
        3
      );
      /* #@range_end(direct_call) */
      next();
    });
    it('コールバックを呼び出す', (next) => {
      var succ = (n) => {
        return n + 1;
      };
      /* #@range_begin(call_callback) */
      var setupCallBack = (callback) => {
        /* コールバック関数を実行する無名関数を返す */
        return (arg) => {  
          return callback(arg);
        };
      };
      /* コールバック関数を設定する */
      var doCallBack = setupCallBack(succ);  
      expect(
        doCallBack(2) // 設定されたコールバック関数を実行する
      ).to.eql(
        3
      );
      /* #@range_end(call_callback) */
      next();
    });
    it('リストのmap', (next) => {
      /* #@range_begin(list_map) */
      /* map:: FUN[T => T] => LIST[T] =>  LIST[T] */
      var map = (callback) => {
        return (alist) => {
          return match(alist,{
            empty: (_) => {
              return list.empty();
            },
            cons: (head, tail) => {
              /* コールバック関数を実行する */
              return list.cons(callback(head),  
                               map(callback)(tail)); // 再帰呼び出し
            }
          });
        };
      };
      /* #@range_end(list_map) */
      // var map = (alist,callback) => {
      //   return match(alist,{
      //     empty: (_) => {
      //       return list.empty();
      //     },
      //     cons: (head, tail) => {
      //       /* コールバック関数を実行する */
      //       return list.cons(callback(head),  
      //                        map(tail,callback)); // 再帰呼び出し
      //     }
      //   });
      // };
      var squareCallback = (n) => { // 要素を2乗する関数を渡す
        return n * n;
      };
      // var squareList = map(squareCallback)(numbers);
      /* 要素を2倍する関数を渡す */
      var doubleCallback = (n) => { 
        return n * 2;
      };
      /* #@range_begin(list_map_test) */
      /* map処理の対象となる数値のリスト */
      var numbers = list.cons(1,
                              list.cons(2,
                                        list.cons(3,
                                                  list.empty())));
      /* 要素を2倍にするmap処理 */
      var mapDouble = map((n) => { 
        return n * 2;
      });
      expect(
        compose(list.toArray,mapDouble)(numbers)
      ).to.eql(
        [2,4,6]
      );
      /* 要素を2乗にするmap処理 */
      var mapSquare = map((n) => { 
        return n * n;
      });
      expect(
        compose(list.toArray,mapSquare)(numbers)
      ).to.eql(
        [1,4,9]
      );
      /* #@range_end(list_map_test) */
      next();
    });
    describe('イベント駆動', () => {
      it('オブジェクトの値をイベント駆動で取得する', (next) => {
        var processEvent = (event) => {
          return (callback) => {
            return callback(event);
          };
        };
        var anEvent = {"temperture": 26.0};
        expect(
          processEvent(anEvent)((theEvent) => {
            return theEvent.temperture;
          })
        ).to.eql(
          26
        );
        var extractTemperture = (event) => {
          return processEvent(event)((theEvent) => {
            return theEvent.temperture;
          });
        };
        expect(
          extractTemperture(anEvent)
        ).to.eql(
          26
        );
        next();
      });
      it('イベント駆動システムを実装する', (next) => {
        /* #@range_begin(event_driven_system) */
        var eventSystem = () => {
          var handlers = object.empty(); // イベントハンドラを格納する変数
          return {
            on: (eventName, callback) => {
              handlers = object.set(eventName, callback, handlers);
              return null;
            },
            emit: (eventName, arg) => {
              return object.get(eventName, handlers)(arg);  // 該当するイベントハンドラを起動する
            }
          };
        };
        /* #@range_end(event_driven_system) */
        var eventLoop = (eventSystem) => {
          
        };
        /* #@range_begin(event_driven_system_test) */
        var eventDrivenServer = eventSystem(); // イベント駆動システムを初期化する
        /* イベント started を登録する */
        eventDrivenServer.on("started", (_) => { // startedイベントで実行されるコールバック関数を渡す
          return "event started";
        });
        /* イベント terminated を登録する */
        eventDrivenServer.on("terminated", (exitCode) => { // terminatedイベントで実行されるコールバック関数 
          return "event terminated with " + exitCode;
        });
        /**** テスト ****/
        /* イベント started を生じさせる */
        expect(
          eventDrivenServer.emit("started", null)
        ).to.eql(
          "event started"
        );
        /* イベント terminated を生じさせる */
        expect(
          eventDrivenServer.emit("terminated",404)
        ).to.eql(
          "event terminated with 404"
        );
        /* #@range_end(event_driven_system_test) */
        next();
      });
    });
    describe('コールバックによるリストの再帰関数', () => {
      it('リストのsum', (next) => {
        var list = {
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
          /* #@range_begin(list_sum) */
          sum: (alist) => {
            return (accumulator) => {
              return match(alist,{
                empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return list.sum(tail)(accumulator + head);
                }
              });
            };
          },
          /* #@range_end(list_sum) */
          /* #@range_begin(list_sum_callback) */
          sumWithCallBack: (alist) => {
            return (accumulator) => {
              return (CALLBACK) => { // コールバック関数を受け取る
                return match(alist,{
                  empty: (_) => {
                    return accumulator;
                  },
                  cons: (head, tail) => {
                    return CALLBACK(head)( // コールバック関数を呼び出す
                      list.sumWithCallBack(tail)(accumulator)(CALLBACK)
                    );
                  }
                });
              };
            };
          }
          /* #@range_end(list_sum_callback) */
        };
        /* #@range_begin(list_sum_callback_test) */
        var numbers = list.cons(1, 
                                list.cons(2,
                                          list.cons(3,
                                                    list.empty())));
        /* sumWithCallBack関数に渡すコールバック関数 */
        var callback = (n) => {  
          return (m) => {
            return n + m;
          };
        };
        expect(
          list.sumWithCallBack(numbers)(0)(callback)
        ).to.eql(
          6  // 1 + 2 + 3 = 6
        );
        /* #@range_end(list_sum_callback_test) */
        expect(
          list.sum(numbers)(0)
        ).to.eql(
          6
        );
        next();
      });
      it('リストのlength', (next) => {
        var list = {
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
          /* #@range_begin(list_length) */
          length: (alist) => {
            return (accumulator) => {
              return match(alist,{
                empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return list.length(tail)(accumulator + 1);
                }
              });
            };
          },
          /* #@range_end(list_length) */
          /* #@range_begin(list_length_callback) */
          lengthWithCallBack: (alist) => {
            return (accumulator) => {
              return (CALLBACK) => { // コールバック関数を受け取る
                return match(alist,{
                  empty: (_) => {
                    return accumulator;
                  },
                  cons: (head, tail) => {
                    return CALLBACK(head)(
                      list.lengthWithCallBack(tail)(accumulator)(CALLBACK)
                    );
                  }
                });
              };
            };
          }
          /* #@range_end(list_length_callback) */
        };
        var numbers = list.cons(1, 
                                list.cons(2,
                                          list.cons(3,
                                                    list.empty())));
        expect(
          list.length(numbers)(0)
        ).to.eql(
          3
        );
        /* #@range_begin(list_length_callback_test) */
        /* lengthWithCallBack関数に渡すコールバック関数 */
        var callback = (n) => {  
          return (m) => {
            return 1 + m;
          };
        };
        expect(
          list.lengthWithCallBack(numbers)(0)(callback)
        ).to.eql(
          3
        );
        /* #@range_end(list_length_callback_test) */
        next();
      });
      it('リストのproduct', (next) => {
        var productWithCallBack = (alist) => {
          return (accumulator) => {
            return (CALLBACK) => {
              return match(alist,{
                empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return CALLBACK(head)(productWithCallBack(tail)(accumulator)(CALLBACK));
                }
              });
            };
          };
        };
        var callback = (n) => {  
          return (m) => {
            return n * m;
          };
        };
        var numbers = list.cons(1, 
                                list.cons(2,
                                          list.cons(3,
                                                    list.empty())));
        expect(
          productWithCallBack(numbers)(1)(callback)
        ).to.eql(
          6
        );
        next();
      });
      it('リストのreverse', (next) => {
        var reverserWithCallBack = (alist) => {
          return (accumulator) => {
            return (CALLBACK) => {
              return match(alist,{
                empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return CALLBACK(head)(reverserWithCallBack(tail)(accumulator)(CALLBACK));
                }
              });
            };
          };
        };
        var callback = (x) => {
          return (xs) => {
            return list.match(xs, {
              empty: (_) => {
                return list.cons(x, list.empty());
              },
              cons: (head, tail) => {
                return list.cons(head, callback(x)(tail)); 
              }
            });
          };
        };
        var numbers = list.cons(1, 
                                list.cons(2,
                                          list.cons(3,
                                                    list.empty())));
        expect(
          list.toArray(reverserWithCallBack(numbers)(list.empty())(callback))
        ).to.eql(
          [3,2,1]
        );
        next();
      });
      it('リストのall', (next) => {
        var allWithCallBack = (alist) => {
          return (accumulator) => {
            return (CALLBACK) => {
              return match(alist,{
                empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return CALLBACK(head)(allWithCallBack(tail)(accumulator)(CALLBACK));
                }
              });
            };
          };
        };
        var callback = (n) => {  
          return (m) => {
            return n && m;
          };
        };
        var allTrueList = list.cons(true, 
                                list.cons(true,
                                          list.cons(true,
                                                    list.empty())));
        expect(
          allWithCallBack(allTrueList)(true)(callback)
        ).to.eql(
          true
        );
        var notAllTrueList = list.cons(true, 
                                list.cons(true,
                                          list.cons(false,
                                                    list.empty())));
        expect(
          allWithCallBack(notAllTrueList)(true)(callback)
        ).to.eql(
          false
        );
        next();
      });
    });
    it('ストリームのmap', (next) => {
      /* #@range_begin(stream_map) */
      var map = (lazyList) => {
        return (callback) => {
          return match(lazyList,{
            empty: (_) => {
              return stream.empty();
            },
            cons: (head, tailThunk) => {
              return stream.cons(callback(head), (_) => {
                return map(tailThunk())(callback);
              });
            }
          });
        };
      };
      /* #@range_end(stream_map) */
      /* #@range_begin(stream_map_test) */
      var numberStream = stream.cons(1, (_) => {
        return stream.cons(2,(_) => {
          return stream.empty();
        });
      });
      var double = (number) => {
        return number * 2;
      };
      var doubled_stream = map(numberStream)(double);
      expect(
        stream.head(doubled_stream)
      ).to.eql(
        2
      );
      expect(
        stream.toArray(doubled_stream)
      ).to.eql(
        [2,4]
      );
      var stringStream = stream.cons("a", (_) => {
        return stream.cons("b",(_) => {
          return stream.empty();
        });
      });
      var upper = (string) => {
        return string.toUpperCase();
      };
      expect(
        stream.toArray(map(stringStream)(upper))
      ).to.eql(
        ["A","B"]
      );
      /* #@range_end(stream_map_test) */
      next();
    });
  });
  // #### 非同期処理にコールバック関数を渡す 
  describe('非同期処理にコールバック関数を渡す', () => {
    it("たらいまわし関数", (next) => {
      /* #@range_begin(tarai_function) */
      /* たらいまわし関数 */
      var tarai = (x,y,z) => {
        if(x > y) {
          return tarai(tarai(x - 1, y, z), 
                       tarai(y - 1, z, x), 
                       tarai(z - 1, x, y));
        } else {
          return y;
        }
      };
      expect(
        tarai(1 * 2, 1, 0)
      ).to.eql(
        2 
      );
      /* #@range_end(tarai_function) */
      next();
    });
  });
  // #### 畳み込み関数に関数を渡す
  describe('畳み込み関数に関数を渡す', () => {
    /* #@range_begin(list_foldr) */
    var foldr = (alist) => {
      return (accumulator) => {
        return (callback) => {
          return match(alist,{
            empty: (_) => {
              return accumulator;
            },
            cons: (head, tail) => {
              return callback(head)(foldr(tail)(accumulator)(callback));
            }
          });
        };
      };
    };
    /* #@range_end(list_foldr) */
    it("foldrでsumを作る", (next) => {
      /* #@range_begin(foldr_sum) */
      var sum = (alist) => {
        return foldr(alist)(0)((item) => {
          return (accumulator) => {
            return accumulator + item;
          };
        });
      };
      /* #@range_end(foldr_sum) */
      /* list = [1,2,3,4] */
      var seq = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
      expect(
        sum(seq)
      ).to.eql(
        10  // 1 + 2 + 3 + 4 = 10
      );
      next();
    });
    it("foldrでlength関数を作る", (next) => {
      /* #@range_begin(foldr_length) */
      var length = (alist) => {
        return foldr(alist)(0)((item) => {
          return (accumulator) => {
            return accumulator + 1;
          };
        });
      };
      /* #@range_end(foldr_length) */
      /* list = [1,2,3,4] */
      var seq = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
      expect(
        length(seq)
      ).to.eql(
        4
      );
      next();
    });
    it("foldrでproductを作る", (next) => {
      /* #@range_begin(foldr_product) */
      var product = (alist) => {
        return foldr(alist)(1)((item) => {
          return (accumulator) => {
            return accumulator * item;
          };
        });
      };
      /********* テスト **********/
      /* list = [1,2,3] */
      var seq = list.cons(1,
                          list.cons(2,
                                    list.cons(3,
                                              list.empty())));
      expect(
        product(seq)
      ).to.eql(
        6 // 1 * 2 * 3 = 6
      );
      /* #@range_end(foldr_product) */
      next();
    });
    it("foldrで reverse関数を作る", (next) => {
      var list = {
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
        toArray: (alist) => {
          var toArrayAux = (alist,accumulator) => {
            return match(alist, {
              empty: (_) => {
                return accumulator;  
              },
              cons: (head, tail) => {
                return toArrayAux(tail, accumulator.concat(head));
              }
            });
          };
          return toArrayAux(alist, []);
        },
        /* #@range_begin(foldr_reverse) */
        /* list.append関数は、2つのリストを連結する */
        append: (xs) => {
          return (ys) => {
            return match(xs, {
              empty: (_) => {
                return ys;
              },
              cons: (head, tail) => {
                return list.cons(head, list.append(tail)(ys)); 
              }
            });
          };
        },
        /* list.reverse関数は、リストを逆転する */
        reverse: (alist) => {
          return foldr(alist)(list.empty(0))((item) => {
            return (accumulator) => {
              return list.append(accumulator)(list.cons(item,
                                                        list.empty()));
            };
          });
        }
        /* #@range_end(foldr_reverse) */
      };
      /* list = [1,2,3,4] */
      var seq = list.cons(1,
                          list.cons(2,
                                    list.cons(3,
                                              list.cons(4,
                                                        list.empty()))));
      expect(
        list.toArray(list.reverse(seq))
      ).to.eql(
        [ 4, 3, 2, 1]
      );
      next();
    });
    it("foldrでfind関数を作る", (next) => {
      var even = (n) => {
        return (n % 2) === 0;
      };
      var list = {
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
        /* #@range_begin(foldr_find) */
        /* list.find関数は、条件に合致した要素をリストから探す */
        find:  (alist) => {
          return (predicate) => { // 要素を判定する述語関数
            return foldr(alist)(null)((item) => { // foldrを利用する
              return (accumulator) => {
                /* 要素が見つかった場合、その要素を返す */
                if(predicate(item) === true) { 
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
      var numbers = list.cons(1,
                              list.cons(2,
                                        list.cons(3,
                                                  list.empty())));
      expect(
        list.find(numbers)(even) // 最初に登場する偶数の要素を探す
      ).to.eql(
        2
      );
      next();
    });
    it("foldrでall関数を作る", (next) => {
      /* #@range_begin(foldr_all) */
      var all = (alist) => {
        return foldr(alist)(true)((item) => {
          return (accumulator) => {
            return accumulator && truthy(item);
          };
        });
      };
      var seq = list.cons(true,list.cons(1,list.cons(null,list.cons("a",list.empty()))));
      expect(
        all(seq)
      ).to.eql(
        false
      );
      /* #@range_end(foldr_all) */
      next();
    });
    it("foldrでtoArray関数を作る", (next) => {
      /* #@range_begin(foldr_toarray) */
      var toArray = (alist) => {
        return foldr(alist)([])((item) => {
          return (accumulator) => {
            return [item].concat(accumulator);
          };
        });
      };
      var seq = list.cons(true,list.cons(1,list.cons(null,list.cons("a",list.empty()))));
      expect(
        toArray(seq)
      ).to.eql(
        [ true, 1, null, 'a' ]
      );
      /* #@range_end(foldr_toarray) */
      next();
    });
    it("foldrで map関数を作る", (next) => {
      /* #@range_begin(foldr_map) */
      var map = (alist) => {
        return (callback) => { // 個々の要素を変換するコールバック関数
          return foldr(alist)(list.empty())((item) => {
            return (accumulator) => {
              return list.cons(callback(item), accumulator);
            };
          });
        };
      };
      /****** テスト ******/
      /* list = [1,2,3] */
      var seq = list.cons(1,
                          list.cons(2,
                                    list.cons(3,
                                              list.empty())));
      expect(
        list.toArray(map(seq)(double))
      ).to.eql(
        [ 2, 4, 6] // 2 * [1,2,3] = [2,4,6]
      );
      /* #@range_end(foldr_map) */
      next();
    });
    it("Array#reduceで list#fromArray関数を作る", (next) => {
      /* #@range_begin(list_fromArray) */
      var fromArray = (array) => {
        return array.reduce((accumulator, item) => {
          return list.append(accumulator)(list.cons(item, list.empty()));
        }, list.empty());
      };
      /******* テスト *******/
      var theList = fromArray([0,1,2,3]);
      expect(
        list.toArray(theList)
      ).to.eql(
        [0,1,2,3]
      );
      /* #@range_end(list_fromArray) */
      next();
    });
  });
  // ### 継続で未来を渡す
  describe('継続で未来を渡す', () => {
    // #### 継続とは何か
    describe('継続とは何か', () => {
      it("succ関数の継続", (next) => {
        /* #@range_begin(succ_cps) */
        /* continues関数は、succ(n)のあとに続く継続 */
        var succ = (n, continues) => { 
          return continues(n + 1);
        };
        /* #@range_end(succ_cps) */
        /* #@range_begin(succ_cps_test) */
        /* identity関数を継続として渡すことで、
           succ(1)の結果がそのまま返る */
        expect(
          succ(1, identity) 
        ).to.eql(
          2
        );
        /* #@range_end(succ_cps_test) */
        next();
      });
      it("算術の継続", (next) => {
        var identity = (any) => { // 値をそのまま返すだけの継続
          return any;
        };
        /* #@range_begin(continuation_in_arithmetic) */
        /* 継続渡しのsucc関数 */
        var succ = (n, continues) => { 
          return continues(n + 1);
        };
        /* 継続渡しのadd関数 */
        var add = (n,m, continues) => { 
          return continues(n + m);
        };
        /* 継続渡しの succ関数と add関数を使って 
           add(2, succ(3)) を計算する */
        expect(
          succ(3, (succResult) => {
            return add(2, succResult, identity);
          })
        ).to.eql(
          6
        );
        /* #@range_end(continuation_in_arithmetic) */
        next();
      });
      it("継続としての蓄積変数", (next) => {
        /* #@range_begin(accumulator_as_continuation) */
        var succCPS = (n, accumulator) => {
          return n + 1 + accumulator;
        };
        expect(
          succCPS(1, 0)
        ).to.eql(
          2
        );
        var multiplyCPS = (n,m, accumulator) => {
          return n * m * accumulator;
        };
        expect(
          multiplyCPS(succCPS(1, 0), 3, 1) // 蓄積変数を渡す
        ).to.eql(
          6
        );
        expect(
          multiplyCPS(succCPS(1, 0), succCPS(3, 0), 1) // 蓄積変数を渡す
        ).to.eql(
          8
        );
        /* #@range_end(accumulator_as_continuation) */
        next();
      });
      it("クライアントサーバー通信の継続", (next) => {
        
        next();
      });
    });
    describe("継続で未来を選ぶ", () => {
      it("find関数", (next) => {
        /* #@range_begin(list_find) */
        var find = (alist,init, predicate) => {
          var continuesOnRecursion = (alist) => { // 反復処理を続ける継続
            return find(alist, init, predicate);  // find関数を再帰的に呼び出す
          };
          var escapesFromRecursion = identity; // 反復処理を脱出する継続

          return list.match(alist, {
            empty: () => {
              return escapesFromRecursion(init); // 反復処理を抜ける
            },
            cons: (head, tail) => { 
              if(predicate(head) === true) {
                return escapesFromRecursion(head); // escapesFromRecursionで反復処理を脱出する
              } else {
                return continuesOnRecursion(tail); // continuesOnRecursionで次の反復処理を続ける
              };
            }
          });
        };
        /* #@range_end(list_find) */
        /* #@range_begin(list_find_test) */
        var theList = list.fromArray([0,1,2,3]);
        expect(
          find(theList,null, (item) => {
            return (item === 4);
          })
        ).to.eql(
          null
        );
        expect(
          find(theList,null, (item) => {
            return (item === 2);
          })
        ).to.eql(
          2
        );
        /* #@range_end(list_find_test) */
        next();
      }); 
      it("継続渡しfind関数", (next) => {
        /* #@range_begin(stream_find_cps) */
        var find = (aStream,
                    predicate, 
                    continuesOnFailure, 
                    continuesOnSuccess) => {
                      return list.match(aStream, {
                        /* リストの最末尾に到着した場合
                           成功継続で反復処理を抜ける */
                        empty: () => {
                          return continuesOnSuccess(null); 
                        },
                        cons: (head, tailThunk) => { 
                          /* 目的の要素を見つけた場合
                             成功継続で反復処理を脱出する */
                          if(predicate(head) === true) { 
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
        /* #@range_begin(stream_find_continuations) */
        /* 成功継続では、反復処理を脱出する */
        var continuesOnSuccess = identity; 

        /* 失敗継続では、反復処理を続ける */
        var continuesOnFailure = (aStream,
                                  predicate, 
                                  continuesOnRecursion, 
                                  escapesFromRecursion) => { 
                                    /* find関数を再帰的に呼び出す */
                                    return find( 
                                      aStream, 
                                      predicate, 
                                      continuesOnRecursion, 
                                      escapesFromRecursion
                                    );  
                                  };
        /* #@range_end(stream_find_continuations) */
        // upto3変数は、1から3までの有限ストリーム
        var upto3 = stream.cons(1,(_) => {
          return stream.cons(2, (_) => {
            return stream.cons(3, (_) => {
              return stream.empty();
            });
          });
        });
        expect(
          find(upto3, (item) => {
            return (item === 4); // 4を探します
          }, continuesOnFailure, continuesOnSuccess)
        ).to.eql(
          null // リスト中に4の要素はないので、nullになります
        );
        /* #@range_begin(stream_find_cps_test) */
        // integers変数は、無限の整数ストリーム
        var integers = stream.enumFrom(0);
        
        /* 無限の整数列のなかから100を探す */
        expect(
          find(integers, (item) => {
            return (item === 100); 
          }, continuesOnFailure, continuesOnSuccess)
        ).to.eql(
          100 // 100を見つけて返ってくる
        );
        /* #@range_end(stream_find_cps_test) */
        next();
      }); 
      it("継続による反復処理", (next) => {
        /* #@range_begin(loop_cps) */
        var loop = (predicate, accumulator) => {
          return (continues) => {
            if(predicate(accumulator)){
              return loop(predicate, continues(accumulator))(continues);
            } else {
              return accumulator;
            }
          };
        };
        var lessThan = (n) => {
          return (x) => {
            return x < n;
          };
        };
        var succ = (n) => {
          return n + 1;
        };
        expect(
          loop(lessThan(3), 0)(succ)
        ).to.eql(
          3
        );
        /* #@range_end(loop_cps) */
        next();
      }); 
    }); 
    // #### 非決定計算機を作る
    describe("非決定計算機を作る", () => {
      var exp = {
        match : (anExp, pattern) => {
          return anExp.call(exp, pattern);
        },
        /* #@range_begin(amb_expression) */
        amb : (alist) => {
          return (pattern) => {
            return pattern.amb(alist);
          };
        },
        /* #@range_end(amb_expression) */
        num : (n) => {
          return (pattern) => {
            return pattern.num(n);
          };
        },
        add : (exp1, exp2) => {
          return (pattern) => {
            return pattern.add(exp1, exp2);
          };
        }
      };
      /* #@range_begin(amb_calculate) */
      // <section id='amb_calculate'>非決定性計算機の評価関数</section>
      var calculate = (anExp, 
                       continuesOnSuccess, 
                       continuesOnFailure) => {
                         /* 式に対してパターンマッチを実行する */
                         return exp.match(anExp, { 
                           /* #@range_begin(amb_calculate_num) */
                           /* 数値を評価する */
                           num: (n) => {
                             return continuesOnSuccess(n, continuesOnFailure);
                           },
                           /* #@range_end(amb_calculate_num) */
                           /* #@range_begin(amb_calculate_add) */
                           /* 足し算の式を評価する */
                           add: (x, y) => {
                             /* まず引数xを評価する */
                             return calculate(x,(resultX, continuesOnFailureX) => { 
                               /* 次に引数yを評価する */
                               return calculate(y, (resultY, continuesOnFailureY) => { 
                                 /* 引数xとyがともに成功すれば、両者の値で足し算を計算する */
                                 return continuesOnSuccess(resultX + resultY, continuesOnFailureY); 
                               }, continuesOnFailureX); /* y の計算に失敗すれば、xの失敗継続を渡す */
                             }, continuesOnFailure);    /* x の計算に失敗すれば、おおもとの失敗継続を渡す */
                           },
                           /* #@range_end(amb_calculate_add) */
                           /* #@range_begin(amb_calculate_amb) */
                           /* amb式を評価する */
                           amb: (choices) => {
                              var calculateAmb = (choices) => {
                              // var calculateAmb = (choices, continuesOnSuccess, continuesOnFailure) => {
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
                                 cons: (head, tail) => { 
                                   return calculate(head, continuesOnSuccess, (_) => { 
                                     /* 失敗継続で後尾を計算する */
                                     return calculateAmb(tail);
                                     // return calculateAmb(tail, continuesOnSuccess, continuesOnFailure);
                                   });
                                 }
                               });
                             };
                             return calculateAmb(choices);
                             // return calculateAmb(choices, continuesOnSuccess, continuesOnFailure);
                           }
                           /* #@range_end(amb_calculate_amb) */
                         });
                       };
      /* #@range_end(amb_calculate) */
      /* #@range_begin(amb_driver) */
      var driver = (expression) =>{
        /* 中断された計算を継続として保存する変数 */
        var suspendedComputation = null; 
        /* 成功継続 */
        var continuesOnSuccess = (anyValue, 
                                  continuesOnFailure) => {
                                    /* 再開に備えて、失敗継続を保存しておく */
                                    suspendedComputation = continuesOnFailure; 
                                    return anyValue;
                                  };
        /* 失敗継続 */
        var continuesOnFailure = () => {
          return null;
        };

        /* 内部に可変な状態suspendedComputationを持つクロージャーを返す */
        return () => {
          /* 中断された継続がなければ、最初から計算する */
          if(suspendedComputation === null) { 
            return calculate(expression, 
                             continuesOnSuccess, 
                             continuesOnFailure);
          } else { /* 中断された継続があれば、その継続を実行する */
            return suspendedComputation();
          }
        };
      };
      /* #@range_end(amb_driver) */
      it("amb[1,2] + 3  = amb[4, 5]", (next) => {
        var ambExp = exp.add(exp.amb(list.cons(exp.num(1),list.cons(exp.num(2), list.empty()))), 
                             exp.num(3));
        var calculator = driver(ambExp);
        expect(
          calculator()
        ).to.eql(
          4 // 1 + 3 = 4
        );
        expect(
          calculator()
        ).to.eql(
          5 // 2 + 3 = 5 
        );
        expect(
          calculator()
        ).to.eql(
          null
        );
        next();
      });
      it("amb[1,2] + amb[3,4] = amb[4, 5, 5, 6]", (next) => {
        /* #@range_begin(amb_test) */
        /* amb[1,2] + amb[3,4] = 4, 5, 5, 6 */
        var ambExp = exp.add(
          exp.amb(list.fromArray([exp.num(1),exp.num(2)])),
          exp.amb(list.fromArray([exp.num(3),exp.num(4)])));
        var calculator = driver(ambExp);
        expect(
          calculator()
        ).to.eql(
          4 // 1 + 3 = 4
        );
        expect(
          calculator()
        ).to.eql(
          5 // 2 + 3 = 5
        );
        expect(
          calculator()
        ).to.eql(
          5 // 1 + 4 = 5
        );
        expect(
          calculator()
        ).to.eql(
          6 // 2 + 4 = 6
        );
        expect(
          calculator()
        ).to.eql(
          null // これ以上の候補はないので、計算は終了
        );
        /* #@range_end(amb_test) */
        next();
      });
      it("amb[1,2,3] + amb[10,20] = amb[11,21,12,22,13,23]", (next) => {
        var ambExp = exp.add(
          exp.amb(list.fromArray([exp.num(1),exp.num(2),exp.num(3)])),
          exp.amb(list.fromArray([exp.num(10),exp.num(20)])));
        var calculator = driver(ambExp);
        expect(
          calculator()
        ).to.eql(
          11 // 1 + 10 = 11
        );
        expect(
          calculator()
        ).to.eql(
          21 // 1 + 20 = 21
        );
        expect(
          calculator()
        ).to.eql(
          12 // 2 + 10 = 12
        );
        expect(
          calculator()
        ).to.eql(
          22 // 2 + 20 = 22
        );
        expect(
          calculator()
        ).to.eql(
          13 // 3 + 10 = 13
        );
        expect(
          calculator()
        ).to.eql(
          23 // 3 + 20 = 23
        );
        expect(
          calculator()
        ).to.eql(
          null // これ以上の候補はないので、計算は終了
        );
        next();
      });
      it("amb[1,2] + amb[10,20,30] = amb[11,21,31,12,22,32]", (next) => {
        var ambExp = exp.add(
          exp.amb(list.fromArray([exp.num(1),exp.num(2)])),
          exp.amb(list.fromArray([exp.num(10),exp.num(20),exp.num(30)])));
        var calculator = driver(ambExp);
        expect(
          calculator()
        ).to.eql(
          11 // 1 + 10 = 11
        );
        expect(
          calculator()
        ).to.eql(
          21 // 1 + 20 = 21
        );
        expect(
          calculator()
        ).to.eql(
          31 // 1 + 30 = 31
        );
        expect(
          calculator()
        ).to.eql(
          12 // 2 + 10 = 12
        );
        expect(
          calculator()
        ).to.eql(
          22 // 2 + 20 = 22
        );
        expect(
          calculator()
        ).to.eql(
          32 // 2 + 30 = 32
        );
        expect(
          calculator()
        ).to.eql(
          null // これ以上の候補はないので、計算は終了
        );
        next();
      });
    }); 
  }); // 継続を渡す
}); // 関数を渡す

// ## モナドを作る
describe('モナドを作る', () => {
  describe('モナドの原理', () => {
    it("Kleisli合成", (next) => {
      var flatMap = (instanceM) => {
        return (transform) => {
          return transform(instanceM); 
        };
      };
      var compose = (f, g) => {
        return (x) => {
          return flatMap(f(x))(g);
        };
      };
      var f = (n) => {
        return n + 1;
      };
      var g = (n) => {
        return 2 * n;
      };
      var h = (n) => {
        return - n;
      };
      expect(
        compose(compose(f,g),h)(2)
      ).to.eql(
        compose(f,compose(g,h))(2)
      );
      next();
    });
  }); 
  // ### 恒等モナド
  describe('恒等モナド', () => {
    var ID = {
    /* #@range_begin(identity_monad) */
      /* unit:: T => ID[T] */
      unit: (value) => {  // 単なる identity関数と同じ
        return value;
      },
      /* flatMap:: ID[T] => FUN[T => ID[T]] => ID[T] */
      flatMap: (instanceM) => {
        return (transform) => {
          return transform(instanceM); // 単なる関数適用と同じ
        };
      },
    /* #@range_end(identity_monad) */
      compose: (f, g) => {
        return (x) => {
          return ID.flatMap(f(x))(g);
        };
      }
    };
    it("恒等モナドのunit関数", (next) => {
      /* #@range_begin(identity_monad_unit_test) */
      expect(
        ID.unit(1)
      ).to.eql(
        1
      );
      /* #@range_end(identity_monad_unit_test) */
      next();
    });
    it("恒等モナドのflatMap関数", (next) => {
      var succ = (n) => {
        return n + 1;
      };
      /* #@range_begin(identity_monad_flatMap_test) */
      expect(
        ID.flatMap(ID.unit(1))((one) => {    
          return ID.unit(succ(one));
        })
      ).to.eql(
        succ(1)
      );
      /* #@range_end(identity_monad_flatMap_test) */
      var succ = (n) => {
        return n + 1;
      };
      var double = (m) => {
        return m * 2;
      };
      /* #@range_begin(flatMap_and_composition) */
      expect(
        ID.flatMap(ID.unit(1))((one) => {    
          /* succ関数を適用する */
          return ID.flatMap(ID.unit(succ(one)))((two) => { 
            /* double関数を適用する */
            return ID.unit(double(two));  
          });
        })
      ).to.eql(
        compose(double,succ)(1)
      );
      /* #@range_end(flatMap_and_composition) */
      next();
    });
    describe("恒等モナドのモナド則", () => {
      /* #@range_begin(identity_monad_laws) */
      it("flatMap(instanceM)(unit) === instanceM", (next) => {
        /* flatMap(instanceM)(unit) === instanceM の一例 */
        var instanceM = ID.unit(1);
        /* #@range_begin(identity_monad_laws_right_unit_law) */
        expect(
          ID.flatMap(instanceM)(ID.unit)
        ).to.eql(
          instanceM
        );
        /* #@range_end(identity_monad_laws_right_unit_law) */
        next();
      });
      it("flatMap(unit(value))(f) == f(value)", (next) => {
        /* flatMap(unit(value))(f) === f(value) */
        var f = (n) => {
          return ID.unit(n + 1);
        };
        /* #@range_begin(identity_monad_laws_left_unit_law) */
        expect(
          ID.flatMap(ID.unit(1))(f)
        ).to.eql(
          f(1)
        );
        /* #@range_end(identity_monad_laws_left_unit_law) */
        next();
      });
      it("flatMap(flatMap(instanceM)(f))(g) == flatMap(instanceM)((x) => flatMap(f(x))(g))", (next) => {
        /* 
           flatMap(flatMap(instanceM)(f))(g) 
           === 
           flatMap(instanceM)((x) => { 
              return flatMap(f(x))(g); } 
           } 
        */
        var f = (n) => {
          return ID.unit(n + 1);
        };
        var g = (n) => {
          return ID.unit(- n);
        };
        var instanceM = ID.unit(1);
        /* #@range_begin(identity_monad_laws_associative_law) */
        expect(
          ID.flatMap(ID.flatMap(instanceM)(f))(g)
        ).to.eql(
          ID.flatMap(instanceM)((x) => {
            return ID.flatMap(f(x))(g);
          })
        );
        /* #@range_end(identity_monad_laws_associative_law) */
        /* #@range_end(identity_monad_laws) */
        next();
      });
      it("恒等モナドのKleisli合成オペレータ", (next) => {
        /* #@range_begin(identity_monad_kleisli) */
        var f = (n) => {
          return n + 1;
        };
        var g = (n) => {
          return 2 * n;
        };
        var h = (n) => {
          return - n;
        };
        expect(
          ID.compose(ID.compose(f,g),h)(2)
        ).to.eql(
          ID.compose(f,ID.compose(g,h))(2)
        );
        /* #@range_end(identity_monad_kleisli) */
        next();
      });
    });
    it("identity#flatMap", (next) => {
      /* #@range_begin(identity_monad_test) */
      var instance = ID.unit(1);
      expect(
        ID.flatMap(instance)((n) => {
          return ID.unit(n * 2);
        })
      ).to.eql(
        ID.unit(2)
      );
      expect(
        ID.flatMap(instance)((n) => {
          return ID.flatMap(ID.unit(n * 2))((m) => {
            return ID.unit(m * 3);
          });
        })
      ).to.eql(
        ID.unit(6)
      );
      expect(
        ID.flatMap(instance)((n) => {
          return ID.flatMap(ID.unit(n))((m) => {
            return ID.unit(m * n);
          });
        })
      ).to.eql(
        ID.unit(1)
      );
      /* #@range_end(identity_monad_test) */
      next();
    });
  });
  // ### Maybeモナド
  describe('Maybeモナドを作る', () => {
    var id = (any) => {
      return any;
    };
    // var match = (exp, pattern) => {
    //   return exp.call(pattern, pattern);
    // };
    describe('Maybeモナドを作る', () => {
      /* #@range_begin(algebraic_type_maybe) */
      var maybe = {
        match: (exp, pattern) => {
          return exp.call(pattern, pattern);
        },
        just: (value) => {
          return (pattern) => {
            return pattern.just(value);
          };
        },
        nothing: (_) => {
          return (pattern) => {
            return pattern.nothing(_);
          };
        }
      };
      /* #@range_end(algebraic_type_maybe) */
      var MAYBE = {
        /* #@range_begin(maybe_monad) */
        /* unit:: T => MAYBE[T] */
        unit: (value) => {
          return maybe.just(value);
        },
        /* flatMap:: MAYBE[T] => FUN[T => MAYBE[U]] => MAYBE[U] */
        flatMap: (instanceM) => {
          return (transform) => {
            return maybe.match(instanceM,{
              /* 正常な値の場合は、transform関数を計算する */
              just: (value) => { 
                return transform(value);
              },
              /* エラーの場合は、何もしない */
              nothing: (_) => { 
                return maybe.nothing();
              }
            });
          };
        },
        /* ヘルパー関数  */
        getOrElse: (instanceM) => {
          return (alternate) => {
            return maybe.match(instanceM,{
              just: (value) => {
                return value;
              },
              nothing: (_) => {
                return alternate;
              }
            });
          };
        },
        /* #@range_end(maybe_monad) */
        /* #@range_begin(maybe_monad_helper) */
        isEqual: (maybeA,maybeB) => {
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
        },
        map: (maybeInstance) => {
          return (transform) => {
            return maybe.match(maybeInstance,{
              just: (value) => {
                return MAYBE.unit(transform(value));
              },
              nothing: (_) => {
                return maybe.nothing(_);
              }
            });
          };
        }
      };
      /* #@range_end(maybe_monad_helper) */
      it("map id == id", (next) => {
        /* #@range_begin(maybe_monad_test) */
        var justOne = maybe.just(1);
        expect(
          MAYBE.isEqual(MAYBE.map(justOne)(id),
                        id(justOne))
        ).to.be(
          true
        );
        expect(
          MAYBE.isEqual(MAYBE.map(maybe.nothing())(id),
                        id(maybe.nothing()))
        ).to.be(
          true
        );
        /* #@range_end(maybe_monad_test) */
        next();
      });
      it("add(maybe, maybe)", (next) => {
        /* #@range_begin(maybe_monad_add_test) */
        /* 足し算を定義する */
        var add = (maybeA,maybeB) => {
          return MAYBE.flatMap(maybeA)((a) => {
            return MAYBE.flatMap(maybeB)((b) => {
              return MAYBE.unit(a + b);
            });
          });
        };
        var justOne = maybe.just(1);
        var justTwo = maybe.just(2);

        expect(
          MAYBE.getOrElse(add(justOne,justOne))(null) 
        ).to.eql(
          2
        );
        expect(
          MAYBE.getOrElse(add(justOne,maybe.nothing()))(null)
        ).to.eql(
          null
        );
        /* #@range_end(maybe_monad_add_test) */
        next();
      });
      /*
        it("list(maybe)", (next) => {
        var add = (maybeA,maybeB) => {
        return maybe.flatMap(maybeA)((a) => {
        return maybe.flatMap(maybeB)((b) => {
        return maybe.unit(a + b);
        });
        });
        };
        var sum = (alist) => {
        var sumHelper = (alist, accumulator) => {
        return match(list,{
        empty: () => {
        return accumulator;
        },
        cons: (head, tail) => {
        return maybe.flatMap(accumulator)((accumulatorValue) => {
        return maybe.flatMap(head)((headValue) => {
        return sumHelper(tail, maybe.unit(accumulatorValue + headValue));
        });
        });
        }
        });
        };
        return sumHelper(alist, li
        };
        var justOne = just(1);
        var justTwo = just(2);
        var justThree = just(3);
        expect(
        maybe.isEqual(add(justOne,justTwo))(justThree)
        ).to.eql(
        true
        );
        next();
        });
      */                          
    });
  });
  // ### IOモナド
  describe('IOモナド', () => {
    /* #@range_begin(pair_datatype) */
    var pair = {
      /* pair のデータ構造 */
      cons: (left, right) => {
        return (pattern) => {
          return pattern.cons(left, right);
        };
      },
      /* ペアの右側を取得する */
      right: (tuple) => {
        return match(tuple, {
          cons: (left, right) => {
            return right;
          }
        });
      },
      /* ペアの左側を取得する */
      left: (tuple) => {
        return match(tuple, {
          cons: (left, right) => {
            return left;
          }
        });
      }
    };
    /* #@range_end(pair_datatype) */
    describe('外界を引数に持つIOモナド', () => {
      var IO = {
        /* #@range_begin(io_monad_definition_with_world) */
        /* unit:: T => IO[T] */
        unit: (any) => {
          return (world) =>  {  // worldは現在の外界
            return pair.cons(any, world);
          };
        },
        /* flatMap:: IO[T] => FUN[T => IO[U]] => IO[U] */
        flatMap: (instanceA) => {
          return (actionAB) => { // actionAB:: FUN[T => IO[U]]
            return (world) => {
              var newPair = instanceA(world); // 現在の外界のなかで instanceAのIOアクションを実行する
              return pair.match(newPair,{
                cons: (value, newWorld) => {
                  return actionAB(value)(newWorld); // 新しい外界のなかで、actionAB(value)で作られたIOアクションを実行する
                }
              });
            };
          };
        },
        /* #@range_end(io_monad_definition_with_world) */
        /* #@range_begin(io_monad_definition_with_world_helper_function) */
        /* done:: T => IO[T] */
        done: (any) => {
          return IO.unit();
        },
        /* run:: IO[A] => A */
        run: (instance) => {
          return (world) => {
            var newPair = instance(world); // IOモナドのインスタンス(アクション)を現在の外界に適用する
            return pair.left(newPair);     // 結果だけを返す
          };
        }
        /* #@range_end(io_monad_definition_with_world_helper_function) */
      }; // IO monad
      IO.println = (message) => {
        return (world) => { // IOモナドを返す
          console.log(message);
          return IO.unit(null)(world);
        };
      };
      /* #@range_begin(run_println) */
      /* 初期の外界に null をバインドする */
      var initialWorld = null; 
      expect(
        IO.run(IO.println("我輩は猫である"))(initialWorld)
      ).to.eql(
        null
      );
      /* #@range_end(run_println) */
    });
    describe('外界を引数に持たないIOモナド', () => {
      var fs = require('fs');
      // ## 'IO' monad module
      /* #@range_begin(io_monad_definition) */
      var IO = {
        /* unit:: T => IO[T] */
        unit : (any) => {
          return (_) =>  { // 外界を明示する必要はない
            return any;
          };
        },
        /* flatMap:: IO[T] => FUN[T => IO[U]] => IO[U] */
        flatMap : (instanceA) => {
          return (actionAB) => { // actionAB:: a -> IO[b]
            return IO.unit(IO.run(actionAB(IO.run(instanceA))));
          };
        },
        /* done:: T => IO[T] */
        done : (any) => {
          return IO.unit();
        },
        /* run:: IO[A] => A */
        run : (instance) => {
          return instance();
        },
        /* readFile:: STRING => IO[STRING] */
        readFile : (path) => {
          return (_) => {
            var fs = require('fs');
            var content = fs.readFileSync(path, 'utf8');
            return IO.unit(content)();
          };
        },
        /* println:: STRING => IO[null] */
        println : (message) => {
          return (_) => {
            console.log(message);
            return IO.unit(null)();
          };
        },
        writeFile : (path) => {
          return (content) => {
            return (_) => {
              var fs = require('fs');
              fs.writeFileSync(path,content);
              return IO.unit(null)();
            };
          };
        }
      }; // IO monad
      /* #@range_end(io_monad_definition) */
      it('IO.println', (next) => {
        /* #@range_begin(run_println_without_world) */
        expect(
          /* 外界を指定する必要はありません */
          IO.run(IO.println("名前はまだない")) 
        ).to.eql(
          null
        );
        /* #@range_end(run_println_without_world) */
        next();
      });
      it('IOモナドは合成可能である', (next) => {
        /* #@range_begin(io_monad_is_composable) */
        /* IO.seq:: IO[a] => IO[b] => IO[b] */
        IO.seq = (instanceA) => {
          return (instanceB) => {
            return IO.flatMap(instanceA)((a) => {
              return instanceB;
            });
          };
        };
        IO.seqs = (alist) => {
          return list.foldr(alist)(list.empty())(IO.done());
        };
        /* IO.putc:: CHAR => IO[] */
        IO.putc = (character) => {
          return (io) => {
            process.stdout.write(character);
            return null;
          };
        };
        /* IO.puts:: LIST[CHAR] => IO[] */
        // ~~~haskell
        // puts list = seqs (map putc list)
        // ~~~
        IO.puts = (alist) => {
          return match(alist, {
            empty: () => {
              return IO.done();
            },
            cons: (head, tail) => {
              return IO.seq(IO.putc(head))(IO.puts(tail));
            }
          });
        };
        /* IO.getc:: IO[CHAR] */
        IO.getc =  () => {
          var continuation = () => {
            var chunk = process.stdin.read();
            return chunk;
          }; 
          process.stdin.setEncoding('utf8');
          return process.stdin.on('readable', continuation);
        };
        /* #@range_end(io_monad_is_composable) */
        
        next();
      });
      it('IOモナドで参照透過性を確保する', (next) => {
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
    }); // IOモナド
  }); // IOモナド
}); // モナド
