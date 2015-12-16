"use strict";

var expect = require('expect.js');

var truthy = (any) => {
  return any !== false && any != null;
};

var double = (number) => {
  return number * 2;
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
  // return data(pattern);
};

var id = (any) => {
  return any;
};

// 'string' module
// ==============
/* #@range_begin(string_module) */
var string = {
  head: (str) => {
    expect(str).to.a('string');
    return str[0];
  },
  tail: (str) => {
    expect(str).to.a('string');
    return str.substring(1);
  },
  isEmpty: (str) => {
    return str.length === 0;
  },
  toArray: (str) => {
    expect(str).to.a('string');
    var glue = (item) => {
      return (rest) => {
        return [item].concat(rest);
      };
    };
    if(string.isEmpty(str)) {
      return [];
    } else {
      return [string.head(str)].concat(string.toArray(string.tail(str)));
    }
  }
};
/* #@range_end(string_module) */

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
  expect(
    string.toArray("abc")
  ).to.eql(
    ['a','b','c']
  );
  next();
});

var list  = {
  empty: (_) => {
    return (pattern) => {
      return pattern.empty();
    };
  },
  cons: (value, seq) => {
    return (pattern) => {
      return pattern.cons(value, seq);
    };
  },
  head: (seq) => {
    var self = this;
    return match(seq, {
      empty: (_) => {
        return undefined;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: (seq) => {
    var self = this;
    return match(seq, {
      empty: (_) => {
        return undefined;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  isEmpty: (seq) => {
    var self = this;
    return match(seq, {
      empty: (_) => {
        return true;
      },
      cons: (head, tail) => {
        return false;
      }
    });
  },
  // list#concat
  // concat:: LIST[T] -> LIST[T] -> LIST[T]
  concat: (xs) => {
    var self = this;
    return (ys) => {
      if(list.isEmpty(xs)){
        return ys;
      } else {
        return list.cons(list.head(xs),(list.concat(list.tail(xs))(ys)));
      }
    };
  },
  last: (seq) => {
    var self = this;
    return match(seq, {
      empty: (_) => {
        return undefined;
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
  // join:: LIST[LIST[T]] -> LIST[T]
  join: (list_of_list) => {
    var self = this;
    if(self.isEmpty(list_of_list)){
      return list.empty();
    } else {
      return list.concat(list.head(list_of_list))(list.join(list.tail(list_of_list)));
    }
  },
  // foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T
  foldr: (seq) => {
    return (accumulator) => {
      return (glue) => {
        expect(glue).to.a('function');
        return match(seq,{
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
  // map:: LIST[T] -> FUNC[T -> T] -> LIST[T]
  map: (seq) => {
    var self = this;
    return (transform) => {
      return match(seq,{
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
  reverse: (seq) => {
    var self = this;
    var reverseAux = (seq, accumulator) => {
      return match(seq, {
        empty: (_) => {
          return accumulator;  // 空のリストの場合は終了
        },
        cons: (head, tail) => {
          return reverseAux(tail, list.cons(head, accumulator));
        }
      });
    };
    return reverseAux(seq, list.empty());
  },
  /* #@range_end(list_reverse) */
  // ## list.filter
  /* #@range_begin(list_filter) */
  filter: (seq) => {
    var self = this;
    return (predicate) => {
      expect(predicate).to.a('function');
      var filterAux = (seq, accumulator) => {
        return match(seq,{
          empty: (_) => {
            return accumulator;
          },
          cons: (head,tail) => {
            if(predicate(head) === true){
              return list.concat(list.concat(accumulator)(list.cons(head, list.empty())))(filterAux(tail, accumulator));
            } else  {
              return filterAux(tail, accumulator);
            }
          }
        });
      };
      return filterAux(seq, list.empty());
    };
  },
  // list#length
  length: (seq) => {
    return match(seq,{
      empty: (_) => {
        return 0;
      },
      cons: (head,tail) => {
        return list.foldr(seq)(0)((item) => {
          return (accumulator) => {
            return 1 + accumulator;
          };
        });
      }
    });
  },
  any: (seq) => {
    return (predicate) => {
      expect(predicate).to.a('function');
      return match(seq,{
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
      // return compose(self.list.or.bind(self))(self.flip.bind(self)(self.list.map.bind(self))(predicate))(list);
    };
  },
  /* #@range_end(list_filter) */
  toArray: (seq) => {
    var self = this;
    var toArrayAux = (seq,accumulator) => {
      return match(seq, {
        empty: (_) => {
          return accumulator;  // 空のリストの場合は終了
        },
        cons: (head, tail) => {
          return toArrayAux(tail, accumulator.concat(head));
        }
      });
    };
    return toArrayAux(seq, []);
  },
  // fromArray: (array) => {
  //   return array.reduce((accumulator, item) => {
  //     return list.concat(accumulator)(list.cons(item, list.empty()));
  //   });
  // },
  /* #@range_begin(list_fromString) */
  fromString: (str) => {
    expect(str).to.a('string');
    if(string.isEmpty(str)) {
      return list.empty();
    } else {
      return list.cons(string.head(str), list.fromString(string.tail(str)));
    }
  }
  /* #@range_end(list_fromString) */
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
  // init = reverse . tail . reverse
  var init = (seq) => {
    var self = this;
    return compose(list.reverse,
                   compose(list.tail,list.reverse))(seq);
  };
  var seq = list.cons(1, list.cons(2,list.cons(3,list.empty())));
  expect(
    list.toArray(init(seq))
  ).to.eql(
    [1,2]
  );

  // expect(
  //   list.toArray(list.fromArray([1,2,3]))
  // ).to.eql(
  //   [1,2]
  // );
  expect(
    list.toArray(list.fromString("abc"))
  ).to.eql(
    ['a','b','c']
  );
  next();
});



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
  // head:: STREAM -> MAYBE[STREAM]
  head: (lazyList) => {
    return match(lazyList,{
      empty: (_) => {
        return undefined;
      },
      cons: (value, tailThunk) => {
        return value;
      }
    });
  },
  // tail:: STREAM -> MAYBE[STREAM]
  tail: (lazyList) => {
    return match(lazyList,{
      empty: (_) => {
        return undefined;
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
  // ## stream#map
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
  // ## stream#concat
  concat: (xs) => {
    return (ysThunk) => {
      return match(xs,{
        empty: (_) => {
          return ysThunk();
        },
        cons: (head,tailThunk) => {
          return stream.cons(head,(_) => {
            return stream.concat(tailThunk())(ysThunk);
          });
        }
      });
    };
  },
  // ## stream#flatten
  // flatten :: STREAM[STREAM[T]] => STREAM[T]
  flatten: (lazyList) => {
    return match(lazyList,{
      empty: (_) => {
        return stream.empty();
      },
      cons: (head,tailThunk) => {
        return stream.concat(head)((_) => {
          return stream.flatten(tailThunk());
        });
      }
    });
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
  // ### stream#fromList
  fromArray: (array) => {
    return array.reduce((accumulator, item) => {
      return stream.concat(accumulator)(stream.cons(item, (_) => {
        return stream.empty();
      }));
    });
  },
  // ### stream#take
  // take:: STREAM -> NUMBER -> STREAM
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
  /* #@range_begin(stream_filter) */
  filter: (lazyList) => {
	return (predicate) => {
      expect(predicate).to.a('function');
	  return match(lazyList,{
		empty: (_) => {
          return stream.empty();
		},
		cons: (head,tailThunk) => {
		  if(predicate(head)){
			return stream.cons(head,(_) => {
			  return stream.filter(tailThunk())(predicate);
			});
		  } else {
			return stream.filter(tailThunk())(predicate);
		  }
		}
	  });
	};
  }
  /* #@range_end(stream_filter) */
}; // stream

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
});

describe('高階関数', () => {
  describe('カリー化', () => {
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
    it('カリー化されていない multiplyOf関数', (next) => {
      /* #@range_begin(multiplyOf_uncurried) */
      var multiplyOf = (n,m) => {
        if(m % n === 0) {
          return true;
        } else {
          return false;
        }
      };
      expect(
        multiplyOf(2,4)
      ).to.eql(
        true
      );
      expect(
        multiplyOf(3,4)
      ).to.eql(
        false
      );
      /* #@range_end(multiplyOf_uncurried) */
      next();
    });
    it('カリー化による関数の部品化', (next) => {
      /* #@range_begin(multiplyOf_curried) */
      var multiplyOf = (n) => {
        return (m) => {
          if(m % n === 0) {
            return true;
          } else {
            return false;
          }
        };
      };
      /* #@range_end(multiplyOf_curried) */
      /* #@range_begin(multiplyOf_curried_test) */
      var twoFold = multiplyOf(2);
      var threeFold = multiplyOf(3);
      expect(
        twoFold(2)
      ).to.eql(
        true
      );
      expect(
        threeFold(3)
      ).to.eql(
        true
      );
      /* #@range_end(multiplyOf_curried_test) */
      next();
    });

    describe('通常の関数とカリー化関数の相互変換', () => {
      it('通常の関数をカリー化する', (next) => {
        /* #@range_begin(curry_function_definition) */
        var curry = (fun) => {
          return (x,optionalY) => {
            if(arguments.length > 1){
              return fun(x, optionalY);
              //return fun.call(this, x,optionalY);
            } else {
              return (y) =>  {
                return fun(x, y);
                // return fun.call(this, x,y);
              };
            }
          };
        };
        var add = (x,y) => {
          return x + y;
        };
        expect(
          curry(add)(1)(2)
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
    describe('関数合成のカリー化', () => {
      /* #@range_begin(compose_definition_curried) */
      var compose = (f) => {
        return (g) => {
          return (arg) =>{
            return f(g(arg));
          };
        };
      };
      /* #@range_end(compose_definition_curried) */

      /* #@range_begin(flip) */
      var flip = (fun) => {
        return  (f) => {
          return (g) => {
            return fun(g)(f);
            // return fun.call(this, g)(f);
          };
        };
      };
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
      /* #@range_end(flip) */
      it('カリー化の合成で乗算と否定の合成は成功する', (next) => {
        /* #@range_begin(compose_negate_multiply_successful) */
        var compose = (f) => {
          return (g) => {
            return (arg) =>{
              return f(g(arg));
            };
          };
        };
        var negate = (x) => {
          return - x;
        };
        var add = (x) => {
          return (y) => {
            return x + y;
          };
        };
        var subtract = (x) => {
          return (y) => {
            return x - y;
          };
        };
        expect(
          compose(negate)(add(2))(3)
        ).to.eql(
            -5
        );
        /* #@range_end(compose_negate_multiply_successful) */
        next();
      });
      it('カリー化による関数の合成', (next) => {
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
        /* #@range_begin(compose_and_uncurry) */
        var negate = (x) => {
          return -x;
        };
        var multiply = (x) => {
          return (y) => {
            return x * y;
          };
        };
        expect(
          compose(negate)(uncurry(multiply))(2,3)
        ).to.eql(
            -6
        );
        /* #@range_end(compose_and_uncurry) */
        next();
      });
      it('マイナスのマイナスはプラス', (next) => {
        /* #@range_begin(composition_example_negation_twice) */
        var negate = (n) => {
          return - n;
        };
        expect(
          compose(negate)(negate)(2)
        ).to.eql(
          2
        );
        /* #@range_end(composition_example_negation_twice) */
        next();
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
          compose(increment)(decrement)(5)
        ).to.eql(
          5
        );
        // expect(__.compose.bind(__)(decrement)(increment)(5)).to.be(5);
        // expect(__.compose.bind(__)(increment)(increment)(5)).to.be(7);
        // // (n * 2) + 1
        // expect(__.compose.bind(__)(increment)(double)(5)).to.be(11);
        // // (n + 1) * 2
        // expect(__.compose.bind(__)(double)(increment)(5)).to.be(12);
        next();
      });
      describe("pipe関数による合成", () => {
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
          var last = (seq) => {
            return compose(list.head)(list.reverse)(seq);
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
          var last = (seq) => {
            return pipe(list.reverse)(list.head)(seq);
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
      });
      // it("'compose two argument functions'", function(next) {
      //   var negate = function(x) {
      //     return -x;
      //   };
      //   var multiply = function(x,y){
      //     return x * y;
      //   };
      //   expect((__.compose.bind(__)(negate)(multiply))(2,3)).to.eql(-6);
      //   next();
      // });
      // it("compose several functions", function(next) {
      //   var not = function(x){
      //     return ! x;
      //   };
      //   expect(
      //     __.compose.bind(__)(not)(math.isEqual(3))(3)
      //   ).to.eql(
      //       false
      //   );
      //   // expect(
      //   //   __.compose.bind(__)(__.not)(math.isEqual(3))(3)
      //   // ).to.eql(
      //   //  false
      //   // );
      //   next();
      // });
      it('再帰によるlast', (next) => {
        /* #@range_begin(list_last_recursive) */
        var last = (seq) => {
          return match(seq, {
            empty: (_) => {
              return undefined;
            },
            cons: (head, tail) => {
              return match(tail, {
                empty: (_) => {
                  return head;
                },
                cons: (head, _) => {
                  return last(tail);
                }
              });
            }
          });
        };
        var sequence = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
        expect(
          last(sequence)
        ).to.eql(
          4
        );
        /* #@range_end(list_last_recursive) */
        next();
      });
      it('合成によるlast', (next) => {
        /* #@range_begin(list_last_compose) */
        var last = (seq) => {
          return compose(list.head)(list.reverse)(seq);
        };
        var sequence = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
        expect(
          last(sequence)
        ).to.eql(
          4
        );
        /* #@range_end(list_last_compose) */
        next();
      });
      it('length関数の抽象的な定義', (next) => {
        /* #@range_begin(abstract_length) */
        // length = sum . map(\x -> 1)
        var sum = (seq) => {
          var sumHelper = (seq, accumulator) => {
            return match(seq,{
              empty: (_) => {
                return accumulator;
              },
              cons: (head, tail) => {
                return sumHelper(tail, accumulator + head);
              }
            });
          };
          return sumHelper(seq,0);
        };
        var one = (_) => {
          return 1;
        };
        var length = (seq) => {
          return compose(sum)(flip(list.map)(one))(seq);
        };
        var sequence = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
        expect(
          length(sequence)
        ).to.eql(
          4
        );
        /* #@range_end(abstract_length) */
        next();
      });
      it('init関数の抽象的な定義', (next) => {
        /* #@range_begin(abstract_init) */
        // init = reverse . tail . reverse 
        var init = (seq) => {
		  return compose(list.reverse)(compose(list.tail)(list.reverse))(seq);
        };
        var sequence = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
        expect(
          list.toArray(init(sequence))
        ).to.eql(
          [1,2,3]
        );
        /* #@range_end(abstract_init) */
        next();
      });
    }); // 関数合成のカリー化
    it('リストの逆順を求める', (next) => {
      var seq = {
        match: (data, pattern) => {
          return data(pattern);
        },
        empty: (pattern) => {
          return pattern.empty;
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
            },
          });
        },
        head: (list) => {
          return match(list, {
            empty: undefined, // 空のリストには先頭要素はありません
            cons: (head, tail) => {
              return head;
            },
          });
        },
        tail: (list) => {
          return match(list, {
            empty: undefined,  // 空のリストには末尾要素はありません
            cons: (head, tail) => {
              return tail;
            },
          });
        }
      };
      /* #@range_begin(list_reverse) */
      var reverse = (list) => {
        return (accumulator) => {
          return seq.match(list, {
            empty: accumulator,  // 空のリストの場合は終了
            cons: (head, tail) => {
              return reverse(tail)(seq.cons(head, accumulator))
            },
          });
        };
      };
      // toArray:: LIST -> ARRAY -> ARRAY
      var toArray = (list) => {
        var toArrayAux = (list) => {
          return (accumulator) => {
            return seq.match.call(seq,
                                  list, {
                                    empty: accumulator,  // 空のリストの場合は終了
                                    cons: (head, tail) => {
                                      return toArrayAux(tail)(accumulator.concat(head))
                                    }
                                  });
          };
        };
        return toArrayAux(list)([])
      };
      /**************** テスト ****************/
      expect(
        toArray(reverse(seq.cons(1, seq.cons(2,seq.empty)))(seq.empty))
      ).to.eql(
        [2,1]
      );
      /* #@range_end(list_reverse) */
      next();
    });
  });
  describe('クロージャーで状態をカプセル化する', () => {
    describe('単純なクロージャーの例', (next) => {
      it('counter関数の例', (next) => {
        /* #@range_begin(counter_as_closure) */
        var counter = (init) => {
          var _init = init;
          return (_) => {
            _init = _init + 1;
            return _init;
          };
        };
        var counterFromZero = counter(0);
        expect(
          counterFromZero()
        ).to.eql( 
          1
        );
        expect(
          counterFromZero()
        ).to.eql( 
          2
        );
        /* #@range_end(counter_as_closure) */
        next();
      });
    });
    describe('関数とデータの類似性', (next) => {
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
            empty: undefined, // 空のリストには先頭要素はありません
            cons: (head, tail) => {
              return head;
            },
          });
        };
        var tail = (list) => {
          return match(list, {
            empty: undefined,  // 空のリストには末尾要素はありません
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
        // var list2function = (list) => {
        //    return (any) => {
        //      if(head(list)) {
        //        if(head(list) === any){
        //          return
        //        } else {
        //        }
        //    } else {
        //    }
        // };
        next();
      });
      describe('クロージャーによる「環境」の実装', () => {
        /* #@range_begin(environment_in_closure) */
        // ## 空の環境
        var emptyEnv = (variable) => {
          return undefined;
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
            undefined
          );
          var newEnv = extendEnv('a',1, emptyEnv);
          expect(
            lookupEnv("a", newEnv)
          ).to.be(
            1
          );
          expect(((_) => {
            // 空の辞書を作成する
            var initEnv = emptyEnv;
            // var a = 1 を実行して、辞書を拡張する
            var firstEnv = extendEnv("a", 1, initEnv);
            // var b = 3 を実行して、辞書を拡張する
            var secondEnv = extendEnv("b",3, firstEnv);
            // 辞書から b の値を参照する
            return lookupEnv("b",secondEnv);
          })()).to.eql(
            3
          );
          expect(((_) => {
            // 空の辞書を作成する
            var initEnv = emptyEnv;
            // var x = 1 を実行して、辞書を拡張する
            var xEnv = extendEnv("x", 1, initEnv);
            // var z = 2 を実行して、辞書を拡張する
            var zEnv = extendEnv("z", 2, xEnv);
            // 内部のスコープで var x = 3 を実行して、辞書を拡張する
            var xEnvInner = extendEnv("x",3, zEnv);
            // 内部のスコープで var y = 4 を実行して、辞書を拡張する
            var innerMostEnv = extendEnv("y",4, xEnvInner);
            // 一番内側のスコープを利用して x + y + z を計算する
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
      it('multiplyOf関数は参照透過である', (next) => {
        var multiplyOf = (n) => {
          return (m) => {
            if(m % n === 0) {
              return true;
            } else {
              return false;
            }
          };
        };
        /* #@range_begin(multiplyOf_is_transparent) */
        expect(
          multiplyOf(2)(4)
        ).to.eql(
          multiplyOf(2)(4)
        );
        expect(
          multiplyOf(3)(5)
        ).to.eql(
          multiplyOf(3)(5)
        );
        /* #@range_end(multiplyOf_is_transparent) */
        next();
      });
      it('参照透過でないクロジャーの例', (next) => {
        /* #@range_begin(counter_is_not_transparent) */
        var counter = (init) => {
          var _init = init;
          return (_) => {
            _init = _init + 1;
            return _init;
          };
        };
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
        // チャーチ数 church numeral
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
        var counter = (init) => {
          var _init = init;
          return (_) => {
            _init = _init + 1;
            return _init;
          };
        };
        /* #@range_begin(closure_as_counter) */
        expect(
          one(counter(0))()
        ).to.eql(
          1
        );
        expect(
          two(counter(0))()
        ).to.eql(
          2
        );
        expect(
          add(one)(two)(counter(0))()
        ).to.eql(
          3
        );
        /* #@range_end(closure_as_counter) */
        next();
      });
    });
    describe('不変なデータ型を作る', () => {
      it('不変なオブジェクト型を作る', (next) => {
        // var objects = {
        //   empty: {
        //   },
        //   set: (key,value,obj) => {
        //     expect(obj).to.an('object');
        //     obj[key] = value;
        //     return obj;
        //   },
        //   get: (key,obj) => {
        //     expect(obj).to.an('object');
        //     return obj[key];
        //   },
        //   isEmpty: (obj) => {
        //     expect(obj).to.an('object');
        //     var hasOwnProperty = Object.prototype.hasOwnProperty;
        //     for(var key in obj){
        //       if(hasOwnProperty.call(obj, key))
        //         return false;
        //     }
        //   },
        //   isNotEmpty: (obj) => {
        //     expect(obj).to.an('object');
        //     return ! this.objects.isEmpty(obj);
        //   },
        // };
        /* #@range_begin(immutable_object_type) */
        var objects = {
          empty: (key) => {
            return undefined;
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
            }
          }
        };
        /* #@range_end(immutable_object_type) */
        /* #@range_begin(immutable_object_type_test) */
        expect(
          objects.get("R2D2", objects.set("R2D2", "Star Wars", objects.set("HAL9000","2001: a space odessay",objects.empty)))
        ).to.eql(
          "Star Wars"
        )
        expect(
          objects.get("R2D2", objects.set("HAL9000","2001: a space odessay",objects.empty))
        ).to.eql(
          undefined
        )
        expect(
          objects.get("HAL9000", objects.set.call(objects,"C3PO", "Star Wars", objects.set.call(objects,"R2D2", "Star Wars", objects.set.call(objects,"HAL9000","2001: a space odessay",objects.empty))))
        ).to.eql(
          "2001: a space odessay"
        )
        /* #@range_end(immutable_object_type_test) */
        next();
      });
      it('不変なオブジェクト型を作る(改良版)', (next) => {
        /* #@range_begin(immutable_object_type_improved) */
        var objects = {
          empty: (_) => {
            return undefined;
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
            }
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
            }, self.empty)
          }
        };
        /* #@range_end(immutable_object_type_improved) */
        /* #@range_begin(immutable_object_type_improved_test) */
        expect(
          objects.get("R2D2", objects.set("HAL9000","2001: a space odessay",objects.empty))
        ).to.eql(
          undefined
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
        // (define (cons x y)
        //   (lambda (m) (m x y)))
        // (define (car z)
        //   (z (lambda (p q) p)))
        // (define (cdr z)
        //   (z (lambda (p q) q)))
        /* #@range_begin(immutable_list) */
        var seq = {
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
        var list = seq.cons(1,seq.cons(2,seq.cons(3,seq.empty)));
        expect(
          seq.head(list)
        ).to.eql(
          1
        )
        expect(
          seq.head(seq.tail(list))
        ).to.eql(
          2
        )
        expect(
          seq.at(0,list)
        ).to.eql(
          1
        )
        expect(
          seq.at(1,list)
        ).to.eql(
          2
        )
        expect(
          seq.at(2,list)
        ).to.eql(
          3
        )
        /* #@range_end(immutable_list) */
        next();
      });
      // it('不変な配列型', (next) => {
      //   var arrays = {
      //     empty: [],
      //     cons: (any,array) => {
      //       expect(array).to.an('array');
      //       return [any].concat(array);
      //     },
      //     head: (ary) => {
      //       expect(ary).to.an('array');
      //       return ary[0];
      //     },
      //     tail: (ary) => {
      //       expect(ary).to.an('array');
      //       expect(self.isNonEmpty(ary)).to.be.ok();
      //       return ary.slice(1,ary.length);
      //     },
      //     get: (index,ary) => {
      //       expect(index).to.be.a('number');
      //       expect(ary).to.an('array');
      //       return ary[index];
      //     },
      //     isEmpty: (ary) => {
      //       expect(ary).to.an('array');
      //       return self.equal.call(self,ary.length)(0);
      //     },
      //     isNotEmpty: (ary) => {
      //       expect(ary).to.an('array');
      //       return self.not.call(self,self.arrays.isEmpty(ary));
      //     }
      //   };
      //   expect(
      //     arrays.cons(1,arrays.empty)
      //   ).to.eql(
      //     [1]
      //   )
      //   next();
      // });
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
        // head:: STREAM -> MAYBE[STREAM]
        var head = (lazyList) => {
          return match(lazyList,{
            empty: (_) => {
              return undefined;
            },
            cons: (value, tailThunk) => {
              return value;
            }
          });
        };
        // tail:: STREAM -> MAYBE[STREAM]
        var tail = (lazyList) => {
          return match(lazyList,{
            empty: (_) => {
              return undefined;
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
        var concat = (xs) => {
          return (ysThunk) => {
            return match(xs,{
              empty: (_) => {
                return ysThunk();
              },
              cons: (head,tailThunk) => {
                return cons(head,(_) => {
                  return concat(tailThunk())(ysThunk);
                });
              }
            });
          };
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
        // ### stream#fromList
        var fromArray = (array) => {
          return array.reduce((accumulator, item) => {
            return concat(accumulator)(cons(item, (_) => {
              return empty();
            }));
          });
        };
        // ### stream#take
        // take:: STREAM -> NUMBER -> STREAM
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
          // stream = [1,2]
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
          // ones = [1,1,1,1,...]
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
          // ones = [1,2,3,4,...]
          var intgersFrom = (n) => {
            return cons(n, (_) => {
              return intgersFrom(n + 1);
            });
          };
          it("無限の整数列をテストする", (next) => {
            expect(
              head(intgersFrom(1))
            ).to.eql(
              1
            );
            expect(
              head(tail(intgersFrom(1)))
            ).to.eql(
              2
            );
            expect(
              toArray(take(intgersFrom(1))(10))
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
            var evenIntegers = stream.filter(intgersFrom(1))(even);
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
            var multiplyOf = (n) => {
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
	            if(multiplyOf(k)(n)) {
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
            
            var primes = stream.filter(intgersFrom(1))(isPrime);
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
    describe('クロージャーでジェネレーターを作る', () => {
      /* #@range_begin(generator_in_closure) */
      var generator = (seed) => {
        return (current) => {
          return (stepFunction) => {
            return stream.cons(current(seed),
                               (_) => { 
                                 return generator(stepFunction(seed))(current)(stepFunction) 
                               });
          };
        };
      };
      var id = (any) => { return any; };
      var succ = (n) => { return n + 1; };
      var integers = generator(0)(id)(succ);
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
      // describe('streamとgenerator', () => {
      //   var integers = generator(0)(id)(succ);
      //   var double = (n) => {
      //     return n * 2;
      //   };
      //   var doubles = stream.map.call(stream,
      //                                 integers, double);
      //   expect(
      //     stream.head(integers)
      //   ).to.eql(
      //     0
      //   );

      // });
    });
  }); // クロージャーで状態をカプセル化する
  describe('関数を渡す', () => {
    describe('コールバックを渡す', () => {
      it('直接コールする', (next) => {
        /* #@range_begin(direct_call) */
        var succ = function(n){
          return n + 1;
        };
        var directCall = function(n){
          return succ(n);
        };
        expect(
          directCall(2)
        ).to.eql(
          3
        );
        /* #@range_end(direct_call) */
        next();
      });
      it('コールバックを呼び出す', (next) => {
        /* #@range_begin(call_callback) */
        var succ = (n) => {
          return n + 1;
        };
        var call_callback = (callback) => {
          return (arg) => {
            return callback(arg);
          };
        };
        expect(
          call_callback(succ)(2)
        ).to.eql(
          3
        );
        /* #@range_end(call_callback) */
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
          // var extract = (key) => {
          //   return (object) => {
          //  return object[key];
          //   };
          // };
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
          // var merge = (obj1,obj2) => {
          //   for (var attrname in obj2) { 
          //     obj1[attrname] = 
          //       obj2[attrname]; 
          //   }
          //   return obj1;
          // };
          // var eventSystem = (init) => {
          //   var subscriptions = init;
          //   return (self) => {
          //     return {
          //       subscribe: (eventName, callback) => {
          //         merge(self.subscriptions,{
          //           eventName: eventName,
          //           callback: callback
          //         });
          //       },
          //       trigger: (eventName, arg) => {
          //         eventName.subscriptions[eventName].callback(arg);
          //       }
          //     };
          //   };
          // };
          // var initilizedEventSystem = eventSystem({})(eventSystem);
          /* #@range_begin(event_driven_system) */
          var es = {
            empty: (eventName) => {
              return undefined;
            },
            lookup: (eventName, eventSystem) => {
              return eventSystem(eventName);
            },
            on: (eventName, callback, eventSystem) => {
              return (queryEventName) => {
                if(eventName === queryEventName) {
                  return callback;
                } else {
                  return es.lookup(queryEventName, eventSystem);
                }
              };
            },
            emit: (eventName, arg, eventSystem) => {
              return es.lookup(eventName, eventSystem)(arg);
            }
          };
          /* #@range_end(event_driven_system) */
          /* #@range_begin(event_driven_system_test) */
          // イベント駆動システムを初期化する
          var eventSystem = es.empty; 
          // イベント started を登録する
          eventSystem = es.on("started", (_) => {
            return "event started";
          }, eventSystem);
          // イベント terminated を登録する
          eventSystem = es.on("terminated", (exitCode) => {  
            return "event terminated with " + exitCode;
          }, eventSystem);
          /**** テスト ****/
          // イベント started を生じさせる
          expect(
            es.emit("started", null, eventSystem)
          ).to.eql(
            "event started"
          );
          // イベント terminated を生じさせる
          expect(
            es.emit("terminated",404, eventSystem)
          ).to.eql(
            "event terminated with 404"
          );
          /* #@range_end(event_driven_system_test) */
          expect(
            es.lookup("started", eventSystem)()
          ).to.eql(
            "event started"
          );
          expect(
            es.lookup("terminated", eventSystem)(503)
          ).to.eql(
            "event terminated with 503"
          );
          next();
        });
      });
      describe('リストの再帰関数', () => {
        it('リストの合計', (next) => {
          /* #@range_begin(list_sum) */
          var sum = (seq) => {
            return (accumulator) => {
              return match(seq,{
                empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return sum(tail)(accumulator + head);
                }
              });
            };
          };
          var numberList = list.cons(1, list.cons(2,list.cons(3,list.empty())));
          expect(
            sum(numberList)(0)
          ).to.eql(
            6
          );
          /* #@range_end(list_sum) */
          /* #@range_begin(list_sum_callback) */
          var sumWithCallbak = (seq) => {
            return (accumulator) => {
              return (callback) => {
                return match(seq,{
                  empty: (_) => {
                    return accumulator;
                  },
                  cons: (head, tail) => {
                    return sumWithCallbak(tail)(callback(accumulator)(head))(callback);
                  }
                });
              };
            };
          };
          var add = (n) => {
            return (m) => {
              return n + m;
            };
          };
          expect(
            sumWithCallbak(numberList)(0)(add)
          ).to.eql(
            6
          );
          /* #@range_end(list_sum_callback) */
          next();
        });
        it('リストの長さ', (next) => {
          /* #@range_begin(list_length) */
          var length = (seq) => {
            return (accumulator) => {
              return match(seq,{
                 empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return length(tail)(accumulator + 1);
                }
              });
            };
          };
          var numberList = list.cons(1, list.cons(2,list.cons(3,list.empty())));
          expect(
            length(numberList)(0)
          ).to.eql(
            3
          );
          /* #@range_end(list_length) */
          /* #@range_begin(list_length_callback) */
          var lengthWithCallbak = (seq) => {
            return (accumulator) => {
              return (callback) => {
                return match(seq,{
                  empty: (_) => {
                    return accumulator;
                  },
                  cons: (head, tail) => {
                    return lengthWithCallbak(tail)(callback(accumulator))(callback);
                  }
                });
              };
            };
          };
          var add = (n) => {
            return (m) => {
              return n + m;
            };
          };
          expect(
            lengthWithCallbak(numberList)(0)(add(1))
          ).to.eql(
            3
          );
          /* #@range_end(list_length_callback) */
          next();
        });
        // it('リストの逆転', (next) => {
        //   /* #@range_begin(list_reverse) */
        //   var length = (seq) => {
        //  return (accumulator) => {
        //    return match(seq,{
        //      empty: (_) => {
        //        return accumulator;
        //      },
        //      cons: (head, tail) => {
        //        return length(tail)(accumulator + 1);
        //      }
        //    });
        //  };
        //   };
        //   var numberList = list.cons(1, list.cons(2,list.cons(3,list.empty())));
        //   expect(
        //  length(numberList)(0)
        //   ).to.eql(
        //  3
        //   );
        //   /* #@range_end(list_reverse) */
        //   var lengthWithCallbak = (seq) => {
        //      return (accumulator) => {
        //        return (callback) => {
        //          return match(seq,{
        //            empty: (_) => {
        //              return accumulator;
        //            },
        //          cons: (head, tail) => {
        //            return lengthWithCallbak(tail)(callback(accumulator))(callback);
        //          }
        //          });
        //        };
        //      };
        //   };
        //   var add = (n) => {
        //      return (m) => {
        //        return n + m;
        //      };
        //   };
        //   expect(
        //      lengthWithCallbak(numberList)(0)(add(1))
        //   ).to.eql(
        //      3
        //   );
        //   next();
        // });
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

        // var request = {
        //   login: (user, password) => {
        //  return (pattern) => {
        //    expect(pattern).to.an('object');
        //    return pattern.login(user, password);
        //  };
        //   },
        //   logout: (session) => {
        //  return (pattern) => {
        //    expect(pattern).to.an('object');
        //    return pattern.logout(session);
        //  };
        //   }
        // };
        // var subscribe = (init) => {
        //   var subscriptions = init;
        //   return (request) => {
        //  return subscriptions.concat([request]);
        //   };
        // };
        next();
      });
    });
    describe('畳み込み関数で反復処理を渡す', () => {
      // var seq  = {
      //   match: (data, pattern) => {
      //     var self = this;
      //     return data(pattern);
      //   },
      //   empty: (_) => {
      //     return (pattern) => {
      //       return pattern.empty();
      //     };
      //   },
      //   cons: (value, list) => {
      //     return (pattern) => {
      //       return pattern.cons(value, list);
      //     };
      //   },
      //   head: (list) => {
      //     var self = this;
      //     return self.match(list, {
      //       empty: (_) => {
      //         return undefined;
      //       },
      //       cons: (head, tail) => {
      //         return head;
      //       },
      //     });
      //   },
      //   tail: (list) => {
      //     var self = this;
      //     return self.match(list, {
      //       empty: (_) => {
      //         return undefined;
      //       },
      //       cons: (head, tail) => {
      //         return tail;
      //       },
      //     });
      //   },
      //   isEmpty: (list) => {
      //     var self = this;
      //     return seq.match(list, {
      //       empty: (_) => {
      //         return true;
      //       },
      //       cons: (head, tail) => {
      //         return false;
      //       },
      //     });
      //   },
      // // concat:: LIST[T] -> LIST[T] -> LIST[T]
      // concat: (xs) => {
      //    var self = this;
      //    return (ys) => {
      //      if(self.isEmpty(xs)){
      //        return ys;
      //      } else {
      //        return self.cons(self.head(xs),(self.concat(self.tail(xs))(ys)));
      //      }
      //    };
      // },
      // // concat:: LIST[LIST[T]] -> LIST[T]
      // join: (list_of_list) => {
      //    var self = this;
      //    if(self.isEmpty(list_of_list)){
      //      return self.empty();
      //    } else {
      //      return self.concat(seq.head(list_of_list))(self.join(seq.tail(list_of_list)));
      //    }
      // },
      /* #@range_begin(foldr_toArray) */
      // toArray: (list) => {
      //   var self = this;
      //   return self.foldr(list)([])(function (item) {
      //     return (accumulator) => {
      //       return [item].concat(accumulator);
      //     };
      //   });
      // }
      /* #@range_end(foldr_toArray) */
      // };
      describe('畳み込み関数foldr', () => {
        /* #@range_begin(list_foldr) */
        var foldr = (seq) => {
          return (accumulator) => {
            return (glue) => {
              expect(glue).to.a('function');
              return match(seq,{
                empty: (_) => {
                  return accumulator;
                },
                cons: (head, tail) => {
                  return glue(head)(list.foldr(tail)(accumulator)(glue));
                }
              });
            };
          };
        };
        /* #@range_end(list_foldr) */
        it("foldrでsumを作る", (next) => {
          /* #@range_begin(foldr_sum) */
          var sum = (seq) => {
            return foldr(seq)(0)((item) => {
              return (accumulator) => {
                return accumulator + item;
              };
            });
          };
          // list = [1,2,3,4]
          var seq = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
          expect(
            sum(seq)
          ).to.eql(
            10  // 1 + 2 + 3 + 4 = 10
          );
          /* #@range_end(foldr_sum) */
          next();
        });
        it("foldrでproductを作る", (next) => {
          /* #@range_begin(foldr_product) */
          var product = (seq) => {
            return foldr(seq)(1)((item) => {
              return (accumulator) => {
                return accumulator * item;
              };
            });
          };
          // list = [1,2,3,4]
          var seq = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
          expect(
            product(seq)
          ).to.eql(
            24 // 1 * 2 * 3 * 4 = 24
          );
          /* #@range_end(foldr_product) */
          next();
        });
        it("foldrでlength関数を作る", (next) => {
          /* #@range_begin(foldr_length) */
          var length = (seq) => {
            return foldr(seq)(0)((item) => {
              return (accumulator) => {
                return accumulator + 1;
              };
            });
          };
          // list = [1,2,3,4]
          var seq = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
          expect(
            length(seq)
          ).to.eql(
            4
          );
          /* #@range_end(foldr_length) */
          next();
        });
        it("foldrでall関数を作る", (next) => {
          /* #@range_begin(foldr_all) */
          var all = (seq) => {
            return foldr(seq)(true)((item) => {
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
          var toArray = (seq) => {
            return foldr(seq)([])((item) => {
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
        it("foldrで reverse関数を作る", (next) => {
          /* #@range_begin(foldr_reverse) */
          var reverse = (seq) => {
            return foldr(seq)(list.empty())((item) => {
              return (accumulator) => {
                return list.concat(accumulator)(list.cons(item,list.empty()));
              };
            });
          };
          // list = [1,2,3,4]
          var seq = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
          expect(
            list.toArray(reverse(seq))
          ).to.eql(
            [ 4, 3, 2, 1]
          );
          /* #@range_end(foldr_reverse) */
          next();
        });
        it("foldrで map関数を作る", (next) => {
          /* #@range_begin(foldr_map) */
          var map = (seq) => {
            return (transform) => {
              return foldr(seq)(list.empty())((item) => {
                return (accumulator) => {
                  return list.cons(transform(item), accumulator);
                };
              });
            };
          };
          // list = [1,2,3,4]
          var seq = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
          expect(
            list.toArray(map(seq)(double))
          ).to.eql(
            [ 2, 4, 6, 8]
          );
          /* #@range_end(foldr_map) */
          next();
        });
      });
    }); // 畳み込み関数で反復処理を渡す
    describe('継続を渡す', () => {
      describe("継続の導入例", () => {
        it("算術の継続", (next) => {
		  /* #@range_begin(continuation_in_arithmetic) */
          var succ = (n) => {
            return n + 1;
          };
          var continuation = (any) => { // 値をそのまま返すだけの継続
            return any;
          };
          expect(
            id(succ(1))
          ).to.eql(
            2
          );
          var succCPS = (n, continues) => {
            return continues(n + 1);
          };
          expect(
            succCPS(1, id)
          ).to.eql(
            2
          );
          var multiply = (n,m) => {
            return n * m;
          };
          expect(
            multiply(succ(1), 3)
          ).to.eql(
            6
          );
          var multiplyCPS = (n,m, continues) => {
            return continues(n * m);
          };
          expect(
            multiplyCPS(succCPS(1, continuation), 3,continuation) // 継続を渡す
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
      it("継続による非決定計算", (next) => {
        // (define (eval-amb exp env succeed fail)
        //   (if (null? (cdr exp)) ; (car exp) is the word AMB
        //       (fail) ; no more args, call failure cont.
        //       (eval (cadr exp) ; Otherwise evaluate the first arg
        //         env
        //         succeed ; with my same success continuation
        //         (lambda () ; but with a new failure continuation:
        //           (eval-amb (cons ’amb (cddr exp)) ; try the next argument
        //           env
        //           succeed
        //           fail)))))
        /* #@range_begin(amb) */
        // var match = (exp, pattern) => {
        //   return exp.call(pattern, pattern);
        // };
        var evalAmb = (amb, continues, continuesOnFailure) => {
          var self = this;
          return match.call(self, amb, {
            amb: (seq) => {
              return match.call(list, seq, {
                empty: (_) => {
                  return continuesOnFailure();
                },
                cons: (head, tail) => {
                  var newContinuesOnFailure = (_) => {
                    return calculate(amb(tail), continues, continuesOnFailure);
                  };
                  return calculate(head, continues, newContinuesOnFailure);
                }
              });
            }
          });
        };
        var calculate = (exp, continues, continuesOnFailure) => {
          var self = this;
          return match.call(self, exp, {
            amb: (seq) => {
              return evalAmb(exp, continues, continuesOnFailure);
            },
            num: (x) => {
              return continues(x);
            },
            add: (x, y) => {
              return calculate.call(self, x, continues, continuesOnFailure) 
                + calculate.call(self, y, continues, continuesOnFailure);
            },
            mul: (x, y) => {
              return calculate.call(self, x, continues, continuesOnFailure) * calculate.call(self,y, continues, continuesOnFailure);
            }
          });
        };
            // eq: (exp1, exp2) => {
            //   if(calculate(exp1, continues, continuesOnFailure) === calculate(exp2, continues, continuesOnFailure)){
                
            //   } else {
            //   }
              
            // },
        var amb = (list) => {
          return (pattern) => {
            return pattern.amb(list);
          };
        };
        var num = (n) => {
          return (pattern) => {
            return pattern.num(n);
          };
        };
        var add = (x, y) => {
          return (pattern) => {
            return pattern.add(x, y);
          };
        };
        var mul = (x, y) => {
          return (pattern) => {
            return pattern.mul(x, y);
          };
        };
        var continues = {
	  	  normally: (result) => {
	  	    return result;
	  	  },
          onFailure: (exception) => {
            return exception;
	  	  }
        };
        var exp = add(num(1), mul(num(2), num(3)));
        expect(
          calculate(exp, continues.normally, continues.onFailure)
        ).to.eql(
          7
        );
        var exp = add(num(1), mul(num(2), num(3)));
        expect(
          calculate(exp, continues.normally, continues.onFailure)
        ).to.eql(
          7
        );
        // ambExp = amb[1,2] + 1  = amb[2, 3]
        var ambExp = add(amb(list.cons(num(1),list.cons(num(2), list.empty()))), 
                         num(1));
        expect(
          calculate(ambExp, continues.normally, continues.onFailure)
        ).to.eql(
          2
        );
        // // ambExp = [1,2] + (2 * 3) = [6, 12]
        // var ambExp = add(
        //   amb(
        //     list.cons(num(1),list.cons(num(2), list.empty()))), 
        //   mul(num(2), num(3)));
        // expect(
        //   calculate(ambExp, continues.normally, continues.abnormally)
        // ).to.eql(
        //   7
        // );
        /* #@range_end(amb) */
        next();
      }); 
    }); // 継続を渡す
  }); // 関数を渡す
  describe('コンビネーター', () => {
    it('multiplyOfコンビネータ', (next) => {
      /* #@range_begin(multiplyOf_combinator) */
      var multiplyOf = (n) => {
        return (m) => {
          if(m % n === 0) {
            return true;
          } else {
            return false;
          }
        };
      };
      var even = multiplyOf(2);
      
      expect(
        even(2)
      ).to.eql(
        true
      );
      /* #@range_end(multiplyOf_combinator) */
      next();
    }); 
    it('論理コンビネータ', (next) => {
      var multiplyOf = (n) => {
        return (m) => {
          if(m % n === 0) {
            return true;
          } else {
            return false;
          }
        };
      };
      var even = multiplyOf(2);
      /* #@range_begin(not_combinator) */
      // not :: FUNC[ANY -> BOOL] -> ANY -> BOOL
      var not = (predicate) => { // predicate:: NUMBER->BOOL
        return (arg) => { // ANY->BOOL型の関数を返す
          return ! predicate(arg); // !演算子で論理を反転させる
        };
      };
      var odd = not(even); // notコンビネータでodd関数を定義する

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
      /* #@range_end(not_combinator) */
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
        // is:: FUNC[ANY -> BOOL] -> ANY -> BOOL
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
        // eq:: ANY -> ANY -> BOOL
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
        // multiplyOf:: NUM -> NUM -> BOOL
        var multiplyOf = (n) => {
          expect(n).to.a('number');
          return (m) => {
            expect(n).to.a('number');
            return eq(remainder(m)(n))(0);
          };
        };
        // even:: NUM -> BOOL
        var even = multiplyOf(2);
        // odd:: NUM -> BOOL
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
        // positive:: NUM -> BOOL
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
            if(multiplyOf(k)(n)) {
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
          factors :: Integer -> [Integer]
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
        
        // var cond = (predicate) => {
        //   return (thenClause) => {
        //     return (elseClause) => {
        //       if(truthy(predicate)
        //     };
        //   };
        // };
        // // abs:: NUM -> NUM
        // var abs = (n) => {
        //   expect(n).to.a('number');
        //   return cond(positive)(n)(- n);
        // };
      });
      describe('文字列検証コンビネータ', () => {
        // is:: FUNC[ANY -> BOOL] -> ANY -> BOOL
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
        // eq:: ANY -> ANY -> BOOL
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
          // 文字列をリスト型に変換する
          var stringAsList = list.fromString("abcd");
          // 文字列の長さが6より長いかどうかを判定する
          expect(
            is(greater(6))(list.length(stringAsList))
          ).to.be(
            false
          );
          // 文字列の長さが3より長く6より短いかどうかを判定する
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
          // null and undefined are "empty"
          if (obj == null) return true;
          // Assume if it has a length property with a non-zero value
          // that that property is correct.
          if (obj.length > 0)    return false;
          if (obj.length === 0)  return true;
          // Otherwise, does it have any properties of its own?
          // Note that this doesn't handle
          // toString and valueOf enumeration bugs in IE < 9
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
        // validate: PARSER -> JSON -> PARSERESULT
        var validate = (validator, continues, continuesInFailure) => {
          return (inputObject) => {
            return validator(inputObject, continues, continuesInFailure);
          };
        };
        /* 基本パーサー */
        // succeed:: ANY => LIST => RESULT
        var succeed = (value, continues, continuesInFailure) => {
          return (inputObject) => {
            return continues(result.successful(value, inputObject));
          };
        };
        // fail:: (ANY) => LIST => PARSERESULT
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
    it('Y combinator', function(next) {
	  /* #@range_begin(Y_combinator) */
	  var Y = (F) => {
	    return (function(g) {
		  return function(x) {
		    return F(g(g))(x);
		  };
	    })(function(g) {
		  return function(x) {
		    return F(g(g))(x);
		  };
	    });
	  };
	  /* #@range_end(Y_combinator)  */
	  /* #@range_begin(Y_combinator_test) */
	  var factorial = function(fact) {
	    return function(n) {
		  if (n == 0) {
		    return 1;
		  } else {
		    return n * fact(n - 1);
		  }
	    };
	  };
 	  var fact = Y(factorial);
	  expect(
	    fact(3)
	  ).to.eql(
	    6
	  );
	  // Y x = x(Y x)
	  /* #@range_end(Y_combinator_test) */
	  next();
    });
    
  }); // コンビネータ
  describe('モナドを作る', () => {
    describe('恒等モナド', () => {
      /* #@range_begin(identity_monad) */
      var identity = {
        // ## identity#unit
        unit: (value) => {
          var self = this;
          return value;
        },
        flatMap: (instance) => {
          var self = this;
          return (transform) => {
            expect(transform).to.a('function');
            return transform(instance);
          };
        }
      };
      /* #@range_end(identity_monad) */
      it("identity#flatMap", (next) => {
        /* #@range_begin(identity_monad_test) */
        var instance = identity.unit(1);
        expect(
          identity.flatMap(instance)((n) => {
            return identity.unit(n * 2);
          })
        ).to.eql(
          identity.unit(2)
        );
        expect(
          identity.flatMap(instance)((n) => {
            return identity.flatMap(identity.unit(n * 2))((m) => {
              return identity.unit(m * 3);
            });
          })
        ).to.eql(
          identity.unit(6)
        );
        expect(
          identity.flatMap(instance)((n) => {
            return identity.flatMap(identity.unit(n))((m) => {
              return identity.unit(m * n);
            });
          })
        ).to.eql(
          identity.unit(1)
        );
        /* #@range_end(identity_monad_test) */
        next();
      });
    });
    describe('Maybeモナドを作る', () => {
      var id = (any) => {
        return any;
      };
      var match = (exp, pattern) => {
        return exp.call(pattern, pattern);
      };
      describe('Maybeモナドを作る', () => {
        /* #@range_begin(maybe_monad) */
        var just = (value) => {
          return (pattern) => {
            return pattern.just(value);
          };
        };
        var nothing = (_) => {
          return (pattern) => {
            return pattern.nothing(_);
          };
        };
        var unit = (value) => {
          if(value){
            return just(value);
          } else {
            return nothing(null);
          }
        };
        var isEqual = (maybeA) => {
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
        };
        var map = (maybe) => {
          return (transform) => {
            expect(transform).to.a('function');
            return match(maybe,{
              just: (value) => {
                return unit(transform(value));
              },
              nothing: (_) => {
                return nothing(_);
              }
            });
          };
        };
        var flatMap = (maybe) => {
          return (transform) => {
            expect(transform).to.a('function');
            return match(maybe,{
              just: (value) => {
                return transform(value);
              },
              nothing: (_) => {
                return nothing(_);
              }
            });
          };
        };
        /* #@range_end(maybe_monad) */
        it("map id == id", (next) => {
          /* #@range_begin(maybe_monad_test) */
          var justOne = just(1);
          expect(
            isEqual(map(justOne)(id))(id(justOne))
          ).to.be(
            true
          );
          expect(
            isEqual(map(nothing())(id))(id(nothing()))
          ).to.be(
            true
          );
          /* #@range_end(maybe_monad_test) */
          next();
        });
        it("add(maybe, maybe)", (next) => {
          /* #@range_begin(maybe_monad_add_test) */
          var add = (maybeA,maybeB) => {
            return flatMap(maybeA)((a) => {
              return flatMap(maybeB)((b) => {
                return unit(a + b);
              });
            });
          };
          var justOne = just(1);
          var justTwo = just(2);
          var justThree = just(3);
          expect(
            isEqual(add(justOne,justTwo))(justThree)
          ).to.eql(
            true
          );
          expect(
            isEqual(add(justOne,nothing()))(nothing())
          ).to.eql(
            true
          );
          /* #@range_end(maybe_monad_add_test) */
          next();
        });
      });
    });
    describe('Listモナド', () => {
      var seq  = {
        match: (data, pattern) => {
          var self = this;
          return data(pattern);
        },
        compose: (f,g) => {
          return (arg) => {
            return f(g(arg));
          };
        },
        flip: (fun) => {
          return  (f) => {
            return (g) => {
              return fun(g)(f);
            };
          };
        },
        empty: (_) => {
          return (pattern) => {
            return pattern.empty();
          };
        },
        cons: (value, list) => {
          return (pattern) => {
            return pattern.cons(value, list);
          };
        },
        head: (list) => {
          var self = this;
          return self.match(list, {
            empty: (_) => {
              return undefined;
            },
            cons: (head, tail) => {
              return head;
            },
          });
        },
        tail: (list) => {
          var self = this;
          return self.match(list, {
            empty: (_) => {
              return undefined;
            },
            cons: (head, tail) => {
              return tail;
            },
          });
        },
        isEmpty: (list) => {
          var self = this;
          return seq.match(list, {
            empty: (_) => {
              return true;
            },
            cons: (head, tail) => {
              return false;
            },
          });
        },
        toArray: (list) => {
          var self = this;
          return self.foldr(list)([])(function (item) {
            return (accumulator) => {
              return [item].concat(accumulator);
              // return accumulator.concat(item);
            };
          });
        },
        // concat:: LIST[T] -> LIST[T] -> LIST[T]
        concat: (xs) => {
          var self = this;
          return (ys) => {
            if(self.isEmpty(xs)){
              return ys;
            } else {
              return self.cons(self.head(xs),(self.concat(self.tail(xs))(ys)));
            }
          };
        },
        // concat:: LIST[LIST[T]] -> LIST[T]
        join: (list_of_list) => {
          var self = this;
          if(self.isEmpty(list_of_list)){
            return self.empty();
          } else {
            return self.concat(seq.head(list_of_list))(self.join(seq.tail(list_of_list)));
          }
        },
        // foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T
        foldr: (list) => {
          var self = this;
          return (accumulator) => {
            return (glue) => {
              expect(glue).to.a('function');
              if(self.isEmpty(list)){
                return accumulator;
              } else {
                var item = self.head(list);
                var tail = self.tail(list);
                return glue(item)(self.foldr(tail)(accumulator)(glue));
              }
            };
          };
        },
        /* #@range_begin(list_monad_map) */
        // map:: LIST[T] -> FUNC[T -> T] -> LIST[T]
        map: (list) => {
          var self = this;
          return (transform) => {
            expect(transform).to.a('function');
            return seq.match(list,{
              empty: (_) => {
                return self.empty;
              },
              cons: (x,xs) => {
                return self.cons(transform(x),self.map(xs)(transform));
              }
            });
            // return self.foldr(list)(self.empty())(function (item){
            //    return (accumulator) => {
            //      return seq.match(list,{
            //        empty: (_) => {
            //          return accumulator;
            //        },
            //        cons: (x,xs) => {
            //          return transform();
            //        }
            //      });
            //    };
            // });
            // var glue = self.compose(self.cons.bind(self),transform.bind(self));
            // return self.foldr(list)(self.empty())(glue);
            // var glue = self.compose(self.cons.bind(self),transform.bind(self));
            // return self.foldr(list)(self.empty())(glue);
          };
        },
        /* #@range_end(list_monad_map) */
        /* #@range_begin(list_monad_definition) */
        // ### list#unit
        unit: (value) => {
          var self = this;
          return self.cons(value, seq.empty());
        },
        // ### monad.list#flatMap
        flatMap: (instance) => {
          var self = this;
          return (transform) => {
            expect(transform).to.a('function');
            return self.join(self.map(instance)(transform.bind(self)));
          };
        }
        /* #@range_end(list_monad_definition) */
      }; // end of seq
      it("'list#empty'", (next) => {
        seq.match(seq.empty,{
          empty: (_) => {
            expect(true).ok();
          },
          cons: (x,xs) => {
            expect().fail()
          }
        });
        next();
      })
      it("'list#isEmpty'", (next) => {
        expect(
          seq.isEmpty(seq.empty())
        ).to.eql(
          true
        );
        expect(
          seq.isEmpty(seq.cons(1,seq.empty()))
        ).to.eql(
          false
        );
        next();
      });
      it("'list#cons'", (next) => {
        seq.match(seq.cons(1,seq.empty()),{
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
          seq.head(seq.cons(1,seq.empty()))
        ).to.eql(
          1
        )
        next();
      })
      it("'list#tail'", (next) => {
        expect(
          seq.head(seq.tail(seq.cons(1,seq.cons(2,seq.empty()))))
        ).to.eql(
          2
        )
        next();
      })
      it("'list#concat'", (next) => {
        var list = seq.concat(seq.cons(1,seq.empty()))(seq.cons(2,seq.empty()));
        expect(
          seq.head(list)
        ).to.eql(
          1
        )
        expect(
          seq.head(seq.tail(list))
        ).to.eql(
          2
        )
        expect(
          seq.isEmpty(seq.tail(seq.tail(list)))
        ).to.eql(
          true
        )
        next();
      });
      it("'list#join'", (next) => {
        // list = [[1,2],[3,4]]
        var list_of_list = seq.cons(seq.cons(1,seq.cons(2,seq.empty())),
                                    seq.cons(seq.cons(3,seq.cons(4,seq.empty())),
                                             seq.empty));
        // joined_list = [1,2,3,4]
        var joined_list = seq.join(list_of_list)
        expect(
          seq.head(joined_list)
        ).to.eql(
          1
        )
        expect(
          seq.head(seq.tail(joined_list))
        ).to.eql(
          2
        )
        expect(
          seq.isEmpty(seq.tail(seq.tail(joined_list)))
        ).to.eql(
          false
        )
        next();
      });
      it("'list#foldr'", (next) => {
        // list = [1,2,3,4]
        var list = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty()),seq.empty)))
        expect(
          seq.foldr(list)(0)(function (item){
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
        // list = [1,2,3,4]
        var list = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty()),seq.empty)))
        expect(
          seq.toArray(list)
        ).to.eql(
          [1,2,3,4]
        )
        next();
      })
      it("'list#map'", (next) => {
        // list = [1,2,3,4]
        var list = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty()),seq.empty)))
        expect(
          seq.toArray(seq.map(list)(function (item){
            return item * 2;
          }))
        ).to.eql(
          [2,4,6,8]
        )
        next();
      })
      it("'list#unit'", (next) => {
        // list = [1]
        var list = seq.unit(1);
        expect(
          seq.toArray(list)
        ).to.eql(
          [1]
        )
        next();
      })
      it("'list#flatMap'", (next) => {
        // list = [1,2,3]
        var list = seq.concat(seq.unit(1))(seq.concat(seq.unit(2))(seq.unit(3)))
        expect(
          seq.toArray(seq.flatMap(list)(function (item){
            return seq.concat(seq.unit(item))(seq.unit(- item));
          }))
        ).to.eql(
          [1,-1,2,-2,3,-3]
        );
        next();
      });
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
            return self.maybe.nothing(undefined);
          }
        },
        get: (maybe) => {
          return match(maybe,{
            just: (value) => {
              return value;
            },
            nothing: (_) => {
              return undefined;
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
        // head:: STREAM -> MAYBE[STREAM]
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
        // tail:: STREAM -> MAYBE[STREAM]
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
        // unit:: ANY -> STREAM
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
        // append: (stream1, stream2) => {
        //   // var self = this;
        //   return match(stream1,{
        //     empty: (_) => {
        //       return stream2;
        //     },
        //     cons: (head1,tailThunk1) => {
        //       return match(stream2,{
        //         empty: (_) => {
        //           return stream1;
        //         },
        //         cons: (head2,tailThunk2) => {
        //           return stream.cons(head1,() => {
        //             return stream.append(tailThunk1(),stream2)});
        //         }
        //       });
        //     }
        //   });
        // },
        // ## stream#concat
        concat: (xs) => {
          return (ysThunk) => {
            return match(xs,{
              empty: (_) => {
                return ysThunk();
              },
              cons: (head,tailThunk) => {
                return stream.cons(head,(_) => {
                  return stream.concat(tailThunk())(ysThunk);
                });
              }
            });
          };
        },
        // ## stream#flatten
        // flatten :: STREAM[STREAM[T]] => STREAM[T]
        flatten: (lazyList) => {
          return match(lazyList,{
            empty: (_) => {
              return stream.empty();
            },
            cons: (head,tailThunk) => {
              return stream.concat(head)((_) => {
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
        // lazyList = [1,2]
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
      it("stream#concat", (next) => {
        var xs = stream.cons(1, (_) => {
          return stream.empty();
        });
        var ysThunk = (_) => {
          return stream.cons(2, (_) => {
            return stream.empty();
          });
        };
        var concatenatedStream = stream.concat(xs)(ysThunk);
        expect(
          maybe.get(stream.head(concatenatedStream))
        ).to.eql(
          1
        );
        expect(
          maybe.get(stream.head(maybe.get(stream.tail(concatenatedStream))))
        ).to.eql(
          2
        );
        next();
      });
      it("stream#flatten", (next) => {
        // innerStream = [1,2]
        var innerStream = stream.cons(1, (_) => {
          return stream.cons(2,(_) => {
            return stream.empty();
          });
        });
        // outerStream = [[1,2]]
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
          // lazyList = [1,2]
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
          // nestedStream = [[1,2],[3,4]]
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
    describe('IOモナド', () => {
      var fs = require('fs');
      // ## 'IO' monad module
      /* #@range_begin(io_monad_definition) */
      var IO = {
        unit: (sideeffect) => {
          var self = this;
          return {
            run: sideeffect
          };
        },
        empty: () => {
          var self = this;
          return self.monad.IO.unit(() => {
            // do nothing
          });
        },
        join: (io1, io2) => {
          return self.monad.IO.unit(() => {
            io1.run();
            io2.run();
          });
        },
        // map :: IO[A] => FUN[A => B] => IO[B]
        map: (io) => {
          var self = this;
          return (transform) => { // transform :: A => B
            return self.monad.IO.unit(() => {
              return transform(io.run());
            });
          };
        },
        // flatMap :: IO[A] => FUN[A => IO[B]] => IO[B]
        flatMap: (io) => {
          var self = this;
          return (transform) => {
            return self.monad.IO.unit(() => {
              return transform(io.run()).run();
            });
          };
        },
        // readFile:: STRING -> IO[STRING]
        readFile: (path) => {
          var self = this;
          return self.monad.IO.unit(() => {
            return fs.readFileSync(path, 'utf8');
          });
        },
        // writeFile:: STRING -> IO()
        writeFile: (path, content) => {
          var self = this;
          return self.monad.IO.unit(() => {
            fs.writeFileSync(path, content);
          });
        },
        // print:: STRING -> IO()
        print: (message) => {
          var self = this;
          return self.monad.IO.unit(() => {
            console.log(message);
            return message;
          });
        }
      }; // IO module
      /* #@range_end(io_monad_definition) */
    }); // IOモナド
  }); // モナド
});
