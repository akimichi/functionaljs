"use strict";

var expect = require('expect.js');

var match = (data, pattern) => {
  return data.call(pattern, pattern);
};

// 'string' module
// ==============
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
    return match(alist, {
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
  // list#concat
  // concat:: LIST[LIST[T]] -> LIST[T]
  // concat [] = []
  // concat (xs:xss) = append(xs, xss)
  // or,
  // concat xss = foldr xss [] append
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
  // join:: LIST[LIST[T]] -> LIST[T]
  join: (list_of_list) => {
    return list.concat(list_of_list);
  },
  // foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T
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
  // map:: LIST[T] -> FUNC[T -> T] -> LIST[T]
  map: (alist) => {
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
  // ## list.filter
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
  // list#length
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
  /* #@range_begin(list_fromString) */
  fromString: (str) => {
    expect(str).to.a('string');
    if(string.isEmpty(str)) {
      return list.empty();
    } else {
      return list.cons(string.head(str), list.fromString(string.tail(str)));
    }
  },
  /* #@range_end(list_fromString) */
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
  // ## list#drop
  // drop :: List => List
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
};

var pair = {
  // pair の代数的データ構造
  cons: (left, right) => {
    return (pattern) => {
      return pattern.cons(left, right);
    };
  },
  match : (data, pattern) => {
    return data.call(pair, pattern);
  },
  // ペアの右側を取得する
  right: (tuple) => {
    return pair.match(tuple, {
      cons: (left, right) => {
        return right;
      }
    });
  },
  // ペアの左側を取得する
  left: (tuple) => {
    return pair.match(tuple, {
      cons: (left, right) => {
        return left;
      }
    });
  }
};

module.exports = {
  // unit:: a -> IO a
  unit: (any) => {
    return (_) =>  {
      return any;
    };
  },
  // done:: T -> IO T
  done: (any) => {
    return this.unit();
  },
  // flatMap:: IO a -> (a -> IO b) -> IO b
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return this.unit(this.run(actionAB(this.run(instanceA))));
    };
  },
  // run:: IO A -> A
  run: (instance) => {
    return instance();
  },
  // readFile:: STRING => IO[STRING]
  readFile: (path) => {
    return (io) => {
      var fs = require('fs');
      var content = fs.readFileSync(path, 'utf8');
      return content;
    };
  },
  // println:: STRING => IO[null]
  println: (message) => {
    return (io) => {
      console.log(message);
      return null;
    };
  },
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
    return match(alist, {
      empty: () => {
        return IO.done();
      },
      cons: (head, tail) => {
        return IO.seq(IO.putc(head))(IO.puts(tail));
      }
    });
  },
  // IO.getc :: IO[CHAR]
  getc:  () => {
    var continuation = () => {
      var chunk = process.stdin.read();
      return chunk;
    }; 
    process.stdin.setEncoding('utf8');
    return process.stdin.on('readable', continuation);
  }
};
