"use strict";

/*
 $ node --harmony bin/cat_with_lineno.js test/resources/dream.txt
*/

var expect = require('expect.js');
var pair = require('./pair');
// var list = require('../lib/list');
// var list = require('./list');
// var string = require('./string');

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
    return list.match(xss,{
      empty: (_) => {
        return list.empty();
      },
      cons: (xs,xss) => {
        return list.append(xs)(xss);
      }
    });
  },
  last: (alist) => {
    return list.match(alist, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return list.match(tail, {
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
        return list.match(alist,{
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
      return list.match(alist,{
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
  /* #@range_begin(list_reverse) */
  reverse: (alist) => {
    var reverseAux = (alist, accumulator) => {
      return list.match(alist, {
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
  // ## list.filter
  /* #@range_begin(list_filter) */
  filter: (alist) => {
    return (predicate) => {
      return list.match(alist,{
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
    return list.match(alist,{
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
      return list.match(alist,{
        empty: (_) => {
          return false;
        },
        cons: (head,tail) => {
          if(predicate(head)) {
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
      return list.match(alist, {
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
  drop: (alist) => {
    return (n) => {
      expect(n).to.be.a('number');
      expect(n).to.be.greaterThan(-1);
      if (n === 0)
        return alist;
      else {
        if(list.isEmpty(alist))
          return list.empty();
        else {
          return list.drop(list.tail(alist))(n-1);
        }
      }
    };
  }
};

// var IO = require('./io_without_world');
var IO = {
  // unit:: a -> IO a
  unit: (any) => {
    return (_) =>  {
      return any;
    };
  },
  // flatMap:: IO a -> (a -> IO b) -> IO b
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return actionAB(IO.run(instanceA)); // instanceAのIOアクションを実行し、続いて actionABを実行する
    };
  },
  /* #@range_begin(io_monad_definition_with_world_helper_function) */
  // done:: T -> IO[]
  done: (any) => {
    return IO.unit();
  },
  // run:: IO[A] -> A
  run: (instance) => {
    return instance();
  },
  // seq:: IO[T] => IO[U] => IO[V]
  seq: (instanceA) => {
    return (instanceB) => {
      IO.run(instanceA);
      return IO.unit(IO.run(instanceB));
      // return IO.flatMap(instanceA)((a) => {
      //   IO.run(instanceA);
      //   return IO.unit(IO.run(instanceB));
      // });
      // return IO.flatMap(instanceA)((a) => {
      //   return instanceB;
      // });
    };
  },
  seqs: (alist) => {
    return list.foldr(alist)(list.empty())(IO.done());
  },
  // IO.putc:: CHAR => IO[]
  putc: (character) => {
    process.stdout.write(character);
    return IO.unit(null);
  },
  // IO.puts:: LIST[CHAR] => IO[]
  // ~~~haskell
  // puts list = seqs (map putc list)
  // ~~~
  // puts: (str) => {
  //   // console.log(str);
  //   var theList = list.fromString(str);
  //   // return list.foldr(list.map(theList)(IO.putc))(IO.done())(IO.putc('\n'));
  //   return list.match(theList, {
  //     empty: () => {
  //       return IO.done();
  //     },
  //     cons: (head, tail) => {
  //       return IO.seq(IO.putc(head))(IO.puts(tail));
  //     }
  //   });
  // },
  puts: (alist) => {
    // return list.foldr(list.map(alist)(IO.putc))(IO.done())(IO.putc('\n'));
    return list.match(alist, {
      empty: () => {
        return IO.putc("\n");
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
  },
  // readFile:: STRING => IO[STRING]
  readFile: (path) => {
    var fs = require('fs');
    return IO.unit(fs.readFileSync(path, 'utf8'));
  },
  // println:: STRING => IO[null]
  println: (message) => {
    console.log(message);
    return IO.unit(null);
  }
};

var path = process.argv[2];

IO.flatMap(IO.readFile(path))((content) => { // contentにはファイルの内容が入っている
  return IO.flatMap(IO.puts(list.fromString(content)))((_) => {
    return IO.done(_);
  });
})();
// IO.flatMap(IO.readFile(path))((content) => { // contentにはファイルの内容が入っている
//   return IO.flatMap(IO.println(content))((_) => {
//     return IO.done(_);
//   });
// })();
