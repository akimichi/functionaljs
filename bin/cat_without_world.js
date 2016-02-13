"use strict";

/*
 $ node --harmony bin/cat.js test/resources/dream.txt

*/

var pair = require('./pair');
// var list = require('./list');

var string = {
  head: (str) => {
    return str[0];
  },
  tail: (str) => {
    return str.substring(1);
  },
  isEmpty: (str) => {
    return str.length === 0;
  },
  toArray: (str) => {
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
        return list.append(xs,xss);
      }
    });
    // return list.foldr(list_of_list)(list.empty())(list.append);
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
          return list.cons(transform(head),list.map(tail)(transform));
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
            return list.cons(head,list.filter(tail)(predicate));
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
    return array.reduce((accumulator, item) => {
      return list.append(accumulator)(list.cons(item, list.empty()));
    }, list.empty());
  },
  fromString: (str) => {
    if(string.isEmpty(str)) {
      return list.empty();
    } else {
      return list.cons(string.head(str), list.fromString(string.tail(str)));
    }
  },
};

/* #@range_begin(io_monad_definition) */
var IO = {
  // unit:: T => IO[T]
  unit: (any) => {
    return (_) =>  { // 外界を指定しない
      return any;  // 値だけを返す
    };
  },
  // flatMap:: IO[T] => FUN[T => IO[S]] => IO[S]
  flatMap: (instanceA) => {
    return (actionAB) => { // actionAB:: a -> IO b
      return actionAB(IO.run(instanceA)); // instanceAのIOアクションを実行し、続いて actionABを実行する
    };
  },
  // done:: T => IO[T]
  done: (any) => {
    return IO.unit();
  },
  // run:: IO[A] => A
  run: (instance) => {
    return instance();
  },
  // readFile:: STRING => IO[STRING]
  readFile: (path) => {
    var fs = require('fs');
    return IO.unit(fs.readFileSync(path, 'utf8'));
  },
  // println:: STRING => IO[]
  println: (message) => {
    console.log(message);
    return IO.unit();
  },
  /* #@range_begin(IO_seq) */
  // IO.seq関数は、2つのIOアクションを続けて実行する
  // IO.seq:: IO[T] => IO[U] => IO[V]
  seq: (instanceA) => {
    return (instanceB) => {
      IO.run(instanceA); // 最初のIOアクションである instanceAを実行する
      return IO.unit(IO.run(instanceB)); // 続いて、 instanceB を実行する
    };
  },
  /* #@range_end(IO_seq) */
  /* #@range_begin(IO_putStrLn) */
  // IO.putChar関数は、一文字を出力する 
  // IO.putChar:: CHAR => IO[]
  putChar: (character) => {
    process.stdout.write(character); // 1文字だけ画面に出力する
    return IO.unit(null);
  },
  // IO.putStr関数は、文字のリストを連続して出力する 
  // IO.putStr:: LIST[CHAR] => IO[]
  putStr: (alist) => {
    return list.match(alist, {
      empty: () => {
        return IO.done();
      },
      cons: (head, tail) => {
        return IO.seq(IO.putChar(head))(IO.putStr(tail));
      }
    });
  },
  // IO.putStrLn関数は、文字列を出力し、最後に改行を出力する
  // IO.putStrLn:: LIST[CHAR] => IO[]
  putStrLn:(alist) => {
    return IO.seq(IO.putStr(alist))(IO.putChar("\n"));
  }
  /* #@range_end(IO_putStrLn) */
};
/* #@range_end(io_monad_definition) */

/* 
// #@range_begin(io_monad_combined)
var path = process.argv[2];

var composed_action = IO.flatMap(IO.readFile(path))((content) => { // ファイルを読みこむ 
  return IO.flatMap(IO.println(content))((_) => { // 読み込んだ内容を画面に出力する
    return IO.done(_);
  });
});

IO.run(composed_action); // 合成されたIOアクションを実行する
// #@range_end(io_monad_combined) 
// IO.flatMap(IO.readFile(path))((content) => {
//   return IO.flatMap(IO.println(content))((_) => {
//     return IO.done(_);
//   });
// })(); // 外界を明示的に渡さず、単に関数を実行するだけ
*/

// #@range_begin(run_putStrLn)
var path = process.argv[2];

var cat_action = IO.flatMap(IO.readFile(path))((content) => { // ファイルを読みこむ 
  var string_as_list = list.fromString(content); // 文字列を文字のリストに変換しておく
  return IO.flatMap(IO.putStrLn(string_as_list))((_) => { // putStrLnを実行する 
    return IO.done(_);
  });
});

IO.run(cat_action); // 合成されたIOアクションを実行する
// #@range_end(run_putStrLn)
