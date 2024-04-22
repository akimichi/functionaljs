"use strict";

var expect = require('expect.js');
var Data = require('./data.js');
// var string = require('./string');

// リストモジュール
// =============
// 書籍とは若干異なる実装をしています。


const List = {
  type : (pattern) => {
    return pattern.list();    
  },
  match : (data,pattern) => {
    return pattern(data);    
  },
  empty: (_) => {
    return {
      type: (pattern) => {
        return pattern.list();
      },
      match: (pattern) => {
        return pattern.empty();
      }
    };
  },
  cons: (head, tail) => {
    return {
      type: (pattern) => {
        return pattern.list();
      },
      match: (pattern) => {
        return pattern.cons(head, tail);
      }
    };
  },
  unit: (value) => {
    return List.cons(value, List.empty());
  },
  head: (data) => {
    return data.match({
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: (data) => {
    return data.match({
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  isEmpty: (list) => {
    return list.match({
      empty: (_) => {
        return true;
      },
      cons: (head, tail) => {
        return false;
      }
    });
  },
  isEqual: (xs, ys) => {
    var self = this;
    if(self.isEmpty(xs)) {
        if(self.isEmpty(ys)) {
            return true;
        } else {
            return false;        
        }
    } else {
        if(self.isEmpty(ys)) {
            return false;
        } else {
            return xs.match({
                empty: (_) => {
                    return false;
                },
                cons: (x, xtail) => {
                    return ys.match({
                        empty: (_) => {
                            return false;
                        },
                        cons: (y, ytail) => {
                            if(x === y) {
                                return self.isEqual(xtail, ytail);
                            } else {
                                return false;
                            }
                        }
                    });
                 }
            });
        }
    }
  },
  // ### monad.list#flatten
  // ~~~haskell
  // flatten :: [[a]] -> [a]
  // flatten =  foldr (++) []
  // ~~~
  flatten: (instanceMM) => {
    return instanceMM.match({
      empty: (_) => {
        return List.empty();
      },
      cons: (head,tail) => {
        return List.append(head)(List.flatten(tail));
      }
    });
  },
  // map:: LIST[T] -> FUNC[T -> T] -> LIST[T]
  // map: (alist) => {
  //   var self = this;
  //   return (transform) => {
  //     return this.match(alist,{
  //       empty: (_) => {
  //         return this.empty();
  //       },
  //       cons: (head,tail) => {
  //         return this.cons(transform(head),this.map(tail)(transform));
  //       }
  //     });
  //   };
  // },
  // ### List#map
  map: (instanceM) => {
    return (transform) => {
      // return self.match(instanceM,{
      return instanceM.match({
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
  // ### monad.list#flatMap
  flatMap: (instanceM) => {
    return (transform) => { // FUN[T->LIST[T]]
      return List.flatten(List.map(instanceM)(transform));
    };
  },
  // append:: LIST[T] -> LIST[T] -> LIST[T]
  // append [] ys = ys
  // append (x:xs) ys = x : (xs ++ ys)
  append: (xs) => {
    return (ys) => {
      return xs.match({
        empty: (_) => {
          return ys;
        },
        cons: (head, tail) => {
          return List.cons(head, List.append(tail)(ys)); 
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
    var self = this;
    return xss.match({
      empty: (_) => {
        return this.empty();
      },
      cons: (xs,xss) => {
        return this.append(xs)(xss);
      }
    });
  },
  last: (alist) => {
    var self = this;
    return alist.match({
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return tail.match({
          empty: (_) => {
            return head;
          },
          cons: (head, _) => {
            return self.last(tail);
          }
        });
      }
    });
  },
  // join:: LIST[LIST[T]] -> LIST[T]
  join: (list_of_list) => {
    return this.concat(list_of_list);
  },
  // foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T
  foldr: (alist) => {
    return (accumulator) => {
      return (glue) => {
        expect(glue).to.a('function');
        // return Data.match({
        return alist.match({
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            return glue(head)(List.foldr(tail)(accumulator)(glue));
          }
        });
      };
    };
  },
  any: (alist) => {
    var self = this;
    var or = (alist) => {
      return alist.match({
        empty: (_) => {
          return false;
        },
        cons: (head, tail) => {
          return head || or(tail);
        }
      });
    };
    return (predicate) => {
        return or(List.map(alist)(predicate));
    };
    // return (predicate) => {
    //   expect(predicate).to.a('function');
    //   return alist.match({
    //     empty: (_) => {
    //       return false;
    //     },
    //     cons: (head,tail) => {
    //       if(predicate(head)) {
    //         return true;
    //       } else {
    //         return self.any(tail)(predicate);
    //       }
    //     }
    //   });
    // };
  },
  // elem x xs = any (==x) xs
  elem: (alist) => {
    var isEqual = (x) => {
      return (y) => {
        return x === y;
      };
    };
    return (item) => {
      return List.any(alist)(isEqual(item));
    };
  },
  fromArray: (array) => {
    expect(array).to.an('array');
    return array.reduce((accumulator, item) => {
      return List.append(accumulator)(List.cons(item, List.empty()));
    }, List.empty());
  },
  toArray: (alist) => {
    var toArrayAux = (alist,accumulator) => {
      return alist.match({
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
  // list#length
  length: (alist) => {
    var self = this;
    return self.foldr(alist)(0)((_) => {
      return (accumulator) => {
        return 1 + accumulator;
      };
    });
    // return alist.match({
    //   empty: (_) => {
    //     return 0;
    //   },
    //   cons: (head,tail) => {
    //     return self.foldr(alist)(0)((item) => {
    //       return (accumulator) => {
    //         return 1 + accumulator;
    //       };
    //     });
    //   }
    // });
  },
  // list#filter
  filter: (alist) => {
    return (predicate) => {
      return alist.match({
        empty: (_) => {
          return List.empty();
        },
        cons: (head,tail) => {
          if(predicate(head) === true){
            return List.cons(head, List.filter(tail)(predicate));
          } else {
            return List.filter(tail)(predicate);
          }
        }
      });
    };
  },
  fromString: (str) => {
    expect(str).to.a('string');
    if(str.length === 0) {
      return List.empty();
    } else {
      var head = str[0];
      var tail = str.substring(1);
      return List.cons(head, List.fromString(tail));
    }
  },
};
module.exports = List
