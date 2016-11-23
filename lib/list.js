"use strict";

var expect = require('expect.js');
var Data = require('./data.js');
// var string = require('./string');

// リストモジュール
// =============
// 書籍とは若干異なる実装をしています。



module.exports = {
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
    var self = this;
    return self.cons(value, self.empty());
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
    var self = this;
    return instanceMM.match({
      empty: (_) => {
        return self.empty();
      },
      cons: (head,tail) => {
        return self.append(head)(self.flatten(tail));
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
    var self = this;
    return (transform) => {
      // return self.match(instanceM,{
      return instanceM.match({
        empty: (_) => {
          return self.empty();
        },
        cons: (head,tail) => {
          return self.cons(transform(head),
                           self.map(tail)(transform));
        }
      });
    };
  },
  // ### monad.list#flatMap
  flatMap: (instanceM) => {
    var self = this;
    return (transform) => { // FUN[T->LIST[T]]
      return self.flatten(self.map(instanceM)(transform));
    };
  },
  // append:: LIST[T] -> LIST[T] -> LIST[T]
  // append [] ys = ys
  // append (x:xs) ys = x : (xs ++ ys)
  append: (xs) => {
    var self = this;
    return (ys) => {
      return xs.match({
        empty: (_) => {
          return ys;
        },
        cons: (head, tail) => {
          return self.cons(head, self.append(tail)(ys)); 
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
    var self = this;
    return (accumulator) => {
      return (glue) => {
        expect(glue).to.a('function');
        // return Data.match({
        return alist.match({
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            return glue(head)(self.foldr(tail)(accumulator)(glue));
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
        return or(self.map(alist)(predicate));
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
    var self = this;
    var isEqual = (x) => {
      return (y) => {
        return x === y;
      };
    };
    return (item) => {
      return self.any(alist)(isEqual(item));
    };
  },
  fromArray: (array) => {
    var self = this;
    expect(array).to.an('array');
    return array.reduce((accumulator, item) => {
      return self.append(accumulator)(self.cons(item, self.empty()));
    }, self.empty());
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
    var self = this;
    return (predicate) => {
      return alist.match({
        empty: (_) => {
          return self.empty();
        },
        cons: (head,tail) => {
          if(predicate(head) === true){
            return self.cons(head,self.filter(tail)(predicate));
          } else {
            return self.filter(tail)(predicate);
          }
        }
      });
    };
  },
  fromString: (str) => {
    var self = this;
    expect(str).to.a('string');
    if(str.length === 0) {
      return self.empty();
    } else {
      var head = str[0];
      var tail = str.substring(1);
      return self.cons(head, self.fromString(tail));
    }
  },
};
//   /* #@range_begin(list_reverse) */
//   // foldl:: LIST[T] -> T -> FUNC[T -> LIST] -> T
//   // foldl: (alist) => {
//   //   var self = this;
//   //   return (accumulator) => {
//   //     return (glue) => {
//   //       expect(glue).to.a('function');
//   //       return alist.match({
//   //         empty: (_) => {
//   //           return accumulator;
//   //         },
//   //         cons: (head, tail) => {
//   //           return glue(head)(self.foldr(tail)(accumulator)(glue));
//   //         }
//   //       });
//   //     };
//   //   };
//   // },
//   /* #@range_begin(list_reverse) */
//   reverse: (alist) => {
//     var self = this;
//     var reverseAux = (alist, accumulator) => {
//       return this.match(alist, {
//         empty: (_) => {
//           return accumulator;  // 空のリストの場合は終了
//         },
//         cons: (head, tail) => {
//           return reverseAux(tail, this.cons(head, accumulator));
//         }
//       });
//     };
//     return reverseAux(alist, this.empty());
//   },
//   /* #@range_end(list_reverse) */
//   // list#length
//   length: (alist) => {
//     return this.match(alist,{
//       empty: (_) => {
//         return 0;
//       },
//       cons: (head,tail) => {
//         return this.foldr(alist)(0)((item) => {
//           return (accumulator) => {
//             return 1 + accumulator;
//           };
//         });
//       }
//     });
//   },
//   toArray: (alist) => {
//     var toArrayAux = (alist,accumulator) => {
//       return this.match(alist, {
//         empty: (_) => {
//           return accumulator;  // 空のリストの場合は終了
//         },
//         cons: (head, tail) => {
//           return toArrayAux(tail, accumulator.concat(head));
//         }
//       });
//     };
//     return toArrayAux(alist, []);
//   },
//   fromArray: (array) => {
//     var self = this;
//     expect(array).to.an('array');
//     return array.reduce((accumulator, item) => {
//       return self.append(accumulator)(self.cons(item, self.empty()));
//     }, self.empty());
//   },
//   /* #@range_begin(list_fromString) */
//   fromString: (str) => {
//     var self = this;
//     expect(str).to.a('string');
//     if(string.isEmpty(str)) {
//       return self.empty();
//     } else {
//       return self.cons(string.head(str), self.fromString(string.tail(str)));
//     }
//   },
//   /* #@range_end(list_fromString) */
//   at: (alist) => {
//     return (index) => {
//       expect(index).to.a('number');
//       expect(index).to.be.greaterThan(-1);
//       if (index === 0) {
//         return this.head(alist);
//       } else {
//           return this.at(this.tail(alist))(index - 1);
//       }
//     };
//   },
//   take: (alist) => {
//     return (n) => {
//       expect(n).to.a('number');
//       expect(n).to.be.greaterThan(-1);
//       if (n === 0) {
//         return this.empty();
//       } else {
//         return this.cons(this.head)(this.take(this.tail)(n-1));
//       }
//     };
//   },
//   // ## list#drop
//   // drop :: List => List
//   drop: function(list){
//     var self = this;
//     return (n) => {
//       expect(n).to.be.a('number');
//       expect(n).to.be.greaterThan(-1);
//       if (n === 0)
//         return list;
//       else {
//         if(self.this.isEmpty.bind(self)(list))
//           return self.this.empty;
//         else {
//           var tail = this.tail;
//           return self.this.drop.bind(self)(tail)(n-1);
//         }
//       }
//     };
//   },
//   /* #@range_begin(list_generate) */
//   generate: (alist) => {
//     var theList = alist;
//     return (_) => {
//       return this.match(theList,{
//         empty: (_) => {
//           return null; 
//         },
//         cons: (head,tail) => {
//           theList = tail;
//           return head;
//         }
//       });
//     };
//   },
//   /* #@range_end(list_generate) */
//   // lines: (alist) => {
//   //   var self = this;
//   //   // var alist = self.fromString(astring);
//   //   // self.match(alist,{
//   //   //   empty: (_) => {
//   //   //   },
//   //   //   cons: (head, tail) => {
//   //   //     if(head === "\n") {
//   //   //       return 
//   //   return self.foldr(alist)(self.unit(self.empty()))((item) => {
//   //     return (accumulator) => {
//   //       if(item === "\n") {
//   //         return self.match(alist,{
//   //           empty: () => {
//   //             return accumulator;
//   //           },
//   //           cons: (head, tail) => {
//   //             return self.append(accumulator)(self.lines(tail));
//   //           }
//   //         });
//   //       } else {
//   //         return self.match(accumulator,{
//   //           empty: () => {
//   //             return accumulator;
//   //           },
//   //           cons: (head, tail) => {
//   //             self.append(self.last(accumulator))(self.unit(item))
//   //           }
//   //         });
//   //       }
                     
//   // }
// };
