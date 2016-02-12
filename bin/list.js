"use strict";

var expect = require('expect.js');
var string = require('./string');

module.exports = {
  match : (data, pattern) => {
    var self = this;
    return data.call(self, pattern);
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
    var self = this;
    return this.match(alist, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: (alist) => {
    var self = this;
    return this.match(alist, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  isEmpty: (alist) => {
    var self = this;
    return self.match(alist, {
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
      return this.match(xs, {
        empty: (_) => {
          return ys;
        },
        cons: (head, tail) => {
          return this.cons(head, this.append(tail)(ys)); 
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
    return this.match(xss,{
      empty: (_) => {
        return this.empty();
      },
      cons: (xs,xss) => {
        return this.append(xs,xss);
      }
    });
  },
  last: (alist) => {
    var self = this;
    return this.match(alist, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return this.match(tail, {
          empty: (_) => {
            return head;
          },
          cons: (head, _) => {
            return this.last(tail);
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
        return self.match(alist,{
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
  // map:: LIST[T] -> FUNC[T -> T] -> LIST[T]
  map: (alist) => {
    var self = this;
    return (transform) => {
      return this.match(alist,{
        empty: (_) => {
          return this.empty();
        },
        cons: (head,tail) => {
          return this.cons(transform(head),this.map(tail)(transform));
        }
      });
    };
  },
  /* #@range_begin(list_reverse) */
  reverse: (alist) => {
    var self = this;
    var reverseAux = (alist, accumulator) => {
      return this.match(alist, {
        empty: (_) => {
          return accumulator;  // 空のリストの場合は終了
        },
        cons: (head, tail) => {
          return reverseAux(tail, this.cons(head, accumulator));
        }
      });
    };
    return reverseAux(alist, this.empty());
  },
  /* #@range_end(list_reverse) */
  // ## list.filter
  /* #@range_begin(list_filter) */
  filter: (alist) => {
    return (predicate) => {
      return this.match(alist,{
        empty: (_) => {
          return this.empty();
        },
        cons: (head,tail) => {
          if(predicate(head)){
            return this.cons(head,(_) => {
              return this.filter(tail)(predicate);
            });
          } else {
            return this.filter(tail)(predicate);
          }
        }
      });
    };
  },
  // list#length
  length: (alist) => {
    return this.match(alist,{
      empty: (_) => {
        return 0;
      },
      cons: (head,tail) => {
        return this.foldr(alist)(0)((item) => {
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
      return this.match(alist,{
        empty: (_) => {
          return false;
        },
        cons: (head,tail) => {
          if(predicate(head)) {
            return true;
          } else {
            return this.any(tail)(predicate);
          }
        }
      });
      // return compose(self.list.or.bind(self))(self.flip.bind(self)(self.list.map.bind(self))(predicate))(list);
    };
  },
  /* #@range_end(list_filter) */
  toArray: (alist) => {
    var toArrayAux = (alist,accumulator) => {
      return this.match(alist, {
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
      return this.append(accumulator)(this.cons(item, this.empty()));
    }, this.empty());
  },
  /* #@range_begin(list_fromString) */
  fromString: (str) => {
    var self = this;
    expect(str).to.a('string');
    if(this.isEmpty(str)) {
      return this.empty();
    } else {
      return this.cons(string.head(str), this.fromString(this.tail(str)));
    }
  },
  /* #@range_end(list_fromString) */
  at: (alist) => {
    return (index) => {
      expect(index).to.a('number');
      expect(index).to.be.greaterThan(-1);
      if (index === 0) {
        return this.head(alist);
      } else {
          return this.at(this.tail(alist))(index - 1);
      }
    };
  },
  take: (alist) => {
    return (n) => {
      expect(n).to.a('number');
      expect(n).to.be.greaterThan(-1);
      if (n === 0) {
        return this.empty();
      } else {
        return this.cons(this.head)(this.take(this.tail)(n-1));
      }
    };
  },
  // ## list#drop
  // drop :: List => List
  drop: function(list){
    var self = this;
    return (n) => {
      expect(n).to.be.a('number');
      expect(n).to.be.greaterThan(-1);
      if (n === 0)
        return list;
      else {
        if(self.this.isEmpty.bind(self)(list))
          return self.this.empty;
        else {
          var tail = this.tail;
          return self.this.drop.bind(self)(tail)(n-1);
        }
      }
    };
  },
  /* #@range_begin(list_generate) */
  generate: (alist) => {
    var theList = alist;
    return (_) => {
      return this.match(theList,{
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
