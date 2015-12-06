(function(exports) {

  "use strict";

  // ストリームでユーザー入力を制御する

  var expect = require('expect.js');
  // var readline = require('readline');
  // var rl = readline.createInterface({
  //   input: process.stdin,
  //   output: process.stdout
  // });
  var self = {
    match: (data, pattern) => {
      // return data(pattern);
      return data.call(pattern, pattern);
    },
    string: {
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
      concat: (str1, str2) => {
        
        
      },
      toArray: (str) => {
        expect(str).to.a('string');
        var glue = (item) => {
          return (rest) => {
            return [item].concat(rest);
          };
        };
        if(self.string.isEmpty(str)) {
          return [];
        } else {
          return [self.string.head(str)].concat(self.string.toArray(self.string.tail(str)));
        }
      }
    },
    stream: {
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
        return self.match(lazyList,{
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
        return self.match(lazyList,{
          empty: (_) => {
            return undefined;
          },
          cons: (head, tailThunk) => {
            return tailThunk();
          }
        });
      },
      isEmpty: (lazyList) => {
        return self.match(lazyList,{
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
          return self.match(lazyList,{
            empty: (_) => {
              return self.stream.empty();
            },
            cons: (head,tailThunk) => {
              return self.stream.cons(transform(head),(_) => {
                return self.stream.map(tailThunk())(transform);
              });
            }
          });
        };
      },
      // ## stream#concat
      concat: (xs) => {
        return (ysThunk) => {
          return self.match(xs,{
            empty: (_) => {
              return ysThunk();
            },
            cons: (head,tailThunk) => {
              return self.stream.cons(head,(_) => {
                return self.stream.concat(tailThunk())(ysThunk);
              });
            }
          });
        };
      },
      // ## stream#flatten
      // flatten :: STREAM[STREAM[T]] => STREAM[T]
      flatten: (lazyList) => {
        return self.match(lazyList,{
          empty: (_) => {
            return self.stream.empty();
          },
          cons: (head,tailThunk) => {
            return self.stream.concat(head)((_) => {
              return self.stream.flatten(tailThunk());
            });
          }
        });
      },
      // ### stream#take
      // take:: STREAM -> NUMBER -> STREAM
      take: (lazyList) => {
        return (number) => {
          expect(number).to.a('number');
          expect(number).to.be.greaterThan(-1);
          return self.match(lazyList,{
            empty: (_) => {
              return self.stream.empty();
            },
            cons: (head,tailThunk) => {
              if(number === 0) {
                return self.stream.empty();
              } else {
                return self.stream.cons(head,(_) => {
                  return self.stream.take(tailThunk())(number -1);
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
	      return self.match(lazyList,{
		    empty: (_) => {
              return self.stream.empty();
		    },
		    cons: (head,tailThunk) => {
		      if(predicate(head)){
			    return self.stream.cons(head,(_) => {
			      return self.stream.filter(tailThunk())(predicate);
			    });
		      } else {
			    return self.stream.filter(tailThunk())(predicate);
		      }
		    }
	      });
	    };
      },
      toArray: (lazyList) => {
        return self.match(lazyList,{
          empty: (_) => {
            return [];
          },
          cons: (head,tailThunk) => {
            return self.match(tailThunk(),{
              empty: (_) => {
                return [head];
              },
              cons: (head_,tailThunk_) => {
                return [head].concat(self.stream.toArray(tailThunk()));
              }
            });
          }
        });
      },
      // ### stream#fromList
      fromArray: (array) => {
        return array.reduce((accumulator, item) => {
          return self.stream.concat(accumulator)(self.stream.cons(item, (_) => {
            return self.stream.empty();
          }));
        });
      },
      toString: (lazyList) => {
        return self.match(lazyList,{
          empty: (_) => {
            return [];
          },
          cons: (head,tailThunk) => {
            return self.match(tailThunk(),{
              empty: (_) => {
                return "";
              },
              cons: (head_,tailThunk_) => {
                return head + self.stream.toString(tailThunk());
              }
            });
          }
        });
      },
      fromString: (str) => {
        expect(str).to.a('string');
        if(self.string.isEmpty(str)) {
          return self.stream.empty();
        } else {
          return self.stream.cons(self.string.head(str), () => {
            self.stream.fromString(self.string.tail(str));
          });
        }
      }
    } // stream
  }; // self
  
  module.exports = self;
  
})(module.exports);
