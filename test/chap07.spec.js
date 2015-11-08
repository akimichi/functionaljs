"use strict";

var expect = require('expect.js');

var seq = {
  empty: (index) => {
    return true;
  },
  cons: (head,tail) => {
    return (f) => {
      return f(head,tail);
    };
  },
  head: (list) => {
    return list((head,tail) => {
      return head;
    });
  },
  tail: (list) => {
    return list((head,tail) => {
      return tail;
    });
  },
  at: (index,list) => {
    if(index === 0){
      return this.head(list);
    } else {
      return this.at(index -1, this.tail(list));
    }
  }
};

var stream = {
  empty: (index) => {
    return true;
  },
  cons: (head,tailThunk) => {
    return (f) => {
      return f(head,tailThunk);
    };
  },
  head: (lazyList) => {
    return lazyList((head,tailThunk) => {
      return head;
    });
  },
  tail: (lazyList) => {
    return lazyList((head,tailThunk) => {
      return tailThunk();
    });
  },
  at: (index,lazyList) => {
    if(index === 0){
      return this.head(lazyList);
    } else {
      return this.at(index -1, this.tail(lazyList));
    }
  },
  // map: (lazyList, transform) => {
  //   var x = transform(this.head(lazyList));
  //   var xs = this.tail(lazyList);
  //   return this.cons(x, this.map(xs, transform));
  // }
};

describe('高階関数', () => {
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
        mkObject: (obj) => {
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
        objects.get("HAL9000", objects.mkObject.call(objects,{"HAL9000" : "2001: a space odessay", "R2D2": "Star Wars"}))
      ).to.eql(
        "2001: a space odessay"
      );
      expect(
        objects.get("R2D2", objects.mkObject.call(objects,{"HAL9000" : "2001: a space odessay", "R2D2": "Star Wars"}))
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
    it('不変なストリーム型', (next) => {
      /* #@range_begin(immutable_stream) */
      var stream = {
        empty: (index) => {
          return true;
        },
        cons: (head,tailThunk) => {
          return (f) => {
            return f(head,tailThunk);
          };
        },
        head: (lazyList) => {
          return lazyList((head,tailThunk) => {
            return head;
          });
        },
        tail: (lazyList) => {
          return lazyList((head,tailThunk) => {
            return tailThunk();
          });
        },
        at: (index,lazyList) => {
          if(index === 0){
            return this.head(lazyList);
          } else {
            return this.at(index -1, this.tail(lazyList));
          }
        }
      };

      /* #@range_end(immutable_stream) */
      next();
    });
  });
  describe('クロージャー', () => {
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
    describe('ジェネレーター', () => {
      /* #@range_begin(generator_in_closure) */
      var generator = (seed) => {
        return (current) => {
          return (stepFunction) => {
            return stream.cons(current(seed),
                               (_) => { return generator(stepFunction(seed))(current)(stepFunction) })
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
  });
  describe('コールバックを渡す', () => {
    it('イベント駆動', (next) => {
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
      var extract = (key) => {
        return (object) => {
          return object[key]
        };
      }
      var extractTemperture = (event) => {
        return processEvent(event)(extract("temperture"));
        // return processEvent(event)((theEvent) => {
        //   return theEvent.temperture;
        // })
      };
      expect(
        extractTemperture(anEvent)
      ).to.eql(
        26
      );
      next();
    });
  });
  describe('コンビネーター', () => {
    it('コンビネーター・ライブラリー', (next) => {
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
      )
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
      )
      expect(
        within(20)(30)(get('temp')(data))(id)
      ).to.eql(
        true
      )
      expect(
        within(20)(30)(data)(get('temp'))
      ).to.eql(
        true
      )
      expect(
        within(20)(30)(data)((data) => {
          return get('temp')(data)
        })
      ).to.eql(
        true
      )
      /* #@range_end(combinator_library_test) */
      next();
    });
  });
  describe('モナドを作る', () => {
    describe('Maybeモナド', () => {
      var id = (any) => {
        return any;
      };
      var match = (exp, pattern) => {
        return exp.call(pattern, pattern);
      };
      it('代数的データ型を作る')
      describe('Maybeモナドを作る', () => {
        /* #@range_begin(maybe_monad) */
        var just = (value) => {
          return (pattern) => {
            return pattern.just(value);
          };
        };
        var nothing = ((_) => {
          return (pattern) => {
            return pattern.nothing(_);
          };
        })(null);
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
                return nothing;
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
                return nothing;
              }
            });
          };
        };
        /* #@range_end(maybe_monad) */
        it("map id == id", function(next){
          /* #@range_begin(maybe_monad_test) */
          var justOne = just(1);
          expect(
            isEqual(map(justOne)(id))(id(justOne))
          ).to.be(
            true
          );
          expect(
            isEqual(map(nothing)(id))(id(nothing))
          ).to.be(
            true
          );
          /* #@range_end(maybe_monad_test) */
          next();
        });
        it("add(maybe, maybe)", (next) => {
          /* #@range_begin(maybe_monad_add_test) */
          var add = (maybeA) => {
            return (maybeB) => {
              return flatMap(maybeA)((a) => {
                return flatMap(maybeB)((b) => {
                  return unit(a + b);
                });
            });
            };
          };
          var justOne = just(1);
          var justTwo = just(2);
          var justThree = just(3);
          expect(
            isEqual(add(justOne)(justTwo))(justThree)
          ).to.eql(
            true
          );
          expect(
            isEqual(add(justOne)(nothing))(nothing)
          ).to.eql(
            true
          );
          /* #@range_end(maybe_monad_add_test) */
          next();
        });
      });
    });
  });
});
