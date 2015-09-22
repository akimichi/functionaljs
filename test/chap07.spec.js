"use strict";

var expect = require('expect.js');

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
      )
      expect(
        objects.get("HAL9000", objects.mkObject.call(objects,{"HAL9000" : "2001: a space odessay", "R2D2": "Star Wars"}))
      ).to.eql(
        "2001: a space odessay"
      )
      expect(
        objects.get("R2D2", objects.mkObject.call(objects,{"HAL9000" : "2001: a space odessay", "R2D2": "Star Wars"}))
      ).to.eql(
        "Star Wars"
      )
      /* #@range_end(immutable_object_type_improved_test) */
      next();
    });
    it('不変な配列型', (next) => {
      // (define (cons x y)
      //   (lambda (m) (m x y)))
      // (define (car z)
      //   (z (lambda (p q) p)))
      // (define (cdr z)
      //   (z (lambda (p q) q)))
      /* #@range_begin(immutable_array_type) */
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
      /* #@range_end(immutable_array_type) */
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
});
