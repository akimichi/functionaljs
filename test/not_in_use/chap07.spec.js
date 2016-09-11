
  describe('高階関数によるベクトル演算', () => {
    var mkVector =  (alist) => {
      return (index) => {
        return list.at(alist)(index);
      };
    };
    var zero = mkVector(list.cons(0, list.cons(0, list.empty())));
    var add = (vs) => {
      return (ws) => {
        return (index) => {
          return vs(index) + ws(index);
        };
      };
    };
    var innerProduct = (vs) => {
      return (ws) => {
        var product = (index) => {
          return vs(index) * ws(index);
        };
        var innerProductHelper = (indexes, accumulator) => {
          var index = stream.head(indexes);
          if(truthy(vs(index)) && truthy(ws(index))) {
            return innerProductHelper(stream.tail(indexes), accumulator + product(index));
          } else {
            return accumulator;
          }
        };
        var naturals = stream.enumFrom(0);
        return innerProductHelper(naturals, 0);
      };
    };
    it('ベクトルの内積を innerProduct で計算する', (next) => {
      var vs = mkVector(list.cons(1, list.cons(0, list.empty())));
      var ws = mkVector(list.cons(0, list.cons(1, list.empty())));
      expect(
        innerProduct(vs)(ws)
      ).to.eql(
        0
      );
      expect(
        innerProduct(mkVector(list.cons(-1, list.cons(-2, 
                                                      list.cons(1,
                                                                list.empty())))))(
          mkVector(list.cons(1, list.cons(-1, list.cons(2,list.empty()))))
        )
      ).to.eql(
        3
      );
      next();
    });
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
      /* is:: FUNC[ANY -> BOOL] -> ANY -> BOOL A*/
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
      /* eq:: ANY -> ANY -> BOOL */
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
      /* multipleOf:: NUM -> NUM -> BOOL */
      var multipleOf = (n) => {
        expect(n).to.a('number');
        return (m) => {
          expect(n).to.a('number');
          return eq(remainder(m)(n))(0);
        };
      };
      /* even:: NUM -> BOOL */
      var even = multipleOf(2);
      /* odd:: NUM -> BOOL */
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
      /* positive:: NUM -> BOOL */
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
          if(multipleOf(k)(n)) {
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
        factors:: Integer -> [Integer]
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
    });
    describe('文字列検証コンビネータ', () => {
      /* is:: FUNC[ANY -> BOOL] -> ANY -> BOOL */
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
      /* eq:: ANY -> ANY -> BOOL */
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
        /* 文字列をリスト型に変換する */
        var stringAsList = list.fromString("abcd");
        /* 文字列の長さが6より長いかどうかを判定する */
        expect(
          is(greater(6))(list.length(stringAsList))
        ).to.be(
          false
        );
        /* 文字列の長さが3より長く6より短いかどうかを判定する */
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
        /* null and null are "empty" */
        if (obj == null) return true;
        /* Assume if it has a length property with a non-zero value
           that that property is correct. */
        if (obj.length > 0)    return false;
        if (obj.length === 0)  return true;
        /* Otherwise, does it have any properties of its own?
         Note that this doesn't handle
         toString and valueOf enumeration bugs in IE < 9 */
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
      /* validate: PARSER -> JSON -> PARSERESULT */
      var validate = (validator, continues, continuesInFailure) => {
        return (inputObject) => {
          return validator(inputObject, continues, continuesInFailure);
        };
      };
      /* 基本パーサー */
      /* succeed:: ANY => LIST => RESULT */
      var succeed = (value, continues, continuesInFailure) => {
        return (inputObject) => {
          return continues(result.successful(value, inputObject));
        };
      };
      /* fail:: (ANY) => LIST => PARSERESULT */
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
      compose(increment,decrement)(5)
    ).to.eql(
      5
    );
    next();
  });
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
    var last = (alist) => {
      return compose(list.head)(list.reverse)(alist);
    };
    var sequence = list.cons(1,list.cons(2,list.cons(3,list.cons(4,list.empty()))));
    expect(
      last(sequence)
    ).to.eql(
      4
    );
    next();
  });

      // var Y = (F) => {
      //   return ((g) => {
      //     return (x) =>  {
      //       return F(g(g))(x);
      //     };
      //   })((g) =>  {
      //     return (x) => {
      //       return F(g(g))(x);
      //     };
      //   });
      // };
      it('counter関数における、外側のスコープからのクロージャーによる自由変数の捕捉', (next) => {
        var _init = 0;
        var counter = (_) => {  // クロージャーを返す
          _init = _init + 1;
          return _init;
        };
        expect(
          counter()
        ).to.eql( 
          1
        );
        _init = 100;
        expect(
          counter()
        ).to.eql( 
          101
        );
        next();
      });
      it('関数の外側にあるスコープによる自由変数の捕捉', (next) => {
        var sleep = require('sleep-async')();
        /* #@range_begin(captures_free_variable_outside_function) */
        var startUpTime = Date.now();
        
        var application = {
          timeLapse: () => {
            var now = Date.now();
            /* 外側のスコープにある startUpTime変数を捕捉している */
            return now - startUpTime; 
          }
        };
        /* #@range_end(captures_free_variable_outside_function) */
        sleep.sleep(5000, () => {
          expect(
            application.timeLapse()
          ).to.be.greaterThan(
            1
          );
        });
        next();
      });
        it('オブジェクト型は不変ではない', (next) => {
          /* #@range_begin(object_is_not_immutable) */
          var object = {
            a: 1
          };
          expect(
            object.a
          ).to.eql(
            1
          );
          object.a = 2;
          expect(
            object.a
          ).to.eql(
            2 // 1だった値が2に変更されています
          );
          /* #@range_end(object_is_not_immutable) */
          next();
        });

        it('不変なオブジェクト型を作る', (next) => {
          /* #@range_begin(immutable_object_type) */
          var object = {
            empty: (_) => {
              return null;
            },
            get: (key, obj) => {
              return obj(key);
            },
            set: (key, value, obj) => {
              return (key2) => {
                if(key === key2) {
                  return value;
                } else {
                  return object.get(key2,obj);
                }
              };
            }
          };
          /* #@range_end(immutable_object_type) */
          /* #@range_begin(immutable_object_type_test) */
          expect(
            object.get("R2D2", 
                       object.set("R2D2", "Star Wars", 
                                  object.set("HAL9000","2001: a space odessay",
                                             object.empty)))
          ).to.eql(
            "Star Wars"
          );
          expect(
            object.get("R2D2", 
                       object.set("HAL9000","2001: a space odessay",
                                  object.empty))
          ).to.eql(
            null
          );
          expect(
            object.get("HAL9000", 
                       object.set("C3PO", "Star Wars", 
                                  object.set("R2D2", "Star Wars", 
                                             object.set("HAL9000","2001: a space odessay",
                                                        object.empty))))
          ).to.eql(
            "2001: a space odessay"
          );
          /* #@range_end(immutable_object_type_test) */
          next();
        });
        it('不変なオブジェクト型を作る(改良版)', (next) => {
          /* #@range_begin(immutable_object_type_improved) */
          var objects = {
            empty: (_) => {
              return null;
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
              };
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
              }, self.empty);
            }
          };
          /* #@range_end(immutable_object_type_improved) */
          /* #@range_begin(immutable_object_type_improved_test) */
          expect(
            objects.get("R2D2", objects.set("HAL9000","2001: a space odessay",objects.empty))
          ).to.eql(
            null
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
          // ~~~scheme
          // (define (cons x y)
          //   (lambda (m) (m x y)))
          // (define (car z)
          //   (z (lambda (p q) p)))
          // (define (cdr z)
          //   (z (lambda (p q) q)))
          // ~~~
          /* #@range_begin(immutable_list) */
          var list = {
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
          var theList = list.cons(1,list.cons(2,list.cons(3,list.empty)));
          expect(
            list.head(theList)
          ).to.eql(
            1
          );
          expect(
            list.head(list.tail(theList))
          ).to.eql(
            2
          )
          expect(
            list.at(0,theList)
          ).to.eql(
            1
          )
          expect(
            list.at(1,theList)
          ).to.eql(
            2
          )
          expect(
            list.at(2,theList)
          ).to.eql(
            3
          );
          /* #@range_end(immutable_list) */
          next();
        });
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
          /* head:: STREAM -> MAYBE[STREAM] */
          var head = (lazyList) => {
            return match(lazyList,{
              empty: (_) => {
                return null;
              },
              cons: (value, tailThunk) => {
                return value;
              }
            });
          };
          /* tail:: STREAM -> MAYBE[STREAM] */
          var tail = (lazyList) => {
            return match(lazyList,{
              empty: (_) => {
                return null;
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
          // ### stream#take
          /* take:: STREAM -> NUMBER -> STREAM */
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
            /* stream = [1,2] */
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
            /* ones = [1,1,1,1,...] */
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
            /* ones = [1,2,3,4,...] */
            var enumFrom = (n) => {
              return cons(n, (_) => {
                return enumFrom(n + 1);
              });
            };
            it("無限の整数列をテストする", (next) => {
              expect(
                head(enumFrom(1))
              ).to.eql(
                1
              );
              expect(
                head(tail(enumFrom(1)))
              ).to.eql(
                2
              );
              expect(
                toArray(take(enumFrom(1))(10))
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
              var evenIntegers = stream.filter(even)(enumFrom(1));
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
              this.timeout(7000);
              var multipleOf = (n) => {
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
                  if(multipleOf(k)(n)) {
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
              
              var primes = stream.filter(isPrime)(enumFrom(1));
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

          it('素数のジェネレータ',(next) => {
            this.timeout(7000);

            var multipleOf = (n) => {
              return (m) => {
                if(n % m === 0) {
                  return true;
                } else {
                  return false;
                }
              };
            };
            var leastDivisor = (n) => {
              expect(n).to.a('number');
              var leastDivisorHelper = (k, n) => {
                expect(k).to.a('number');
                expect(n).to.a('number');
                if(multipleOf(n)(k)) {
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
            var integers = stream.enumFrom(1);
            /* 素数のストリーム */
            var sieve = (aStream) => {
              return stream.match(aStream, {
                empty: () => { return null; },
                cons: (head, tailThunk) => {
                  return stream.cons(head, (_) => {
                    return sieve(stream.filter(
                      (item) => { 
                        return ! multipleOf(item)(head);  
                      }
                    )(tailThunk()));
                  }); 
                }
              });
            };
            next();
          });

        it('リスト・ジェレネータ', (next) => {
          /* #@range_begin(list_generator_test) */
          var generator = list.generate(list.cons(1, list.cons(2, list.empty())));
          expect(
            generator()
          ).to.eql(
            1
          );
          expect(
            generator()
          ).to.eql(
            2
          );
          /* #@range_end(list_generator_test) */
          next();
        });
        it('ストリーム・ジェレネータ', (next) => {
          /* #@range_begin(stream_generator_test) */
          var generator = stream.generate(stream.cons(1, (_) => {
            return stream.cons(2, (_) => {
              return stream.empty();
            });
          }));
          expect(
            generator()
          ).to.eql(
            1
          );
          expect(
            generator()
          ).to.eql(
            2
          );
          /* #@range_end(stream_generator_test) */
          next();
        });

        describe('ジェネレータ・コンビネータ', () => {
          var identity = (any) => { return any; };
          var succ = (n) => { return n + 1; };
          /* #@range_begin(generator_in_closure) */
          var generator = (init) => {
            return (current) => {
              return (step) => {
                return stream.cons(current(init),
                                   (_) => { 
                                     return generator(step(init))(current)(step);
                                   });
              };
            };
          };
          var integers = generator(0)(identity)(succ);
          it('integers をテストする', (next) =>{
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
            next();
          });
          it('ジェレネータによる単体テストの自動生成', (next) => {
            /* #@range_begin(generate_unit_tests) */
            var even = (n) => {
              return 0 === (n % 2);
            };
            var evenIntegers = stream.filter(even)(integers);
            stream.forEach(stream.take(evenIntegers)(10))((n) => {
              return expect(
                even(n)
              ).to.eql(
                true
              );
            });
            /* #@range_end(generate_unit_tests) */
            next();
          });
        });

  describe('非同期処理にコールバック関数を渡す', () => {
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
      /* #@range_begin(event_driven_system) */
      var eventSystem = () => {
        var handlers = object.empty(); // イベントハンドラを格納する変数
        return {
          on: (eventName, callback) => {
            handlers = object.set(eventName, callback, handlers);
            return null;
          },
          emit: (eventName, arg) => {
            return object.get(eventName, handlers)(arg);  // 該当するイベントハンドラを起動する
          }
        };
      };
      /* #@range_end(event_driven_system) */
      var eventLoop = (eventSystem) => {
        
      };
      /* #@range_begin(event_driven_system_test) */
      var eventDrivenServer = eventSystem(); // イベント駆動システムを初期化する
      /* イベント started を登録する */
      eventDrivenServer.on("started", (_) => { // startedイベントで実行されるコールバック関数を渡す
        return "event started";
      });
      /* イベント terminated を登録する */
      eventDrivenServer.on("terminated", (exitCode) => { // terminatedイベントで実行されるコールバック関数 
        return "event terminated with " + exitCode;
      });
      /**** テスト ****/
      /* イベント started を生じさせる */
      expect(
        eventDrivenServer.emit("started", null)
      ).to.eql(
        "event started"
      );
      /* イベント terminated を生じさせる */
      expect(
        eventDrivenServer.emit("terminated",404)
      ).to.eql(
        "event terminated with 404"
      );
      /* #@range_end(event_driven_system_test) */
      next();
    });
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
    next();
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

  describe('モナドの原理', () => {
    it("Kleisli合成", (next) => {
      var flatMap = (instanceM) => {
        return (transform) => {
          return transform(instanceM); 
        };
      };
      var compose = (f, g) => {
        return (x) => {
          return flatMap(f(x))(g);
        };
      };
      var f = (n) => {
        return n + 1;
      };
      var g = (n) => {
        return 2 * n;
      };
      var h = (n) => {
        return - n;
      };
      expect(
        compose(compose(f,g),h)(2)
      ).to.eql(
        compose(f,compose(g,h))(2)
      );
      next();
    });
  }); 

      it("恒等モナドのKleisli合成オペレータ", (next) => {
        /* #@range_begin(identity_monad_kleisli) */
        var f = (n) => {
          return n + 1;
        };
        var g = (n) => {
          return 2 * n;
        };
        var h = (n) => {
          return - n;
        };
        expect(
          ID.compose(ID.compose(f,g),h)(2)
        ).to.eql(
          ID.compose(f,ID.compose(g,h))(2)
        );
        /* #@range_end(identity_monad_kleisli) */
        next();
      });
