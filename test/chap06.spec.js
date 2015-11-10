"use strict";

var expect = require('expect.js');

// 関数の使い方
// ========
describe('関数の使い方', () => {
  // ## 関数の基本
  describe('関数の基本', () => {
    var seq = {
      match: (data, pattern) => {
        return data(pattern);
      },
      empty: (pattern) => {
        return pattern.empty;
      },
      cons: (value, list) => {
        return (pattern) => {
          return pattern.cons(value, list);
        };
      },
      isEmpty: (list) => {
        return match(list, { // match関数で分岐する
          empty: true,
          cons: (head, tail) => { // headとtailにそれぞれ先頭要素、末尾要素が入る
            return false;
          },
        });
      },
      head: (list) => {
        return match(list, {
          empty: undefined, // 空のリストには先頭要素はありません
          cons: (head, tail) => {
            return head;
          },
        });
      },
      tail: (list) => {
        return match(list, {
          empty: undefined,  // 空のリストには末尾要素はありません
          cons: (head, tail) => {
            return tail;
          },
        });
      }
    };
    var infiniteLoop = (_) => {
      return infiniteLoop(_);
    };
    describe('関数の定義', () => {
      it('関数の変数へのバインド', (next) => {
        /* #@range_begin(function_bound_to_variable) */
        var id = (any) => {
          return any;
        };
        /* #@range_end(function_bound_to_variable) */
        /* #@range_begin(identity_function_test) */
        expect(
          id(1)
        ).to.eql(
          1
        );
        expect(
          id("a")
        ).to.eql(
          "a"
        );
        /* #@range_end(identity_function_test) */
        next();
      });
      it('関数とリストの類似性', (next) => {
        var match = (data, pattern) => {
          return data(pattern);
        };
        var empty = (pattern) => {
          return pattern.empty;
        };
        var cons = (value, list) => {
          return (pattern) => {
            return pattern.cons(value, list);
          };
        };
        var isEmpty = (list) => {
          return match(list, { // match関数で分岐する
            empty: true,
            cons: (head, tail) => { // headとtailにそれぞれ先頭要素、末尾要素が入る
              return false;
            },
          });
        };
        var head = (list) => {
          return match(list, {
            empty: undefined, // 空のリストには先頭要素はありません
            cons: (head, tail) => {
              return head;
            },
          });
        };
        var tail = (list) => {
          return match(list, {
            empty: undefined,  // 空のリストには末尾要素はありません
            cons: (head, tail) => {
              return tail;
            },
          });
        };
        /*
          ~~~haskell
          list2fct :: Eq a => [(a,b)] -> a -> b
          list2fct [] _ = error "function not total"
          list2fct ((u,v):uvs) x | x == u = v
          | otherwise = list2fct uvs x
          fct2list :: (a -> b) -> [a] -> [(a,b)]
          fct2list f xs = [ (x, f x) | x <- xs ]
          ~~~
        */
        // var list2function = (list) => {
        //    return (any) => {
        //      if(head(list)) {
        //        if(head(list) === any){
        //          return
        //        } else {
        //        }
        //    } else {
        //    }
        // };
        next();
      });
    });
    describe('関数の評価戦略', () => {
      /* #@range_begin(strict_evaluation_in_javascript) */
      var left = (x,y) => {
        return x;
      };
      expect(
        left(1, 2)
      ).to.eql(
        1
      );
      var infiniteLoop = (_) => {
        return infiniteLoop(_);
      };
      
      /* このテストは実行されると無限ループになるのでコメントアウトしています
         expect(
         left(1, infiniteLoop())
         ).to.be.ok()
      */
      /* #@range_end(strict_evaluation_in_javascript) */
      
    });
    describe('関数を合成する', () => {
      it('関数を連続して適用する', (next) => {
        /* #@range_begin(function_applied_sequentially) */
        var double = (n) => {
          return n * 2;
        };
        var negate = (n) => {
          return - n;
        };
        expect(
          negate(double(2))
        ).to.eql(
            -4
        )
        /* #@range_end(function_applied_sequentially) */
        /* #@range_begin(function_applied_twice) */
        var twice = (f) => {
          return (n) => {
            return f(f(n));
          };
        };
        var succ = (x) => {
          return x + 1;
        };
        expect(
          twice(succ)(2)
        ).to.eql(
          4
        );
        /* #@range_end(function_applied_twice) */
        /* #@range_begin(function_applied_ntimes) */
        var applyNtimes = (n) => {
          return (func) => {
            return (init) => {
              return (accumulator) => {
                if(n === 0) {
                  return accumulator;
                } else {
                  return applyNtimes(n - 1)(func)(init)(func(accumulator))
                };
              };
            };
          };
        };
        expect(
          applyNtimes(4)(succ)(0)(0) // succ(succ(succ(succ(0))))
        ).to.eql(
          4
        );
        /* #@range_end(function_applied_ntimes) */
        next();
      });
      describe('関数合成', () => {
        /* #@range_begin(compose_definition) */
        var compose = (f,g) => {
          return (arg) =>{
            return f(g(arg));
          };
        };
        /* #@range_end(compose_definition) */
        it('否定を合成する', (next) => {
          /* #@range_begin(compose_negation) */
          var negate = (n) => {
            return - n;
          };
          expect(
            compose(negate,negate)(2)
          ).to.eql(
            2
          );
          /* #@range_end(compose_negation) */
          next();
        });
        it('1個の引数の関数を合成する', (next) => {
          /* #@range_begin(compose_one_argument_functions) */
          var succ = (n) => {
            return n + 1;
          };
          var prev = (n) => {
            return n - 1;
          };
          expect(
            compose(prev,succ)(5)
          ).to.eql(
            5
          );
          expect(
            compose(prev,succ)(2)
          ).to.eql(
            2
          );
          /* #@range_end(compose_one_argument_functions) */
          next();
        });
        it('乗算と否定の合成は失敗する', (next) => {
          /* #@range_begin(compose_negate_multiply) */
		  // var negate = (x) => {
		  // 	return - x;
		  // };
		  // var multiply = (x, y) => {
		  // 	return x * y;
		  // };
		  // expect(
		  // 	compose(negate, negate)(2)
		  // ).to.eql(
		  // 	2
		  // );
		  // expect(
		  // 	compose(negate, multiply)(2,3)
		  // ).to.be(null);
          /* #@range_end(compose_negate_multiply) */
          next();
        });
		
        // it("'compose two argument functions'", function(next) {
        //   var negate = function(x) {
        //     return -x;
        //   };
        //   var multiply = function(x,y){
        //     return x * y;
        //   };
        //   expect((__.compose.bind(__)(negate)(multiply))(2,3)).to.eql(-6);
        //   next();
        // });
        // it("compose several functions", function(next) {
        //   var not = function(x){
        //     return ! x;
        //   };
        //   expect(
        //     __.compose.bind(__)(not)(math.isEqual(3))(3)
        //   ).to.eql(
        //       false
        //   );
        //   // expect(
        //   //   __.compose.bind(__)(__.not)(math.isEqual(3))(3)
        //   // ).to.eql(
        //   //  false
        //   // );
        //   next();
        // });
      });
      it('リストの逆順を求める', (next) => {
        /* #@range_begin(list_reverse) */
        var reverse = (list) => {
          return (accumulator) => {
            return seq.match(list, {
              empty: accumulator,  // 空のリストの場合は終了
              cons: (head, tail) => {
                return reverse(tail)(seq.cons(head, accumulator))
              },
            });
          }
        };
        // toArray:: LIST -> ARRAY -> ARRAY
        var toArray = (list) => {
          var toArrayAux = (list) => {
            return (accumulator) => {
              return seq.match(list, {
                empty: accumulator,  // 空のリストの場合は終了
                cons: (head, tail) => {
                  return toArrayAux(tail)(accumulator.concat(head))
                },
              });
            };
          };
          return toArrayAux(list)([])
        };
        /**************** テスト ****************/
        expect(
          toArray(reverse(seq.cons(1, seq.cons(2,seq.empty)))(seq.empty))
        ).to.eql(
          [2,1]
        );
        /* #@range_end(list_reverse) */
        next();
      });
    });
    describe('関数の純粋性', () => {
      describe('副作用が参照透過性を損なうこと', () => {
        it('console.logが参照透過性を損なうこと', (next) => {
          /* #@range_begin(log_destroys_referential_transparency) */
          expect(
            console.log("this is a test")
          ).to.eql(
            console.log("this is anoter test")
          )
          /* #@range_end(log_destroys_referential_transparency) */
          next();
        });
        describe('ファイル操作が参照透過性を損なうこと', () => {
          /* #@range_begin(fileio_destroys_referential_transparency) */
          var fs = require('fs');
          after(() => { // テストの終了時に実行される
            fs.writeFileSync('test/resources/file.txt', "This is a test.");
          });
          it('ファイルを操作する', (next) => {
            expect(
              fs.readFileSync("test/resources/file.txt", 'utf8')
            ).to.be(
              "This is a test."
            );
            fs.writeFileSync('test/resources/file.txt', "This is another test.")
            expect(
              fs.readFileSync("test/resources/file.txt", 'utf8')
            ).not.to.be(
              "This is a test."
            );
            next();
          });
          /* #@range_end(fileio_destroys_referential_transparency) */
        });
      });
      describe('副作用の分離', () => {
        it('kestrelコンビネーター', (next) => {
          // this.timeout(2000);
          // var timeout = setTimeout(next, 2000);
          /* #@range_begin(kestrel_combinator) */
          var kestrel = (x) => {
            return (y) => {
              return x;
            };
          };
          /* #@range_end(kestrel_combinator) */
          /* #@range_begin(kestrel_combinator_test) */
          expect(
            kestrel(1)(2)
          ).to.eql(
            1
          );
          /*
            expect(
            kestrel(1)(infiniteLoop())
            ).to.eql(
            1
            );
          */ 
          /* #@range_end(kestrel_combinator_test) */
          // setTimeout(function(){
          //   // happens 0.5 seconds later:
          //   expect(
          //  kestrel(1)(infiniteLoop())
          //   ).to.eql(
          //  1
          //   );
          //   next(); // this tells mocha to run the next test
          // },500);
          next();
        });
      });
    });
  });
  describe('高階関数', () => {
    var seq = {
      match: (data, pattern) => {
        return data.call(pattern,pattern);
      },
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
    describe('カリー化', () => {
      describe('関数合成のカリー化', () => {
        /* #@range_begin(compose_definition_curried) */
        var compose = (f) => {
          return (g) => {
            return (arg) =>{
              return f(g(arg));
            };
          };
        };
        /* #@range_end(compose_definition_curried) */

        /* #@range_begin(flip) */
        var flip = (fun) => {
          return  (f) => {
            return (g) => {
              return fun(g)(f);
              // return fun.call(this, g)(f);
            };
          };
        };
        var callee = (n) => {
          return (callback) => {
            return callback(n);
          };
        };
        var succ = (n) => {
          return n + 1;
        };
        expect(flip(callee)(succ)(2)).to.eql(3);
        /* #@range_end(flip) */

        it('マイナスのマイナスはプラス', (next) => {
          /* #@range_begin(composition_example_negation_twice) */
          var negate = (n) => {
            return - n;
          };
          expect(
            compose(negate)(negate)(2)
          ).to.eql(
            2
          );
          /* #@range_end(composition_example_negation_twice) */
          next();
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
            compose(increment)(decrement)(5)
          ).to.eql(
            5
          );
          // expect(__.compose.bind(__)(decrement)(increment)(5)).to.be(5);
          // expect(__.compose.bind(__)(increment)(increment)(5)).to.be(7);
          // // (n * 2) + 1
          // expect(__.compose.bind(__)(increment)(double)(5)).to.be(11);
          // // (n + 1) * 2
          // expect(__.compose.bind(__)(double)(increment)(5)).to.be(12);
          next();
        });
        // it("'compose two argument functions'", function(next) {
        //   var negate = function(x) {
        //     return -x;
        //   };
        //   var multiply = function(x,y){
        //     return x * y;
        //   };
        //   expect((__.compose.bind(__)(negate)(multiply))(2,3)).to.eql(-6);
        //   next();
        // });
        // it("compose several functions", function(next) {
        //   var not = function(x){
        //     return ! x;
        //   };
        //   expect(
        //     __.compose.bind(__)(not)(math.isEqual(3))(3)
        //   ).to.eql(
        //       false
        //   );
        //   // expect(
        //   //   __.compose.bind(__)(__.not)(math.isEqual(3))(3)
        //   // ).to.eql(
        //   //  false
        //   // );
        //   next();
        // });
      });
      it('リストの逆順を求める', (next) => {
        var seq = {
          match: (data, pattern) => {
            return data(pattern);
          },
          empty: (pattern) => {
            return pattern.empty;
          },
          cons: (value, list) => {
            return (pattern) => {
              return pattern.cons(value, list);
            };
          },
          isEmpty: (list) => {
            return match(list, { // match関数で分岐する
              empty: true,
              cons: (head, tail) => { // headとtailにそれぞれ先頭要素、末尾要素が入る
                return false;
              },
            });
          },
          head: (list) => {
            return match(list, {
              empty: undefined, // 空のリストには先頭要素はありません
              cons: (head, tail) => {
                return head;
              },
            });
          },
          tail: (list) => {
            return match(list, {
              empty: undefined,  // 空のリストには末尾要素はありません
              cons: (head, tail) => {
                return tail;
              },
            });
          }
        };
        /* #@range_begin(list_reverse) */
        var reverse = (list) => {
          return (accumulator) => {
            return seq.match(list, {
              empty: accumulator,  // 空のリストの場合は終了
              cons: (head, tail) => {
                return reverse(tail)(seq.cons(head, accumulator))
              },
            });
          }
        };
        // toArray:: LIST -> ARRAY -> ARRAY
        var toArray = (list) => {
          var toArrayAux = (list) => {
            return (accumulator) => {
              return seq.match.call(seq,
                                    list, {
                empty: accumulator,  // 空のリストの場合は終了
                cons: (head, tail) => {
                  return toArrayAux(tail)(accumulator.concat(head))
                },
              });
            };
          };
          return toArrayAux(list)([])
        };
        /**************** テスト ****************/
        expect(
          toArray(reverse(seq.cons(1, seq.cons(2,seq.empty)))(seq.empty))
        ).to.eql(
          [2,1]
        );
        /* #@range_end(list_reverse) */
        next();
      });
    });
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
      describe('恒等モナド', () => {
        /* #@range_begin(identity_monad) */
		var identity = {
		  // ## identity#unit
		  unit: (value) => {
			var self = this;
			return value;
		  },
		  flatMap: (instance) => {
			var self = this;
			return (transform) => {
			  expect(transform).to.a('function');
			  return transform(instance);
			};
		  }
		};
        /* #@range_end(identity_monad) */
		it("identity#flatMap", (next) => {
          /* #@range_begin(identity_monad_test) */
		  var instance = identity.unit(1);
		  expect(
			identity.flatMap(instance)((n) => {
			  return identity.unit(n * 2);
			})
		  ).to.eql(
			identity.unit(2)
		  );
		  expect(
			identity.flatMap(instance)((n) => {
			  return identity.flatMap(identity.unit(n * 2))((m) => {
				return identity.unit(m * 3);
			  });
			})
		  ).to.eql(
			identity.unit(6)
		  );
		  expect(
			identity.flatMap(instance)((n) => {
			  return identity.flatMap(identity.unit(n))((m) => {
				return identity.unit(m * n);
			  });
			})
		  ).to.eql(
			identity.unit(1)
		  );
          /* #@range_end(identity_monad_test) */
		  next();
		});
	  });
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
      describe('Listモナド', () => {
		var seq  = {
		  match: (data, pattern) => {
			var self = this;
			return data(pattern);
		  },
          compose: (f,g) => {
			return (arg) => {
              return f(g(arg));
			};
          },
		  flip: (fun) => {
			return  (f) => {
              return (g) => {
				return fun(g)(f);
              };
			};
          },
		  empty: (_) => {
			return (pattern) => {
			  return pattern.empty();
			};
		  },
		  cons: (value, list) => {
			return (pattern) => {
			  return pattern.cons(value, list);
			};
		  },
		  head: (list) => {
			var self = this;
			return self.match(list, {
			  empty: (_) => {
				return undefined;
			  },
			  cons: (head, tail) => {
				return head;
			  },
			});
		  },
		  tail: (list) => {
			var self = this;
			return self.match(list, {
			  empty: (_) => {
				return undefined;
			  },
			  cons: (head, tail) => {
				return tail;
			  },
			});
		  },
		  isEmpty: (list) => {
			var self = this;
			return seq.match(list, {
			  empty: (_) => {
				return true;
			  },
			  cons: (head, tail) => {
				return false;
			  },
			});
		  },
		  // concat:: LIST[T] -> LIST[T] -> LIST[T]
		  concat: (xs) => {
			var self = this;
			return (ys) => {
			  if(self.isEmpty(xs)){
				return ys;
			  } else {
				return self.cons(self.head(xs),(self.concat(self.tail(xs))(ys)));
			  }
			};
		  },
		  // concat:: LIST[LIST[T]] -> LIST[T]
		  join: (list_of_list) => {
			var self = this;
			if(self.isEmpty(list_of_list)){
			  return self.empty();
			} else {
			  return self.concat(seq.head(list_of_list))(self.join(seq.tail(list_of_list)));
			}
		  },
		  // foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T
		  foldr: (list) => {
			var self = this;
			return (accumulator) => {
			  return (glue) => {
				expect(glue).to.a('function');
				if(self.isEmpty(list)){
				  return accumulator;
				} else {
				  var item = self.head(list);
				  var tail = self.tail(list);
				  return glue(item)(self.foldr(tail)(accumulator)(glue));
				}
			  };
			};
		  },
		  map: (list) => {
			var self = this;
			return (transform) => {
			  expect(transform).to.a('function');
			  var glue = self.compose(self.cons.bind(self))(transform.bind(self));
			  return self.foldr(list)(self.empty())(glue);
			};
		  },
		  // ### list#unit
		  unit: (value) => {
			var self = this;
			return self.cons(value, seq.empty());
		  },
		  // ### monad.list#flatMap
		  flatMap: (instance) => {
			var self = this;
			return (transform) => {
			  expect(transform).to.a('function');
			  return self.join(
				self.flip.call(self,
							   self.map.bind(self))(transform.bind(self))(instance));
			};
		  }
		}; // end of seq
		it("'list#empty'", (next) => {
		  seq.match(seq.empty,{
			empty: (_) => {
			  expect(true).ok();
			},
			cons: (x,xs) => {
			  expect().fail()
			}
		  });
		  next();
		})
		it("'list#isEmpty'", (next) => {
		  expect(
			seq.isEmpty(seq.empty())
		  ).to.eql(
			true
		  );
		  expect(
			seq.isEmpty(seq.cons(1,seq.empty()))
		  ).to.eql(
			false
		  );
		  next();
		})
		it("'list#cons'", (next) => {
		  seq.match(seq.cons(1,seq.empty()),{
			empty: (_) => {
			  expect().fail()
			},
			cons: (x,xs) => {
			  expect(x).to.eql(1)
			}
		  });
		  next();
		})
		it("'list#head'", (next) => {
		  expect(
			seq.head(seq.cons(1,seq.empty()))
		  ).to.eql(
			1
		  )
		  next();
		})
		it("'list#tail'", (next) => {
		  expect(
			seq.head(seq.tail(seq.cons(1,seq.cons(2,seq.empty()))))
		  ).to.eql(
			2
		  )
		  next();
		})
		it("'list#concat'", (next) => {
		  var list = seq.concat(seq.cons(1,seq.empty()))(seq.cons(2,seq.empty()));
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
			seq.isEmpty(seq.tail(seq.tail(list)))
		  ).to.eql(
			true
		  )
		  next();
		});
		it("'list#join'", (next) => {
		  // list = [[1,2],[3,4]]
		  var list_of_list = seq.cons(seq.cons(1,seq.cons(2,seq.empty())),
									  seq.cons(seq.cons(3,seq.cons(4,seq.empty())),
											   seq.empty));
		  // joined_list = [1,2,3,4]
		  var joined_list = seq.join(list_of_list)
		  expect(
			seq.head(joined_list)
		  ).to.eql(
			1
		  )
		  expect(
		  	seq.head(seq.tail(joined_list))
		  ).to.eql(
		  	2
		  )
		  expect(
		  	seq.isEmpty(seq.tail(seq.tail(joined_list)))
		  ).to.eql(
		  	false
		  )
		  next();
		});
		it("'list#foldr'", (next) => {
		  // list = [1,2,3,4]
		  var list = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty()),seq.empty)))
		  expect(
			seq.foldr(list)(0)(function (item){
			  return (accumulator) => {
				return accumulator + item;
			  };
			})
		  ).to.eql(
			10
		  )
		  next();
		})
      });
    });
	describe('プログラミング言語を作る', () => {
	  
    });
  });
});
