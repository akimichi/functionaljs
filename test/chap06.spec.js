"use strict";

var expect = require('expect.js');

var match = (data, pattern) => {
  return data(pattern);
};


var uncurry = (fun) => {
  return function() {
    var result = fun;
    for (var i = 0; i < arguments.length; i++)
      result = result(arguments[i]);
    return result;
  };
};

var flip = (fun) => {
  return  (f) => {
    return (g) => {
      return fun(g)(f);
    };
  };
};

var id = (any) => {
  return any;
};

// var compose = (f) => {
//   var self = this;
//   return (g) => {
//     return (arg) => {
//       return f.call(self,
//                     g.call(self,arg));
//     };
//   };
// };
var compose = (f) => {
  return (g) => {
    return (arg) => {
      return f(g(arg));
    };
  };
};

// 関数の使い方
// ========
describe('関数の使い方', () => {
  // ## 関数の基本
  describe('関数の基本', () => {
    var seq = {
      match: (data, pattern) => {
        return data(pattern);
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
      isEmpty: (list) => {
        return match(list, { // match関数で分岐する
          empty: true,
          cons: (head, tail) => { // headとtailにそれぞれ先頭要素、末尾要素が入る
            return false;
          }
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
          }
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
    });
    describe('関数の適用', () => {
      describe('関数適用と置換ルール', () => {
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
        describe('thunkを使う', () => {
          /* #@range_begin(definition_thunk_force) */
          var thunk = (func) => {
            return (arg) => {
              return (_) => { // サンクを返す
                return func(arg);
              };
            };
          };
          var force = (thunk) => {
            return thunk();
          };
          /* #@range_end(definition_thunk_force) */
          it('thunkによる遅延評価', (next) => {
            /* #@range_begin(nonstrict_function_via_thunk) */
            var multiply = (thunkX,thunkY) => {
              if(force(thunkX) === 0){
                return 0;
              } else {
                return thunkX() * thunkY(); // サンクを評価する
              }
            };
            var id = (any) => {
              return any;
            };
            expect(
              multiply((_) => { // サンクで包む
                return 0;
              }, (_) => {
                return 2;
              })
            ).to.eql(
              0
            );
            /* #@range_end(nonstrict_function_via_thunk) */
            next();
          });
          it('thunkによる条件式', (next) => {
            /* #@range_begin(functionalIf_via_thunk) */
            var functionalIf = (predicate, trueClauseThunk, falseClauseThunk) => {
              if(predicate){
                return trueClauseThunk(); // 判定式が真の場合に実行する
              } else {
                return falseClauseThunk();  // 判定式が真の場合に実行する
              }
            };
            /* テスト */
            expect(
              functionalIf((2 < 3), (_) => {
                return 2;
              }, (_) => {
                return 3;
              })
            ).to.eql(
              2
            );
            /* #@range_end(functionalIf_via_thunk) */
            // expect(
            //   functionalIf((2 > 3), 2, 3)
            // ).to.eql(
            //   3
            // );
            next();
          });
          describe('thunkによるStream型', () => {
            /* #@range_begin(stream_with_thunk) */
            var stream = {
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
                return match(lazyList,{
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
                return match(lazyList,{
                  empty: (_) => {
                    return undefined;
                  },
                  cons: (head, tailThunk) => {
                    return tailThunk();
                  }
                });
              },
              map: (lazyList) => {
                return (transform) => {
                  return match(lazyList,{
                    empty: (_) => {
                      return stream.empty();
                    },
                    cons: (head,tailThunk) => {
                      return stream.cons(transform(head),(_) => {
                        return stream.map(tailThunk())(transform);
                      });
                    }
                  });
                };
              },
              toArray: (lazyList) => {
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
                        return [head].concat(stream.toArray(tailThunk()));
                      }
                    });
                  }
                });
              }
            };
            /* #@range_end(stream_with_thunk) */
              // isEmpty: (lazyList) => {
              //   return match(lazyList,{
              //     empty: (_) => {
              //       return true;
              //     },
              //     cons: (head,tailThunk) => {
              //       return false;
              //     }
              //   });
              // },
              // map: (lazyList) => {
              //   return (transform) => {
              //     return match(lazyList,{
              //       empty: (_) => {
              //         return stream.empty();
              //       },
              //       cons: (head,tailThunk) => {
              //         return stream.cons(transform(head),(_) => {
              //           return stream.map(tailThunk())(transform)});
              //       }
              //     });
              //   };
              // },
              // // ## stream#concat
              // concat: (xs) => {
              //   return (ysThunk) => {
              //     return match(xs,{
              //       empty: (_) => {
              //         return ysThunk();
              //       },
              //       cons: (head,tailThunk) => {
              //         return stream.cons(head,(_) => {
              //           return stream.concat(tailThunk())(ysThunk);
              //         });
              //       }
              //     });
              //   };
              // },
              // // ## stream#flatten
              // // flatten :: STREAM[STREAM[T]] => STREAM[T]
              // flatten: (lazyList) => {
              //   return match(lazyList,{
              //     empty: (_) => {
              //       return stream.empty();
              //     },
              //     cons: (head,tailThunk) => {
              //       return stream.concat(head)((_) => {
              //         return stream.flatten(tailThunk());
              //       });
              //     }
              //   });
              // }
            it("stream#cons", (next) => {
              // var lazyList = stream.cons(1, (_) => {
              //   return stream.cons(2,thunk(stream.empty)());
              // });
              /* #@range_begin(stream_with_thunk_test) */
              var lazyList = stream.cons(1, (_) => {
                return stream.cons(2,(_) => {
                  return stream.empty();
                });
              });
              expect(
                stream.head(lazyList)
              ).to.eql(
                1
              );
              /* #@range_end(stream_with_thunk_test) */
              next();
            });
            it("stream#map", (next) => {
              /* #@range_begin(stream_map_test) */
              var lazyList = stream.cons('a', (_) => {
                return stream.cons('b',(_) => {
                  return stream.empty();
                });
              });
              var capitalise = (ch) => {
                return ch.toUpperCase();
              };

              expect(
                stream.toArray(stream.map(lazyList)(capitalise))
              ).to.eql(
                ["A","B"]
              );
              /* #@range_end(stream_map_test) */
              next();
            });
            describe("無限ストリーム", () => {
              /* #@range_begin(infinite_stream) */
              // ones = [1,1,1,1,...]
              var ones = stream.cons(1, (_) => {
                return ones;
              });
              expect(
                stream.head(ones)
              ).to.eql(
                1
              );
              expect(
                stream.head(stream.tail(ones))
              ).to.eql(
                1
              );
              /* #@range_end(infinite_stream) */
              /* #@range_begin(infinite_integer) */
              // ones = [1,2,3,4,...]
              var intgersFrom = (n) => {
                return stream.cons(n, (_) => {
                  return intgersFrom(n + 1);
                });
              };
              /* #@range_end(infinite_integer) */
              it("無限の整数列をテストする", (next) => {
                /* #@range_begin(infinite_integer_test) */
                expect(
                  stream.head(intgersFrom(1))
                ).to.eql(
                  1
                );
                expect(
                  stream.head(stream.tail(intgersFrom(1)))
                ).to.eql(
                  2
                );
                /* #@range_end(infinite_integer_test) */
                next();
              });
            });
          });
        }); // thunk
      });
      describe('再帰的な関数適用', () => {
        var seq  = {
          match: (data, pattern) => {
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
              }
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
            return self.match(list, {
              empty: (_) => {
                return true;
              },
              cons: (head, tail) => {
                return false;
              }
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
          last: (xs) => {
            var self = this;
            return self.match(xs, {
              empty: (_) => {
                return undefined;
              },
              cons: (head, tail) => {
                return self.match(tail, {
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
          // concat:: LIST[LIST[T]] -> LIST[T]
          join: (list_of_list) => {
            var self = this;
            if(self.isEmpty(list_of_list)){
              return self.empty();
            } else {
              return self.concat(seq.head(list_of_list))(self.join(self.tail(list_of_list)));
            }
          },
          // foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T
          foldr: (list) => {
            var self = this;
            return (accumulator) => {
              return (glue) => {
                expect(glue).to.a('function');
                return self.match(list,{
                  empty: (_) => {
                    return accumulator;
                  },
                  cons: (head, tail) => {
                    return glue(head)(self.foldr(tail)(accumulator)(glue));
                  }
                });
                // if(self.isEmpty(list)){
                //   return accumulator;
                // } else {
                //   var item = self.head(list);
                //   var tail = self.tail(list);
                //   return glue(item)(self.foldr(tail)(accumulator)(glue));
                // }
              };
            };
          },
          // map:: LIST[T] -> FUNC[T -> T] -> LIST[T]
          map: (list, transform) => {
            var self = this;
            return self.match(list,{
              empty: (_) => {
                return self.empty();
              },
              cons: (x,xs) => {
                return self.cons(transform(x),self.map(xs,transform));
              }
            });
          },
          /* #@range_begin(list_reverse) */
          // reverse: (list, accumulator) => {
          //    var self = this;
          //    return self.match(list, {
          //     empty: (_) => {
          //        return accumulator;  // 空のリストの場合は終了
          //     },
          //     cons: (head, tail) => {
          //        return self.reverse(tail, self.cons(head, accumulator));
          //     }
          //    });
          // },
          /* #@range_end(list_reverse) */
          /* #@range_begin(list_filter) */
          filter: (list) => {
            var self = this;
            return (predicate) => {
              expect(predicate).to.a('function');
              var filterAux = (list, accumulator) => {
                return self.match(list,{
                  empty: (_) => {
                    return accumulator;
                  },
                  cons: (head,tail) => {
                    if(predicate(head) === true){
                      return self.concat(self.concat(accumulator)(self.cons(head, self.empty())))(filterAux(tail, accumulator));
                      // return self.concat(accumulator)(self.cons(head, self.empty()));
                    } else  {
                      return filterAux(tail, accumulator);
                    }
                  }
                });
              };
              return filterAux(list, self.empty());
            };
          },
          /* #@range_end(list_filter) */
          /* #@range_begin(list_toarray) */
          toArray: (list) => {
            var self = this;
            var toArrayAux = (list,accumulator) => {
              return self.match(list, {
                empty: (_) => {
                  return accumulator;  // 空のリストの場合は終了
                },
                cons: (head, tail) => {
                  return toArrayAux(tail, accumulator.concat(head));
                }
              });
            };
            return toArrayAux(list, []);
          }
          /* #@range_end(list_toarray) */
        };
        it('dividesTimesの例', (next) => {
          /* #@range_begin(dividesTimes) */
          var multiplyOf = (n,m) => {
            if(m % n === 0) {
              return true;
            } else {
              return false;
            }
          };
          expect(
            multiplyOf(3,6)
          ).to.eql(
            true
          );
          var dividesTimes = (n, m) => {
            var dividesTimesHelper = (n,m,accumulator) => {
              if(n > m) {
                return accumulator;
              } else {
                if(multiplyOf(n,m)) {
                  return dividesTimesHelper(n,m - n,accumulator + 1);
                } else {
                  return accumulator;
                  // return dividesTimesHelper(n,m,accumulator);
                }
              }
            };
            return dividesTimesHelper(n,m,0);
          };
          expect(
            dividesTimes(3,6)
          ).to.eql(
            2
          );
          expect(
            dividesTimes(3,7)
          ).to.eql(
            0
          );
          /* #@range_end(dividesTimes) */
          next();
        });
        it('list#lastをテストする', (next) => {
          /* #@range_begin(list_last_test) */
          // list = [1,2,3,4]
          var list = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty()))));
          expect(
            seq.last(list)
          ).to.eql(
            4
          );
          /* #@range_end(list_last_test) */
          next();
        });
        it('list#mapをテストする', (next) => {
          /* #@range_begin(list_map_test) */
          // list = [1,2,3,4]
          var list = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty))));
          expect(
            seq.toArray(seq.map(list,(item) => {
              return item * 2;
            }))
          ).to.eql(
            [2,4,6,8]
          );
          /* #@range_end(list_map_test) */
          next();
        });
        it('list#filterテストする', (next) => {
          /* #@range_begin(list_filter_test) */
          // list = [1,2,3,4]
          var list = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty))));
          var even = (n) => {
            return (n % 2) === 0;
          };
          expect(
            seq.toArray(seq.filter(list)(even))
          ).to.eql(
            [2,4]
          );
          /* #@range_end(list_filter_test) */
          next();
        });
        it('list#reverseをテストする', (next) => {
          var reverse = (list,accumulator) => {
            return seq.match(list, {
              empty: (_) => {
                return accumulator;  // 空のリストの場合は終了
              },
              cons: (head, tail) => {
                return reverse(tail, seq.cons(head, accumulator));
              }
            });
          };
          // /**************** テスト ****************/
          /* #@range_begin(list_reverse_test) */
          var list = seq.cons(1, seq.cons(2,seq.empty()));
          expect(
            seq.toArray(reverse(list, seq.empty()))
            // seq.toArray(seq.reverse(list, seq.empty()))
          ).to.eql(
            [2,1]
          );
          /* #@range_end(list_reverse_test) */
          next();
        });
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
          );
          /* #@range_end(function_applied_sequentially) */
          /* #@range_begin(function_applied_twice) */
          var twice = (f,arg) => {
            return f(f(arg));
          };
          var succ = (x) => {
            return x + 1;
          };
          expect(
            twice(succ,2)
          ).to.eql(
            4
          );
          /* #@range_end(function_applied_twice) */
          /* #@range_begin(function_applied_ntimes) */
          var applyNTimes = (n, func) => {
            var applyNTimesHelper = (n, func) => {
              return (init) => {
                return (accumulator) => {
                  if(n === 0) {
                    return accumulator;
                  } else {
                    return applyNTimesHelper(n - 1,func)(init)(func(accumulator));
                  };
                };
              };
            };
            return applyNTimesHelper(n,func)(0);
          };
          expect(
            applyNTimes(4,succ)(0)
            // applyNTimes(4)(succ)(0)(0) // succ(succ(succ(succ(0))))
          ).to.eql(
            4
          );
          /* #@range_end(function_applied_ntimes) */
          next();
        });
        describe('関数合成', () => {
          /* #@range_begin(compose_definition) */
          var compose = (f,g,arg) => {
            return f(g(arg));
          };
          /* #@range_end(compose_definition) */
          it('否定を合成する', (next) => {
            /* #@range_begin(compose_negation) */
            var negate = (n) => {
              return -1 *  n;
            };
            expect(
              compose(negate,negate, 2)
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
              compose(prev,succ,5)
            ).to.eql(
              5
            );
            expect(
              compose(prev,succ,2)
            ).to.eql(
              2
            );
            /* #@range_end(compose_one_argument_functions) */
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
        it('再帰によるlast', (next) => {
          /* #@range_begin(list_last_recursive) */
          var last = (list) => {
            return seq.match(list, {
              empty: (_) => {
                return undefined;
              },
              cons: (head, tail) => {
                return seq.match(tail, {
                  empty: (_) => {
                    return head;
                  },
                  cons: (head, _) => {
                    return last(tail);
                  }
                });
              }
            });
          };
          var sequence = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty()))));
          expect(
            last(sequence)
          ).to.eql(
            4
          );
          /* #@range_end(list_last_recursive) */
          next();
        });
        it('合成によるlast', (next) => {
          /* #@range_begin(list_last_compose) */
          var sequence = seq.cons(1,seq.cons(2,seq.cons(3,seq.cons(4,seq.empty()))));
          var last = (list) => {
            return compose(seq.head)(seq.reverse)(list);
          };
          /* #@range_end(list_last_compose) */
          next();
        });
      }); // 関数を合成する
    }); // 関数の適用
  }); // 関数の基本
  describe('演算子', () => {

  });
  describe('関数の純粋性', () => {
    describe('副作用が参照透過性を損なうこと', () => {
      it('console.logが参照透過性を損なうこと', (next) => {
        /* #@range_begin(log_destroys_referential_transparency) */
        expect(
          console.log("this is a test")
        ).to.eql(
          console.log("this is anoter test")
        );
        /* #@range_end(log_destroys_referential_transparency) */
        next();
      });
      describe('ファイル操作が参照透過性を損なうこと', () => {
        /* #@range_begin(fileio_destroys_referential_transparency) */
        var fs = require('fs');
        before(() => { // テストの実行前に実行される
          fs.writeFileSync('test/resources/file.txt', "This is a test.");
        });
        it('ファイルを操作する', (next) => {
          var text = fs.readFileSync("test/resources/file.txt", 'utf8');
          expect(
            fs.readFileSync("test/resources/file.txt", 'utf8')
          ).to.eql(
            "This is a test."
          );
          fs.writeFileSync('test/resources/file.txt', "This is another test.");
          expect(
            fs.readFileSync("test/resources/file.txt", 'utf8')
          ).not.to.be( // notで否定をテストしている
            "This is a test."
          );
          /* #@range_end(fileio_destroys_referential_transparency) */
          next();
        });
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
      it('tapコンビネーター', (next) => {
        /* #@range_begin(tap_combinator) */
        var tap = (target) => {
          var original = target;
          return function doSideEffect(sideEffect) {
            sideEffect(target);
            expect(original).to.eql(target);
            return target;
          };
        };
        /* #@range_end(tap_combinator) */
        /* #@range_begin(tap_combinator_test) */
        var consoleSideEffect = (any) => {
          console.log(any);
        };
        var target = 1;
        expect(
          tap(target)(consoleSideEffect)
        ).to.eql(
          1
        );
        var updateSideEffect = (n) => {
          n = n + 1;
          return n;
        };
        expect(
          tap(target)(updateSideEffect)
        ).to.eql(
          target
        );
        /* #@range_end(tap_combinator_test) */
        next();
      });
    });
  }); // 関数の純粋性
});
