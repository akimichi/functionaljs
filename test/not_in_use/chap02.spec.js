      // it('配列の状態を表示する', (next) => {
      //   /* #@range_begin(array_destroys_referential_transparency_log) */
      //   var add = (n,m) => {
      //     return n + m;
      //   };
      //   var array = [];
      //   array.push(1);
      //   console.log(array); // [ 1 ]
      //   expect(
      //     add(array.pop(),2)
      //   ).to.eql(
      //     3
      //   );
      //   array.push(2);
      //   console.log(array); // [ 2 ]
      //   expect(
      //     add(array.pop(),2)
      //   ).to.eql(
      //     4
      //   );
      //   /* #@range_end(array_destroys_referential_transparency_log) */
      //   next();
      // });
    // it('参照不透明な関数では同値なものは置換できない', (next) => {
    //   /* #@range_begin(unequals_replacement)  */
    //   var wrapper = (f) => {
    //     return (args) => {
    //       return f.call(f, args);
    //     };
    //   };
    //   var now = (_) => {
    //     return Date.now();
    //   };
    //   var b = 1;
    //   var a = b;
    //   var aResult = wrapper(now)(a);
    //   /* #@range_end(unequals_replacement)  */
    //   next();
    // });
    // describe('参照透明な関数では同値なものは置換可能である', () => {
    //   it('同じ引数のsquare関数は置換可能である', (next) => {
    //     /* #@range_begin(equals_replacement)  */
    //     var square = (n) => {
    //       return n * n;
    //     };
    //     var b = 1;
    //     var a = b;
    //     expect(
    //       square(a)
    //     ).to.eql(
    //       square(b)
    //     );
    //     expect(
    //       square(b)
    //     ).to.eql(
    //       square(a)
    //     );
    //     expect(
    //       square(a)
    //     ).to.eql(
    //       square(a)
    //     );
    //     expect(
    //       square(b)
    //     ).to.eql(
    //       square(b)
    //     );
    //     /* #@range_end(equals_replacement)  */
    //     next();
    //   });
        // it('reduceを定義する', (next) => {
        //   /* #@range_begin(reduce_definition) */
        //   var reduce = (init,glue) => {
        //     return (array) => { // 関数が返る
        //       if(array.length === 0){
        //         return init;
        //       } else {
        //         var accumulator = glue(array[0], init);
        //         var tail = array.slice(1,array.length);
        //         return reduce(accumulator,glue)(tail);
        //       }
        //     };
        //   };
        //   /* #@range_end(reduce_definition) */
        //   /* #@range_begin(function_returning_function_test) */
        //   var adder = (x,y) => {
        //     return x + y;
        //   };
        //   var sum = reduce(0,adder);
        //   expect(
        //     sum([1,2,3,4])
        //   ).to.eql(
        //     10
        //   );
        //   /* #@range_end(function_returning_function_test) */
        //   next();
        // });
      // it('数学モジュールの例', (next) => {
      //   var math = {
      //     PI: 3.14
      //   };
      //   var area = (radius) => {
      //     return radius * radius * math.PI;
      //   };
      //   expect(
      //     area(1)
      //   ).to.eql(
      //     3.14
      //   );
      //   next();
      // });

        // var take = (n, astream) => {
        //   if(n === 1) {
        //     return astream[0];
        //   } else {
        //     return [astream[0]].concat(take(n-1, astream[1]()));
        //   }
        // };
        // var repeat = (n) => {
        //   return [n, (_) => {
        //     return repeat(n);
        //   }];
        // };
        // expect(
        //   take(3, repeat(2))
        // ).to.eql(
        //   [2,2,2]
        // );
        // var ones = repeat(1);
        // expect(
        //   take(2, ones)
        // ).to.eql(
        //   [1,1]
        // );
        // var replicate = (n, x) => {
        //   return take(n, repeat(x));
        // };
        // expect(
        //   replicate(4, 3)
        // ).to.eql(
        //   [3,3,3,3]
        // );
        // var upto = (m, n) => {
        //   if(m > n) {
        //     return [];
        //   } else {
        //     return [m, (_) => {
        //       return upto(m+1,n);
        //     }];
        //   };
        // };
        // expect(
        //   take(3, upto(2,10))
        // ).to.eql(
        //   [2,3,4]
        // );
        // var mod = (n,m) => {
        //   return n % m;
        // };
        // var even = (n) => {
        //   return mod(n,2) === 0;
        // };
        // var adder = (m) => {
        //   return (n) => {
        //     return m + n;
        //   };
        // };
        // var succ = adder(1);
        // var map = (aStream) => {
        //   return (transform) => {
        //     var head = aStream[0];
        //     return [transform(head), (_) => {
        //       return map(aStream[1]())(transform);
        //     }];
        //   };
        // };



      // describe('副作用を関数のスコープに閉じこめる', () => {
      //   /* #@range_begin(action) */
      //   var action = (io) => {
      //     return (_) => { // 入出力を関数で包み込む
      //       return io;
      //     };
      //   };
      //   /* #@range_end(action) */
      //   /* #@range_begin(reader_and_writer) */
      //   var fs = require('fs'); // ファイルを操作するライブラリーfsをロードする
      //   var read = (path) => { // ファイルを読み込む操作を関数で包みこむ
      //     return fs.readFileSync(path, 'utf8');
      //   };
      //   var write = (path, content) => { // ファイルを書き込む操作を関数で包みこむ
      //     return fs.writeFileSync(path,content);
      //   };
      //   var reader = (path) => { // ファイルを読み込む操作を関数で包みこむ
      //     return action(fs.readFileSync(path, 'utf8'));
      //   };
      //   var writer = (path, content) => { // ファイルを書き込む操作を関数で包みこむ
      //     return action(fs.writeFileSync(path,content));
      //   };
      //   /* #@range_end(reader_and_writer) */
      //   /* #@range_begin(fileio_actions) */
      //   var fileio_actions = () => {
      //     write('test/resources/test.txt', 1);
      //     read('test/resources/test.txt');
      //     write('test/resources/test.txt', 2);
      //     return read('test/resources/test.txt');
      //   };
      //   /* #@range_end(fileio_actions) */
      //   expect(
      //     fileio_actions()
      //   ).to.eql(
      //     2
      //   );
      //   expect(
      //     fileio_actions()
      //   ).to.eql(
      //     2
      //   );
      // });

    // describe('部品の独立性', () => {
      // var not = (predicate) => {
      //   return (arg) => {
      //     return ! predicate(arg);
      //   };
      // };
      // var array = {
      //   cons: (head, tail) => {
      //     return [head].concat(tail);
      //   },
      //   empty: (_) => {
      //     return [];
      //   },
      //   head: (anArray) => {
      //     return anArray[0];
      //   },
      //   tail: (anArray) => {
      //     return anArray.slice(1,array.length(anArray));
      //   },
      //   length: (anArray) => {
      //     return anArray.length;
      //   },
      //   isEmpty: (anArray) => {
      //     return array.length(anArray) === 0;
      //   },
      //   fromString: (str) => {
      //     if(string.isEmpty(str)) {
      //       return array.empty();
      //     } else {
      //       return array.cons(string.head(str), 
      //                         array.fromString(string.tail(str)));
      //     }
      //   },
      //   takeWhile: (anArray) => {
      //     return (predicate) => {
      //       if(array.isEmpty(anArray)){
      //         return array.empty(); 
      //       } else {
      //         var head = array.head(anArray);
      //         var tail = array.tail(anArray);
      //         if(predicate(head) === true) {
      //           return array.cons(head,
      //                             array.takeWhile(tail)(predicate));
      //         } else {
      //           return array.empty();
      //         }
      //       }
      //     };
      //   },
      //   dropWhile: (anArray) => {
      //     return (predicate) => {
      //       if(array.isEmpty(anArray)){
      //         return [];
      //       } else {
      //         var head = array.head(anArray);
      //         var tail = array.tail(anArray);
      //         if(predicate(head) === true) {
      //           return array.dropWhile(tail)(predicate);
      //         } else {
      //           return anArray;
      //         }
      //       };
      //     };
      //   },
      //   span: (anArray) => {
      //     return (predicate) => {
      //       if(array.isEmpty(anArray)){
      //         return [];
      //       } else {
      //         var head = array.head(anArray);
      //         var tail = array.tail(anArray);
      //         return [array.takeWhile(anArray)(predicate),
      //                 array.dropWhile(anArray)(predicate)];
      //       };
      //     };
      //   },
      //   break: (anArray) => {
      //     return (predicate) => {
      //       return array.span(anArray)(not(predicate));
      //     };
      //   },  
      //   lines: (xs) => {
      //     var isNewline = (ch) => {
      //       return ch === '\n';
      //     };
      //     var apair = array.break(xs)(isNewline);
      //     var ys = apair[0];
      //     var zs = apair[1];

      //     if(array.isEmpty(zs)){
      //       return [ys];
      //     } else {
      //       var head = array.head(zs);
      //       var tail = array.tail(zs);
      //       return array.cons(ys, array.lines(tail));
      //     };
      //   }
      // };
      // var string = {
      //   head: (str) => {
      //     return str[0];
      //   },
      //   tail: (str) => {
      //     return str.substring(1);
      //   },
      //   isEmpty: (str) => {
      //     return str.length === 0;
      //   },
      //   add: (strL, strR) => {
      //     return strL + strR;
      //   },
      //   toArray: (str) => {
      //     if(string.isEmpty(str)) {
      //       return [];
      //     } else {
      //       return array.cons(string.head(str),
      //                         string.toArray(string.tail(str)));
      //     }
      //   },
      //   fromArray: (anArray) => {
      //     return anArray.reduce((accumulator, item) => {
      //       return string.add(accumulator, item);
      //     }, "");
      //   },
      //   lines: (str) => {
      //     var isNewline = (ch) => {
      //       return ch === '\n';
      //     };
      //     var apair = array.break(array.fromString(str))(isNewline);
      //     var ys = apair[0];
      //     var zs = apair[1];

      //     if(array.isEmpty(zs)){
      //       return [string.fromArray(ys)];
      //     } else {
      //       var tail = array.tail(zs);
      //       return array.cons(string.fromArray(ys), 
      //                         string.lines(string.fromArray(tail)));
      //     };
      //   }
      // };
      // describe('array', () => {
      //   it('array#head', (next) => {
      //     expect(
      //       array.head([0,1,2])
      //     ).to.eql(
      //       0
      //     );
      //     next();
      //   });
      //   it('array#tail', (next) => {
      //     expect(
      //       array.tail([0,1,2])
      //     ).to.eql(
      //       [1,2]
      //     );
      //     next();
      //   });
      //   it('array#cons', (next) => {
      //     expect(
      //       array.cons(0,[1,2])
      //     ).to.eql(
      //       [0,1,2]
      //     );
      //     next();
      //   });
      //   it('array#fromString', (next) => {
      //     expect(
      //       array.fromString("123")
      //     ).to.eql(
      //       [1,2,3]
      //     );
      //     next();
      //   });
      //   it('array#takeWhile', (next) => {
      //     var theArray = [1,2,3]; 
      //     var even = (n) => {
      //       return 0 === (n % 2);
      //     };
      //     var odd = not(even);
      //     expect(
      //       array.takeWhile(theArray)(odd)
      //     ).to.eql(
      //       [1]
      //     );
      //     next();
      //   });
      //   it('array#dropWhile', (next) => {
      //     var theArray = [1,2,3]; 
      //     var even = (n) => {
      //       return 0 === (n % 2);
      //     };
      //     var odd = not(even);
      //     expect(
      //       array.dropWhile(theArray)(odd)
      //     ).to.eql(
      //       [2,3]
      //     );
      //     next();
      //   });
      //   it('array#span', (next) => {
      //     var theArray = [1,2,3]; 
      //     var even = (n) => {
      //       return 0 === (n % 2);
      //     };
      //     expect(
      //       array.span(theArray)(even)
      //     ).to.eql(
      //       [[],[1,2,3]]
      //     );
      //     var odd = not(even);
      //     expect(
      //       array.span(theArray)(odd)
      //     ).to.eql(
      //       [[1],[2,3]]
      //     );
      //     next();
      //   });
      //   it('array#break', (next) => {
      //     var theArray = [1,2,3]; 
      //     var even = (n) => {
      //       return 0 === (n % 2);
      //     };
      //     expect(
      //       array.break(theArray)(even)
      //     ).to.eql(
      //       [[1],[2,3]]
      //     );
      //     var odd = not(even);
      //     expect(
      //       array.break(theArray)(odd)
      //     ).to.eql(
      //       [[],[1,2,3]]
      //     );
      //     var isNewline = (ch) => {
      //       return ch === '\n';
      //     };
      //     expect(
      //       array.break(['a','b','c','\n','d','e','f'])(isNewline)
      //     ).to.eql(
      //       [['a','b','c'],['\n','d','e','f']]
      //     );
      //     next();
      //   });
      //   it('array#lines', (next) => {
      //     var theArray = array.fromString("abc\ndef"); 
      //     expect(
      //       array.lines(theArray)
      //     ).to.eql(
      //       [ [ 'a', 'b', 'c' ], [ 'd', 'e', 'f' ] ]
      //     );
      //     next();
      //   });
      // });
      // describe('string', () => {
      //   it('string#head', (next) => {
      //     expect(
      //       string.head("abc")
      //     ).to.eql(
      //       'a'
      //     );
      //     next();
      //   });
      //   it('string#tail', (next) => {
      //     expect(
      //       string.tail("abc")
      //     ).to.eql(
      //       "bc"
      //     );
      //     next();
      //   });
      //   it('string#fromArray', (next) => {
      //     expect(
      //       string.fromArray(['a','b','c'])
      //     ).to.eql(
      //       "abc"
      //     );
      //     next();
      //   });
      //   it('string#toArray', (next) => {
      //     expect(
      //       string.toArray("abc")
      //     ).to.eql(
      //       ['a','b','c']
      //     );
      //     next();
      //   });
      //   it('string#lines', (next) => {
      //     expect(
      //       string.lines("abc\ndef")
      //     ).to.eql(
      //       [ 'abc', 'def' ]
      //     );
      //     next();
      //   });
    //   });
    // });
    // it('統一的なインターフェイス', (next) => {
    //   /* #@range_begin(books_as_array) */
    //   var books = [
    //     {name: "こころ", author: ["夏目漱石"], genre: "文学"},
    //     {name: "夢十夜", author: ["夏目漱石"], genre: "文学"},
    //     {name: "ソクラテスの弁明", author: ["プラトン"], genre: "哲学"},
    //     {name: "国家", author: ["プラトン"], genre: "哲学"},
    //     {name: "プログラミング言語C", author: ["カーニハン","リッチー"], genre: "コンピュータ"},

    //     {name: "計算機プログラムの構造と解釈", author: ["サスマン","エイベルソン"], genre: "コンピュータ"},
    //   ];
    //   /* #@range_end(books_as_array) */
    //   var get = (object) => {
    //     return (key) => {
    //       return object[key];
    //     };
    //   };
    //   /* #@range_begin(pluck) */
    //   var pluck = (key) => {
    //     return (object) => {
    //       return object[key];
    //     };
    //   };
    //   /* #@range_end(pluck) */
    //   /* #@range_begin(mapWith) */
    //   var mapWith = (func) => {
    //     return (array) => {
    //       return array.map(func);
    //     };
    //   };
    //   /* #@range_end(mapWith) */
    //   var multiplier = (n) => {
    //     return (m) => {
    //       return n * m;
    //     };
    //   };
    //   var square = (n) => {
    //     return multiplier(n)(n);
    //   };
    //   expect(
    //     mapWith(square)([1,2,3])
    //   ).to.eql(
    //     [1,4,9]
    //   );
    //   expect(
    //     /* #@range_begin(mapWith_pluck) */
    //     mapWith(pluck("name"))(books)
    //     /* #@range_end(mapWith_pluck) */
    //   ).to.eql(
    //     ['こころ','夢十夜','ソクラテスの弁明','国家','プログラミング言語C','計算機プログラムの構造と解釈']
    //   );
    //   /* #@range_begin(filterWith) */
    //   var filterWith = (predicate) => {
    //     return (array) => {
    //       return array.filter(predicate);
    //     };
    //   };
    //   /* #@range_end(filterWith) */
    //   var isEqual = (a,b) => {
    //     return a === b;
    //   };
    //   expect(
    //     /* #@range_begin(filterWith_pluck) */
    //     filterWith((book) => { 
    //       return pluck("genre")(book) ===  "文学";
    //     })(books)
    //     /* #@range_end(filterWith_pluck) */
    //   ).to.eql(
    //     /* #@range_begin(filterWith_pluck_result) */
    //     [
    //       {name: "こころ", author: ["夏目漱石"], genre: "文学"},
    //       {name: "夢十夜", author: ["夏目漱石"], genre: "文学"},
    //     ]
    //     /* #@range_end(filterWith_pluck_result) */
    //   );
    //   expect(
    //     /* #@range_begin(map_filter) */
    //     mapWith(pluck("name"))(filterWith((book) => { 
    //       return pluck("genre")(book) ===  "哲学";
    //     })(books))
    //     /* #@range_end(map_filter) */
    //   ).to.eql(
    //     /* #@range_begin(map_filter_result) */
    //     ["ソクラテスの弁明", "国家"]
    //     /* #@range_end(map_filter_result) */
    //   );
      
    //   /* #@range_begin(doesContain) */
    //   var doesContain = (value) => {
    //     return (array) => {
    //       return array.reduce((accumulator, item) => {
    //         return accumulator || (item === value);
    //       },false);
    //     };
    //   };
    //   /* #@range_end(doesContain) */
    //   var doesMatch = (predicate) => {
    //     return (array) => {
    //       return array.reduce((accumulator, item) => {
    //         return accumulator || predicate(item);
    //       },false);
    //     };
    //   };
    //   expect(
    //     /* #@range_begin(filterWith_doesContain) */
    //     filterWith((book) => { 
    //       return doesContain("カーニハン")(pluck("author")(book));
    //     })(books)
    //     /* #@range_end(filterWith_doesContain) */
    //   ).to.eql(
    //     /* #@range_begin(filterWith_doesContain_result) */
    //     [
    //       {name: "プログラミング言語C", author: ["カーニハン","リッチー"], genre: "コンピュータ"},
    //     ]
    //     /* #@range_end(filterWith_doesContain_result) */
    //   );
    //   /* #@range_begin(findWith) */
    //   var findWith = (predicate) => {
    //     return (array) => {
    //       return array.filter(predicate)[0];
    //     };
    //   };
    //   /* #@range_end(findWith) */
    //   expect(
    //     findWith((book) => { 
    //       return pluck("genre")(book) === "哲学";
    //     })(books)
    //   ).to.eql(
    //     {name: "ソクラテスの弁明", author: ["プラトン"], genre: "哲学"}
    //   );
    //   next();
    // });

      expect(
        birthYear(birthday)
      ).to.eql(
        1999
      );
describe('副作用の種類', () => {
  describe('副作用としての代入', () => {
    it('配列は参照透明性を破壊する', (next) => {
      /* #@range_begin(array_destroys_referential_transparency) */
      var array = [];
      array.push(1);
      expect(
        array.pop()
      ).to.eql(
        1
      );
      array.push(2);
      expect(
        array.pop()
      ).to.eql(
        2
      );
      /* #@range_end(array_destroys_referential_transparency) */
      next();
    });
  });
});
        it('画面出力を分離する', (next) => {
          /* #@range_begin(tap_console_log) */
          var tap = (target, sideEffect) => {
            sideEffect(target);
            return target;
          };
          var logger = (value) =>{
            console.log(value);
          };
          /* #@range_end(tap_console_log) */
          next();
        });
        // var constant = (any) => {
        //   return (_) => {
        //     return any;
        //   };
        // };
        // var alwaysOne = constant(1); 
      it('allの定義', (next) => {
        /* #@range_begin(all_in_array_reduce) */
        var all = (array) => {
          return array.reduce((accumulator, item) => {
            return accumulator && item; /* 論理和を実行する */
          }); /* 第2引数を指定していない場合は、
                 配列の先頭要素が変数accumulatorの初期値になる */
        };
        /* #@range_end(all_in_array_reduce)  */
        expect(
          all([true,true,true])
        ).to.eql(
          true
        );
        expect(
          /* #@range_begin(all_in_array_reduce_test) */
          all([true,false,true])
          /* #@range_end(all_in_array_reduce_test) */
        ).to.eql(
          /* #@range_begin(all_in_array_reduce_test_result) */
          false
          /* #@range_end(all_in_array_reduce_test_result) */
        );
        next();
      });
      it('maxの定義', (next) => {
        /* #@range_begin(max_in_array_reduce) */
        var max = (array) => {
          var bigger = (n,m) => {
            if(n > m) {
              return n;
            } else {
              return m;
            };
          };
          return array.reduce((accumulator, item) => {
            return bigger(accumulator, item);
          });
        };
        /* #@range_end(max_in_array_reduce)  */
        expect(
          max([1,2,3,4])
        ).to.eql(
          4
        );
        expect(
          max([1,-2,3,-4])
        ).to.eql(
          3
        );
        next();
      });
      it('reverseの定義', (next) => {
        /* #@range_begin(reverse_in_array_reduce) */
        var reverse = (array) => {
          return array.reduce((accumulator, currentValue) => {
            return [currentValue].concat(accumulator);
          },[]);
        };
        expect(reverse([1,2,3,4])).to.eql([4,3,2,1]);
        /* #@range_end(reverse_in_array_reduce)  */
        next();
      });
      it('reduceによるfilterの定義', (next) => {
        var even = (n) => {
          return n % 2 === 0;
        };
        /* #@range_begin(filter_in_array_reduce) */
        var filter = (array) => {
          return (predicate) => {
            return array.reduce((accumulator, item) => {
              if(predicate(item)) {
                return accumulator.concat(item);
              } else {
                return accumulator;
              }
            },[]); // 蓄積変数の初期値として空の配列[]を指定する
          };
        };
        /* #@range_end(filter_in_array_reduce)  */
        expect(
          filter([1,2,3])(even)
        ).to.eql(
          [2]
        );
        next();
      });
          // /* #@range_begin(flip_definition) */
          // var flip = (fun) => {
          //   return  (x) => {
          //     return (y) => {
          //       return fun(y)(x);
          //     };
          //   };
          // };
          // /* #@range_end(flip_definition) */

            var ones = [1, (_) => {
              return ones;
            }];
            expect(
              ones[0]
            ).to.eql(
              1
            );
            expect(
              ones[1]()[0]
            ).to.eql(
              1
            );
            it('ストリームのフィルタリング', (next) => {
              var filter = (predicate) => {
                return (aStream) => {
                  var head = aStream[0]; // ストリームの先頭要素を取り出す
                  if(predicate(head) === true) { // 先頭要素headが条件に合致している場合
                    return [head, (_) => {
                      return filter(predicate)(aStream[1]());
                    }];
                  } else {                       // 先頭要素headが条件に合致していない場合
                    return filter(predicate)(aStream[1]());
                  }
                };
              };
              var multipleOf = (n) => {
                return (m) => {
                  if((n % m) === 0) {
                    return true;
                  } else {
                    return false;
                  }
                };
              };
              /* #@range_begin(odd_stream) */
              var odd = (n) => {
                return (n % 2) === 1; 
              };
              var oddStream = filter(odd)(enumFrom(1));
              /* #@range_end(odd_stream) */
              expect(
                take(5,filter(odd)(enumFrom(1)))
              ).to.eql(
                [ 1, 3, 5, 7, 9 ] 
              );
              next();
            });

            /* #@range_begin(oddStream_from_iterate) */
            var twoStep = (n) => {
              return n + 2;
            };
            var oddStream = iterate(1)(twoStep);
            /* #@range_end(oddStream_from_iterate) */
            expect(
              take(3,oddStream)
            ).to.eql(
              [1,3,5]
            );
            expect(
              // 3番目の偶数を求める
              /* #@range_begin(third_element_of_odd_stream) */
              elemAt(3)(oddStream)
              /* #@range_end(third_element_of_odd_stream) */
            ).to.eql(
              /* #@range_begin(third_element_of_odd_stream_result) */
              5
              /* #@range_end(third_element_of_odd_stream_result) */
            );
            var even = (n) => {
              var mod = (n,m) => {
                return n % m;
              };
              return mod(n,2) === 0;
            };
            expect(
              take(3,filter(even)(enumFrom(2)))
            ).to.eql(
              [2,4,6]
            );
          expect(
            /* #@range_begin(third_element_of_evenArray) */
            elemAt(3)([2,4,6])
            /* #@range_end(third_element_of_evenArray) */
          ).to.eql(
            /* #@range_begin(third_element_of_eventArray_result) */
            6
            /* #@range_end(third_element_of_eventArray_result) */
          );

    it('参照透過性のあるコードはテストが容易である', (next) => {
      var adder = (m) => {
        return (n) => {
          return m + n;
        };
      };
      expect(
        adder(1)(2)
      ).to.eql(
        3
      );
      next();
    });

        // var elemAt = (n) => {
        //   return (aStream) => {
        //     if(n === 1) {
        //       return aStream[0];
        //     } else {
        //       return elemAt(n-1)(aStream[1]());
        //     };
        //   };
        // };
        // var scanl = (aStream) => {
        //   return (glue) => {
        //     var out = [aStream[0], (_) => {
        //       return zipWith(glue)(aStream[1](), out);
        //     }];
        //     return out;
        //   };
        // };
        // var conjunction = (x,y) => {
        //   return x && y;  // 論理和をとる
        // };
        // var zipWith = (glue) => {
        //   return (xs, ys) => {
        //     if(xs.length === 0) {
        //       return []; 
        //     } 
        //     if(ys.length === 0) {
        //       return []; 
        //     } 
        //     return [glue(xs[0], ys[0]), (_) => {
        //       return zipWith(glue)(xs[1](), ys[1]());
        //     }];
        //   };
        // };
        // var listAnd = (aStream) => {
        //   var out = [aStream[0], (_) => {
        //     return zipWith((x,y) => { 
        //       return x && y;
        //     })(aStream[1](), out);
        //   }];
        //   return out;
        // };
      // var take = (n) => {
      //   return (aStream) => {
      //     if(n === 0) {
      //       return [];
      //     } else {
      //       return [aStream[0]].concat(take(n-1)(aStream[1]()));
      //     }
      //   };
      // };
      // var filter = (predicate) => {
      //   return (aStream) => {
      //     var head = aStream[0];
      //     if(predicate(head) === true) {
      //       return [head, (_) => {
      //         return filter(predicate)(aStream[1]());
      //       }];
      //     } else {
      //       return filter(predicate)(aStream[1]());
      //     }
      //   };
      // };

      // ~~~
      // node> var one = 1;
      // node> var ichi = one;
      // node> ichi === one;
      // true
      // node> one = 2;
      // 2
      // node> ichi === one;
      // false
      // ~~~
