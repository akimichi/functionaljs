"use strict";

// 第2章 なぜ関数型プログラミングが重要か
// ========

var expect = require('expect.js');

var compose = (f,g) => {
  return (arg) => {
    return f(g(arg));
  };
};

// ## 2.2 関数型プログラミングの特徴
describe('関数型プログラミングの特徴', () => {
  // ### ファーストクラスオブジェクトとしての関数
  describe('ファーストクラスオブジェクトとしての関数', () => {
    it('数値はファーストクラスオブジェクトである', (next) => {
      /*  #@range_begin(number_as_first_class_citizen) */
      // 値を変数にバインドする
      var zero = 0;
      var name = "Haskell Curry";
      // 値をデータ構造に埋めこむ
      var birthday = {
        year: 1999,
        month: 1,
        day: 12
      };
      // 関数から値を返す
      var birthYear = (birthdayObject) => {
        return birthdayObject.year;
      };
      /* #@range_end(number_as_first_class_citizen) */
      // 値を関数に渡す
      expect(
        Math.sqrt(1)
      ).to.eql(
        1
      );
      expect(
        birthYear(birthday)
      ).to.eql(
        1999
      );
      next();
    });
    it('関数は変数にバインドできる', (next) => {
      /* #@range_begin(function_bound_to_variable) */
      var succ = (n) => {
        return n + 1;
      };
      /* #@range_end(function_bound_to_variable) */
      expect(
        succ(1) // 変数succを用いてλ式を呼びだす
      ).to.eql(
        2
      );
      next();
    });
    it('関数をオブジェクトに埋めこむ', (next) => {
      /* #@range_begin(function_embedded_in_object) */
      var math = {
        add: (n,m) => {
          return n + m;
        }
      };
      expect(
        math.add(1,2)
      ).to.eql(
        3
      );
      /* #@range_end(function_embedded_in_object) */
      next();
    });
    // ### 高階関数
    describe('高階関数', () => {
      // forEach文によるsumの定義
      it('forEach文によるsumの定義', (next) => {
        /* #@range_begin(sum_forEach) */
        var sum = (array) => {
          var result = 0;
          array.forEach((item) => { // forEachに関数を渡す
            result = result + item;
          });
          return result;
        };
        /* #@range_end(sum_forEach)  */
        expect(
          sum([1,2,3,4])
        ).to.eql(
          10
        );
        next();
      });
      describe('関数を返す', () => {
        // adderを定義する
        it('adderを定義する', (next) => {
          /* #@range_begin(adder_definition) */
          var adder = (n) => {
            return (m) => { // 関数を返す
              return n + m;
            };
          };
          /* #@range_end(adder_definition) */
          var succ = adder(1);
          expect(
            succ(0)
          ).to.eql(
            1
          );
          next();
        });
        it('reduceを定義する', (next) => {
          /* #@range_begin(reduce_definition) */
          var reduce = (init,glue) => {
            return (array) => { // 関数が返る
              if(array.length === 0){
                return init;
              } else {
                var accumulator = glue(array[0], init);
                var tail = array.slice(1,array.length);
                return reduce(accumulator,glue)(tail);
              }
            };
          };
          /* #@range_end(reduce_definition) */
          /* #@range_begin(function_returning_function_test) */
          var adder = (x,y) => {
            return x + y;
          };
          var sum = reduce(0,adder);
          expect(
            sum([1,2,3,4])
          ).to.eql(
            10
          );
          /* #@range_end(function_returning_function_test) */
          next();
        });
      });
    });
  });
});
// ## 参照透過性
describe('参照透過性', () => {
  // ### 参照透明性が成立する場面
  describe('参照透明性が成立する場面', () => {
    // #### 値の参照透過性
    it('値の参照透過性', (next) => {
      /* #@range_begin(number_has_referential_transparency) */
      expect(
        2
      ).to.eql(
        2
      );
      /* #@range_end(number_has_referential_transparency) */
      next();
    });
    // #### 変数の参照透過性
    describe('変数の参照透明性', () => {
      it('変数が参照透明性を持つ場合', (next) => {
        /* #@range_begin(variable_is_referential_transparent) */
        var x = 1;
        expect(
          x 
        ).to.eql(
          x
        );
        var y = x;
        expect(
          y
        ).to.eql(
          1
        );
        /* #@range_end(variable_is_referential_transparent) */
        next();
      });
      it('変数は参照透明性を持つとは限らない', (next) => {
        /* #@range_begin(variable_isnt_referential_transparent) */
        var foo = 2;
        var bar = foo;
        foo = 3;
        expect(
          foo
        ).to.not.eql(
          bar
        );
        /* #@range_end(variable_isnt_referential_transparent) */
        next();
      });
    });
    // #### 関数の参照透過性
    describe('関数の参照透明性', () => {
      // succ関数は参照透明性を持つ
      it('succ関数は参照透明性を持つ', (next) => {
        var succ = (x) => {
          return x + 1;
        };
        expect(
          succ(1)
        ).to.eql(
          succ(1)
        );
        expect(
          succ(1) // 1回目の実行
        ).to.eql(
          2
        );
        expect(
          succ(1) // 2回目の実行
        ).to.eql(
          2
        );
        next();
      });
    });
    describe('参照透明な関数では同値なものは置換可能である', () => {
      it('同じ引数のsquare関数は置換可能である', (next) => {
        /* #@range_begin(equals_replacement)  */
        var square = (n) => {
          return n * n;
        };
        var b = 1;
        var a = b;
        expect(
          square(a)
        ).to.eql(
          square(b)
        );
        expect(
          square(b)
        ).to.eql(
          square(a)
        );
        expect(
          square(a)
        ).to.eql(
          square(a)
        );
        expect(
          square(b)
        ).to.eql(
          square(b)
        );
        /* #@range_end(equals_replacement)  */
        next();
      });
    });
  });
  // ### 参照透過性を破壊するもの
  describe('参照透過性を破壊するもの', () => {
    // 可変なデータは参照透明性を破壊する
    // ~~~
    // node> var array = [1];
    // node> array
    // [ 1 ]
    // node> array.push(2);
    // 2
    // node> array
    // [ 1, 2 ]
    // ~~~
    it('可変なデータは参照透明性を破壊する', (next) => {
      var array = [1];
      expect(
        array
      ).to.eql(
        [1]
      );
      array.push(2);
      expect(
        array
      ).to.eql(
        [1,2]
      );
      next();
    });
    // 代入操作は参照透明性を破壊する
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
    it('代入操作は参照透明性を破壊する', (next) => {
      /* #@range_begin(assignment_breaks_referential_transparency) */
      var x = 0;
      var add = (y) => {
        x = x + 1; // 代入で変数を更新する
        return x + y;
      };
      /* #@range_end(assignment_breaks_referential_transparency)  */
      expect(
        add(1)
      ).to.eql(
        2
      );
      next();
    });
    it('参照不透明な関数では同値なものは置換できない', (next) => {
      /* #@range_begin(unequals_replacement)  */
      var wrapper = (f) => {
        return (args) => {
          return f.call(f, args);
        };
      };
      var now = (_) => {
        return Date.now();
      };
      var b = 1;
      var a = b;
      var aResult = wrapper(now)(a);
      /* #@range_end(unequals_replacement)  */
      next();
    });
    it('Date.now関数は参照透明性を持たない', (next) => {
      /* #@range_begin(datenow_is_not_transparent) */
      var a = Date.now();
      /* 時間を1秒進める */
      var sleep = require('sleep-async')();
      sleep.sleep(2000, () => {
        expect(
          a
        ).to.not.eql( /* 等しくないことをテストしている */
          Date.now()
        );
      });
      /* #@range_end(datenow_is_not_transparent) */
      next();
    });
    // 命令型な階乗関数
    it('命令型な階乗関数', (next) => {
      /* #@range_begin(imperative_factorial) */
      var factorial = (n) => {
        /* 変数resultに結果が入る */
        var result = 1;             
        /* 変数timesは反復の回数を数える */
        var times = 1;              
        /* while文は反復を処理する */
        while(times < n + 1) {      
          /* 変数resultを代入で更新する */
          result = result * times;  
          /* 変数timesを代入で更新する */
          times = times + 1;        
        }
        return result;
      };
      /* #@range_end(imperative_factorial) */
      expect(
        factorial(2)
      ).to.eql(
        2
      );
      expect(
        factorial(3)
      ).to.eql(
        6
      );
      expect(
        factorial(4)
      ).to.eql(
        24
      );
      next();
    });
  });
  // 副作用と入出力
  describe('副作用と入出力', () => {
    it('ファイル入出力が参照透明性を破壊する例', (next) => {
      /* #@range_begin(fileio_breaks_referential_transparency)  */
      var fs = require('fs');
      var read = (path) => {
        var readValue = fs.readFileSync(path);
        return parseInt(readValue);
      };
      var write = (path, n) => {
        fs.writeFileSync(path, n);
        return n;
      };
      /* #@range_end(fileio_breaks_referential_transparency)  */
      write('test/resources/io.txt', 1);
      expect(
        read('test/resources/io.txt')
      ).to.eql(
        1
      );
      write('test/resources/io.txt', 2); // ここでファイルに値を書きこむ
      expect(
        read('test/resources/io.txt')
      ).to.eql(
        2
      );
      next();
    });
  });
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
      it('配列の状態を表示する', (next) => {
        /* #@range_begin(array_destroys_referential_transparency_log) */
        var add = (n,m) => {
          return n + m;
        };
        var array = [];
        array.push(1);
        console.log(array); // [ 1 ]
        expect(
          add(array.pop(),2)
        ).to.eql(
          3
        );
        array.push(2);
        console.log(array); // [ 2 ]
        expect(
          add(array.pop(),2)
        ).to.eql(
          4
        );
        /* #@range_end(array_destroys_referential_transparency_log) */
        next();
      });
    });
  });
  // ### 参照透明性を保証する
  describe('参照透明性を保証する', () => {
    // #### 値の参照透過性を保証する（可変なデータの排除）
    describe('値の参照透過性を保証する（可変なデータの排除）', () =>  {
      it('不変なデータ型', (next) => {
        /* #@range_begin(immutable_datatype) */
        var empty =  (_) => {
          return null;
        };
        var get = (key, obj) => {
          return obj(key);
        };
        var set = (key, value, obj) => {
          return (key2) => {
            if(key === key2) {
              return value;
            } else {
              return get(key2,obj);
            }
          };
        };
        /* #@range_end(immutable_datatype) */
        next();
      });
    });
    // #### 変数の参照透過性を保証する(代入の排除)
    describe('変数の参照透過性を保証する(代入の排除)', () => {
      it('命令型プログラミングによる乗算', (next) => {
        /* #@range_begin(imperative_addition) */
        var add = (x,y) => {
          var times = 0;          
          var result = x;         

          /* while文で反復を処理する */
          while(times < y){       
            result = result + 1;
            times = times + 1;    
          };
          return result;
        };
        /* #@range_end(imperative_addition) */
        expect(
          add(2,3)
        ).to.eql(
          5
        );
        var x = 4;
        var y = 5;
        expect(
          add(x,y)
        ).to.eql(
          9
        );
        expect(
          x
        ).to.eql(
          4
        );
        next();
      });
      it('関数型プログラミングによる乗算', (next) => {
        /* #@range_begin(functional_addition) */
        var add = (x,y) => {
          if(y < 1){
            return x;
          } else {
            /* 新しい引数でadd関数を再帰的に呼び出す */
            return add(x + 1, y - 1); 
          }
        };
        /* #@range_end(functional_addition) */
        expect(add(2,1)).to.eql(3);
        expect(add(2,2)).to.eql(4);
        expect(add(3,2)).to.eql(5);
        expect(add(12,5)).to.eql(17);
        next();
      });
    });
    // #### 関数の参照透過性を保証する（副作用の分離）
    describe('関数の参照透過性を保証する（副作用の分離）', () =>  {
      it('副作用が分離されていないコード', (next) => {
        /* #@range_begin(age_sideeffect) */
        var age = (birthYear) => {
          /* todayは現時点の日付データ */
          var today = new Date();
          /* getFullYear関数は日付データにもとづいて現時点の西暦を返す */
          var thisYear = today.getFullYear(); 
          return thisYear - birthYear;
        };
        /* #@range_end(age_sideeffect) */
        next();
      });
      it('副作用が分離されているコード', (next) => {
        /* #@range_begin(age_without_sideeffect) */
        var age = (birthYear, thisYear) => {
          return thisYear - birthYear;
        };
        /* #@range_end(age_without_sideeffect) */
        next();
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
      describe('副作用を関数のスコープに閉じこめる', () => {
        /* #@range_begin(action) */
        var action = (io) => {
          return (_) => { // 入出力を関数で包み込む
            return io;
          };
        };
        /* #@range_end(action) */
        /* #@range_begin(reader_and_writer) */
        var fs = require('fs'); // ファイルを操作するライブラリーfsをロードする
        var read = (path) => { // ファイルを読み込む操作を関数で包みこむ
          return fs.readFileSync(path, 'utf8');
        };
        var write = (path, content) => { // ファイルを書き込む操作を関数で包みこむ
          return fs.writeFileSync(path,content);
        };
        var reader = (path) => { // ファイルを読み込む操作を関数で包みこむ
          return action(fs.readFileSync(path, 'utf8'));
        };
        var writer = (path, content) => { // ファイルを書き込む操作を関数で包みこむ
          return action(fs.writeFileSync(path,content));
        };
        /* #@range_end(reader_and_writer) */
        /* #@range_begin(fileio_actions) */
        var fileio_actions = () => {
          write('test/resources/test.txt', 1);
          read('test/resources/test.txt');
          write('test/resources/test.txt', 2);
          return read('test/resources/test.txt');
        };
        /* #@range_end(fileio_actions) */
        expect(
          fileio_actions()
        ).to.eql(
          2
        );
        expect(
          fileio_actions()
        ).to.eql(
          2
        );
      });
    });
  });
});
// ## 関数型プログラミングの利点
describe('関数型プログラミングの利点', () => {
  // ### 高いモジュール性
  describe('モジュール性とは何か', () => {
    // it('名前空間としてのモジュール', (next) => {
    //   /* #@range_begin(module_as_namespace) */
    //   /* 数値計算のモジュール */
    //   var math = {
    //     add: (n, m) => { // 数値の足し算
    //       return n + m;
    //     },
    //     multiply: (n, m) => {
    //       return n * m;
    //     }
    //   };
    //   /* 文字列操作のモジュール */
    //   var string = {
    //     add: (strL, strR) => { // 文字列の連結
    //       return strL.concat(strR);
    //     },
    //     multiply: (str, nTimes) => {
    //       //
    //     }
    //   };
    //   /* #@range_end(module_as_namespace) */
    //   next();
    // });
    // #### 部品の独立性
    describe('部品の独立性', () => {
      var not = (predicate) => {
        return (arg) => {
          return ! predicate(arg);
        };
      };
      var array = {
        cons: (head, tail) => {
          return [head].concat(tail);
        },
        empty: (_) => {
          return [];
        },
        head: (anArray) => {
          return anArray[0];
        },
        tail: (anArray) => {
          return anArray.slice(1,array.length(anArray));
        },
        length: (anArray) => {
          return anArray.length;
        },
        isEmpty: (anArray) => {
          return array.length(anArray) === 0;
        },
        fromString: (str) => {
          if(string.isEmpty(str)) {
            return array.empty();
          } else {
            return array.cons(string.head(str), 
                              array.fromString(string.tail(str)));
          }
        },
        takeWhile: (anArray) => {
          return (predicate) => {
            if(array.isEmpty(anArray)){
              return array.empty(); 
            } else {
              var head = array.head(anArray);
              var tail = array.tail(anArray);
              if(predicate(head) === true) {
                return array.cons(head,
                                  array.takeWhile(tail)(predicate));
              } else {
                return array.empty();
              }
            }
          };
        },
        dropWhile: (anArray) => {
          return (predicate) => {
            if(array.isEmpty(anArray)){
              return [];
            } else {
              var head = array.head(anArray);
              var tail = array.tail(anArray);
              if(predicate(head) === true) {
                return array.dropWhile(tail)(predicate);
              } else {
                return anArray;
              }
            };
          };
        },
        span: (anArray) => {
          return (predicate) => {
            if(array.isEmpty(anArray)){
              return [];
            } else {
              var head = array.head(anArray);
              var tail = array.tail(anArray);
              return [array.takeWhile(anArray)(predicate),
                      array.dropWhile(anArray)(predicate)];
            };
          };
        },
        break: (anArray) => {
          return (predicate) => {
            return array.span(anArray)(not(predicate));
          };
        },  
        lines: (xs) => {
          var isNewline = (ch) => {
            return ch === '\n';
          };
          var apair = array.break(xs)(isNewline);
          var ys = apair[0];
          var zs = apair[1];

          if(array.isEmpty(zs)){
            return [ys];
          } else {
            var head = array.head(zs);
            var tail = array.tail(zs);
            return array.cons(ys, array.lines(tail));
          };
        }
      };
      var string = {
        head: (str) => {
          return str[0];
        },
        tail: (str) => {
          return str.substring(1);
        },
        isEmpty: (str) => {
          return str.length === 0;
        },
        add: (strL, strR) => {
          return strL + strR;
        },
        toArray: (str) => {
          if(string.isEmpty(str)) {
            return [];
          } else {
            return array.cons(string.head(str),
                              string.toArray(string.tail(str)));
          }
        },
        fromArray: (anArray) => {
          return anArray.reduce((accumulator, item) => {
            return string.add(accumulator, item);
          }, "");
        },
        lines: (str) => {
          var isNewline = (ch) => {
            return ch === '\n';
          };
          var apair = array.break(array.fromString(str))(isNewline);
          var ys = apair[0];
          var zs = apair[1];

          if(array.isEmpty(zs)){
            return [string.fromArray(ys)];
          } else {
            var tail = array.tail(zs);
            return array.cons(string.fromArray(ys), 
                              string.lines(string.fromArray(tail)));
          };
        }
      };
      describe('array', () => {
        it('array#head', (next) => {
          expect(
            array.head([0,1,2])
          ).to.eql(
            0
          );
          next();
        });
        it('array#tail', (next) => {
          expect(
            array.tail([0,1,2])
          ).to.eql(
            [1,2]
          );
          next();
        });
        it('array#cons', (next) => {
          expect(
            array.cons(0,[1,2])
          ).to.eql(
            [0,1,2]
          );
          next();
        });
        it('array#fromString', (next) => {
          expect(
            array.fromString("123")
          ).to.eql(
            [1,2,3]
          );
          next();
        });
        it('array#takeWhile', (next) => {
          var theArray = [1,2,3]; 
          var even = (n) => {
            return 0 === (n % 2);
          };
          var odd = not(even);
          expect(
            array.takeWhile(theArray)(odd)
          ).to.eql(
            [1]
          );
          next();
        });
        it('array#dropWhile', (next) => {
          var theArray = [1,2,3]; 
          var even = (n) => {
            return 0 === (n % 2);
          };
          var odd = not(even);
          expect(
            array.dropWhile(theArray)(odd)
          ).to.eql(
            [2,3]
          );
          next();
        });
        it('array#span', (next) => {
          var theArray = [1,2,3]; 
          var even = (n) => {
            return 0 === (n % 2);
          };
          expect(
            array.span(theArray)(even)
          ).to.eql(
            [[],[1,2,3]]
          );
          var odd = not(even);
          expect(
            array.span(theArray)(odd)
          ).to.eql(
            [[1],[2,3]]
          );
          next();
        });
        it('array#break', (next) => {
          var theArray = [1,2,3]; 
          var even = (n) => {
            return 0 === (n % 2);
          };
          expect(
            array.break(theArray)(even)
          ).to.eql(
            [[1],[2,3]]
          );
          var odd = not(even);
          expect(
            array.break(theArray)(odd)
          ).to.eql(
            [[],[1,2,3]]
          );
          var isNewline = (ch) => {
            return ch === '\n';
          };
          expect(
            array.break(['a','b','c','\n','d','e','f'])(isNewline)
          ).to.eql(
            [['a','b','c'],['\n','d','e','f']]
          );
          next();
        });
        it('array#lines', (next) => {
          var theArray = array.fromString("abc\ndef"); 
          expect(
            array.lines(theArray)
          ).to.eql(
            [ [ 'a', 'b', 'c' ], [ 'd', 'e', 'f' ] ]
          );
          next();
        });
      });
      describe('string', () => {
        it('string#head', (next) => {
          expect(
            string.head("abc")
          ).to.eql(
            'a'
          );
          next();
        });
        it('string#tail', (next) => {
          expect(
            string.tail("abc")
          ).to.eql(
            "bc"
          );
          next();
        });
        it('string#fromArray', (next) => {
          expect(
            string.fromArray(['a','b','c'])
          ).to.eql(
            "abc"
          );
          next();
        });
        it('string#toArray', (next) => {
          expect(
            string.toArray("abc")
          ).to.eql(
            ['a','b','c']
          );
          next();
        });
        it('string#lines', (next) => {
          expect(
            string.lines("abc\ndef")
          ).to.eql(
            [ 'abc', 'def' ]
          );
          next();
        });
      });
    });
    it('統一的なインターフェイス', (next) => {
      /* #@range_begin(books_as_array) */
      var books = [
        {name: "こころ", author: ["夏目漱石"], genre: "文学"},
        {name: "夢十夜", author: ["夏目漱石"], genre: "文学"},
        {name: "ソクラテスの弁明", author: ["プラトン"], genre: "哲学"},
        {name: "国家", author: ["プラトン"], genre: "哲学"},
        {name: "プログラミング言語C", author: ["カーニハン","リッチー"], genre: "コンピュータ"},

        {name: "計算機プログラムの構造と解釈", author: ["サスマン","エイベルソン"], genre: "コンピュータ"},
      ];
      /* #@range_end(books_as_array) */
      var get = (object) => {
        return (key) => {
          return object[key];
        };
      };
      /* #@range_begin(pluck) */
      var pluck = (key) => {
        return (object) => {
          return object[key];
        };
      };
      /* #@range_end(pluck) */
      /* #@range_begin(mapWith) */
      var mapWith = (func) => {
        return (array) => {
          return array.map(func);
        };
      };
      /* #@range_end(mapWith) */
      var multiplier = (n) => {
        return (m) => {
          return n * m;
        };
      };
      var square = (n) => {
        return multiplier(n)(n);
      };
      expect(
        mapWith(square)([1,2,3])
      ).to.eql(
        [1,4,9]
      );
      expect(
        /* #@range_begin(mapWith_pluck) */
        mapWith(pluck("name"))(books)
        /* #@range_end(mapWith_pluck) */
      ).to.eql(
        ['こころ','夢十夜','ソクラテスの弁明','国家','プログラミング言語C','計算機プログラムの構造と解釈']
      );
      /* #@range_begin(filterWith) */
      var filterWith = (predicate) => {
        return (array) => {
          return array.filter(predicate);
        };
      };
      /* #@range_end(filterWith) */
      var isEqual = (a,b) => {
        return a === b;
      };
      expect(
        /* #@range_begin(filterWith_pluck) */
        filterWith((book) => { 
          return pluck("genre")(book) ===  "文学";
        })(books)
        /* #@range_end(filterWith_pluck) */
      ).to.eql(
        /* #@range_begin(filterWith_pluck_result) */
        [
          {name: "こころ", author: ["夏目漱石"], genre: "文学"},
          {name: "夢十夜", author: ["夏目漱石"], genre: "文学"},
        ]
        /* #@range_end(filterWith_pluck_result) */
      );
      expect(
        /* #@range_begin(map_filter) */
        mapWith(pluck("name"))(filterWith((book) => { 
          return pluck("genre")(book) ===  "哲学";
        })(books))
        /* #@range_end(map_filter) */
      ).to.eql(
        /* #@range_begin(map_filter_result) */
        ["ソクラテスの弁明", "国家"]
        /* #@range_end(map_filter_result) */
      );
      
      /* #@range_begin(doesContain) */
      var doesContain = (value) => {
        return (array) => {
          return array.reduce((accumulator, item) => {
            return accumulator || (item === value);
          },false);
        };
      };
      /* #@range_end(doesContain) */
      var doesMatch = (predicate) => {
        return (array) => {
          return array.reduce((accumulator, item) => {
            return accumulator || predicate(item);
          },false);
        };
      };
      expect(
        /* #@range_begin(filterWith_doesContain) */
        filterWith((book) => { 
          return doesContain("カーニハン")(pluck("author")(book));
        })(books)
        /* #@range_end(filterWith_doesContain) */
      ).to.eql(
        /* #@range_begin(filterWith_doesContain_result) */
        [
          {name: "プログラミング言語C", author: ["カーニハン","リッチー"], genre: "コンピュータ"},
        ]
        /* #@range_end(filterWith_doesContain_result) */
      );
      /* #@range_begin(findWith) */
      var findWith = (predicate) => {
        return (array) => {
          return array.filter(predicate)[0];
        };
      };
      /* #@range_end(findWith) */
      expect(
        findWith((book) => { 
          return pluck("genre")(book) === "哲学";
        })(books)
      ).to.eql(
        {name: "ソクラテスの弁明", author: ["プラトン"], genre: "哲学"}
      );
      next();
    });
    // #### 部品の汎用性
    describe('部品の汎用性', () => {
      describe('reduceによる反復処理の汎用化', () => {
        // forEachメソッドによるsumの定義
        it('forEachメソッドによるsumの定義', (next) => {
          /* #@range_begin(sum_in_array_while) */
          var sum = (array) => {
            /* 結果を格納する変数 */
            var result = 0;
            /* 反復した回数を格納する変数 */
            var index = 0; 
            while(index < array.length) {
              /* 変数resultを代入で更新する */
              result = result + array[index];
              /* 反復回数を更新する */
              index = index + 1;
            }
            return result;
          };
          /* #@range_end(sum_in_array_while)  */
          expect(
            sum([1,2,3,4])
          ).to.eql(
            10
          );
          expect(
            sum([1,2,3,4])
          ).to.eql(
            10
          );
          next();
        });
        // reduceメソッドによるsumの定義
        it('reduceメソッドによるsumの定義', (next) => {
          /* #@range_begin(sum_in_array_reduce) */
          var sum = (array) => {
            return array.reduce(/* 第1引数に関数を渡す */
              (accumulator, item) => { 
                return accumulator + item; /* 足し算を実行する */
              },0); // 第2引数には、蓄積変数の初期値として0を渡す 
          };
          /* #@range_end(sum_in_array_reduce)  */
          expect(
            sum([1,2,3,4])
          ).to.eql(
            10
          );
          next();
        });
        // product関数の定義
        it('product関数の定義', (next) => {
          /* #@range_begin(product_in_array_reduce) */
          var product = (array) => {
            return array.reduce((accumulator, item) => {
              return accumulator * item; /* かけ算を実行する */
            }, 1); // 第2引数には、蓄積変数の初期値として1を渡す 
          };
          /* #@range_end(product_in_array_reduce)  */
          /* #@range_begin(product_in_array_reduce_test) */
          expect(
            product([1,2,3,4])
          ).to.eql(
            24
          );
          /* #@range_end(product_in_array_reduce_test)  */
          next();
        });
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
        // map関数の定義
        it('map関数の定義', (next) => {
          var succ = (n) => {
            return n + 1;
          };
          /* #@range_begin(map_in_array_reduce) */
          var map = (transform) => {
            return (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator.concat(transform(item));
              },[]); // 蓄積変数の初期値として空の配列[]を指定する
            };
          };
          /* #@range_end(map_in_array_reduce)  */
          expect(
            map(succ)([1,2,3])
          ).to.eql(
            [2,3,4]
          );
          var constant = (any) => {
            return (_) => {
              return any;
            };
          };
          var alwaysOne = constant(1); 
          expect(
            /* #@range_begin(map_in_array_reduce_test) */
            map(succ)([1,3,5])
            /* #@range_end(map_in_array_reduce_test) */
          ).to.eql(
            /* #@range_begin(map_in_array_reduce_test_result) */
            [2,4,6]
            /* #@range_end(map_in_array_reduce_test_result) */
          );
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
      });
    });
    // ### 部品を組み合わせる
    describe('部品を組み合わせる', () => {
      // #### 関数適用で部品を組み合わせる
      it('関数適用で部品を組み合わせる', (next) => {
        var map = (transform) => {
          return (array) => {
            return array.reduce((accumulator, item) => {
              return accumulator.concat(transform(item));
            },[]); // 蓄積変数の初期値として空の配列[]を指定する
          };
        };
        var sum = (array) => {
          return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
            return accumulator + item;                 // 足し算を実行する
          },0); // 第2引数には、蓄積変数の初期値として0を渡す 
        };
        // constant関数
        /* #@range_begin(constant) */
        var constant = (any) => {
          return (_) => {
            return any;
          };
        };
        var alwaysOne = constant(1); 
        /* #@range_end(constant) */
        /* #@range_begin(map_alwaysOne) */
        expect(
          map(alwaysOne)([1,2,3])
        ).to.eql(
          [1,1,1]
        );
        /* #@range_end(map_alwaysOne) */
        var flip = (fun) => {
          return  (x) => {
            return (y) => {
              return fun(y)(x);
            };
          };
        };
        // 関数合成によるlength関数の定義
        /* #@range_begin(array_length_in_composition) */
        var length = (array) => { // 引数が配列であることを明示する
          return compose(sum,map(alwaysOne))(array);
        };
        /* #@range_end(array_length_in_composition)  */
        expect(
          length([1,2,3])
        ).to.eql(
          3
        );
        next();
      });
      it('adderからsucc関数を作る', (next) => {
        /* #@range_begin(succ_from_adder) */
        var adder = (m) => {
          return (n) => {
            return m + n;
          };
        };
        var succ = adder(1);
        /* #@range_end(succ_from_adder)  */
        expect(
          succ(1)
        ).to.eql(
          2
        );
        next();
      });
      it('length関数を作る', (next) => {
        var map = (transform) => {
          return (array) => {
            return array.reduce((accumulator, item) => {
              return accumulator.concat(transform(item));
            },[]); // 蓄積変数の初期値として空の配列[]を指定する
          };
        };
        var sum = (array) => {
          return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
            return accumulator + item;                 // 足し算を実行する
          },0); // 第2引数には、蓄積変数の初期値として0を渡す 
        };
        var constant = (any) => {
          return (_) => {
            return any;
          };
        };
        var alwaysOne = constant(1); 
        /* #@range_begin(array_length) */
        var length = (array) => {
          return sum(map(alwaysOne)(array));
        };
        /* #@range_end(array_length)  */
        expect(
          length([1,2,3])
        ).to.eql(
          3
        );
        next();
      });
      // #### 関数合成による処理の合成
      describe('関数合成による処理の合成', () => {
        // compose関数の定義
        /* #@range_begin(function_compose) */
        var compose = (f,g) => {
          return (arg) => {
            return f(g(arg));
          };
        };
        /* #@range_end(function_compose)  */
        it('関数合成でlength関数を作る(ポイントフリー版)', (next) => {
          var map = (transform) => {
            return (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator.concat(transform(item));
              },[]); // 蓄積変数の初期値として空の配列[]を指定する
            };
          };
          var sum = (array) => {
            return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
              return accumulator + item;                 // 足し算を実行する
            },0); // 第2引数には、蓄積変数の初期値として0を渡す 
          };
          var constant = (any) => {
            return (_) => {
              return any;
            };
          };
          var alwaysOne = constant(1); 
          /* #@range_begin(flip_definition) */
          var flip = (fun) => {
            return  (x) => {
              return (y) => {
                return fun(y)(x);
              };
            };
          };
          /* #@range_end(flip_definition) */

          // 関数合成によるlength関数の定義（ポイントフリースタイル）
          /* #@range_begin(array_length_in_composition_with_point_free_style) */
          var length = compose(sum,map(alwaysOne));
          /* #@range_end(array_length_in_composition_with_point_free_style)  */
          expect(
            length([1,2,3])
          ).to.eql(
            3
          );
          next();
        });
      });
      // #### 関数による遅延評価
      describe('関数による遅延評価', () => {
        var take = (n, astream) => {
          if(n === 1) {
            return astream[0];
          } else {
            return [astream[0]].concat(take(n-1, astream[1]()));
          }
        };
        var repeat = (n) => {
          return [n, (_) => {
            return repeat(n);
          }];
        };
        expect(
          take(3, repeat(2))
        ).to.eql(
          [2,2,2]
        );
        var ones = repeat(1);
        expect(
          take(2, ones)
        ).to.eql(
          [1,1]
        );
        var replicate = (n, x) => {
          return take(n, repeat(x));
        };
        expect(
          replicate(4, 3)
        ).to.eql(
          [3,3,3,3]
        );
        var upto = (m, n) => {
          if(m > n) {
            return [];
          } else {
            return [m, (_) => {
              return upto(m+1,n);
            }];
          };
        };
        expect(
          take(3, upto(2,10))
        ).to.eql(
          [2,3,4]
        );
        var mod = (n,m) => {
          return n % m;
        };
        var even = (n) => {
          return mod(n,2) === 0;
        };
        var adder = (m) => {
          return (n) => {
            return m + n;
          };
        };
        var succ = adder(1);
        var map = (aStream) => {
          return (transform) => {
            var head = aStream[0];
            return [transform(head), (_) => {
              return map(aStream[1]())(transform);
            }];
          };
        };
        // 無限ストリームの例
        /* #@range_begin(stream_iterate) */
        var iterate = (init) => {  // 先頭の値を渡す
          return (step) => {       // 次の値との差を計算する関数を渡す
            return [init, (_) => { // ストリーム型を返す
              return iterate(step(init))(step);
            }];
          };
        };
        /* #@range_end(stream_iterate) */
        // 無限の偶数列
        /* #@range_begin(enumFrom_by_iterate) */
        var enumFrom = (n) => {
          return iterate(n)(succ);
        };
        // 自然数列を定義する
        var naturals = enumFrom(1);
        // 偶数列を定義する
        var twoStep = (n) => {
          return n + 2;
        };
        var evenStream = iterate(2)(twoStep);
        /* #@range_end(enumFrom_by_iterate) */
        // filter関数
        /* #@range_begin(stream_filter) */
        var filter = (predicate) => {
          return (aStream) => {
            /* ストリームの先頭要素を取り出す */
            var head = aStream[0];
            /* 先頭要素が条件に合致する場合 */
            if(predicate(head) === true) { 
              return [head, (_) => {
                return filter(predicate)(aStream[1]());
              }];
            } else {                       
              /* 先頭要素が条件に合致しない場合 */
              return filter(predicate)(aStream[1]());
            }
          };
        };
        /* #@range_end(stream_filter) */
        expect(
          take(3,filter(even)(enumFrom(2)))
        ).to.eql(
          [2,4,6]
        );
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
        it('遅延評価の説明', (next) => {
          var map = (transform) => {
            return (array) => {
              return array.reduce((accumulator, item) => {
                return accumulator.concat(transform(item));
              },[]); 
            };
          };
          var sum = (array) => {
            return array.reduce((accumulator, item) => { // 第1引数に関数を渡す
              return accumulator + item;                 // 足し算を実行する
            },0); 
          };
          var constant = (any) => {
            return (_) => {
              return any;
            };
          };
          var alwaysOne = constant(1); 
          var length = compose(sum,map(alwaysOne));
          expect(
            // 正格評価の例
            /* #@range_begin(strict_evaluation) */
            length([1,1+1])
            /* #@range_end(strict_evaluation) */
          ).to.eql(
            /* #@range_begin(strict_evaluation_result) */
            2
            /* #@range_end(strict_evaluation_result) */
          );
          expect(
            // 遅延評価の例
            /* #@range_begin(lazy_evaluation) */
            length([1,(_) => {
                         return 1+1; 
                   }])
            /* #@range_end(lazy_evaluation) */
          ).to.eql(
            /* #@range_begin(lazy_evaluation_result) */
            2
            /* #@range_end(lazy_evaluation_result) */
          );
          next();
        });
        // 無限の整数列
        it('無限の整数列', (next) => {
          /* #@range_begin(enumFrom) */
          var enumFrom = (n) => {
            return [n, (_) => { // ストリームを返す
              return enumFrom(n + 1);
            }];
          };
          /* #@range_end(enumFrom) */
          next();
        });
        // 無限の偶数列を作る
        it('無限の偶数列を作る', (next) => {
          /* #@range_begin(evenStream) */
          var evenFrom = (n) => {
            return [n, (_) => {
              return evenFrom(n + 2);
            }];
          };
          var evenStream = evenFrom(2); 
          /* #@range_end(evenStream) */
          expect(
            take(3, evenStream)
          ).to.eql(
            [2,4,6]
          );
          next();
        });
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
          // elemAt関数
          /* #@range_begin(stream_elemAt) */
          var elemAt = (n) => {
            return (aStream) => {
              if(n === 1) {
                return aStream[0];
              } else {
                return elemAt(n-1)(aStream[1]());
              };
            };
          };
          /* #@range_end(stream_elemAt) */
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
          expect(
            take(5,filter(odd)(enumFrom(1)))
          ).to.eql(
            [ 1, 3, 5, 7, 9 ] 
          );
          // filter関数で無限の偶数列を作る
          /* #@range_begin(evenStream_by_filter) */
          var even = (n) => {
            return (n % 2) === 0; 
          };
          var evenStream = filter(even)(enumFrom(1));
          /* #@range_end(evenStream_by_filter) */
          expect(
            /* #@range_begin(third_element_of_evenStream) */
            elemAt(3)(evenStream)
            /* #@range_end(third_element_of_evenStream) */
          ).to.eql(
            /* #@range_begin(third_element_of_evenStream_result) */
            6
            /* #@range_end(third_element_of_evenStream_result) */
          );
          next();
        });
        // 配列に対するelemAt関数
        it('配列に対するelemAt関数', (next) => {
          /* #@range_begin(array_elemAt) */
          var elemAt = (n) => {
            return (anArray) => {
              if(n === 1) {
                return anArray[0];
              } else {
                var tail = anArray.slice(1,anArray.length);
                return elemAt(n-1)(tail);
              };
            };
          };
          /* #@range_end(array_elemAt) */
          expect(
            /* #@range_begin(third_element_of_evenArray) */
            elemAt(3)([2,4,6])
            /* #@range_end(third_element_of_evenArray) */
          ).to.eql(
            /* #@range_begin(third_element_of_eventArray_result) */
            6
            /* #@range_end(third_element_of_eventArray_result) */
          );
          expect(
            // 4番目の偶数を求める
            /* #@range_begin(fourth_element_of_evenArray) */
            elemAt(4)([2,4,6])
            /* #@range_end(fourth_element_of_evenArray) */
          ).to.eql(
            /* #@range_begin(fourth_element_of_evenArray_result) */
            undefined
            /* #@range_end(fourth_element_of_evenArray_result) */
          );
          next();
        });
        describe('ストリーム', () => {
          // ストリームの例
          it('ストリームの例', (next) => {
            /* #@range_begin(stream_example) */
            var aStream = [1, (_) => { // 後尾は無名関数で表現する
              return 2;
            }];
            /* #@range_end(stream_example) */
            expect(
              /* #@range_begin(stream_head) */
              aStream[0] // ストリームの先頭要素を取得する
              /* #@range_end(stream_head) */
            ).to.eql(
              /* #@range_begin(stream_head_result) */
              1
              /* #@range_end(stream_head_result) */
            );
            expect(
              /* #@range_begin(stream_tail) */
              aStream[1]() // 関数適用でストリームの後尾を取り出す
              /* #@range_end(stream_tail) */
            ).to.eql(
              /* #@range_begin(stream_tail_result) */
              2
              /* #@range_end(stream_tail_result) */
            );
            /* #@range_begin(stream_ones) */
            var ones = [1, (_) => {
              return ones;
            }];
            /* #@range_end(stream_ones) */
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
            next();
          });
        });
      });
    });
  });
  // ### テストが容易である
  describe('テスト', () => {
    // 単体テストの書き方の例
    it('単体テストの書き方の例', (next) => {
      var succ = (n) => {
        return n + 1;
      };
      expect(
        succ(1) // テストしたい式を書く
      ).to.eql(
        2       // 期待する結果を書く
      );
      next();
    });
    it('参照透過性のあるコードはテストが容易である', (next) => {
      var adder = (m) => {
        return (n) => {
          return m + n;
        };
      };
      /* #@range_begin(adder_test) */
      expect(
        adder(1)(2)
      ).to.eql(
        3
      );
      /* #@range_end(adder_test) */
      next();
    });
    describe('参照透過性のないコードはテストが困難である', () => {
      describe('ゲームの勝敗の例', () => {
        // 参照透過性のないコードのテスト
        it("参照透過性のないコードのテスト", (next) => {
          /* #@range_begin(winner_with_sideeffect) */
          var winner = (playerL, playerR) => {
            if(playerR.score > playerL.score) {
              console.log(playerR.name + "が勝者です");
            } else if(playerR.score < playerL.score) {
              console.log(playerL.name + "が勝者です");
            } else {
              console.log("引き分けです");
            }
          }; 
          /* #@range_end(winner_with_sideeffect) */
          var playerA = {
            name: 'a',
            score: 10
          };
          var playerB = {
            name: 'b',
            score: 20
          };
          next();
        });
        it("参照透過性のあるコードのテスト", (next) => {
          // winner関数の分離
          /* #@range_begin(winner_without_sideeffect) */
          /* 勝者を判定する */
          var judge = (playerL, playerR) => {
            if(playerL.score > playerR.score) {
              return playerL;
            } else if(playerL.score < playerR.score) {
              return playerR;
            } else {
              return null;
            }
          }; 
          /* 勝者を告げる文字列を生成する */
          var announce = (winner) => {
            if(winner) {
              return winner.name + "が勝者です";
            } else {
              return "引き分けです";
            }
          };
          /* 勝者を表示する */
          var displayWinner = (winner) => {
            console.log(announce(winner));
          };
          /* #@range_end(winner_without_sideeffect) */
          // 副作用のない関数のテスト
          /* #@range_begin(announce_winner) */
          var socrates = {
            name: 'ソクラテス',
            score: 10
          };
          var plato = {
            name: 'プラトン',
            score: 20
          };
          /* 純粋な関数をテストする */
          expect(
            announce(judge(socrates, plato))
          ).to.eql(
            "プラトンが勝者です"
          );
          /* #@range_end(announce_winner) */
          next();
        });
      });
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
    });
  });
  // ### コードの正しさを証明できる
  describe('コードの正しさを証明できる', () => {
    // #### プロパティテストで正しさを検証する
    describe('プロパティテストで正しさを検証する', () => {
      var adder = (m) => {
        return (n) => {
          return m + n;
        };
      };
      var succ = adder(1);
      var iterate = (step) => {
        return (init) => {
          return [init, (_) => {
            return iterate(step)(step(init));
          }];
        };
      };
      var take = (n) => {
        return (aStream) => {
          if(n === 0) {
            return [];
          } else {
            return [aStream[0]].concat(take(n-1)(aStream[1]()));
          }
        };
      };
      var enumFrom = (from) => {
        return iterate(succ)(from);
      };
      var filter = (predicate) => {
        return (aStream) => {
          var head = aStream[0];
          if(predicate(head) === true) {
            return [head, (_) => {
              return filter(predicate)(aStream[1]());
            }];
          } else {
            return filter(predicate)(aStream[1]());
          }
        };
      };
      it('succ関数の性質テスト', (next) => {
        var elemAt = (n) => {
          return (aStream) => {
            if(n === 1) {
              return aStream[0];
            } else {
              return elemAt(n-1)(aStream[1]());
            };
          };
        };
        var scanl = (aStream) => {
          return (glue) => {
            var out = [aStream[0], (_) => {
              return zipWith(glue)(aStream[1](), out);
            }];
            return out;
          };
        };
        var conjunction = (x,y) => {
          return x && y;  // 論理和をとる
        };
        var zipWith = (glue) => {
          return (xs, ys) => {
            if(xs.length === 0) {
              return []; 
            } 
            if(ys.length === 0) {
              return []; 
            } 
            return [glue(xs[0], ys[0]), (_) => {
              return zipWith(glue)(xs[1](), ys[1]());
            }];
          };
        };
        var listAnd = (aStream) => {
          var out = [aStream[0], (_) => {
            return zipWith((x,y) => { 
              return x && y;
            })(aStream[1](), out);
          }];
          return out;
        };
        // プロパティテストのための関数
        /* #@range_begin(succ_property) */
        /* ストリームのmap関数 */
        var map = (transform) => {
          return (aStream) => {
            var head = aStream[0];
            return [transform(head), (_) => {
              return map(transform)(aStream[1]());
            }];
          };
        };
        /* ストリームの先頭から引数n分だけ取り出すtake関数 */
        var take = (n) => {
          return (aStream) => {
            if(n === 0) {
              return null;
            } else {
              return [aStream[0], (_) => {
                return take(n-1)(aStream[1]());
              }];
            }
          };
        };
        /* ストリームの全ての要素がtrueであるかを判定するall関数 */
        var all = (aStream) => {
          var allHelper = (aStream, accumulator) => {
            var head = aStream[0];
            var newAccumulator = accumulator && head;
            if(aStream[1]() === null){
              return newAccumulator;
            } else {
              return allHelper(aStream[1](), newAccumulator);
            } 
          };
          return allHelper(aStream, true);
        };
        /* 検証の対象となる命題 */
        var proposition = (n) => {
          return succ(0) + succ(n) === succ(succ(n));
        };
        /* #@range_end(succ_property) */
        // succ関数のプロパティテスト
        /* #@range_begin(succ_property_test) */
        /* 100個の整数について命題が正しいかをテストする */
        expect(
          all(
            take(100)(
              map(proposition)(enumFrom(0))
            )
          )
        ).to.eql(
          true
        );
        /* #@range_end(succ_property_test) */
        next();
      });
    });
  });
  // [目次に戻る](http://akimichi.github.io/functionaljs/) [次章に移る](http://akimichi.github.io/functionaljs/chap03.spec.html) 
});
