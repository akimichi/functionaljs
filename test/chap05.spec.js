"use strict";

var expect = require('expect.js');
var util = require('util');

// プログラムをコントロールする仕組み
// ============================
describe('プログラムをコントロールする仕組み', () => {
  // ## 条文分岐
  describe('条文分岐', () => {
    it('偶数の例', (next) => {
      /* ##@range_begin(even_function)*/
      var even = (n) => {
        if((n % 2) === 0) {
          return true;
        } else {
          return false;
        }
      };
      /* ##@range_end(even_function)*/
      expect(
        even(2)
      ).to.eql(
        true
      );
      expect(
        even(3)
      ).to.eql(
        false
      );
      next();
    });
    /*
    it('逆数の例', (next) => {
      // ##@range_begin(inverse_function)
      var inverse = (n) => {
        if(n === 0) {
          return 0;
        } else {
          return 1 / n;
        }
      };
      // ##@range_end(inverse_function)
      expect(
        inverse(2)
      ).to.be(
        0.5
      );
      expect(
        inverse(0)
      ).to.be(
        0
      );
      next();
    });
    */
    it('ifは文である', (next) => {
      /* ##@range_begin(if_statement) */
      if(true) {

      } else {

      }
      /* ##@range_end(if_statement) */
      next();
    });
    it('ifの非正格性', (next) => {
      var infiniteLoop = (x) => {
        return infiniteLoop(x);
      };
      /* ##@range_begin(if_nonstrict) */
      var func = (x) => {
        if(true) {
          return x;
        } else {
          throw {
            name: "",
            message: "ここには決して到達しません"
          };
        }
      };
      /* テスト */
      expect(
        func(1)
      ).to.eql(
        1
      );
      /* ##@range_end(if_nonstrict) */
      next();
    });
    it("三項演算子", (next) => {
      /* ##@range_begin(trinary_if) */
      var signum = (n) => {
        return (n > 0)?  1 : (n === 0)? 0 : -1;
      };
      expect(
        signum(3)
      ).to.eql(
        1
      );
      expect(
        signum(-3)
      ).to.eql(
          -1
      );
      expect(
        signum(0)
      ).to.eql(
        0
      );
      /* ##@range_end(trinary_if) */
      next();
    });
    it('条件式を実装する', (next) => {
      /* #@range_begin(conditional) */
      var cond = (predicate, pattern) => {
        if(predicate){
          return pattern.trueClause();
        } else {
          return pattern.falseClause();
        };
      };
      /* テスト */
      expect(
        cond((2 < 3), {
          trueClause: () => {
            return true;
          },
          falseClause: () => {
            throw {
              message: "ここは到達しません"
            };
          }
        })
      ).to.eql(
        true
      );
      /* #@range_end(conditional) */
      /*
      var conditional = function(pred, trueExpr, falseExpr){
        return (x) => {
          if(pred(x)){
            return trueExpr(x);
          } else {
            return falseExpr(x);
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
        conditional(lessThan(3), function(_){ return 1;},function(_){ return 0;})(2)
      ).to.eql(
        1
      );
      var infiniteLoop = (x) => {
        return infiniteLoop(x);
      };
      expect(conditional(lessThan(3), function(x){ return x;},function(x){ return infiniteLoop(x);})(2)).to.eql(2);
      */
      next();
    });
    it('関数渡しで反復文を構築する', function(next) {
      /* #@range_begin(loop)            */
      var loop = function(pred, accumulator, expression){
        if(pred(accumulator)){
          return loop(pred, expression(accumulator), expression);
        } else {
          return accumulator;
        }
      };
      var lessThan = function(n){
        return function(x){
          return x < n;
        };
      };
      var succ = function(n){
        return n + 1;
      };
      expect(loop(lessThan(3), 0, succ)).to.eql(3);
      /* #@range_end(loop) */
      next();
    });
    describe('真理値', () => {
      it('チャーチの真理値', (next) => {
        /* ##@range_begin(church_truth) */
        var _true = (x) => {
          return (y) => {
            return x;
          };
        };
        var _false = (x) => {
          return (y) => {
            return y;
          };
        };
        var _ifElse = (pred) => {
          return (x) => {
            return (y) => {
              return pred(x)(y);
            };
          };
        };
        var _not = (x) => {
          return (x(_false))(_true);
        };
        var _and = (x) => {
          return (y) => {
            return (x(y))(_false);
          };
        };
        var _or = (x) => {
          return (y) => {
            return (x(_true))(y);
          };
        };
        /* ##@range_end(church_truth) */
        /* ##@range_begin(church_truth_test) */
        expect(
          _true(1)(0)
        ).to.be(
          1
        );
        expect(
          _not(_true)(1)(0)
        ).to.be(
          0
        );
        expect(
          _and(_true)(_true)(1)(0)
        ).to.be(
          1
        );
        expect(
          _ifElse(_true)(1)(0)
        ).to.be(
          1
        );
        expect(
          _ifElse(_false)(1)(0)
        ).to.be(
          0
        );
        expect(((_) => {
          var eq = (x) => {
            return (y) => {
              if(x === y) {
                return _true;
              } else {
                return _false;
              }
            };
          };
          return _ifElse(eq(0)(0))(1)(0);
        })()).to.be(
          1
        );
        expect(
          _ifElse(_true)(_false)(_true)(1)(0)
        ).to.be(
          0
        );

        /* ##@range_end(church_truth_test) */
        next();
      });
    });
  });
  // ## 条件分岐としてのswitch文
  describe('条件分岐としてのswitch文', () => {
    it("信号機の例", (next) => {
      var forward, stop, watchfulForward;
      /* #@range_begin(signal) */
      var move;
      var signal = (light) => {
        switch(light){
        case "green":
          move = forward;
          break;
        case "red":
          move = stop;
          break;
        case "yellow":
          move = watchfulForward;
          break;
        default:
          throw {
            name: "そのような信号はありません",
            message: "unknown: " + light
          };
        }
      }
      /* #@range_end(signal) */
      next();
    });
    it("可変データとのマッチング", (next) => {
      /* #@range_begin(switch_for_mutable) */
      var switch_for_mutable = (array) => {
        switch(array){
        case [1,2,3]:
		  return true;
          break;
        default:
		  return false;
        }
      }
	  /* テスト */
      expect(
        switch_for_mutable([1,2,3])
      ).to.eql(
        false
      );
      /* #@range_end(switch_for_mutable) */
      next();
    });
    it("通貨の例", (next) => {
      /* #@range_begin(currency) */
      var move;
      var signal = (currency) => {
        switch(currency){
        case "yen":
          move = forward;
          break;
        case "dollar":
          move = stop;
          break;
        case "euro":
          move = watchfulForward;
          break;
        default:
          throw {
            name: "対応していない通貨です",
            message: "unknown: " + currency
          };
        }
      }
      /* #@range_end(currency) */
      next();
    });
    it("compare", (next) => {
      /* #@range_begin(compare) */
      var compare =  function(n,m){
        if (n > m) {
          return 1;
        } else {
          if(n === m) {  /* ネストされたif文 */
            return 0;
          } else {
            return -1;
          }
        }
      };
      expect(
        compare(3,2)
      ).to.eql(
        1
      );
      expect(
        compare(1,1)
      ).to.eql(
        0
      );
      expect(
        compare(2,3)
      ).to.eql(
          -1
      );
      /* #@range_end(compare) */
      next();
    });
    // ### コンビネータによる条件分岐の改善
    describe('コンビネータによる条件分岐の改善', () => {
      var multiplyOf = (n) => {
        return (m) => {
          if((m % n) === 0) {
            return true;
          } else {
            return false;
          }
        }
      };
      var twoFold = multiplyOf(2);
      var threeFold = multiplyOf(3);
      var fiveFold = multiplyOf(5);
      it('notコンビネータ', (next) => {
        /* not:: (NUMBER->BOOL) -> NUMBER -> BOOL */
        var not = (predicate) => {
          return (data) => { // NUMBER -> BOOL
            if (predicate(data)) {
              return false;
            } else {
              return true;
            }
          };
        };
      expect(
        not(not(twoFold))(4)
      ).to.eql(
        true
      );
        next();
      });
      it('2と3の倍数で 5の倍数ではない', (next) => {
        /* ##@range_begin(twoFoldAndThreeFoldButNotFiveFold) */
        var twoFold = (n) => { /* 2の倍数を判定する */
          if((n % 2) === 0) {
            return true;
          } else {
            return false;
          }
        };
        /*
          3の倍数を判定する threeFold
          5の倍数を判定する fiveFoldも同様に定義したとする
        */
        var twoFoldAndThreeFoldButNotFiveFold = (n) => {
          if(twoFold(n) && threeFold(n)) { /* 2の倍数かつ3の倍数 */
            if(fiveFold(n)) { /* 5の倍数 */
              return false;
            } else { /* 2の倍数かつ3の倍数で、5の倍数ではない */
              return true;
            }
          } else {
            return false;
          }
        };
        /* テスト */
        expect(
          twoFoldAndThreeFoldButNotFiveFold(6)
        ).to.eql(
          true
        )
        expect(
          twoFoldAndThreeFoldButNotFiveFold(30)
        ).to.eql(
          false
        )
        /* ##@range_end(twoFoldAndThreeFoldButNotFiveFold) */
        next();
      });
      /* ##@range_begin(not_combinator) */
      /* 「~ではない」を表す否定  */
      /* not:: (NUMBER->BOOL) -> (NUMBER->BOOL) */
      var not = (predicate) => { // predicate:: NUMBER->BOOL
        return (number) => {     // NUMBER->BOOL型の関数を返す
          return ! predicate(number); // !演算子で論理を反転させる
        };
      };
      /* ##@range_end(not_combinator) */
      /* ##@range_begin(logical_combinator) */
      /* 「かつ」を表す論理積  */
      /* and:: (NUMBER->BOOL, NUMBER->BOOL) -> (NUMBER->BOOL) */
      var and = (predicateA, predicateB) => {
        return (data) => {
          return predicateA(data) && predicateB(data);
        };
      };
      /* 「もしくは」を表す論理和  */
      var or = (predicateA, predicateB) => {
        return (data) => {
          return predicateA(data) || predicateB(data);
        };
      };
      /* ##@range_end(logical_combinator) */
      it('論理コンビネータのテスト', (next) => {
        expect(
          twoFold(2)
        ).to.eql(
          true
        )
        /* ##@range_begin(logical_combinator_test) */
        expect(
		  /* 「2の倍数かつ3の倍数で、5の倍数ではない」*/
          and(and(twoFold,threeFold),not(fiveFold))(6)
        ).to.eql(
          true
        )
        /* ##@range_end(logical_combinator_test) */
        expect(
           and(twoFold,threeFold)(6)
        ).to.eql(
          true
        )
        expect(
           and(twoFold,threeFold)(4)
        ).to.eql(
          false
        )
        expect(
          and(and(twoFold,threeFold),not(fiveFold))(4)
        ).to.eql(
          false
        )
        /* 2の倍数もしくは3の倍数で5の倍数でもあるもの */
        /* ##@range_begin(another_logical_combinator_test) */
        expect(
		  /* 「2の倍数もしくは3の倍数、かつ5の倍数でもある」 */
          and(or(twoFold,threeFold),fiveFold)(10)
        ).to.eql(
          true
        )
        /* ##@range_end(another_logical_combinator_test) */
        next();
      });
    });
  });
  // ## 代数的データ型
  describe('代数的データ型', () => {
    it('Listを代数的データ型として実装する', (next) => {
      /* #@range_begin(list_in_algebraic_datatype) */
	  /* リストの代数的データ型 */
      var empty = (pattern) => {
        return pattern.empty;
      };
      var cons = (value, list) => {
        return (pattern) => {
          return pattern.cons(value, list);
        };
      };
      /* #@range_end(list_in_algebraic_datatype) */
      /* #@range_begin(match_in_algebraic_datatype) */
	  /* 代数的データ型に対してパターンマッチを実現する関数 */
      var match = (exp, pattern) => { 
        return exp.call(pattern, pattern);
      };
      /* #@range_end(match_in_algebraic_datatype) */
      /* #@range_begin(list_function_using_algebraic_datatype) */
      var isEmpty = (list) => {
        return match(list, { // match関数で分岐する
          empty: true,
          cons: (head, tail) => { // headとtailにそれぞれ先頭要素、末尾要素が入る
            return false;
          },
        })
      };
      var head = (list) => {
        return match(list, {
          empty: undefined, // 空のリストには先頭要素はありません
          cons: (head, tail) => {
            return head;
          },
        })
      };
      var tail = (list) => {
        return match(list, {
          empty: undefined,  // 空のリストには末尾要素はありません
          cons: (head, tail) => {
            return tail;
          },
        })
      };
      /* #@range_end(list_function_using_algebraic_datatype) */
      /* #@range_begin(list_in_algebraic_datatype_test) */
      expect(
        isEmpty(empty)         // empty は空のリストではある
      ).to.eql(
        true
      )
      expect(
        isEmpty(cons(1,empty)) // [1] は空のリストではない
      ).to.eql(
        false
      )
      expect(
        head(cons(1,empty))    // [1]の先頭要素は 1 である
      ).to.be(
        1
      )
      expect(
        isEmpty(tail(cons(1,empty)))     // [1]の末尾要素は空のリストである
      ).to.be(
        true
      )
      expect(
        head(tail(cons(1,cons(2,empty)))) // [1,2]の2番目の要素は2である
      ).to.be(
        2
      )
      /* #@range_end(list_in_algebraic_datatype_test) */
      next();
    });
    it('男女の別をsum型で表現する', (next) => {
      var BMI = (weight /* kg */, height /* cm */) => {
        var height_in_meter = height / 100.0;
        return weight / (height_in_meter * height_in_meter);
      };
      var match = (exp, pattern) => {
        return exp.call(pattern, pattern);
      };
      var male = (data) => {
        return (pattern) => {
          return pattern.male(data);
        }
      };
      var female = (data) => {
        return (pattern) => {
          return pattern.female(data);
        };
      }
      var evaluate = (person) => {
        var self = this;
        return match(person, {
          male: (data) => {
            return {
              BMI: BMI(data.weight, data.height),
              /* total body water */
              TBW: data.weight * 0.6,
              /* estimated blood volume */
              EBV: 0.168 * Math.pow(data.height,3) + 0.050 * data.weight + 0.444
            };
          },
          female: (data) => {
            return {
              BMI: BMI(data.weight, data.height),
              TBW: data.weight * 0.5,
              /* estimated blood volume = 0.250 \times height^3 + 0.625 \times weight - 0.662 */
              EBV: 0.250 * Math.pow(data.height,3) + 0.625 * data.weight - 0.662
            };
          },
        });
      }
      var man = male({
        weight: 72,
        height: 175
      });
      var woman = female({
        weight: 54,
        height: 160
      });
      expect(
        evaluate(man).BMI
      ).to.be(23.510204081632654)
      expect(
        evaluate(woman).TBW
      ).to.be(27)
      next();
    });
    it('数式の例', (next) => {
      /* #@range_begin(algebraic_datatype) */
      var match = (exp, pattern) => {
        return exp.call(pattern, pattern);
      };
      var num = (n) => {
        return (pattern) => {
          return pattern.num(n);
        };
      };
      var add = (x, y) => {
        return (pattern) => {
          return pattern.add(x, y);
        };
      };
      var mul = (x, y) => {
        return (pattern) => {
          return pattern.mul(x, y);
        };
      };
      var calculate = (exp) => {
        return match(exp, { // パターンマッチを実行する
          num: (x) => {
            return x;
          },
          add: (x, y) => {
            return calculate(x) + calculate(y);
          },
          mul: (x, y) => {
            return calculate(x) * calculate(y);
          }
        });
      };
      var exp = add(num(1), mul(num(2), num(3)));
      expect(
        calculate(exp)
      ).to.eql(
        7
      )
      /* #@range_end(algebraic_datatype) */
      next();
    });
  });
  // ## 反復処理の種類と特徴
  describe("反復処理の種類と特徴", function() {
    describe("while文", () => {
      it("カウント", (next) => {
        /* #@range_begin(while_counter) */
        var counter = 0;         // 変数の初期化
        while (counter < 10) {   // 反復の条件
          counter = counter + 1; // 変数の更新
        }
		/* テスト */
        expect(
          counter
        ).to.eql(
          10
        );
        /* #@range_end(while_counter) */
        next();
      });
      /*
      it("length", (next) => {
        // #@range_begin(while_length)
        var array = [1,2,3,4,5];
        var length = (array) => {
         var counter = 0;
         while (counter < 10) {
            counter += 1;
         }
        }
        expect(
          counter
        ).to.eql(
          10
        );
        // #@range_end(while_length)
        next();
      });
      it("whileによるsum", (next) => {
        // #@range_begin(while_sum)
        var array = [1,2,3,4,5];
        var sum = (array) => {
         var counter = 0;
         while (counter < 10) {
            counter += 1;
         }
        }
        expect(
          counter
        ).to.eql(
          10
        );
        // #@range_end(while_sum)
        next();
      });
      */
    });
    it("for文", (next) => {
      /* #@range_begin(for_example) */
      for (var counter = 0; counter < 10; counter += 1) {
        ;
      }
	  /* テスト */
      expect(
        counter
      ).to.eql(
        10
      );
      /* #@range_end(for_example) */
      next();
    });
    describe('forEach文', () => {
      it("forEach文によるsum", (next) => {
        /* #@range_begin(forEach_sum) */
        var array = [1,2,3,4,5];
        var sum = 0;
        array.forEach((element) => {
          sum += element;
        });
		/* テスト */
        expect(
          sum
        ).to.eql(
          15
        );
        /* #@range_end(forEach_sum) */
        next();
      });
      it("forEach文によるlength", (next) => {
        /* #@range_begin(forEach_length) */
        var length = (array) => {
          var result = 0;
          array.forEach((element) => {
            result += 1;
          });
          return result;
        };
        expect(
          length([1,2,3,4,5])
        ).to.eql(
          5
        );
        /* #@range_end(forEach_length) */
        next();
      });
    });
    describe('再帰処理', () => {
      var list = {
        empty: [],
        cons: (n, list) => {
          return [n].concat(list);
        },
        head: (list) => {
          return list[0];
        },
        tail: (list) => {
          return list.slice(1,list.length);
        },
        isEmpty: (list) => {
          return list.length === 0;
        }
      };
      it('lengthの例', (next) => {
        var match = (exp, pattern) => {
          return exp.call(pattern, pattern);
        };
        var empty = (pattern) => {
          return pattern.empty;
        };
        var cons = (x, xs) => {
          return (pattern) => {
            return pattern.cons(x, xs);
          };
        };
        var isEmpty = (list) => {
          return match(list, {
            empty: true,
            cons: (head, tail) => {
              return false;
            },
          })
        };
        var head = (list) => {
          return match(list, {
            empty: undefined,
            cons: (head, tail) => {
              return head;
            },
          })
        };
        var tail = (list) => {
          return match(list, {
            empty: undefined,
            cons: (head, tail) => {
              return tail;
            },
          })
        };
        /* #@range_begin(recursive_length) */
        var length = (list, accumulator) => {
          return match(list, {
            empty: accumulator,
            cons: (head, tail) => {
              return length(tail, accumulator + 1); // length関数を再帰的に呼び出す
            },
          })
        };
		/* テスト */
        expect(
          length(empty, 0)
        ).to.eql(
          0
        )
        expect(
          length(cons(1,empty), 0)
        ).to.eql(
          1
        )
        expect(
          length(cons(1,cons(2,cons(3,empty))),0)
        ).to.eql(
          3
        )
        /* #@range_end(recursive_length) */
        /* #@range_begin(recursive_sum) */
        var sum = (list, accumulator) => {
          return match(list, {
            empty: accumulator,
            cons: (head, tail) => {
              return sum(tail, accumulator + head);
            },
          })
        };
        expect(
          sum(empty, 0)
        ).to.eql(
          0
        )
        expect(
          sum(cons(1,empty), 0)
        ).to.eql(
          1
        )
        expect(
          sum(cons(1,cons(2,cons(3,empty))),0)
        ).to.eql(
          6
        )
        /* #@range_end(recursive_sum) */
        next();
      });
      describe('mapの例', () => {
        /* #@range_begin(recursive_map) */
        var map = (alist, transform) => {
          if (list.isEmpty(alist)) {
            return list.empty;
          } else {
            var x = list.head(alist);
            var xs = list.tail(alist);
            return list.cons(transform(x), map(xs, transform));
          }
        };
        /* #@range_end(recursive_map) */
        var alist = list.cons(1,
                              list.cons(2,
                                        list.cons(3,
                                                  list.empty)));
        var double = (n) => {
          return n * 2;
        };
        var doubledList = map(alist, double);
        expect(
          list.head(doubledList)
        ).to.eql(
          2
        );
        expect(
          list.head(list.tail(doubledList))
        ).to.eql(
          4
        );
        expect(
          list.head(list.tail(list.tail(doubledList)))
        ).to.eql(
          6
        );
      });
      describe('sumの例', () => {
        /* #@range_begin(recursive_sum) */
        var sum = (alist, accumulator) => {
          if (list.isEmpty(alist)) {
            return accumulator;
          } else {
            var x = list.head(alist);
            var xs = list.tail(alist);
            return sum(xs, accumulator + x);
          }
        };
        /* #@range_end(recursive_sum) */
        var alist = list.cons(1,
                              list.cons(2,
                                        list.cons(3,
                                                  list.empty)));
        expect(
          sum(alist, 0)
        ).to.eql(
          6
        );
      });
      describe('maximumの例', () => {
        /* #@range_begin(recursive_maximum) */
        var maximum = (alist, accumulator) => {
          if (list.isEmpty(alist)) {
            return accumulator;
          } else {
            var x = list.head(alist);
            var xs = list.tail(alist);
            if(x > accumulator) {
              return maximum(xs, x);
            } else {
              return maximum(xs, accumulator);
            }
          }
        };
        /* #@range_end(recursive_maximum) */
        var alist = list.cons(2,
                              list.cons(4,
                                        list.cons(1,
                                                  list.empty)));
        expect(
          maximum(alist, 0)
        ).to.eql(
          4
        );
      });
      describe('factorialの例', () => {
        it("factorial by recursive process", function(next) {
          /* #@range_begin(recursive_factorial) */
          var factorial = (n) => {
            if (n === 1) {
              return 1;
            } else {
              return n * factorial(n - 1);
            }
          };
          /* #@range_end(recursive_factorial) */
          expect(
            factorial(6)
          ).to.eql(
            720
          );
          next();
        });
        it("factorial by iterative process", function(next) {
          /* #@range_begin(iterative_factorial) */
          var factorial = (n) => {
            return fact_iter(1, 1, n);
          };
          var fact_iter = (product, counter, max_count) => {
            if (counter > max_count) {
              return product;
            } else {
              return fact_iter(counter * product, counter + 1, max_count);
            }
          };
          expect(factorial(6)).to.eql(720);
          /* #@range_end(iterative_factorial) */
          next();
        });
        it('命令的factorial', function(next) {
          /* #@range_begin(imperative_factorial) */
          var factorial = function(n) {
            var index, result;
            result = 1;
            index = 1;
            while (index < n + 1) {
              result = result * index;
              index = index + 1;
            }
            return result;
          };
          /* #@range_end(imperative_factorial) */
          expect(
            factorial(5)
          ).to.eql(
            120
          );
          next();
        });
      });
    });
  });
});
