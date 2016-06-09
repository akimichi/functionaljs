"use strict";

// プログラムをコントロールする仕組み
// ============================

var expect = require('expect.js');
var util = require('util');

var truthy = (any) => {
  return any !== false && any != null;
};

var match = (data, pattern) => {
  return data.call(pattern, pattern);
};

var list  = {
  empty: (_) => {
    return (pattern) => {
      return pattern.empty();
    };
  },
  cons: (head, tail) => {
    return (pattern) => {
      return pattern.cons(head, tail);
    };
  },
  head: (seq) => {
    return match(seq, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return head;
      }
    });
  },
  tail: (seq) => {
    return match(seq, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return tail;
      }
    });
  },
  isEmpty: (seq) => {
    return match(seq, {
      empty: (_) => {
        return true;
      },
      cons: (head, tail) => {
        return false;
      }
    });
  },
  /* list#append */
  /* append:: LIST[T] -> LIST[T] -> LIST[T] */
  append: (xs) => {
    return (ys) => {
      if (list.isEmpty(xs)) {
        return ys;
      } else {
        return list.cons(list.head(xs), (list.append(list.tail(xs))(ys)));
      }
    };
  },
  last: (seq) => {
    return match(seq, {
      empty: (_) => {
        return null;
      },
      cons: (head, tail) => {
        return match(tail, {
          empty: (_) => {
            return head;
          },
          cons: (head, _) => {
            return list.last(tail);
          }
        });
      }
    });
  },
  /* join:: LIST[LIST[T]] -> LIST[T] */
  join: (list_of_list) => {
    if (self.isEmpty(list_of_list)) {
      return list.empty();
    } else {
      return list.append(list.head(list_of_list))(list.join(list.tail(list_of_list)));
    }
  },
  /* foldr:: LIST[T] -> T -> FUNC[T -> LIST] -> T */
  foldr: (seq) => {
    return (accumulator) => {
      return (glue) => {
        expect(glue).to.a('function');
        return match(seq, {
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            return glue(head)(list.foldr(tail)(accumulator)(glue));
          }
        });
      };
    };
  },
  /* map:: LIST[T] -> FUNC[T -> T] -> LIST[T] */
  map: (seq, transform) => {
    return match(seq, {
      empty: (_) => {
        return list.empty();
      },
      cons: (x, xs) => {
        return list.cons(transform(x), list.map(xs, transform));
      }
    });
  },
  /* #@range_begin(list_reverse) */
  reverse: (seq) => {
    var reverseHelper = (seq, accumulator) => {
      return match(seq, {
        empty: (_) => {  // emptyの場合は、終了条件となる
          return accumulator;
        },
        cons: (head, tail) => { // consの場合は、再帰的に呼び出す
          return reverseHelper(tail, list.cons(head, accumulator));
        }
      });
    };
    return reverseHelper(seq, list.empty());
  },
  /* #@range_end(list_reverse) */
  /* #@range_begin(list_filter) */
  filter: (seq) => {
    return (predicate) => {
      expect(predicate).to.a('function');
      var filterAux = (seq, accumulator) => {
        return match(seq, {
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            if (predicate(head) === true) {
              return list.append(list.append(accumulator)(list.cons(head, list.empty())))(filterAux(tail, accumulator));
            } else  {
              return filterAux(tail, accumulator);
            }
          }
        });
      };
      return filterAux(seq, list.empty());
    };
  },
  /* list#length */
  length: (seq) => {
    return match(seq, {
      empty: (_) => {
        return 0;
      },
      cons: (head, tail) => {
        return list.foldr(seq)(0)((item) => {
          return (accumulator) => {
            return 1 + accumulator;
          };
        });
      }
    });
  },
  any: (seq) => {
    return (predicate) => {
      expect(predicate).to.a('function');
      return match(seq, {
        empty: (_) => {
          return false;
        },
        cons: (head, tail) => {
          if (truthy(predicate(head))) {
            return true;
          } else {
            return list.any(tail)(predicate);
          }
        }
      });
    };
  },
  /* #@range_end(list_filter) */
  toArray: (seq) => {
    var toArrayHelper = (seq, accumulator) => {
      return match(seq, {
        empty: (_) => {
          return accumulator;  // 空のリストの場合は終了
        },
        cons: (head, tail) => {
          return toArrayHelper(tail, accumulator.concat(head));
        }
      });
    };
    return toArrayHelper(seq, []);
  }
};

// ## 条文分岐の種類と特徴
describe('条文分岐の種類と特徴', () => {
  // ### 条文分岐としてのif文
  describe('条件分岐としてのif文', () => {
    it('偶数の例', (next) => {
      /* ##@range_begin(even_function)*/
var even = (n) => {
  if ((n % 2) === 0) { // 2で割った余りが0の場合
    return true;
  } else {             // 2で割った余りが0でない場合
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
      var inverse = (n) => {
      if (n === 0) {
      return 0;
      } else {
      return 1 / n;
      }
      };
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
    it('returnで関数を抜ける', (next) => {
      /* ##@range_begin(even_function_again) */
var even = (n) => {
  if ((n % 2) === 0) {
    /* returnでeven関数を抜けてtrueを返す */
    return true; 
  } else {
    /* returnでeven関数を抜けてfalseを返す */
    return false;
  }
};
      /* ##@range_end(even_function_again) */
      next();
    });
    it("三項演算子", (next) => {
      /* ##@range_begin(trinary_if) */
      var even = (n) => {
        return (n % 2) === 0 ?  true : false;
      };
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
      /* ##@range_end(trinary_if) */
      next();
    });
  });
  // ## 再帰呼び出しの条件
  describe('再帰呼び出しの条件', () => {
    /* ##@range_begin(infiniteLoop) */
var infiniteLoop = (_) => {
  return infiniteLoop(_);
};
    /* ##@range_end(infiniteLoop) */
    it('ifの非正格性', (next) => {
      /* ##@range_begin(if_nonstrict) */
      var infiniteLoop = (_) => {
        return infiniteLoop(_);     /* 同じ関数を実行するので無限ループになります */
      };
      var lessThanFive = (n) => {
        if (n < 5) {
          return true;
        } else {
          return infiniteLoop(); // ここが実行されると無限ループになります
        }
      };
      /* テスト */
      expect(
        lessThanFive(1)
      ).to.eql(
        true
      );
      /* このテストは実行されると無限ループになるのでコメントアウトしています
         expect(
         lessThanFive(10)
         ).to.eql(
         false // 無限ループ
         );
      */
      /* ##@range_end(if_nonstrict) */
      next();
    });
    it('条件式の実装(不完全)', (next) => {
      /* #@range_begin(functional_if_strict) */
      var functionalIf = (predicate, trueClause, falseClause) => {
        if (predicate) {
          return trueClause; // 判定式が真の場合に実行する
        } else {
          return falseClause;  // 判定式が真の場合に実行する
        }
      };
      /* #@range_end(functional_if_strict) */
      /* テスト */
      expect(
        functionalIf((2 < 3), 2, 3)
      ).to.eql(
        2
      );
      expect(
        functionalIf((2 > 3), 2, 3)
      ).to.eql(
        3
      );
      next();
    });
    it('条件式の実装', (next) => {
      /* #@range_begin(functional_if) */
      var functionalIf = (predicate, pattern) => {
        if (predicate) {
          return pattern.thenClause();  // 判定式が真の場合に、pattern.thenClauseの関数を実行する
        } else {
          return pattern.elseClause();  // 判定式が真の場合に、pattern.elseClauseの関数を実行する
        };
      };
      /* テスト */
      expect(
        functionalIf((2 < 3), {
          thenClause: () => { // 関数で包む必要がある
            return true;
          },
          elseClause: () => {
            return infiniteLoop();
          }
        })
      ).to.eql(
        true
      );
      /* #@range_end(functional_if) */
      /* #@range_begin(functional_if_test) */
      var even = (n) => {
        return functionalIf((n % 2) === 0, {
          thenClause: () => {
            return true;
          },
          elseClause: () => {
            return false;
          }
        });
      };
      /* テスト */
      expect(
        even(2)
      ).to.eql(
        true
      );
      /* #@range_end(functional_if_test) */

      /* #@range_begin(multiplyOf)            */
      var multiplyOf = (n) => {
        return (m) => {
          return functionalIf(m % n === 0, {
            thenClause: () => {
              return true;
            },
            elseClause: () => {
              return false;
            }
          });
        };
      };
      /* テスト */
      var threeFold = multiplyOf(3);
      expect(
        threeFold(2)
      ).to.eql(
        false
      );
      expect(
        threeFold(3)
      ).to.eql(
        true
      );
      /* #@range_end(multiplyOf)            */
      next();
    });
  });
  it('関数渡しで反復文を構築する', function(next) {
    /* #@range_begin(loop)            */
    var loop = function(pred, accumulator, expression) {
      if (pred(accumulator)) {
        return loop(pred, expression(accumulator), expression);
      } else {
        return accumulator;
      }
    };
    var lessThan = function(n) {
      return function(x) {
        return x < n;
      };
    };
    var succ = function(n) {
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
            if (x === y) {
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
      switch(light) {
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
    };
    /* #@range_end(signal) */
    next();
  });
  it("可変データとのマッチング", (next) => {
    /* #@range_begin(switch_for_mutable) */
var match_for_mutable = (array) => {
  switch(array) {
  case [1, 2, 3]:  // [1, 2, 3] とマッチさせたい
    return true;   // マッチすれば、trueを返す
    break;
  default:
    return false;  // マッチしなければ、falseを返す
  }
};
/* テスト */
expect(
  match_for_mutable([1, 2, 3])
).to.eql(
  false  // case [1, 2, 3] にはマッチしない
);
    /* #@range_end(switch_for_mutable) */
    next();
  });
  it("通貨の例", (next) => {
    /* #@range_begin(currency) */
    var move;
    var signal = (currency) => {
      switch(currency) {
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
    };
    /* #@range_end(currency) */
    next();
  });
  it("ネストされたif文で3つ以上の条件分岐を表現する", (next) => {
    /* #@range_begin(compare) */
var compare =  (n, m) => {
  if (n > m) {      // nがmよりも大きいケース
    return 1;
  } else {
    if (n === m) {  // ネストされたif文
      return 0;
    } else {
      return -1;
    }
  }
};
/* テスト */
/* 3 は 2 よりも大きい */
expect(
  compare(3, 2)
).to.eql(
  1
);
/* 2 は 3 よりも小さい */
expect(
  compare(2, 3)
).to.eql(
  -1
);
    /* #@range_end(compare) */
    /* 1 と 1 は等しい */
    expect(
      compare(1, 1)
    ).to.eql(
      0
    );
    next();
  });
  it("elseif文で3つ以上の条件分岐を表現する", (next) => {
    /* #@range_begin(elseif) */
var compare =  (n, m) => {
  if (n > m) {
    return 1;
  } else if (n === m) { // elseにif文を続ける
    return 0;
  } else  {
    return -1;
  }
    };
    /* #@range_end(elseif) */
    /* テスト */
    /* 3 は 2 よりも大きい */
    expect(
      compare(3, 2)
    ).to.eql(
      1
    );
    /* 1 と 1 は等しい */
    expect(
      compare(1, 1)
    ).to.eql(
      0
    );
    /* 2 は 3 よりも小さい */
    expect(
      compare(2, 3)
    ).to.eql(
        -1
    );
    next();
  });
  describe('コンビネータライブラリー', () => {
    var event = {
      hour: 13,
      temperture: 25,
      heater: true
    };
    it('ifで実装する', (next) => {
      /*
        昼間(12~18)は26~28度
        夜(19~23)は24~26度
        深夜(0~6)は21~23度
        朝(7~11)は24~26度
      */
      /* thermostat:: (EVENT, BOOL) -> BOOL */
      var thermostat = (event) => {
        if (event.hour > 12 && event.hour < 19) {
          if (event.temperture > 28) {
            return false;
          } else {
            if (event.temperture < 26) {
              return true;
            } else {
              return event.heater;
            }
          }
        }
        if (event.hour > 18 && event.hour < 24) {
          if (event.temperture > 26) {
            return false;
          } else {
            if (event.temperture < 24) {
              return true;
            } else {
              return event.heater;
            }
          }
        }
        if (event.hour > 0 && event.hour < 7) {
          if (event.temperture > 23) {
            return false;
          } else {
            if (event.temperture < 21) {
              return true;
            } else {
              return event.heater;
            }
          }
        }
        if (event.hour > 6 && event.hour < 12) {
          if (event.temperture > 26) {
            return false;
          } else {
            if (event.temperture < 24) {
              return true;
            } else {
              return event.heater;
            }
          }
        }
      };
      expect(
        thermostat({ hour: 22, temperture: 25, heater: false})
      ).to.eql(
        false
      );
      expect(
        thermostat({ hour: 7, temperture: 22, heater: false})
      ).to.eql(
        true
      );
      next();
    });
    it('コンビネーターで実装する', (next) => {
      var functionalIf = (predicate, pattern) => {
        if (predicate) {
          return pattern.thenClause();
        } else {
          return pattern.elseClause()
        };
      };
      /* expect:: STRING -> OBJECT -> ANY */
      var extract = (key) => {
        return (obj) => {
          return obj[key];
        };
      };
      /* hour:: EVENT -> NUMBER */
      var hour = extract('hour');
      /* temperture:: EVENT -> NUMBER */
      var temperture = extract('temperture');
      /* heater:: EVENT -> BOOL */
      var heater = extract('heater');
      /* and:: (ANY -> BOOL, ANY -> BOOL) -> ANY -> BOOL */
      var and = (former, latter) => {
        expect(former).to.a('function');
        expect(latter).to.a('function');
        return (data) => {
          return former(data) && latter(data);
        };
      };
      var not = (predicate) => {
        return (data) => {
          return ! predicate(data);
        };
      };
      var or = (former, latter) => {
        expect(former).to.a('function');
        expect(latter).to.a('function');
        return (data) => {
          return former(data) || latter(data);
        };
      };
      /* isMoreThan:: NUMBER -> NUMBER -> BOOL */
      var isMoreThan = (n) => {
        expect(n).to.a('number');
        return (m) => {
          expect(m).to.a('number');
          return m > n;
        };
      };
      /* negate:: (NUMBER -> NUMBER -> BOOL) -> BOOL */
      var negate = (predicate) => {
        return (n) => {
          return (m) => {
            return ! predicate(n)(m);
          };
        };
      };
      var isLessThan = negate(isMoreThan);
      expect(
        isMoreThan(3)(4)
      ).to.eql(
        true
      );
      expect(
        negate(isMoreThan)(3)(4)
      ).to.eql(
        false
      );
      expect(
        isLessThan(3)(4)
      ).to.eql(
        false
      );
      /* within:: (NUMBER, NUMBER) -> (EVENT -> ANY) -> EVENT -> BOOL */
      var within = (lower, upper) => {
        return (extractor) => {
          expect(extractor).to.a('function');
          return (event) => {
            expect(event).to.an('object');
            return and(isMoreThan(lower), isLessThan(upper))(extractor(event));
          };
        };
      };
      expect(
        within(12, 18)(hour)({ hour: 16, temperture: 25, heater: false})
      ).to.eql(
        true
      );
      expect(
        within(12, 18)(temperture)({ hour: 16, temperture: 25, heater: false})
      ).to.eql(
        false
      );
      /* afternoon:: EVENT -> BOOL */
      var afternoon = within(12, 18)(hour);
      var night = within(19, 23)(hour);
      var midnight = within(0, 6)(hour);
      var morning = within(7, 12)(hour);
      expect(
        afternoon({ hour: 8, temperture: 25, heater: false})
      ).to.eql(
        false
      );
      /* above:: NUMBER -> (EVENT -> ANY) -> EVENT -> BOOL */
      var above = (threshold) => {
        return (extractor) => {
          expect(extractor).to.a('function');
          return (event) => {
            expect(event).to.an('object');
            return isMoreThan(threshold)(extractor(event));
          };
        };
      };
      expect(
        temperture({ hour: 8, temperture: 25, heater: false})
      ).to.eql(
        25
      );
      expect(
        above(20)(temperture)({ hour: 8, temperture: 25, heater: false})
      ).to.eql(
        true
      );
      /* below:: NUMBER -> (EVENT -> ANY) -> EVENT -> BOOL */
      var below = (threshold) => {
        return (extractor) => {
          expect(extractor).to.a('function');
          return (event) => {
            expect(event).to.an('object');
            return isLessThan(threshold)(extractor(event));
          };
        };
      };
      expect(
        below(20)(temperture)({ hour: 8, temperture: 25, heater: false})
      ).to.eql(
        false
      );

      /* afternoon:: EVENT -> BOOL */
      /* above(28)(temperture):: EVENT -> BOOL */
      expect(
        and(afternoon
            , above(28)(temperture))({ hour: 13, temperture: 29, heater: false})
      ).to.eql(
        true
      );
      thermostat: (timezone, temperaturezone) => {
        return (event) => {

        };
      };
      /* heaterOn:: (TIMEZONE, TEMPERATUREZONE) -> EVENT -> BOOL */

      next();
    });
  });
  // ### コンビネータによる条件分岐の改善
  describe('コンビネータによる条件分岐の改善', () => {
    var multiplyOf = (n) => {
      return (m) => {
        if ((m % n) === 0) {
          return true;
        } else {
          return false;
        }
      };
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
      var twoFold = multiplyOf(2); /* 2の倍数を判定する */
      var threeFold = multiplyOf(3); /* 3の倍数を判定する */
      var fiveFold = multiplyOf(5); /* 5の倍数を判定する */

      var twoFoldAndThreeFoldButNotFiveFold = (n) => {
        if (twoFold(n) && threeFold(n) && ! fiveFold(n)) {
          return true;
        } else {
          return false;
        }
      };
      /* テスト */
      expect(
        twoFoldAndThreeFoldButNotFiveFold(6)
      ).to.eql(
        true
      );
      expect(
        twoFoldAndThreeFoldButNotFiveFold(30)
      ).to.eql(
        false
      );
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
      );
      /* ##@range_begin(logical_combinator_test) */
      expect(
        /* 「2の倍数かつ3の倍数で、5の倍数ではない」*/
        and(and(twoFold, threeFold), not(fiveFold))(6)
      ).to.eql(
        true
      );
      /* ##@range_end(logical_combinator_test) */
      expect(
        and(twoFold, threeFold)(6)
      ).to.eql(
        true
      );
      expect(
        and(twoFold, threeFold)(4)
      ).to.eql(
        false
      );
      expect(
        and(and(twoFold, threeFold), not(fiveFold))(4)
      ).to.eql(
        false
      );
      /* 2の倍数もしくは3の倍数で5の倍数でもあるもの */
      /* ##@range_begin(another_logical_combinator_test) */
      expect(
        /* 「2の倍数もしくは3の倍数、かつ5の倍数でもある」 */
        and(or(twoFold, threeFold), fiveFold)(10)
      ).to.eql(
        true
      );
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
var empty = () => { // 空のリスト
  return (pattern) => {
    return pattern.empty();
  };
};
var cons = (value, list) => { // 空でないリスト
  return (pattern) => {
    return pattern.cons(value, list);
  };
};
    /* #@range_end(list_in_algebraic_datatype) */
    /* #@range_begin(match_in_algebraic_datatype) */
/* 代数的データ構造に対してパターンマッチを実現する関数 */
var match = (data, pattern) => {
  return data(pattern);
};
    /* #@range_end(match_in_algebraic_datatype) */
    /* #@range_begin(list_function_using_algebraic_datatype) */
/* isEmpty関数は、引数alistに渡されたリストが空のリストかどうかを判定する */
var isEmpty = (alist) => {
  /* match関数で分岐する */
  return match(alist, { 
    // emptyにマッチするケース
    empty: (_) => {  
      return true;
    },
    // consにマッチするケース
    cons: (head, tail) => {  // headとtailにそれぞれ先頭と後尾が入る
      return false;
    }
  });
};
/* head関数は、引数alistに渡されたリストの先頭の要素を返す */
var head = (alist) => {
  return match(alist, {
    /* 空のリストには先頭要素はない */
    empty: (_) => {
      return null; 
    },
    cons: (head, tail) => {
      return head;
    }
  });
};
/* tail関数は、引数alistに渡されたリストの後尾のリストを返す */
var tail = (alist) => {
  return match(alist, {
    /* 空のリストには後尾はない */
    empty: (_) => {
      return null;  
    },
    cons: (head, tail) => {
      return tail;
    }
  });
};
    /* #@range_end(list_function_using_algebraic_datatype) */
    /* #@range_begin(list_in_algebraic_datatype_test) */
/* emptyは空のリストか */
expect(
  isEmpty(empty())
).to.eql(
  true
);
/* cons(1, empty())は空のリストか */
expect(
  isEmpty(cons(1, empty()))
).to.eql(
  false
);
/* cons(1, empty())の先頭要素は1である */
expect(
  head(cons(1, empty()))   
).to.eql(
  1
);
/* cons(1, cons(2, empty()))の2番目の要素は2である */
expect(
  head(tail(cons(1, cons(2, empty())))) 
).to.eql(
  2
);
    /* #@range_end(list_in_algebraic_datatype_test) */
    expect(
      isEmpty(tail(cons(1, empty())))     // [1]の末尾要素は空のリストである
    ).to.be(
      true
    );
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
            EBV: 0.168 * Math.pow(data.height, 3) + 0.050 * data.weight + 0.444
          };
        },
        female: (data) => {
          return {
            BMI: BMI(data.weight, data.height),
            TBW: data.weight * 0.5,
            /* estimated blood volume = 0.250 \times height^3 + 0.625 \times weight - 0.662 */
            EBV: 0.250 * Math.pow(data.height, 3) + 0.625 * data.weight - 0.662
          };
        }
      });
    };
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
    ).to.be(23.510204081632654);
    expect(
      evaluate(woman).TBW
    ).to.be(27);
    next();
  });
  it('数式の例', (next) => {
    /* #@range_begin(expression_algebraic_datatype) */
var num = (n) => {
  return (pattern) => {
    return pattern.num(n);
  };
};
var add = (exp1, exp2) => {
  return (pattern) => {
    return pattern.add(exp1, exp2);
  };
};
var mul = (exp1, exp2) => {
  return (pattern) => {
    return pattern.mul(exp1, exp2);
  };
};
    /* #@range_end(expression_algebraic_datatype) */
    /*
    var exp = {
      match : (exp, pattern) => {
        return exp.call(exp, pattern);
      },
      num : (n) => {
        return (pattern) => {
          return pattern.num(n);
        };
      },
      add : (exp1, exp2) => {
        return (pattern) => {
          return pattern.add(exp1, exp2);
        };
      },
      mul : (exp1, exp2) => {
        return (pattern) => {
          return pattern.mul(exp1, exp2);
        };
      }
    };
    */
    /* #@range_begin(expression_algebraic_datatype_recursion) */
var calculate = (exp) => {
  return match(exp, { // パターンマッチを実行する
    num: (n) => {
      return n;
    },
    add: (expL, expR) => {
      /* calculateを再帰的に呼び出して足し算を実行する */
      return calculate(expL) + calculate(expR); 
    },
    mul: (expL, expR) => {
      /* calculateを再帰的に呼び出してかけ算を実行する */
      return calculate(expL) * calculate(expR); 
    }
  });
};
/**** テスト ****/
/* 1 + (2 * 3) を計算する */
var expression = add(num(1),
                     mul(num(2),
                         num(3)));
expect(
  calculate(expression)
).to.eql(
  7
);
    /* #@range_end(expression_algebraic_datatype_recursion) */
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
      var array = [1, 2, 3, 4, 5];
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
      next();
      });
      it("whileによるsum", (next) => {
      var array = [1, 2, 3, 4, 5];
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
var array = [1, 2, 3, 4, 5];
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
      /* テスト */
      expect(
        length([1, 2, 3, 4, 5])
      ).to.eql(
        5
      );
      /* #@range_end(forEach_length) */
      next();
    });
  });
});
// ## 再帰による反復処理
describe('再帰による反復処理', () => {
  describe('複利法の例', () => {
    /* f(n) = f(n-1) * (1 + r) */
    /* #@range_begin(compound_interest) */
var compoundInterest = (a, r, n) => {
  if (n === 0) { // 0年目は利率が付かないので元金がそのまま返る
    return a;
  } else {
    /* compoundInterestの再帰呼び出し */
    return compoundInterest(a, r, n - 1) * (1 + r); 
  }
};
    /* #@range_end(compound_interest) */
    it("複利の計算", (next) => {
      expect(
        compoundInterest(100000, 0.02, 1)
      ).to.eql(
        102000
      );
      expect(
        compoundInterest(100000, 0.02, 2)
      ).to.eql(
        104040
      );
      expect(
        compoundInterest(100000, 0.02, 25)
      ).to.eql(
        164060.59944647306
      );
      next();
    });
  });
  // ## 再帰処理の利点
  describe('再帰処理の利点', () => {
    var match = (exp, pattern) => {
      return exp.call(pattern, pattern);
    };
    var empty = (_) => {
      return (pattern) => {
        return pattern.empty(_);
      };
    };
    var cons = (x, xs) => {
      return (pattern) => {
        return pattern.cons(x, xs);
      };
    };
    var isEmpty = (list) => {
      return match(list, {
        empty: (_) => {
          return true;
        },
        cons: (head, tail) => {
          return false;
        }
      });
    };
    var head = (list) => {
      return match(list, {
        empty: (_) => {
          return null;
        },
        cons: (head, tail) => {
          return head;
        }
      });
    };
    var tail = (list) => {
      return match(list, {
        empty: (_) => {
          return null;
        },
        cons: (head, tail) => {
          return tail;
        }
      });
    };
    it('蓄積変数を持つlength関数', (next) => {
      /* #@range_begin(recursive_length) */
      var length = (list, accumulator) => {
        return match(list, {
          empty: (_) => {
            return accumulator;
          },
          cons: (head, tail) => {
            return length(tail, accumulator + 1); // length関数を再帰的に呼び出す
          }
        });
      };
      /************************ テスト ************************/
      expect(
        length(empty(), 0)                        // []の長さは0
      ).to.eql(
        0
      );
      expect(
        length(cons(1, empty()), 0)                // [1]の長さは1
      ).to.eql(
        1
      );
      expect(
        length(cons(1, cons(2, cons(3, empty()))), 0) // [1, 2, 3]の長さは3
      ).to.eql(
        3
      );
      /* #@range_end(recursive_length) */
      /* #@range_begin(recursive_sum) */
var sum = (alist, accumulator) => {
  return match(alist, {
    empty: (_) => {
      return accumulator;
    },
    cons: (head, tail) => {
      /* sumの再帰呼び出し */
      return sum(tail, accumulator + head); 
    }
  });
};
      /* #@range_end(recursive_sum) */
      /**** テスト ****/
      expect(
        sum(empty(), 0)
      ).to.eql(
        0
      );
      expect(
        sum(cons(1, empty()), 0)
      ).to.eql(
        1
      );
      expect(
        sum(cons(1, cons(2, cons(3, empty()))), 0)
      ).to.eql(
        6
      );
      next();
    });
    it('蓄積変数を持たないsum関数', (next) => {
      /* #@range_begin(recursive_sum_without_accumulator) */
var sum = (list) => {
  /* 蓄積変数を持つ補助関数 */
  var sumHelper = (list, accumulator) => { 
    return match(list, {
      empty: (_) => {
        return accumulator;
      },
      cons: (head, tail) => {
        return sumHelper(tail, accumulator + head);
      }
    });
  };
  return sumHelper(list, 0); // 補助関数を呼び出す
};
      /* #@range_end(recursive_sum_without_accumulator) */
      /**** テスト ****/
      expect(
        sum(empty())
      ).to.eql(
        0
      );
      expect(
        sum(cons(1, cons(2, cons(3, empty()))))
      ).to.eql(
        6
      );
      next();
    });
    it('蓄積変数を持たないlength関数', (next) => {
      /* #@range_begin(recursive_length_without_accumulator) */
var length = (list) => {
  return match(list, {
    /* emptyの場合は、終了条件となる */
    empty: (_) => {    
      return 0;
    },
    /* consの場合は、length関数を再帰的に呼び出す */
    cons: (head, tail) => { 
      return 1 + length(tail);
    }
  });
};
      /* #@range_end(recursive_length_without_accumulator) */
      /************************ テスト ************************/
      expect(
        length(cons(1, cons(2, cons(3, empty())))) // [1, 2, 3]の長さは 3
      ).to.eql(
        3
      );
      next();
    });
  });
  describe('factorialの例', () => {
    it('素朴なfactorialの例', (next) => {
      /* #@range_begin(naive_factorial) */
      var factorial = (n) => {
        if (n === 1) {
          return 1;
        } else {
          return n * factorial(n - 1);
        }
      };
      expect(
        factorial(3)
      ).to.eql(
        6
      );
      /* #@range_end(naive_factorial) */
      next();
    });
    it('末尾再帰によるfactorialの例', (next) => {
      /* #@range_begin(tail_recursive_factorial) */
      var factorial = (n) => {
        var factorialRec = (n, accumulator) => {
          if (n === 1) {
            return 1 * accumulator;
          } else {
            return factorialRec(n - 1, n * accumulator);
          };
        };
        return factorialRec(n, 1);
      };
      expect(
        factorial(3)
      ).to.eql(
        6
      );
      /* #@range_end(tail_recursive_factorial) */
      next();
    });
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
    it('リストのmap', (next) => {
      /* #@range_begin(list_map) */
      /* map :: LIST[T] -> FUN[T -> T] -> LIST[T] */
      var map = (seq, transform) => {
        return match(seq, {
          empty: (_) => {
            return list.empty();
          },
          cons: (head, tail) => {
            return list.cons(transform(head), 
                             map(tail, transform));
          }
        });
      };
      /* #@range_end(list_map) */
      
      /* #@range_begin(list_toArray) */
      var toArray = (seq, callback) => {
        var toArrayHelper = (seq, accumulator) => {
          return match(seq, {
            empty: (_) => {
              return accumulator;  // 空のリストの場合は accumulator を返して終了する
            },
            cons: (head, tail) => { // 空のリストでなければ、再帰的に処理する
              return toArrayHelper(tail, accumulator.concat(head));
            }
          });
        };
        return toArrayHelper(seq, []);
      };
      /* #@range_end(list_toArray) */
      /* #@range_begin(list_map_test) */
      var numberList = list.cons(1,
                                 list.cons(2,
                                           list.empty()));
      var double = (number) => {
        return number * 2;
      };
      var doubledList = map(numberList, double);
      expect(
        list.head(doubledList)
      ).to.eql(
        2
      );
      expect(
        toArray(doubledList)
      ).to.eql(
        [2, 4]
      );
      var stringList = list.cons("a", list.cons("b", list.empty()));
      var upper = (string) => {
        return string.toUpperCase();
      };
      expect(
        toArray(map(stringList, upper))
      ).to.eql(
        ["A", "B"]
      );
      /* #@range_end(list_map_test) */
      next();
    });
    it('リストの連結', (next) => {
      var cons = (x, xs) => {
        return (pattern) => {
          return pattern.cons(x, xs);
        };
      };
      var toArray = (seq, callback) => {
        var toArrayAux = (seq, accumulator) => {
          return match(seq, {
            empty: (_) => {
              return accumulator;
            },
            cons: (head, tail) => {
              return toArrayAux(tail, accumulator.concat(head));
            }
          });
        };
        return toArrayAux(seq, []);
      };
      /* append :: (LIST[T], LIST[T]) -> LIST[T] */
      /* #@range_begin(list_append) */
var append = (xs, ys) => {
  return match(xs, {
    /* emptyの場合は、終了条件となる */
    empty: (_) => { 
      return ys; // xsが空の場合は、ysを返す
    },
    /* consの場合は、append関数を再帰的に呼び出す */
    cons: (head, tail) => { 
      /* xsとysを連結する */
      return cons(head, append(tail, ys));
    }
  });
};
      /* #@range_end(list_append) */
      
      /* #@range_begin(list_append_test) */
      var xs = list.cons(1,
                         list.cons(2,
                                   list.empty()));
      var ys = list.cons(3,
                         list.cons(4,
                                   list.empty()));
      expect(
        toArray(append(xs, ys)) // toArray関数でリストを配列に変換する
      ).to.eql(
        [1, 2, 3, 4]
      );
      /* #@range_end(list_append_test) */
      next();
    });
    it('リストの逆転', (next) => {
      var empty = (_) => {
        return (pattern) => {
          return pattern.empty(_);
        };
      };
      var cons = (x, xs) => {
        return (pattern) => {
          return pattern.cons(x, xs);
        };
      };
      /* #@range_begin(list_reverse) */
var reverse = (seq) => {
  var reverseHelper = (seq, accumulator) => {
    return match(seq, {
      /* emptyの場合は、終了条件となる */
      empty: (_) => {
        return accumulator;
      },
      /* consの場合は、reverse関数を再帰的に呼び出す */
      cons: (head, tail) => {
        return reverseHelper(tail, cons(head, accumulator));
      }
    });
  };
  return reverseHelper(seq, empty());
};
      /* #@range_end(list_reverse) */
      // expect(
      //   reverse(cons(1, cons(2, cons(3, empty()))))
      // ).to.eql(
      //   cons(1, cons(2, cons(3, empty())))
      // );
      next();
    });
  });
  describe('maximumの例', () => {
    /* #@range_begin(recursive_maximum) */
    var maximum = (alist, accumulator) => {
      if (list.isEmpty(alist)) {
        return accumulator;
      } else {
        var x = list.head(alist);
        var xs = list.tail(alist);
        if (x > accumulator) {
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
  describe('自然数の総和', () => {
    it('再帰版と一般形', (next) => {
      /* #@range_begin(triangularRecurrenceForm) */
      var natural_sum = (n) => {
        if (n === 1) {
          return 1;
        } else {
          return natural_sum(n-1) + n;
        }
      };
      expect(
        natural_sum(2)
      ).to.eql(
        3
      );
      expect(
        natural_sum(3)
      ).to.eql(
        6
      );
      /* #@range_end(triangularRecurrenceForm) */
      /* 自然数の総和(一般形) */
      /* #@range_begin(triangularClosedForm) */
      var closedForm = (n) => {
        return (n*(1 + n))/2;
      };
      expect(
        natural_sum(2)
      ).to.eql(
        closedForm(2)
      );
      expect(
        natural_sum(12)
      ).to.eql(
        closedForm(12)
      );
      /* #@range_end(triangularClosedForm) */
      next();
    });
  });
  // ### 再帰処理と帰納法
  describe('再帰処理と帰納法', () => {
    var length = (list) => {
      return match(list, {
        empty: (_) => {    // リストが空のときが終了条件となる
          return 0;
        },
        cons: (head, tail) => {
          return 1 + length(tail);
        }
      });
    };
    var append = (xs, ys) => {
      return match(xs, {
        empty: (_) => {
          return ys;
        },
        cons: (head, tail) => {
          return list.cons(head,
                           append(tail, ys));
        }
      });
    };
    it('リストの長さに関する命題P', (next) => {
      var empty = (_) => {
        return (pattern) => {
          return pattern.empty(_);
        };
      };
      var cons = (x, xs) => {
        return (pattern) => {
          return pattern.cons(x, xs);
        };
      };
      /* #@range_begin(statement_p_test) */
var xs = cons(1, cons(2, empty()));
var ys = cons(3, cons(4, empty()));
expect(
  length(append(xs, ys))  // 命題Pの左辺
).to.eql(
  length(xs) + length(ys) // 命題Pの右辺
);
      /* #@range_end(statement_p_test) */
      next();
    });
  });
}); // 再帰による反復処理
