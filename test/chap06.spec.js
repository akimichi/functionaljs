"use strict";

var expect = require('expect.js');

// 関数の基本
// ========
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
	  // 	return (any) => {
	  // 	  if(head(list)) {
	  // 		if(head(list) === any){
	  // 		  return
	  // 		} else {
	  // 		}
	  // 	} else {
	  // 	}
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
	  var compose = (f) => {
		var self = this;
		return (g) => {
		  return (_) => {
			return f.call(self, g.apply(self, arguments));
		  };
  		};
	  };
	  /* #@range_end(compose_definition) */
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
});
