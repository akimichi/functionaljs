"use strict";

var expect = require('expect.js');

// 関数の基本
// ========
describe('関数の基本', () => {
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
});
