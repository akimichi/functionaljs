"use strict";

var expect = require('expect.js');
var util = require('util');

describe('心の準備', () => {
  describe('DRY原則', () => {
    var add = (x,y) => {
      return x + y;
    };
    it('冗長なコードの例', (next) => {
      /* #@range_begin(redundant_code) */
      var timesForMultiply = (count,arg, memo) => {
        if(count > 1) {
          return timesForMultiply(count-1,arg, arg + memo);
        } else {
          return arg + memo;
        }
      };
      var multiply = (n,m) => {
        return timesForMultiply(n, m, 0);
      };
      var timesForExponential = (count,arg, memo) => {
        if(count > 1) {
          return timesForExponential(count-1,arg, arg * memo);
        } else {
          return arg * memo;
        }
      };
      var exponential = (n,m) => {
        return timesForExponential(m, n, 1);
      };
      /* #@range_end(redundant_code) */
      expect(
        multiply(2,3)
      ).to.eql(
        6
      );
      expect(
        exponential(2,3)
      ).to.eql(
        8
      );
      next();
    });
    it('DRYを適用する', (next) => {
      /* #@range_begin(dry_code) */
      var times = (count,fun,arg, memo) => {
        if(count > 1) {
          return times(count-1,fun,arg, fun(arg,memo));
        } else {
          return fun(arg,memo);
        }
      };
      var add = (n, m) => {
        return n + m;
      };
      var multiply = (n,m) => {
        return times(m, add, n, 0);
      };
      var exponential = (n,m) => {
        return times(m, multiply, n, 1);
      };
      /* #@range_end(dry_code) */
      expect(
        multiply(2,3)
      ).to.eql(
        6
      );
      expect(
        exponential(2,3)
      ).to.eql(
        8
      );
      expect(
        multiply(-2,3)
      ).to.eql(
        -6
      );
      next();
    });
    // it('リファクタリング', (next) => {
    //   var times = (count,fun,arg, memo) => {
    //     if(count > 1) {
    //       return times(count-1,fun,arg, fun(arg,memo));
    //     } else if (count < 0) {
    //       return times(count+1,fun,arg, fun(arg,memo));
    //     } else {
    //       return fun(arg,memo);
    //     }
    //   };
    //   var multiply = (n,m) => {
    //     var add = (n, m) => {
    //       return n + m;
    //     };
    //     var subtract = (n, m) => {
    //       return n - m;
    //     };
    //     var sign = (n,m, memo) => {
    //       if(n>0 && m >0) {
    //         return memo;
    //       } else {
    //         return subtract(0, memo);
    //       };
    //     };
    //     return sign(n,m,times(m, add, n, 0));
    //   };
    //   expect(
    //     multiply(-2,3)
    //   ).to.eql(
    //     -6
    //   );
    //   next();
    //   /*
    //    multiply(2,-3)
    //    times(-3, add, 2, 0);
    //    times(-2, add, 2, add(2,0));
    //    times(-2, add, 2, add(2,0));
    //    */

    // });
    // var movies = [
    //   {title: "2001年宇宙の旅", year: 1968, director: "スタンリー・キューブリック"},
    //   {title: "時計じかけのオレンジ", year: 1971, director: "スタンリー・キューブリック"},
    //   {title: "スターウォーズ:新たなる希望", year: 1977, director: "ジョージ・ルーカス"},
    //   {title: "E.T.", year: 1982, director: "スティーブン・スピルバーグ"},
    //   {title: "ターミネーター", year: 1984, director: "ジェームズ・キャメロン"},
    //   {title: "マトリックス", year: 1999, director: "ウォシャウスキー兄弟"},
    //   {title: "マイノリティ・リポート", year: 2002, director: "スティーブン・スピルバーグ"}
    // ];
    // var sort_by_director = (movies) => {

    // };
  });
  describe('抽象化への指向', () => {
    it('関数抽象の例', (next) => {
      /* #@range_begin(function_abstraction_example) */
      var succ = (n) => {
        return n + 1;
      };
      /* #@range_end(function_abstraction_example) */
      next();
    });
  });
  describe('意味論の存在', () => {
    it('環境の例', (next) => {
      var merge = (obj1,obj2) => {
        var mergedObject = {};
        for (var attrname in obj1) { mergedObject[attrname] = obj1[attrname]; }
        for (var attrname in obj2) { mergedObject[attrname] = obj2[attrname]; }
        return mergedObject;
      };
      /* #@range_begin(environment_example) */
      var emptyEnv = {};
      var extendEnv = (binding, oldEnv) => {
        return merge(binding, oldEnv); // merge(obj1,obj2) はobj1とobj2のオブジェクトとマージする
      };
      var lookupEnv = (name, env) => {
        return env[name];
      };
      /* #@range_end(environment_example) */
      expect(((_) => {
        /* #@range_begin(environment_example_usage) */
        var initEnv = emptyEnv;
        var firstEnv = extendEnv({"a": 1}, initEnv);  // var a = 1;
        var secondEnv = extendEnv({"b": 3}, initEnv); // var b = 3;
        return lookupEnv("b",secondEnv);                 // b
        /* #@range_end(environment_example_usage) */
      })()).to.eql(
        3
      );

      next();
    });
  });
});
