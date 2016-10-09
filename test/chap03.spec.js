"use strict";


// 第3章 心の準備
// ========

// ## 小目次
// <div class="toc">
// <ul class="toc">
//   <li><a href="http://akimichi.github.io/functionaljs/chap03.spec.html#DRY-principle">3.1 DRY原則</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/chap03.spec.html#abstraction-oriented">3.2 抽象化への指向</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/chap03.spec.html#semantics-conscious">3.3 セマンティクスを意識する</a></li>
//   <li><a href="http://akimichi.github.io/functionaljs/chap03.spec.html#test-driven">3.4 テストに親しむ</a></li>
// </ul>
// </div>


var expect = require('expect.js');

// ## 3.1 <section id='DRY-principle'>DRY原則</section>
// 
// > 参考資料: [DRY原則の利用: コードの重複と密結合の間](https://www.infoq.com/jp/news/2012/05/DRY-code-duplication-coupling)
describe('DRY原則', () => {
  var add = (x, y) => {
    return x + y;
  };
  // **リスト3.1** 冗長なコード
  it('冗長なコード', (next) => {
    /* #@range_begin(redundant_code) */
    var timesForMultiply = (count, arg, memo) => {
      if(count > 1) {
        return timesForMultiply(count-1, arg, arg + memo);
      } else {
        return arg + memo;
      }
    };
    var multiply = (n, m) => {
      return timesForMultiply(n, m, 0);
    };
    var timesForExponential = (count, arg, memo) => {
      if(count > 1) {
        return timesForExponential(count-1, arg, arg * memo);
      } else {
        return arg * memo;
      }
    };
    var exponential = (n, m) => {
      return timesForExponential(m, n, 1);
    };
    /* #@range_end(redundant_code) */
    expect(
      multiply(2, 3)
    ).to.eql(
      6
    );
    expect(
      exponential(2, 3)
    ).to.eql(
      8
    );
    next();
  });
  it('DRYを適用する', (next) => {
    // **リスト3.3** DRYなtimes関数
    /* #@range_begin(dry_times) */
    var times = (count, arg, memo, fun) => { // 引数funを追加
      if(count > 1) {
        return times(count-1, arg, fun(arg,memo), fun);
      } else {
        return fun(arg,memo);
      }
    };
    /* #@range_end(dry_times) */

    // **リスト3.4** DRYなかけ算とべき乗
    /* #@range_begin(dry_functions) */
    var add = (n, m) => {
      return n + m;
    };
    /* times関数を利用してmultiply関数を定義する */
    var multiply = (n, m) => {
      return times(m, n, 0, add);
    };
    /* times関数を利用してexponential関数を定義する */
    var exponential = (n, m) => {
      return times(m, n, 1, multiply);
    };
    /* #@range_end(dry_functions) */
    expect(
      multiply(2, 3)
    ).to.eql(
      6
    );
    expect(
      exponential(2, 3)
    ).to.eql(
      8
    );
    expect(
      multiply(-2, 3)
    ).to.eql(
        -6
    );
    next();
  });
});

// ## 3.2 <section id='abstraction-oriented'>抽象化への指向</section>
describe('抽象化への指向', () => {
  // **リスト3.5** 関数という抽象化
  it('関数という抽象化', (next) => {
    /* #@range_begin(function_abstraction_example) */
    var succ = (n) => {
      return n + 1;
    };
    /* #@range_end(function_abstraction_example) */
    next();
  });
  describe('高階関数による抽象化', () => {
    var anArray = [2,3,5,7,11,13];

    // **リスト3.6** for文によるsum関数
    it('for文によるsum関数', (next) => {
      /* #@range_begin(sum_for) */
      var anArray = [2,3,5,7];
      var sum = (array) => {
        var result = 0;
        for(var index = 0; index < array.length; index++){
          result = result + array[index];
        }
        return result;
      };
      sum(anArray);
      /* #@range_end(sum_for) */
      expect(
        sum(anArray)
      ).to.eql(
        17
      );
      next();
    });
    // **リスト3.7** forEachによるsum関数
    it('forEachによるsum関数', (next) => {
      /* #@range_begin(sum_forEach) */
      var sum = (array) => {
        /* 結果を格納する変数result */
        var result = 0;
        array.forEach((item) => {
          result = result + item;
        });
        return result;
      };
      /* #@range_end(sum_forEach) */
      expect(
        sum(anArray)
      ).to.eql(
        41
      );
      next();
    });
    // **リスト3.8** reduceによるsum関数
    it('reduceによるsum関数', (next) => {
      /* #@range_begin(sum_reduce) */
      var sum = (array) => {
        return array.reduce((x, y) => {
          return x + y;
        });
      };
      /* #@range_end(sum_reduce) */
      expect(
        sum(anArray)
      ).to.eql(
        41
      );
      next();
    });
  });
});

// ## 3.3 <section id='semantics-conscious'>セマンティクスを意識する</section>
describe('セマンティクスを意識する', () => {
  // **リスト3.9** 環境という仕組み
  it('環境という仕組み', (next) => {
    /* merge関数は、引数にわたされた2つのオブジェクトを併合する */
    var merge = (obj1, obj2) => {
      var mergedObject = {};
      for (var attrname in obj1) { mergedObject[attrname] = obj1[attrname]; }
      for (var attrname in obj2) { mergedObject[attrname] = obj2[attrname]; }
      return mergedObject;
    };
    // <dl>
    //   <dt>empty</dt>
    //   <dd>空の環境</dd>
    //   <dt>extendEnv </dt>
    //   <dd>環境に変数と値の対応を与えて、辞書を拡張する</dd>
    //   <dt>lookupEnv</dt>
    //   <dd>変数を指定して、環境に記憶されている値を取り出す</dd>
    // </dl>
    /* #@range_begin(environment_example) */
    /* 空の環境 */
    var emptyEnv = {};
    /* 環境を拡張する */
    var extendEnv = (binding, oldEnv) => {
      /* merge(obj1, obj2) は
         obj1とobj2のオブジェクトをマージする関数のこと */
      return merge(binding, oldEnv); 
    };
    /* 変数名に対応する値を環境から取り出す */
    var lookupEnv = (name, env) => {
      return env[name];
    };
    /* #@range_end(environment_example) */
    // ~~~
    // var a = 1;
    // var b = 3;
    // b
    // ~~~
    expect(((_) => {
      // **リスト3.11** リスト 3.10のセマンティクス 
      /* #@range_begin(environment_example_usage) */
      /* 空の辞書を作成する */
      var initEnv = emptyEnv;                       
      /* var a = 1 を実行して、辞書を拡張する */
      var firstEnv = extendEnv({"a": 1}, initEnv);  
      /* var b = 3 を実行して、辞書を拡張する */
      var secondEnv = extendEnv({"b": 3}, firstEnv); 
      /* 辞書から b の値を参照する */
      lookupEnv("b", secondEnv);                
      /* #@range_end(environment_example_usage) */
      return lookupEnv("b", secondEnv);                 
    })()).to.eql(
      3
    );
    next();
  });
});

// ## 3.4 <section id='test-driven'>テストに親しむ</section>
describe('テストに親しむ', () => {
  // ### 単体テストの仕組み
  // > 参考資料: [単体テスト](https://ja.wikipedia.org/wiki/%E5%8D%98%E4%BD%93%E3%83%86%E3%82%B9%E3%83%88)
  describe('単体テストの仕組み', () => {
    // **リスト3.12** アサート関数の例
    //
    // assertライブラリを使う場合
    it('assertによる表明', (next) => {
      /* #@range_begin(assert_assertion) */
      var assert = require("assert");
      assert.equal(1 + 2, 3);
      /* #@range_end(assert_assertion) */
      next();
    });
    // expectライブラリを使う場合
    // > 参考資料: https://github.com/Automattic/expect.js
    it('expectによる表明', (next) => {
      /* #@range_begin(expect_assertion) */
      var expect = require('expect.js');
      expect(
        1 + 2
      ).to.eql(
        3
      );
      /* #@range_end(expect_assertion) */
      next();
    });
  });
});

// [目次に戻る](index.html) [次章に移る](chap04.spec.html) 
