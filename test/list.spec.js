"use strict";

var expect = require('expect.js');
var List = require('../lib/list');
var Pair = require('../lib/pair');
var PP = require('../lib/pprinter');

describe("Listのテスト", () => {
  it("List#cons", (next) => {
    var data = List.cons(1,List.cons(2,List.empty()));
    expect(
      List.head(data)
    ).to.eql(
      1
    );
    expect(
      List.head(List.tail(data))
    ).to.eql(
      2
    );
    next();
  });
  it("isEmpty", (next) => {
    expect(
      List.isEmpty(List.empty())
    ).to.eql(
      true
    );
    expect(
      List.isEmpty(List.cons(1,List.cons(2,List.empty())))
    ).to.eql(
      false
    );
    next();
  });
  it("match", (next) => {
    var list = List.cons(1,List.cons(2,List.empty()));
    list.match({
      cons: (head, tail) => {
        expect(
          head
        ).to.eql(
          1
        );
      }
    });
    next();
  });
  describe("foldr", () => {
    it("'list#foldr'", (next) => {
      var ints = List.cons(1,List.cons(2,List.empty()));
      expect(
        List.foldr(ints)(0)((item) => {
          return (accumulator) => {
            return item + accumulator;
          };
        })
      ).to.eql(
        3
      );
      next();
    });
  });
  describe("リストを作る", () => {
    it("fromArray", (next) => {
      var alist = List.fromArray([1,2,3]);
      expect(
        List.head(alist)
      ).to.eql(
        1
      );
      next();
    });
    it("List#appendでリストを連結する", (next) => {
      var alist = List.fromArray([1,2,3]);
      var blist = List.fromArray([4,5,6]);
      expect(
        PP.print(List.append(alist)(blist))
      ).to.eql(
        "[1,2,3,4,5,6,nil]"
      );
      next();
    });
  });
  describe("List#flatMap", () => {
    it("Pairのリスト", (next) => {
      // alist = [(1,2)]
      var alist = List.fromArray([Pair.cons(1,2)]);
      expect(
        PP.print(List.flatMap(alist)((pair) => {
          return List.unit(pair); 
        }))
      ).to.eql(
        "[(1,2),nil]"
      );
      next();
    });
  });
  describe("リストを操作する", () => {
    it("List#filter", (next) => {
      // alist = [(1,2)]
      var alist = List.fromArray([1,2]);
      expect(
        PP.print(List.filter(alist)((item) => {
          return (item % 2) === 0; 
        }))
      ).to.eql(
        "[2,nil]"
      );
      next();
    });
  });
  describe("リストを走査する", () => {
    it("List#map", (next) => {
      var even = (n) => {
        return (n % 2) === 0;
      };
      expect(
        List.toArray(List.map(List.cons(1,List.cons(2,List.empty())))(even))
      ).to.eql(
        [false,true]
      );
      next();
    });
    it("List#any", (next) => {
      var even = (n) => {
        return (n % 2) === 0;
      };
      expect(
        List.any(List.cons(1,List.cons(2,List.empty())))(even)
      ).to.eql(
        true
      );
      next();
    });
    it("List#elem", (next) => {
      // alist = [(1,2)]
      var alist = List.cons(1,List.cons(2,List.empty()));
      expect(
        List.elem(alist)(1)
      ).to.eql(
        true
      );
      next();
    });
  });
});

// describe("listのテスト", function() {
//   var toArray = list.toArray.bind(list);
//   var fromArray = list.fromArray.bind(list);
//   var fromString = list.fromString.bind(list);
//   var map = list.map.bind(list);
//   var append = list.append.bind(list);
//   var empty = list.empty();

//   var fixtures = {
//     ints: list.fromArray([0,1,2,3])
//   };

//   describe("リストを作る", () => {
//     it("fromArray", (next) => {
//       var theList = fromArray([1,2,3]);
//       expect(
//         list.head(theList)
//       ).to.eql(
//         1
//       );
//       next();
//     });
//     it("fromString", (next) => {
//       var enList = fromString("this is a string");
//       expect(
//         list.head(enList)
//       ).to.eql(
//         't'
//       );
//       expect(
//         list.head(list.tail(enList))
//       ).to.eql(
//         'h'
//       );
//       var jaList = fromString("これは文字列です");
//       expect(
//         list.head(jaList)
//       ).to.eql(
//         'こ'
//       );
//       expect(
//         list.head(list.tail(jaList))
//       ).to.eql(
//         'れ'
//       );
//       next();
//     });
//   });
//   // describe("'list#isEqual'", function() {
//   //   it("'isEqual' to be true", function(next) {
//   //     expect(function(){
//   //       var list1 = __.list.mkList.bind(__)([2,0,3,1]);
//   //       var list2 = __.list.mkList.bind(__)([2,0,3,1]);
//   //       return list1.isEqual(list2);
//   //     }()).to.eql(
//   //       true
//   //     );
//   //     next();
//   //   });
//   //   it("'isEqual' to be false when two list have different length", function(next) {
//   //     expect(function(){
//   //       var list1 = __.list.mkList.bind(__)([2,0,3,1,4]);
//   //       var list2 = __.list.mkList.bind(__)([2,0,3,1]);
//   //       return list1.isEqual(list2);
//   //     }()).to.eql(
//   //       false
//   //     );
//   //     expect(function(){
//   //       var list1 = __.list.mkList.bind(__)([2,0,3,1]);
//   //       var list2 = __.list.mkList.bind(__)([2,0,3,1,4]);
//   //       return list1.isEqual(list2);
//   //     }()).to.eql(
//   //       false
//   //     );
//   //     next();
//   //   });
//   //   it("'isEqual' to be false", function(next) {
//   //     expect(function(){
//   //       var list1 = __.list.mkList.bind(__)([2,0,3,1]);
//   //       var list2 = __.list.mkList.bind(__)([0,2,3,1]);
//   //       return list1.isEqual(list2);
//   //     }()).to.eql(
//   //       false
//   //     );
//   //     next();
//   //   });
//   // });
//   // it("'list#last'", (next) => {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   expect(
//   //     __.list.last.bind(__)(list)
//   //   ).to.eql(
//   //     3
//   //   );
//   //   next();
//   // });
//   // it("'list#at'", (next) => {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   expect(
//   //     __.list.at.call(__,list)(0)
//   //   ).to.eql(
//   //     0
//   //   );
//   //   expect(
//   //     __.list.at.call(__,list)(1)
//   //   ).to.eql(
//   //     1
//   //   );
//   //   expect(
//   //     __.list.at.call(__,list)(2)
//   //   ).to.eql(
//   //     2
//   //   );
//   //   next();
//   // });

//   // it("'list#concat'", function(next) {
//   //   var list1 = __.list.mkList.bind(__)([0,1]);
//   //   var list2 = __.list.mkList.bind(__)([2,3]);
//   //   var result = __.list.concat.bind(__)(list1)(list2);
//   //   expect(
//   //     __.list.length.bind(__)(result)
//   //   ).to.eql(
//   //     4
//   //   );
//   //   expect(
//   //     __.list.toArray.bind(__)(__.list.take.bind(__)(result)(4))
//   //   ).to.eql(
//   //     [0,1,2,3]
//   //   );
//   //   next();
//   // });
//   // it("'list#reverse'", function(next) {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   var result = __.list.reverse.bind(__)(list);
//   //   expect(
//   //     result.head
//   //   ).to.eql(
//   //     3
//   //   );
//   //   expect(
//   //     __.list.length.bind(__)(result)
//   //   ).to.eql(
//   //     4
//   //   );
//   //   next();
//   // });
//   // it("'list#concat'", function(next) {
//   //   expect(function(){
//   //     var list1 = __.list.mkList.bind(__)([0,1]);
//   //     var list2 = __.list.mkList.bind(__)([2,3]);
//   //     return toArray(__.list.concat.bind(__)(list1)(list2));
//   //   }()).to.eql(
//   //     [0,1,2,3]
//   //   );

//   //   next();
//   // });
//   // // ~~~scala
//   // //   for(val x <- m1
//   // //     val y <- m2)
//   // //   yield {x + y}
//   // //
//   // // becomes...
//   // //
//   // // m1.flatMap { x =>  m2.map { y =>
//   // //                             unit (x+y) }}
//   // // ~~~
//   // it("'list#map'", function(next) {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   var result = __.list.map.bind(__)(list)(function(item){
//   //     return item + 10;
//   //   });
//   //   expect(
//   //     result.head
//   //   ).to.eql(
//   //     10
//   //   );
//   //   expect(
//   //     result.tail.head
//   //   ).to.eql(
//   //     11
//   //   );
//   //   expect(
//   //     __.list.length.bind(__)(result)
//   //   ).to.eql(
//   //     4
//   //   );
//   //   // expect(function(){
//   //   //   __.list.map.bind(__)(__.list.empty)(function(item){
//   //   //   return item + 10;
//   //   // });
//   //   // }()).to.eql(
//   //   //   4
//   //   // );
//   //   next();
//   // });
//   // it("'list#filter'", (next) => {
//   //   var even = (n) => {
//   //     return (n % 2) === 0;
//   //   };
//   //   var list = __.list.mkList.bind(__)([0,1,2,3,4]);
//   //   var result = __.list.filter.bind(__)(list)(even);
//   //   expect(
//   //     __.list.toArray.bind(__)(result)
//   //   ).to.eql(
//   //     [0,2,4]
//   //   );
//   //   var odd = (n) => {
//   //     return (n % 2) !== 0;
//   //   };
//   //   expect(
//   //     __.list.toArray.call(__,__.list.filter.call(__,list)(odd))
//   //   ).to.eql(
//   //     [1,3]
//   //   );
//   //   next();
//   // });
//   // it("'list#find'", (next) => {
//   //   var odd = (n) => {
//   //     return (n % 2) !== 0;
//   //   };
//   //   var list = __.list.mkList.call(__,[0,1,2,3,4]);
//   //   expect(
//   //     __.list.find.call(__,list)(odd)
//   //   ).to.eql(
//   //     __.monad.maybe.unit.call(__,1)
//   //   );
//   //   next();
//   // });
//   // it("'list.zip' should zip two lists",function(next){
//   //   var keys = __.list.mkList.bind(__)(["a","b","c"]);
//   //   var values = __.list.mkList.bind(__)([1,2,3]);
//   //   var zipped = __.list.zip.bind(__)(keys)(values);
//   //   expect(
//   //     __.list.length.bind(__)(zipped)
//   //   ).to.eql(
//   //     3
//   //   );
//   //   expect(
//   //     __.list.toArray.bind(__)(zipped)
//   //   ).to.eql(
//   //     [ { type: 'pair', left: 'a', right: 1 },
//   //       { type: 'pair', left: 'b', right: 2 },
//   //       { type: 'pair', left: 'c', right: 3 } ]
//   //   );
//   //   next();
//   // });
//   // it("'list.zipWith' should zip two lists",function(next){
//   //   var keys = __.list.mkList.bind(__)(["a","b","c"]);
//   //   var values = __.list.mkList.bind(__)([1,2,3]);
//   //   var zippedWithPair = __.list.zipWith.bind(__)(__.pair.mkPair.bind(__))(keys)(values);
//   //   expect(
//   //     __.list.length.bind(__)(zippedWithPair)
//   //   ).to.eql(
//   //     3
//   //   );
//   //   expect(
//   //     __.list.toArray.bind(__)(zippedWithPair)
//   //   ).to.eql(
//   //     [ { type: 'pair', left: 'a', right: 1 },
//   //       { type: 'pair', left: 'b', right: 2 },
//   //       { type: 'pair', left: 'c', right: 3 } ]
//   //   );
//   //   next();
//   // });
//   // it("'list#length'", function(next) {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   expect(
//   //     __.list.length.bind(__)(list)
//   //   ).to.eql(
//   //     4
//   //   );
//   //   expect(
//   //     __.list.length.bind(__)(__.list.empty)
//   //   ).to.eql(
//   //     0
//   //   );
//   //   next();
//   // });
//   // it("'list#take'", function(next) {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   var take = __.list.take.bind(__)(list)(2);
//   //   expect(
//   //     __.list.toArray.bind(__)(take)
//   //   ).to.eql(
//   //     [0,1]
//   //   );
//   //   next();
//   // });
//   // it("'list#drop'", function(next) {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   var drop = __.list.drop.bind(__)(list)(2);
//   //   expect(
//   //     __.list.toArray.bind(__)(drop)
//   //   ).to.eql(
//   //     [2,3]
//   //   );
//   //   next();
//   // });
//   // it("'list#splitAt'", (next) => {
//   //   this.timeout(5000);
//   //   var list = __.list.fromArray.call(__,[0,1,2,3]);
//   //   expect(
//   //     __.list.toArray.call(__,__.list.splitAt.call(__,list)(2))
//   //   ).to.eql(
//   //     [[0,1],[2,3]]
//   //   );
//   //   next();
//   // });
//   // it("'list#shred'", (next) => {
//   //   this.timeout(5000);
//   //   var list = __.list.fromArray.call(__,[0,1,2,3,4,5]);
//   //   expect(
//   //     __.list.toArray.call(__,__.list.shred.call(__,list)(2))
//   //   ).to.eql(
//   //     [[0,1],[2,3],[4,5]]
//   //   );
//   //   next();
//   // });
//   // it("'list#shuffle'", (next) => {
//   //   this.timeout(5000);
//   //   var listA = __.list.fromArray.call(__,[0,1,2]);
//   //   var listB = __.list.fromArray.call(__,[3,4,5]);
//   //   expect(
//   //     __.list.toArray.call(__,__.list.shuffle.call(__,listA)(listB))
//   //   ).to.eql(
//   //     [ 0, 3, 1, 4, 2, 5 ]
//   //   );
//   //   next();
//   // });
//   // it("'list#init'", function(next) {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   var init = __.list.init.bind(__)(list);
//   //   expect(
//   //     __.list.toArray.bind(__)(init)
//   //   ).to.eql(
//   //     [0,1,2]
//   //   );
//   //   next();
//   // });
//   // it("'list#sum'", function(next) {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   expect(
//   //     __.list.sum.bind(__)(list)
//   //   ).to.eql(
//   //     6
//   //   );
//   //   next();
//   // });
//   describe("folding higher-functions", () => {
//     var foldr = list.foldr.bind(list);
//     // var foldl = list.foldl.bind(list);
//     // var reduce = list.reduce.bind(list);
//     it("'list#foldr'", (next) => {
//       expect(
//         foldr(fixtures.ints)(0)((item) => {
//           return (accumulator) => {
//             return item + accumulator;
//           };
//         })
//       ).to.eql(
//         6
//       );
//       next();
//     });
//     // it("'list#foldl'", function(next) {
//     //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//     //   expect(
//     //     foldl(list)(0)(function(item){
//     //       return function(accumulator){
//     //         return item + accumulator;
//     //       };
//     //     })
//     //   ).to.eql(
//     //     6
//     //   );
//     //   next();
//     // });
//     // it("'list#reduce'", function(next) {
//     //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//     //   expect(
//     //     reduce(list)(0)(function(item){
//     //       return function(accumulator){
//     //         return item + accumulator;
//     //       };
//     //     })
//     //   ).to.eql(
//     //     6
//     //   );
//     //   next();
//     // });
//   });
//   // it("'list#pairs'", function(next) {
//   //   // > pairs [1, 2, 3, 4]
//   //   // [(1, 2), (2, 3), (3, 4)]
//   //   var list = __.list.mkList.bind(__)([1,2,3,4]);
//   //   expect(
//   //     toArray(__.list.pairs.bind(__)(list))
//   //   ).to.eql(
//   //     [ { type: 'pair', left: 1, right: 2 },
//   //       { type: 'pair', left: 2, right: 3 },
//   //       { type: 'pair', left: 3, right: 4 } ]
//   //   );
//   //   next();
//   // });
//   // it("'list#and'", function(next) {
//   //   expect(function(){
//   //     var list = __.list.mkList.bind(__)([true,true]);
//   //     return __.list.and.bind(__)((list));
//   //   }()).to.eql(
//   //     true
//   //   );
//   //   expect(function(){
//   //     var list = __.list.mkList.bind(__)([false,false]);
//   //     return __.list.and.bind(__)((list));
//   //   }()).to.eql(
//   //     false
//   //   );
//   //   expect(function(){
//   //     var list = __.list.mkList.bind(__)([true,false]);
//   //     return __.list.and.bind(__)((list));
//   //   }()).to.eql(
//   //     false
//   //   );
//   //   expect(function(){
//   //     var list = __.list.mkList.bind(__)([false,true]);
//   //     return __.list.and.bind(__)((list));
//   //   }()).to.eql(
//   //     false
//   //   );
//   //   next();
//   // });
//   // it("'list#or'", function(next) {
//   //   var mkList = __.list.mkList.bind(__);
//   //   var or = __.list.or.bind(__);
//   //   expect(function(){
//   //     return or(mkList([true,true]));
//   //   }()).to.eql(
//   //     true
//   //   );
//   //   expect(function(){
//   //     return or(mkList([false,false]));
//   //   }()).to.eql(
//   //     false
//   //   );
//   //   expect(function(){
//   //     return or(mkList([true,false]));
//   //   }()).to.eql(
//   //     true
//   //   );
//   //   expect(function(){
//   //     return or(mkList([false,true]));
//   //   }()).to.eql(
//   //     true
//   //   );
//   //   next();
//   // });
//   // it("'list#all'", function(next) {
//   //   var mkList = __.list.mkList.bind(__);
//   //   var all = __.list.all.bind(__);
//   //   var even = function(n){
//   //     return (n % 2) === 0;
//   //   };
//   //   expect(function(){
//   //     return all(mkList([2,4,6]))(even);
//   //   }()).to.eql(
//   //     true
//   //   );
//   //   next();
//   // });
//   // it("'list#any'", function(next) {
//   //   var mkList = __.list.mkList.bind(__);
//   //   var any = __.list.any.bind(__);
//   //   var even = function(n){
//   //     return (n % 2) === 0;
//   //   };
//   //   expect(function(){
//   //     return any(mkList([1,3,6]))(even);
//   //   }()).to.eql(
//   //     true
//   //   );
//   //   next();
//   // });
//   // it("'list#merge'", function(next) {
//   //   var listX = mkList([0,2,4]);
//   //   var listY = mkList([1,3,5]);
//   //   var merged = __.list.merge.bind(__)(listX)(listY);
//   //   expect(
//   //     __.list.toArray.bind(__)(merged)
//   //   ).to.eql(
//   //     [ 0, 1, 2, 3, 4, 5 ]
//   //   );
//   //   next();
//   // });
//   // it("'list#halve'", function(next) {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   var halve = __.list.halve.bind(__)(list);
//   //   expect(
//   //     __.list.toArray.bind(__)(halve.left)
//   //   ).to.eql(
//   //     [ 0, 1 ]
//   //   );
//   //   expect(
//   //     __.list.toArray.bind(__)(halve.right)
//   //   ).to.eql(
//   //     [ 2,3 ]
//   //   );
//   //   next();
//   // });
//   // it("'list#sort'", function(next) {
//   //   this.timeout(5000);
//   //   expect(function(){
//   //     var list = __.list.mkList.bind(__)([2,0,3,1]);
//   //     return __.list.toArray.bind(__)(__.list.sort.bind(__)(list));
//   //   }()).to.eql(
//   //     [0,1,2,3]
//   //   );
//   //   expect(function(){
//   //     var nil = __.list.empty;
//   //     return __.list.toArray.bind(__)(__.list.sort.bind(__)(nil));
//   //   }()).to.eql(
//   //     []
//   //   );
//   //   next();
//   // });
//   // it("'list#replicate'", function(next) {
//   //   expect(
//   //     toArray(__.list.replicate.bind(__)(3)("a"))
//   //   ).to.eql(
//   //     ["a","a","a"]
//   //   );
//   //   next();
//   // });
//   // it("'list#unfold'", function(next) {
//   //   this.timeout(5000);
//   //   //  unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
//   //   //  > [10,9,8,7,6,5,4,3,2,1]
//   //   var id = (x) => {
//   //     return x;
//   //   };
//   //   var prev = (x) => {
//   //     return x - 1;
//   //   };
//   //   var isZero = (n) => {
//   //     return n === 0;
//   //   };
//   //   expect(
//   //     toArray(__.list.unfold.call(__,isZero)(id)(prev)(10))
//   //   ).to.eql(
//   //     [10,9,8,7,6,5,4,3,2,1]
//   //   );

//   //   //  fibonacci :: [Integer]
//   //   //  fibonacci = unfoldr (\[a,b] -> Just(a+b,[b,b+a])) [0,1]
//   //   //
//   //   //  Prelude> take 10 fibonacci
//   //   //  [1,2,3,5,8,13,21,34,55,89]
//   //   //
//   //   // var fibonacci = ((initialList) => {
//   //   //   var always = (list) => {
//   //   //     // return true;
//   //   //     var zeroth = __.list.at.call(__,list)(0);
//   //   //     // var first = __.list.at.call(__,list)(1);
//   //   //     return zeroth > 10;
//   //   //   };
//   //   //   var mapper = (list) => {
//   //   //     var zeroth = __.list.at.call(__,list)(0);
//   //   //     var first = __.list.at.call(__,list)(1);
//   //   //     return zeroth + first;
//   //   //   };
//   //   //   var next = (list) => {
//   //   //     var zeroth = __.list.at.call(__,list)(0);
//   //   //     var first = __.list.at.call(__,list)(1);
//   //   //     return __.list.mkList.call(__,[first,zeroth + first]);
//   //   //   };
//   //   //   return __.list.unfold.call(__, always)(mapper)(next)(initialList);
//   //   // })(__.list.mkList.call(__,[0,1]));
//   //   // expect(
//   //   //   toArray(__.list.take.call(__,fibonacci)(5))
//   //   // ).to.eql(
//   //   //   [10,9,8,7,6,5,4,3,2,1]
//   //   // );
//   //   next();
//   // });
//   // it("'list#range'", (next) => {
//   //   expect(
//   //     toArray(__.list.range.call(__,0)(5))
//   //   ).to.eql(
//   //     [0,1,2,3,4,5]
//   //   );
//   //   next();
//   // });
//   // describe("functor laws on list", function() {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   it("map id == id", function(next){
//   //     expect(
//   //       toArray(map(list)(__.id))
//   //     ).to.eql(
//   //       toArray(__.id(list))
//   //     );
//   //     next();
//   //   });
//   //   it("map (f . g)  == map f . map g", function(next){
//   //     var f = (n) => {
//   //       return n + 1;
//   //     };
//   //     var g = (n) => {
//   //       return - n;
//   //     };
//   //     expect(
//   //       toArray(map(list)(__.compose.bind(__)(f)(g)))
//   //     ).to.eql(
//   //       toArray(__.compose.bind(__)(__.flip.bind(__)(map)(f))
//   //                                  (__.flip.bind(__)(map)(g))(list))
//   //     );
//   //     next();
//   //   });
//   // });
//   // describe("monoid laws on list", function() {
//   //   var list = __.list.mkList.bind(__)([0,1,2,3]);
//   //   it("empty `append` xs == xs", function(next){
//   //     expect(
//   //       toArray(append(empty)(list))
//   //     ).to.eql(
//   //       toArray(list)
//   //     );
//   //     next();
//   //   });
//   //   it("xs `append` empty == xs", function(next){
//   //     expect(
//   //       toArray(append(list)(empty))
//   //     ).to.eql(
//   //       toArray(list)
//   //     );
//   //     next();
//   //   });
//   //   it("(xs `append` ys) `append` zs == xs `append` (ys `append` zs)", function(next){
//   //     this.timeout(5000);
//   //     var xs = __.list.mkList.bind(__)([0,1]);
//   //     var ys = __.list.mkList.bind(__)([1,2]);
//   //     var zs = __.list.mkList.bind(__)([2,3]);
//   //     expect(
//   //       toArray(append(append(xs)(ys))(zs))
//   //     ).to.eql(
//   //       toArray(append(xs)(append(ys)(zs)))
//   //     );
//   //     next();
//   //   });
//   // });
// });
