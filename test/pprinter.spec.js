"use strict";

var expect = require('expect.js');
var Pair = require('../lib/pair.js');

var PretterPrinter = {
  pp: (data) => {
    return data.type(data,{
      pair: () => {
        return "pair"; 
      }
    });
  }
};

// describe('PretterPrinter', () => {
//   it("pp", (next) => {
//     var pair = Pair.cons(1,2);
//     expect(
//       Pair.self
//     ).to.eql(
//       pair
//     );
//     // expect(
//     //   PretterPrinter.pp(pair)
//     // ).to.eql(
//     //   1
//     // );
//     next();
//   });
//   // it("'Parser#parse'", (next) => {
//   //   expect(
//   //     Parser.parse(Parser.unit(1))("abc")
//   //   ).to.eql(
//   //     1
//   //   );
//   //   next();
//   // });
  
// });
