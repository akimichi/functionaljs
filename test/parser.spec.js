"use strict";

var expect = require('expect.js');
var List = require('../lib/list.js');
var Pair = require('../lib/pair.js');
var String = require('../lib/string.js');

var ParserResult = {
  unit: (value) => {
    return List.empty();
  }
};

var Parser = {
  parse: (parser) => {
    return (input) => {
      return parser(input);
    };
  },
  item: (input) => {
    return List.match(input,{
      empty: (_) => {
        return List.empty();
      },
      cons: (head, tail) => {
        return List.unit(Pair.cons(head, tail));
      }
    });
  }
};

// describe('パーサーコンビネーター', () => {
//   describe("parse", (next) => {
//     it("item", (next) => {
//       expect(
//         List.toArray(Parser.item(List.empty()))
//       ).to.eql(
//         1
//       );
//       next();
//     });
//   });
// });
