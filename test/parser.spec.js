"use strict";

var expect = require('expect.js');
var List = require('../lib/list.js');
var Pair = require('../lib/pair.js');
var String = require('../lib/string.js');
var Data = require('../lib/data.js');
var PP = require('../lib/pprinter.js');
var Parser = require('../lib/parser.js');

describe('パーサーコンビネーター', () => {
  describe("parse", (next) => {
    it("item", (next) => {
      expect(
        PP.print(Parser.item(List.empty()))
      ).to.eql(
        '[]'
      );
      expect(
        PP.print(Parser.item(String.toList("abc")))
      ).to.eql(
        '[(a,[b,c,nil]),nil]'
      );
      next();
    });
  });
  describe("fmap", (next) => {
    it("toUpper", (next) => {
      var input = String.toList("abc");
      var toUpper = (s) => {
        return s.toUpperCase();
      };
      expect(
        PP.print(
          Parser.parse(Parser.fmap(toUpper)(Parser.item))(input)
        )
      ).to.eql(
        '[(A,[b,c,nil]),nil]'
      );
      next();
    });
  });
});
