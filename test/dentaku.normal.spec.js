"use strict";

var expect = require('expect.js');
var List = require('../lib/list.js');
var Pair = require('../lib/pair.js');
var Data = require('../lib/data.js');
var PP = require('../lib/pprinter.js');
var Parser = require('../lib/parser.js'); 
var Dentaku = require('../lib/dentaku.normal.js');

describe('dentaku.normal', () => {
    it("evaluate", (next) => {
      expect(
        Dentaku.evaluate("123")
      ).to.eql(
        123
      );
      expect(
        Dentaku.evaluate("1 + 2")
      ).to.eql(
        3
      );
      next();
    });
    it("expr", (next) => {
      expect(
        PP.print(Parser.parse(Dentaku.expr())(List.fromString("123")))
      ).to.eql(
        '[(123,[]),nil]'
      );
      next();
    });
    it("factor", (next) => {
      expect(
        PP.print(Parser.parse(Dentaku.factor())(List.fromString("123")))
      ).to.eql(
        '[(123,[]),nil]'
      );
      next();
    });
    it("term", (next) => {
      expect(
        PP.print(Parser.parse(Dentaku.term())(List.fromString("1 * 2")))
      ).to.eql(
        '[(2,[]),nil]'
      );
      next();
    });

});

