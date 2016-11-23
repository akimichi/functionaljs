"use strict";

var expect = require('expect.js');
var List = require('../lib/list.js');
var Pair = require('../lib/pair.js');
var Data = require('../lib/data.js');
var PP = require('../lib/pprinter.js');
var Parser = require('../lib/parser.js');

describe('パーサーコンビネーター', () => {
  var abc = List.fromString("abc");
  describe("parse", (next) => {
    it("pure", (next) => {
      expect(
        PP.print(Parser.parse(Parser.pure(1))(abc))
      ).to.eql(
        '[(1,[a,b,c,nil]),nil]'
      );
      next();
    });
    it("item", (next) => {
      expect(
        PP.print(Parser.item(List.empty()))
      ).to.eql(
        '[]'
      );
      expect(
        PP.print(Parser.item(List.fromString("abc")))
      ).to.eql(
        '[(a,[b,c,nil]),nil]'
      );
      next();
    });
  });
  describe("fmap", (next) => {
    it("toUpper", (next) => {
      var input = List.fromString("abc");
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
  describe("monad", (next) => {
    it("three", (next) => {
      var input = List.fromString("abcdef");
      var three = Parser.flatMap(Parser.item)((x) => {
        return Parser.flatMap(Parser.item)((_) => {
          return Parser.flatMap(Parser.item)((z) => {
            return Parser.pure(Pair.cons(x,z));
          });
        });
      });
      expect(
        PP.print(
          three(input)
        )
      ).to.eql(
        '[((a,c),[d,e,f,nil]),nil]'
      );
      next();
    });
    it("flapMap", (next) => {
        var input = List.fromString("  +  ");
        var add_or_subtract = Parser.alt(Parser.symbol(List.fromString("+")))(Parser.symbol(List.fromString("-")));
        var parser = Parser.flatMap(add_or_subtract)((operator) => {
            return Parser.pure(operator);
        });
        expect(
            PP.print(
                Parser.parse(parser)(input)
                )
            ).to.eql('[([+,nil],[]),nil]');
        next();
    });
  });
  describe("alternative", (next) => {
    it("empty", (next) => {
      var input = List.fromString("abc");
      expect(
        PP.print(
          Parser.parse(Parser.empty)(input)
        )
      ).to.eql(
        '[]'
      );
      next();
    });
    it("alt", (next) => {
      var input = List.fromString("abc");
      expect(
        PP.print(
          Parser.parse(
            Parser.alt(Parser.item)(Parser.pure("d"))
          )(input)
        )
      ).to.eql(
        '[(a,[b,c,nil]),nil]'
      );
      expect(
        PP.print(
          Parser.parse(
            Parser.alt(Parser.empty)(Parser.pure("d"))
          )(input)
        )
      ).to.eql(
        '[(d,[a,b,c,nil]),nil]'
      );
      next();
    });
  });
  describe("派生したパーサー", (next) => {
    it("digit", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.digit.call(Parser)
          )(List.fromString("123"))
        )
      ).to.eql(
        '[(1,[2,3,nil]),nil]'
      );
      next();
    });
    it("alphanum", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.alphanum.call(Parser)
          )(List.fromString("123"))
        )
      ).to.eql(
        '[(1,[2,3,nil]),nil]'
      );

      next();
    });
    it("string", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.string(List.fromString("abc"))
          )(List.fromString("abcdef"))
        )
      ).to.eql(
        '[([a,b,c,nil],[d,e,f,nil]),nil]'
      );

      next();
    });
  });
  describe("manyパーサ", (next) => {
    it("many digit", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.many(Parser.digit.call(Parser))
          )(List.fromString("123abc"))
        )
      ).to.eql(
        '[([1,2,3,nil],[a,b,c,nil]),nil]'
      );
      expect(
        PP.print(
          Parser.parse(
            Parser.many(Parser.digit.call(Parser))
          )(List.fromString("abc"))
        )
      ).to.eql(
        '[([],[a,b,c,nil]),nil]'
      );
      next();
    });
    it("some digit", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.some(Parser.digit.call(Parser))
          )(List.fromString("abc"))
        )
      ).to.eql(
        '[]'
      );
      next();
    });
  });
  it("ident", (next) => {
    expect(
      PP.print(
        Parser.parse(
          Parser.ident.call(Parser)
        )(List.fromString("abc def"))
      )
    ).to.eql(
      '[([a,b,c,nil],[ ,d,e,f,nil]),nil]'
    );
    next();
  });
  it("nat", (next) => {
    expect(
      PP.print(
        Parser.parse(
          Parser.nat.call(Parser)
        )(List.fromString("123"))
      )
    ).to.eql(
      '[(123,[]),nil]'
    );
    next();
  });
  it("space", (next) => {
    expect(
      PP.print(
        Parser.parse(
          Parser.space.call(Parser)
        )(List.fromString("   abc"))
      )
    ).to.eql(
      '[((),[a,b,c,nil]),nil]'
    );
    next();
  });
  it("int", (next) => {
    expect(
      PP.print(
        Parser.parse(
          Parser.int.call(Parser)
        )(List.fromString("-123 abc"))
      )
    ).to.eql(
      '[(-123,[a,b,c,nil]),nil]'
    );
    expect(
      PP.print(
        Parser.parse(
          Parser.int.call(Parser)
        )(List.fromString("123 abc"))
      )
    ).to.eql(
      '[(123,[a,b,c,nil]),nil]'
    );
    next();
  });
  it("float", (next) => {
    expect(
      PP.print(
        Parser.parse(
          Parser.float.call(Parser)
        )(List.fromString("0.1"))
      )
    ).to.eql(
      '[(0.1,[]),nil]'
    );
    expect(
      PP.print(
        Parser.parse(
          Parser.float.call(Parser)
        )(List.fromString("0.123"))
      )
    ).to.eql(
      '[(0.123,[]),nil]'
    );
    expect(
      PP.print(
        Parser.parse(
          Parser.float.call(Parser)
        )(List.fromString("1.1"))
      )
    ).to.eql(
      '[(1.1,[]),nil]'
    );
    expect(
      PP.print(
        Parser.parse(
          Parser.float.call(Parser)
        )(List.fromString("-1.1"))
      )
    ).to.eql(
      '[(-1.1,[]),nil]'
    );
    next();
  });
  describe("トークン", (next) => {
    it("identifier", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.identifier.call(Parser)
          )(List.fromString("   abc"))
        )
      ).to.eql(
        '[([a,b,c,nil],[]),nil]'
      );
      next();
    });
    it("natural", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.natural.call(Parser)
          )(List.fromString("   123   "))
        )
      ).to.eql(
        '[(123,[]),nil]'
      );
      next();
    });
    it("integer", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.integer.call(Parser)
          )(List.fromString("   -123   "))
        )
      ).to.eql(
        '[(-123,[]),nil]'
      );
      next();
    });
    it("numeric", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.numeric.call(Parser)
          )(List.fromString("   -123   "))
        )
      ).to.eql(
        '[(-123,[]),nil]'
      );
      expect(
        PP.print(
          Parser.parse(
            Parser.numeric.call(Parser)
          )(List.fromString("   0.123   "))
        )
      ).to.eql(
        '[(0.123,[]),nil]'
      );
      next();
    });
    it("symbol", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.symbol(List.fromString("+"))
          )(List.fromString("  +  "))
        )
      ).to.eql(
        '[([+,nil],[]),nil]'
      );
      next();
    });
  });
});
