"use strict";

var expect = require('expect.js');
var List = require('../lib/list.js');
var Pair = require('../lib/pair.js');
var String = require('../lib/string.js');
var Data = require('../lib/data.js');
var PP = require('../lib/pprinter.js');
var Parser = require('../lib/parser.js');

describe('パーサーコンビネーター', () => {
  var abc = String.toList("abc");
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
  describe("monad", (next) => {
    it("three", (next) => {
      var input = String.toList("abcdef");
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
  });
  describe("alternative", (next) => {
    it("empty", (next) => {
      var input = String.toList("abc");
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
      var input = String.toList("abc");
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
          )(String.toList("123"))
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
          )(String.toList("123"))
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
            Parser.string(String.toList("abc"))
          )(String.toList("abcdef"))
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
          )(String.toList("123abc"))
        )
      ).to.eql(
        '[([1,2,3,nil],[a,b,c,nil]),nil]'
      );
      expect(
        PP.print(
          Parser.parse(
            Parser.many(Parser.digit.call(Parser))
          )(String.toList("abc"))
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
          )(String.toList("abc"))
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
        )(String.toList("abc def"))
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
        )(String.toList("123"))
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
        )(String.toList("   abc"))
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
        )(String.toList("-123 abc"))
      )
    ).to.eql(
      '[(-123,[a,b,c,nil]),nil]'
    );
    expect(
      PP.print(
        Parser.parse(
          Parser.int.call(Parser)
        )(String.toList("123 abc"))
      )
    ).to.eql(
      '[(123,[a,b,c,nil]),nil]'
    );
    next();
  });
  describe("トークン", (next) => {
    it("identifier", (next) => {
      expect(
        PP.print(
          Parser.parse(
            Parser.identifier.call(Parser)
          )(String.toList("   abc"))
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
          )(String.toList("   123   "))
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
          )(String.toList("   -123   "))
        )
      ).to.eql(
        '[(-123,[]),nil]'
      );
      next();
    });
    
  });
});
