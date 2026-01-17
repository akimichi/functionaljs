"use strict";

import * as List from '../lib/list';
import * as Pair from '../lib/pair';
import * as PP from '../lib/pprinter';
import * as Parser from '../lib/parser';

describe('パーサーコンビネーター', () => {
  const abc = List.fromString("abc");

  describe("parse", () => {
    it("pure", () => {
      expect(PP.print(Parser.parse(Parser.pure(1))(abc))).toEqual('[(1,[a,b,c,nil]),nil]');
    });

    it("item", () => {
      expect(PP.print(Parser.item(List.empty()))).toEqual('[]');
      expect(PP.print(Parser.item(List.fromString("abc")))).toEqual('[(a,[b,c,nil]),nil]');
    });
  });

  describe("fmap", () => {
    it("toUpper", () => {
      const input = List.fromString("abc");
      const toUpper = (s: string): string => s.toUpperCase();
      expect(PP.print(Parser.parse(Parser.fmap(toUpper)(Parser.item))(input))).toEqual('[(A,[b,c,nil]),nil]');
    });
  });

  describe("monad", () => {
    it("three", () => {
      const input = List.fromString("abcdef");
      const three = Parser.flatMap(Parser.item)((x: string) => {
        return Parser.flatMap(Parser.item)((_: string) => {
          return Parser.flatMap(Parser.item)((z: string) => {
            return Parser.pure(Pair.cons(x, z));
          });
        });
      });
      expect(PP.print(three(input))).toEqual('[((a,c),[d,e,f,nil]),nil]');
    });

    it("flapMap", () => {
      const input = List.fromString("  +  ");
      const add_or_subtract = Parser.alt(Parser.symbol(List.fromString("+")))(Parser.symbol(List.fromString("-")));
      const parser = Parser.flatMap(add_or_subtract)((operator: List.List<string>) => {
        return Parser.pure(operator);
      });
      expect(PP.print(Parser.parse(parser)(input))).toEqual('[([+,nil],[]),nil]');
    });
  });

  describe("alternative", () => {
    it("empty", () => {
      const input = List.fromString("abc");
      expect(PP.print(Parser.parse(Parser.empty)(input))).toEqual('[]');
    });

    it("alt", () => {
      const input = List.fromString("abc");
      expect(PP.print(Parser.parse(Parser.alt(Parser.item)(Parser.pure("d")))(input))).toEqual('[(a,[b,c,nil]),nil]');
      expect(PP.print(Parser.parse(Parser.alt(Parser.empty)(Parser.pure("d")))(input))).toEqual('[(d,[a,b,c,nil]),nil]');
    });
  });

  describe("派生したパーサー", () => {
    it("digit", () => {
      expect(PP.print(Parser.parse(Parser.digit())(List.fromString("123")))).toEqual('[(1,[2,3,nil]),nil]');
    });

    it("alphanum", () => {
      expect(PP.print(Parser.parse(Parser.alphanum())(List.fromString("123")))).toEqual('[(1,[2,3,nil]),nil]');
    });

    it("string", () => {
      expect(PP.print(Parser.parse(Parser.string(List.fromString("abc")))(List.fromString("abcdef")))).toEqual('[([a,b,c,nil],[d,e,f,nil]),nil]');
    });
  });

  describe("manyパーサ", () => {
    it("many digit", () => {
      expect(PP.print(Parser.parse(Parser.many(Parser.digit()))(List.fromString("123abc")))).toEqual('[([1,2,3,nil],[a,b,c,nil]),nil]');
      expect(PP.print(Parser.parse(Parser.many(Parser.digit()))(List.fromString("abc")))).toEqual('[([],[a,b,c,nil]),nil]');
    });

    it("some digit", () => {
      expect(PP.print(Parser.parse(Parser.some(Parser.digit()))(List.fromString("abc")))).toEqual('[]');
    });
  });

  it("ident", () => {
    expect(PP.print(Parser.parse(Parser.ident())(List.fromString("abc def")))).toEqual('[([a,b,c,nil],[ ,d,e,f,nil]),nil]');
  });

  it("nat", () => {
    expect(PP.print(Parser.parse(Parser.nat())(List.fromString("123")))).toEqual('[(123,[]),nil]');
  });

  it("space", () => {
    expect(PP.print(Parser.parse(Parser.space())(List.fromString("   abc")))).toEqual('[((),[a,b,c,nil]),nil]');
  });

  it("int", () => {
    expect(PP.print(Parser.parse(Parser.int())(List.fromString("-123 abc")))).toEqual('[(-123,[a,b,c,nil]),nil]');
    expect(PP.print(Parser.parse(Parser.int())(List.fromString("123 abc")))).toEqual('[(123,[a,b,c,nil]),nil]');
  });

  it("float", () => {
    expect(PP.print(Parser.parse(Parser.float())(List.fromString("0.1")))).toEqual('[(0.1,[]),nil]');
    expect(PP.print(Parser.parse(Parser.float())(List.fromString("0.123")))).toEqual('[(0.123,[]),nil]');
    expect(PP.print(Parser.parse(Parser.float())(List.fromString("1.1")))).toEqual('[(1.1,[]),nil]');
    expect(PP.print(Parser.parse(Parser.float())(List.fromString("-1.1")))).toEqual('[(-1.1,[]),nil]');
  });

  describe("トークン", () => {
    it("identifier", () => {
      expect(PP.print(Parser.parse(Parser.identifier())(List.fromString("   abc")))).toEqual('[([a,b,c,nil],[]),nil]');
    });

    it("natural", () => {
      expect(PP.print(Parser.parse(Parser.natural())(List.fromString("   123   ")))).toEqual('[(123,[]),nil]');
    });

    it("integer", () => {
      expect(PP.print(Parser.parse(Parser.integer())(List.fromString("   -123   ")))).toEqual('[(-123,[]),nil]');
    });

    it("numeric", () => {
      expect(PP.print(Parser.parse(Parser.numeric())(List.fromString("   -123   ")))).toEqual('[(-123,[]),nil]');
      expect(PP.print(Parser.parse(Parser.numeric())(List.fromString("   0.123   ")))).toEqual('[(0.123,[]),nil]');
    });

    it("symbol", () => {
      expect(PP.print(Parser.parse(Parser.symbol(List.fromString("+")))(List.fromString("  +  ")))).toEqual('[([+,nil],[]),nil]');
    });
  });
});
