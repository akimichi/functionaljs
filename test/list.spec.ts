"use strict";

import * as List from '../lib/list';
import * as Pair from '../lib/pair';
import * as PP from '../lib/pprinter';

describe("Listのテスト", () => {
  it("List#cons", () => {
    const data = List.cons(1, List.cons(2, List.empty()));
    expect(List.head(data)).toEqual(1);
    expect(List.head(List.tail(data)!)).toEqual(2);
  });

  it("isEmpty", () => {
    expect(List.isEmpty(List.empty())).toEqual(true);
    expect(List.isEmpty(List.cons(1, List.cons(2, List.empty())))).toEqual(false);
  });

  it("match", () => {
    const list = List.cons(1, List.cons(2, List.empty()));
    list.match({
      empty: () => { fail('Should not be empty'); },
      cons: (head, _tail) => {
        expect(head).toEqual(1);
      }
    });
  });

  describe("foldr", () => {
    it("'list#foldr'", () => {
      const ints = List.cons(1, List.cons(2, List.empty()));
      expect(
        List.foldr(ints)(0)((item: number) => {
          return (accumulator: number) => {
            return item + accumulator;
          };
        })
      ).toEqual(3);
    });
  });

  describe("リストを作る", () => {
    it("fromArray", () => {
      const alist = List.fromArray([1, 2, 3]);
      expect(List.head(alist)).toEqual(1);
    });

    it("List#appendでリストを連結する", () => {
      const alist = List.fromArray([1, 2, 3]);
      const blist = List.fromArray([4, 5, 6]);
      expect(PP.print(List.append(alist)(blist))).toEqual("[1,2,3,4,5,6,nil]");
    });
  });

  describe("List#flatMap", () => {
    it("Pairのリスト", () => {
      // alist = [(1,2)]
      const alist = List.fromArray([Pair.cons(1, 2)]);
      expect(
        PP.print(List.flatMap(alist)((pair) => {
          return List.unit(pair);
        }))
      ).toEqual("[(1,2),nil]");
    });
  });

  describe("リストを操作する", () => {
    it("List#filter", () => {
      const alist = List.fromArray([1, 2]);
      expect(
        PP.print(List.filter(alist)((item: number) => {
          return (item % 2) === 0;
        }))
      ).toEqual("[2,nil]");
    });
  });

  describe("リストを走査する", () => {
    it("List#map", () => {
      const even = (n: number): boolean => {
        return (n % 2) === 0;
      };
      expect(
        List.toArray(List.map(List.cons(1, List.cons(2, List.empty())))(even))
      ).toEqual([false, true]);
    });

    it("List#any", () => {
      const even = (n: number): boolean => {
        return (n % 2) === 0;
      };
      expect(
        List.any(List.cons(1, List.cons(2, List.empty())))(even)
      ).toEqual(true);
    });

    it("List#elem", () => {
      const alist = List.cons(1, List.cons(2, List.empty()));
      expect(List.elem(alist)(1)).toEqual(true);
    });
  });
});
