"use strict";

import * as Pair from '../lib/pair';
import * as List from '../lib/list';
import * as PP from '../lib/pprinter';

describe('PrettyPrinter', () => {
  it("print", () => {
    const pair = Pair.cons(1, 2);
    expect(PP.print(pair)).toEqual("(1,2)");

    expect(PP.print(List.cons(1, List.cons(2, List.empty())))).toEqual("[1,2,nil]");

    expect(PP.print(List.cons("a", List.cons("b", List.empty())))).toEqual("[a,b,nil]");

    expect(PP.print(List.empty())).toEqual("[]");

    expect(PP.print(List.cons(Pair.cons(1, 2), List.empty()))).toEqual("[(1,2),nil]");
  });
});
