"use strict";

import * as Text from '../lib/string';
import * as PP from '../lib/pprinter';

describe("Stringのテスト", () => {
  it("'head'", () => {
    expect(Text.head("abc")).toEqual("a");
  });

  it("'tail'", () => {
    expect(Text.tail("abc")).toEqual("bc");
  });

  it("toList", () => {
    expect(PP.print(Text.toList("abc"))).toEqual("[a,b,c,nil]");
  });
});
