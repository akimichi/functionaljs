"use strict";

var expect = require('expect.js');
var List = require('../lib/list');
var String = require('../lib/string');
var PP = require('../lib/pprinter');

describe("Stringのテスト", () => {
  it("'head'", (next) => {
    expect(
      String.head("abc")
    ).to.eql(
      "a"
    );
    next();
  });
  it("'tail'", (next) => {
    expect(
      String.tail("abc")
    ).to.eql(
      "bc"
    );
    next();
  });
  it("toList", (next) => {
    expect(
      PP.print(String.toList.call(String,"abc"))
    ).to.eql(
      "[a,b,c,nil]"
    );
    next();
  });
});
