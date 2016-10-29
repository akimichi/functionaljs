"use strict";

var expect = require('expect.js');
var List = require('./list.js');

module.exports = {
  head: (str) => {
    expect(str).to.a('string');
    return str[0];
  },
  tail: (str) => {
    expect(str).to.a('string');
    return str.substring(1);
  },
  isEmpty: (str) => {
    return str.length === 0;
  },
  /* 文字列を文字のリストに変換する */
  toList: (str) => {
    if(this.isEmpty(str)) {
      return List.empty();
    } else {
      return List.cons(this.head(str), 
                       this.toList(this.tail(str)));
    }
  }
};
