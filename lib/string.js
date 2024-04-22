"use strict";

var expect = require('expect.js');
var Data = require('./data.js');
var List = require('./list.js');

const Text = {
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
  isChar: (str) => {
    return str.length === 1;
  },
  /* 文字列を文字のリストに変換する */
  toList: (str) => {
    expect(str).to.a('string');
    if(Text.isChar(str) === true) {
      return List.cons(str,
                       List.empty());
    } else {
      return List.cons(Text.head(str), 
                       Text.toList(Text.tail(str)));
    }
  }
};
module.exports = Text
