"use strict";

var expect = require('expect.js');

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
  toArray: (str) => {
    expect(str).to.a('string');
    var glue = (item) => {
      return (rest) => {
        return [item].concat(rest);
      };
    };
    if(this.isEmpty(str)) {
      return [];
    } else {
      return [this.head(str)].concat(this.toArray(this.tail(str)));
    }
  }
};
