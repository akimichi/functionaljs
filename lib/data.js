"use strict";

var expect = require('expect.js');

module.exports = {
  type: (data,pattern) => {
    if(data.type === undefined) {
      var typeOf = String(typeof data);
      return pattern[typeOf](data);
      // return pattern.default(data);
    } else {
      return data.type(pattern);
    }
  },
  match: (data,pattern) => {
    return data.match(pattern);
  }
};
