"use strict";

var expect = require('expect.js');

module.exports = {
  type: (data,pattern) => {
    if(data.type === undefined) {
      return pattern.default(data);
    } else {
      return data.type(pattern);
    }
  },
  match: (data,pattern) => {
    return data.match(pattern);
  }
};
