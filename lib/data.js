"use strict";

var expect = require('expect.js');

module.exports = {
  type: (data,pattern) => {
    return data.type(pattern);
  },
  match: (data,pattern) => {
    return data.match(pattern);
  }
};
