"use strict";

var expect = require('expect.js');
var Data = require('../lib/data.js');
var Pair = require('../lib/pair.js');
var List = require('../lib/list.js');

module.exports = {
  print: (data) => {
    var self = this;
    return Data.type(data,{
      pair: () => {
        var left = self.print(Pair.left(data));
        var right = self.print(Pair.right(data));
        return "(" + left + "," + right + ")";
      },
      list: () => {
        var head = self.print(List.head(data));
        var tail = self.print(List.tail(data));
        return "(" + head + "," + tail + ")";
      },
      default: (data) => {
        return data;
      }
    });
  }
};
