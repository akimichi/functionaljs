"use strict";

var expect = require('expect.js');
var Data = require('./data.js');
var Pair = require('./pair.js');
var List = require('./list.js');

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
        return data.match({
          empty: () => {
            return "[]";
          },
          cons: (head, tail) => {
            return "[" + List.foldr(data)("nil")((item) => {
              return (accumulator) => {
                return  self.print(item) + "," + accumulator; 
              };
            }) + "]";
          }
        });
      },
      number: (data) => {
        return data;
      },
      string: (data) => {
        return data;
      }
    });
  }
};
