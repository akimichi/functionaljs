"use strict";

var expect = require('expect.js');
var Data = require('./data.js');
var List = require('./list.js');
// var Pair = require('./pair.js');

module.exports = {
  print: (data) => {
    var self = this;
    return Data.type(data,{
      pair: () => {
        return data.match({
          empty: () => {
            return "()";
          },
          cons: (l, r) => {
            var left = self.print(l);
            var right = self.print(r);
            return "(" + left + "," + right + ")";
          }
        });
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
