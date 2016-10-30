"use strict";

var expect = require('expect.js');
var Data = require('./data.js');
var Pair = require('./pair.js');
var List = require('./list.js');

// newtype Parser a = P(String -> [(a, String)])
module.exports = {
  // pure :: a -> Parser a
  pure: (v) => {
    return (input) => {
      return List.cons(Pair.cons(v,input),
                       List.empty());
    };
  }, 
  // parse :: Parser a -> String -> [(a,String)]
  // parse parser input = parser(input)
  parse: (parser) => {
    return (input) => {
      return parser(input);
    };
  },
  // item :: Parser String
  item: (input) => {
    return input.match({
      empty: (_) => {
        return List.empty();
      },
      cons: (head, tail) => {
        return List.cons(Pair.cons(head, tail),
                         List.empty()); 
      }
    });
  },
  // fmap :: (a -> b) -> Parser a -> Parser b
  fmap: (f) => {
    var self = this;
    return (parserA) => {
      return (input) => {
        return (self.parse(parserA)(input)).match({
          empty: () => {
            return List.empty();
          },
          cons: (pair,_) => {
            return pair.match({
              cons: (v, out) => {
                return List.cons(Pair.cons(f(v), out),
                                 List.empty());
              }
            });
          }
        });
      };
    };
  } 
};
