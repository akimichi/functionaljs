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
  // flatMap :: Parser a -> (a -> Parser b) -> Parser b
  flatMap: (parser) => {
    var self = this;
    return (f) => {
      return (input) => {
        return (self.parse(parser)(input)).match({
          empty: () => {
            return List.empty();
          },
          cons: (pair,_) => {
            return pair.match({
              cons: (v, out) => {
                return self.parse(f(v))(out);
              }
            });
          }
        });
      };
    };
  },
  // empty :: Parser a
  empty: (input) => {
    return List.empty();
  },
  // alt :: Parser a -> Parser a -> Parser b
  alt: (parserA) => {
    var self = this;
    return (parserB) => {
      return (input) => {
        return (self.parse(parserA)(input)).match({
          empty: () => {
            return self.parse(parserB)(input);
          },
          cons: (pair,_) => {
            return pair.match({
              cons: (v, out) => {
                return List.cons(Pair.cons(v,out),
                                 List.empty(0));
              }
            });
          }
        });

      };
    };
  },
  // parse :: Parser a -> String -> [(a,String)]
  // parse parser input = parser(input)
  parse: (parser) => {
    return (input) => {
      return parser(input);
    };
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
  // sat :: (String -> Bool) -> Parser String
  sat: (predicate) => {
    var self = this;
    return self.flatMap(self.item)((x) => {
      if(predicate(x) === true) {
        return self.pure(x);
      } else {
        return self.empty;
      }
    });
  },
  // digit :: Parser String 
  digit: () => { 
    var self = this;
    var isDigit = (x) => {
      return !isNaN(x);
    };
    return self.sat(isDigit);
  }
};