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
  },
  // lower :: Parser String 
  lower: () => { 
    var self = this;
    var isLower = (x) => {
      return x.match(/^[a-z]/);
    };
    return self.sat(isLower);
  },
  // upper :: Parser String 
  upper: () => { 
    var self = this;
    var isUpper = (x) => {
      return x.match(/^[A-Z]/);
    };
    return self.sat(isUpper);
  },
  // letter :: Parser Char
  letter: () => { 
    var self = this;
    var isAlpha = (x) => {
      return x.match(/^[a-zA-Z]/);
    };
    return self.sat(isAlpha);
  },
  // alphanum :: Parser Char
  alphanum: () => { 
    var self = this;
    var isAlphaNum = (x) => {
      return x.match(/^[a-zA-Z0-9]/);
    };
    return self.sat(isAlphaNum);
  },
  // char :: Char -> Parser Char
  char: (x) => { 
    var self = this;
    var isX = (input) => {
      if(x === input){
        return true;
      } else {
        return false;
      }
    };
    return self.sat(isX);
  },
  // string :: String -> Parser String 
  string: (str) => { 
    var self = this;
    return str.match({
      empty: () => {
        return self.pure(List.empty());
      },
      cons: (x,xs) => {
        return self.flatMap(self.char(x))((_) => {
          return self.flatMap(self.string(xs))((_) => {
            return self.pure(List.cons(x,xs));
          });
        });
      }
    }); 
  }
};
