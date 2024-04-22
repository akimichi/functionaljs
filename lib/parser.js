"use strict";

var expect = require('expect.js');
var Data = require('./data.js');
var Pair = require('./pair.js');
var List = require('./list.js');

const Parser = {
// newtype Parser a = P(String -> [(a, String)])
  // pure :: a -> Parser a
  pure: (v) => {
    return (input) => {
      return List.cons(Pair.cons(v,input),
                       List.empty());
    };
  }, 
  // <*> :: Parser (a -> b) -> Parser a -> Parser b
  // pg <*> px = P (\input -> case parse pg input of
  //                          [] -> []
  //                          [(g,out)] -> parse (fmap g px) out)
  apply: (pg) => {
    var self = this;
    return (px) => {
      return (input) => {
        return self.parse(pg)(input).match({
          empty: () => {
            return List.empty();
          },
          cons: (pair,_) => {
            return pair.match({
              cons: (g, out) => {
                return self.parse(self.fmap(g)(px))(out);
              }
            });
          }
        });
      };
    };
  },
  // flatMap :: Parser a -> (a -> Parser b) -> Parser b
  flatMap: (parser) => {
    return (f) => {
      return (input) => {
        return (Parser.parse(parser)(input)).match({
          empty: () => {
            return List.empty();
          },
          cons: (pair,_) => {
            return pair.match({
              cons: (v, out) => {
                return Parser.parse(f(v))(out);
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
    return (parserB) => {
      return (input) => {
        return (Parser.parse(parserA)(input)).match({
          empty: () => {
            return Parser.parse(parserB)(input);
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
    return (parserA) => {
      return (input) => {
        return (Parser.parse(parserA)(input)).match({
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
    return Parser.flatMap(Parser.item)((x) => {
      if(predicate(x) === true) {
        return Parser.pure(x);
      } else {
        return Parser.empty;
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
      if(x.match(/^[a-z]/)){
        return true;
      } else {
        return false;
      } 
    };
    return self.sat(isLower);
  },
  // upper :: Parser String 
  upper: () => { 
    var self = this;
    var isUpper = (x) => {
      if(x.match(/^[A-Z]/)){
        return true;
      } else {
        return false;
      } 
    };
    return self.sat(isUpper);
  },
  // letter :: Parser Char
  letter: () => { 
    var self = this;
    var isAlpha = (x) => {
      if(x.match(/^[a-zA-Z]/)){
        return true;
      } else {
        return false;
      } 
    };
    return self.sat(isAlpha);
  },
  // alphanum :: Parser Char
  // > Parses a letter or digit (a character between '0' and '9'). Returns the parsed character.
  alphanum: () => { 
    var self = this;
    var isAlphaNum = (x) => {
      if(x.match(/^[a-zA-Z0-9]/)) {
        return true;
      } else {
        return false;
      } 
    };
    return self.sat(isAlphaNum);
  },
  // char :: Char -> Parser Char
  char: (x) => { 
    var isX = (input) => {
      if(x === input){
        return true;
      } else {
        return false;
      }
    };
    return Parser.sat(isX);
  },
  // string :: String -> Parser String 
  string: (str) => { 
    return str.match({
      empty: () => {
        return Parser.pure(List.empty());
      },
      cons: (x,xs) => {
        return Parser.flatMap(Parser.char(x))((_) => {
          return Parser.flatMap(Parser.string(xs))((_) => {
            return Parser.pure(List.cons(x,xs));
          });
        });
      }
    }); 
  },
  // many :: f a -> f [a]
  many: (x) => {
    return Parser.alt(Parser.some(x))(Parser.pure(List.empty()));
  },
  // some :: f a -> f [a]
  some: (x) => {
    return Parser.flatMap(x)((a) => {
      return Parser.flatMap(Parser.many(x))((b) => {
        return Parser.pure(List.cons(a,b));
      });
    }); 
  },
  ident: () => {
    var self = this;
    return self.flatMap(self.lower.call(self))((x) => {
      return self.flatMap(self.many(self.alphanum.call(self)))((xs) => {
        return self.pure(List.cons(x,xs));
      });
    });
  },
  nat: () => {
    var self = this;
    var read = (xs) => {
      var list2str = (xs) => {
        return List.foldr(xs)("")((x) => {
          return (accumulator) => {
            return x + accumulator;
          };
        });
      };
      return parseInt(list2str(xs),10);
    };
    return self.flatMap(self.some(self.digit.call(self)))((xs) => {
      return self.pure(read(xs));
    });
  },
  space: () => {
    var isSpace = (x) => {
      if(x.match(/^[ \t]/)) {
        return true;
      } else {
        return false;
      } 
    };
    return Parser.flatMap(Parser.many(Parser.sat(isSpace)))((_) => {
      return Parser.pure(Pair.empty());
    });
  },
  int: () => {
    var self = this;
    return self.alt(
      self.flatMap(self.char("-"))((_) => {
        return self.flatMap(self.nat.call(self))((n) => {
          return self.pure(-n);
        });
      })
    )(
      self.nat.call(self)
    );
  },
  float: () => {
    var self = this;
    var minus = self.char("-");
    var dot = self.char(".");
    return self.alt(
                self.flatMap(minus)((_) => {
                    return self.flatMap(self.nat.call(self))((n) => {
                        return self.flatMap(dot)((_) => {
                            return self.flatMap(self.nat.call(self))((m) => {
                                return self.pure(-n - m * (1 / Math.pow(10, Math.floor(1+Math.log10(m))) ));
                            });
                        });
                    });
                })
            )(
                self.flatMap(self.nat.call(self))((n) => {
                    return self.flatMap(dot)((_) => {
                        return self.flatMap(self.nat.call(self))((m) => {
                            return self.pure(n + m * (1 / Math.pow(10, Math.floor(1+Math.log10(m))) ));
                        });
                    });
                })
             );
  },
  // token :: Parser a -> Parser a
  token: (parser) => {
    return Parser.flatMap(Parser.space)((_) => {
      return Parser.flatMap(parser)((v) => {
        return Parser.flatMap(Parser.space)((_) => {
          return Parser.pure(v);
        });
      });
    });
  },
  identifier: () => {
    var self = this;
    return self.token(self.ident.call(self));
  },
  natural: () => {
    var self = this;
    return self.token(self.nat.call(self));
  },
  integer: () => {
    var self = this;
    return self.token(self.int.call(self));
  },
  numeric: () => {
    return Parser.token(Parser.alt(Parser.float)(Parser.int));
  },
  symbol: (xs) => {
    return Parser.token(Parser.string(xs));
  }
};
module.exports = Parser
