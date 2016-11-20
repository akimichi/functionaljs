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
  },
  // many :: f a -> f [a]
  many: (x) => {
    var self = this;
    return self.alt(self.some(x))(self.pure(List.empty()));
  },
  // some :: f a -> f [a]
  some: (x) => {
    var self = this;
    return self.flatMap(x)((a) => {
      return self.flatMap(self.many(x))((b) => {
        return self.pure(List.cons(a,b));
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
    var self = this;
    var isSpace = (x) => {
      if(x.match(/^[ \t]/)) {
        return true;
      } else {
        return false;
      } 
    };
    return self.flatMap(self.many(self.sat(isSpace)))((_) => {
      return self.pure(Pair.empty());
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
    var self = this;
    return self.flatMap(self.space.call(self))((_) => {
      return self.flatMap(parser)((v) => {
        return self.flatMap(self.space.call(self))((_) => {
          return self.pure(v);
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
    var self = this;
    return self.token(self.alt(self.float.call(self))(self.int.call(self)));
  },
  symbol: (xs) => {
    var self = this;
    return self.token(self.string(xs));
  }
};
