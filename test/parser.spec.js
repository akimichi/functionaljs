"use strict";

var __ = require('kansuu.js');
var expect = require('expect.js');

describe('パーサーコンビネーター', () => {
  describe('代数的データ構造を用いた第2版', () => {

    var match = (exp, pattern) => {
      return exp(pattern);
      //return exp.call(pattern, pattern);
    };
    var list = {
      // match: (exp, pattern) => {
      //    return exp.call(pattern, pattern);
      // },
      empty: (_) => {
        return (pattern) => {
          return pattern.empty();
        };
      },
      cons: (value, list) => {
        return (pattern) => {
          return pattern.cons(value, list);
        };
      },
      isEmpty: (seq) => {
        return match(seq, {
          empty: (_) => {
            return true;
          },
          cons: (head, tail) => {
            return false;
          },
        });
      },
      head: (seq) => {
        return match(seq, {
          empty: (_) => {
            return undefined;
          },
          cons: (head, tail) => {
            return head;
          },
        });
      },
      tail: (seq) => {
        return match(seq, {
          empty: (_)=> {
            return undefined;
          },
          cons: (head, tail) => {
            return tail;
          },
        });
      },
      fromString: (string) => {
        if(string.length === 0) {
          return list.empty();
        } else if(string.length === 1) {
          return list.cons(string[0],list.empty());
        } else {
          return list.cons(string[0],string.substring(1));
        }
      },
      toArray: (seq) => {
        var toArrayAux = (seq) => {
          var self = this;
          return (accumulator) => {
            return match(seq, {
              empty: (_) => {
                return accumulator;
              },
              cons: (head, tail) => {
                return toArrayAux.call(self, tail)(accumulator.concat(head))
              },
            });
          };
        };
        return toArrayAux(seq)([]);
      }
    };
    /* パース結果の代数的データ型 */
    var parseResult = {
      failed: (message) => {
        return (pattern) => {
          return pattern.failed(message);
        };
      },
      successful: (value, input) => {
        return (pattern) => {
          return pattern.successful(value, input);
        };
      }
    };
    // parse: PARSER -> LIST -> PARSERESULT
    var parse = (parser) => {
      return (list) => {
        return parser(list);
      };
    };
    /* 基本パーサー */
    /* #@range_begin(succeed_fail) */
    // succeed:: ANY => LIST => RESULT
    var succeed = (value) => {
      return (seq) => {
        return parseResult.successful(value, seq);
      };
    };
    // fail:: (ANY) => LIST => PARSERESULT
    var fail = (message) => {
      return (seq) => {
        return parseResult.failed(message);
      };
    };
    /* #@range_end(succeed_fail) */
    it('list#fromString', (next) => {
      expect(
        list.isEmpty(list.fromString(""))
      ).to.eql(
        true
      );
      var listFromString = list.fromString("abc");
      expect(
        list.head(listFromString)
      ).to.eql(
        "a"
      );
      next();
    });
    it('list#toArray', (next) => {
      expect(
        list.toArray(list.cons(1,list.empty()))
      ).to.eql(
        [1]
      );
      expect(
        list.toArray(list.cons("a",list.empty()))
      ).to.eql(
        ["a"]
      );
      next();
    });
    /* #@range_begin(succeed_fail) */
    it('succeed', (next) => {
      match(parse(succeed(1)("hello")),{
        failed: () => {
          expect().fail()
        },
        successful: (value, seq) => {
          expect(
            value
          ).to.eql(
            1
          );
          expect(
            seq
          ).to.eql(
            "hello"
          );
        }
      });
      match(parse(succeed(['+', '-']))(""),{
        failed: () => {
          expect().fail()
        },
        successful: (value, input) => {
          expect(
            value
          ).to.eql(
            ['+', '-']
          );
          expect(
            input
          ).to.eql(
            ""
          );
        }
      });
      match(parse(succeed(10))(""),{
        failed: () => {
          expect().fail()
        },
        successful: (value, input) => {
          expect(
            value
          ).to.eql(
            10
          );
          expect(
            input
          ).to.eql(
            ""
          );
        }
      });
      next();
    });
    it('fail', (next) => {
      match(parse(fail("message"))("hello"),{
        failed: (message) => {
          expect(
            message
          ).to.eql(
            "message"
          );
        },
        successful: (value, input) => {
          expect().fail();
        }
      });
      next();
    });
    /* #@range_end(succeed_fail) */
    /* #@range_begin(satisfy)  */
    // satisfy:: FUNC[T=>BOOL] => LIST => PARSERESULT
    var satisfy = (predicate) => {
      return (inputList) => {
        return match(inputList, {
          empty: (_) => {
            return fail("failed");
          },
          cons: (head, tail) => {
            if(predicate(head)){
              return succeed(head)(tail);
            } else {
              return fail("failed");
            }
          }
        });
        /* #@range_end(satisfy)       */
      };
    };
    it('satisfy', (next) => {
      var inputList = list.fromString("abc");
      var isA = (input) => {
        return (input === "a");
      };
      match(satisfy(isA)(inputList),{
        failed: (_) => {
          expect().fail("should not happen");
        },
        successful: (value, input) => {
          expect(value).to.eql("a")
        }
      });
      next();
    });
    // character: Char => PARSERESULT => LIST => PARSERESULT
    var character = (ch) => {
      var predicate = (head) => {
        if(ch === head){
          return true;
        } else {
          return false;
        }
      };
      return satisfy(predicate);
    };
    var seq = (firstParser, nextParser) => {
      return (inputList) => {
        return match(parse(firstParser)(inputList),{
          successful: (value, nextInputList) => {
            return parse(nextParser(value)(nextInputList));
          },
          failed: (message) => {
            return parseResult.failed(message);
          }
        });
      };
    };
    // string: LIST[CHAR] =>
    var string = (string) => {
      return match(string, {
        empty: (_) => {
          return succeed(list.empty());
        },
        cons: (head, tail) => {
          return seq(character(head),function(x){
            return seq(string(tail),function(xs){
              return succeed(list.cons(x,xs));
            });
          });
        }
      });
    };
    // it('string parser', (next) => {
    //   var stringList = list.fromString("abc");
    //   var inputList = list.fromString("abcdef");
    //   var parseResult = parse(string(stringList)(inputList));
    //   match(parseResult,{
    //  failed: (_) => {
    //    expect().fail("should not happen")
    //  },
    //  successful: (value, nextInput) => {
    //    expect(value).to.eql("a")
    //  }
    //   })
    //   next();
    // });


    // (\x. \y. y x) (2 + 2) (\x. x + 1)
    // => (\y. y (2 + 2)) (\x. x + 1)
    // => (\x. x + 1) (2 + 2)
    // => (2 + 2) + 1
    // => 4 + 1
    // => 5
  });
  describe('第1版', () => {
    var parser = {
      /*        #@range_begin(succeed_fail)      */
      // succeed:: T => String => {value:T, input:String}
      succeed: function(value){
        return function(input){
          return {value: value, input: input};
        };
      },
      // succeed:: () => String => {}
      fail: function(){
        return function(input){
          return {};
        };
      },
      /*        #@range_end(succeed_fail)      */
      /*        #@range_begin(satisfy)      */
      // satisfy:: (T=>Bool) => String => {value:T, input:String}
      satisfy: function(predicate){
        return function(context){
          return function(input){
            if(__.isEmpty(input)) {
              return context.fail()();
            } else {
              var head = __.head(input);
              var tail = __.tail(input);
              if(predicate(head)){
                return context.succeed(head)(tail);
              } else {
                return context.fail()();
              }
            }
          };
        }(this);
      },
      /* #@range_end(satisfy)       */
      /*        #@range_begin(parse) */
      // parse:
      parse: function(parser){
        return function(input){
          return parser(input);
        };
      },
      /*
        #@range_end(parse) */
      item: function(){
        return function(input){
          if(__.isEmpty(input)) {
            return [];
          } else {
            var head = __.head(input);
            var tail = __.tail(input);
            return {value: head, input: tail};
          }
        };
      },
      /* #@range_begin(char) */
      // char: Char => {value:T, input:String}
      char: function(ch){
        var predicate = function(head){
          if(ch === head){
            return true;
          } else {
            return false;
          }
        };
        return this.satisfy(predicate);
      },
      /* #@range_end(char) */
      /* #@range_begin(string) */
      // char: Char => {value:T, input:String}
      string: function(string){
        return function(context){
          if(__.isEmpty(string)) {
            return context.succeed('');
          } else {
            var head = __.head(string);
            var tail = __.tail(string);

            return context.seq(context.char(head),function(x){
              return context.seq(context.string(tail),function(xs){
                return context.succeed(x + xs);
              });
            });
          }
        }(this);
      },
      /* #@range_end(string) */
      lex: function(regex){
        return function(context){
          var predicate = function(ch){
            if(regex.test(ch)){
              return true;
            } else {
              return false;
            }
          };
          return context.satisfy(predicate);
        }(this);
      },
      digit: function(){
        return function(context){
          return context.lex(/\d/);
        }(this);
      },
      lower: function(){
        return function(context){
          return context.lex(/[a-z]/);
        }(this);
      },
      upper: function(){
        return function(context){
          return context.lex(/[A-Z]/);
        }(this);
      },
      letter: function(){
        return function(context){
          return context.lex(/[a-zA-Z]/);
        }(this);
      },
      alphanum: function(){
        return function(context){
          return context.lex(/\w/);
        }(this);
      },
      ident: function(){ // Parser String
        return function(context){
          return context.seq(context.lower(), function(x){
            return context.seq(context.many(context.alphanum())(__.op["+"]), function(xs){
              return context.succeed(x + xs);
            });
          });
        }(this);
      },
      /* #@range_begin(space) */
      space: function(){ // Parser ()
        return function(context){
          var isSpace = function(x){
            if(x === ' '){
              return true;
            } else {
              return false;
            }
          };
          return context.seq(context.many(context.satisfy(isSpace))(__.op["+"]), function(dummy){
            return context.succeed('');
          });
        }(this);
      },
      /* #@range_end(space) */
      /* #@range_begin(token) */
      // token::
      token: function(parser){ // Parser a => Parser a
        return function(context){
          return context.seq(context.space(), function(_){
            return context.seq(parser, function(v){
              return context.seq(context.space(), function(_){
                return context.succeed(v);
              });
            });
          });
        }(this);
      },
      /* #@range_end(token) */
      identifier: function(_){
        return function(context){
          return context.token(context.ident());
        }(this);
      },
      natural: function(_){
        return function(context){
          return context.token(context.nat());
        }(this);
      },
      symbol: function(string){
        return function(context){
          return context.token(context.string(string));
        }(this);
      },
      /* #@range_begin(seq) */
      // seq:: (Parser, Parser) => Parser
      seq: function(first, next){
        return function(context){
          return function(input){
            var firstResult = context.parse(first)(input);
            if(__.isEmpty(firstResult)) {
              return {};
            } else {
              var firstValue = firstResult.value;
              var nextInput = firstResult.input;
              return context.parse(next(firstValue))(nextInput);
            }
          };
        }(this);
      },
      /* #@range_end(seq) */
      /* #@range_begin(alt) */
      // alt:: (Parser, Parser) => Parser
      alt: function(first, alternative){
        return function(context){
          return function(input){
            var firstResult = context.parse(first)(input);
            if(__.isEmpty(firstResult)) {
              return context.parse(alternative)(input);
            } else {
              return firstResult;
            }
          };
        }(this);
      },
      /* #@range_end(alt) */
      /*
        #@range_begin(many1)
      */
      // Parser a => Parser [a]
      many1: function(parser){
        return function(context){
          return function(operator){
            return context.seq(parser,function(v){
              return context.seq(context.many(parser)(operator),function(vs){
                return context.succeed(operator(v,vs));
              });
            });
          };
        }(this);
      },
      /*  #@range_end(many1) */
      /* #@range_begin(many)  */
      // Parser a => Parser [a]
      many: function(parser){
        return function(context){
          return function(operator){
            return context.alt(context.many1(parser)(operator),context.succeed([]));
          };
        }(this);
      },
      /* #@range_end(many)  */
      /* #@range_begin(nat)  */
      nat: function(){ // Parser String
        return function(context){
          return context.seq(context.many1(context.digit())(__.op["+"]), function(digits){
            return context.succeed(Number(digits));
          });
        }(this);
      },
      /* #@range_end(nat)  */
    };

    /* #@range_begin(succeed_fail) */
    it('succeed', function(next) {
      expect(parser.parse(parser.succeed(1))("hello")).to.eql( { value : 1, input : 'hello' } );
      expect(parser.parse(parser.succeed(['+', '-']))("")).to.eql( { value : ['+', '-'], input : '' } );
      expect(parser.parse(parser.succeed(10))("")).to.eql( { value : 10, input : '' } );
      expect(parser.parse(parser.succeed(parseInt("10")))("")).to.eql( { value : 10, input : '' } );
      next();
    });
    it('fail', function(next) {
      expect(parser.parse(parser.fail())("hello")).to.eql( {} );
      next();
    });
    /* #@range_end(succeed_fail) */
    it('item', function(next) {
      expect(parser.parse(parser.item())("")).to.eql( [] );
      expect(parser.parse(parser.item())("hello")).to.eql( { value : 'h', input : 'ello' } );
      next();
    });
    it('char', function(next) {
      /* #@range_begin(char) */
      var plus = parser.char('+');
      expect(parser.parse(plus)("-")).to.eql( {} );
      expect(parser.parse(plus)("+")).to.eql( {value: '+', input: ''} );
      expect(parser.parse(plus)("-+")).to.eql( {} );
      /* #@range_end(char) */
      next();
    });
    describe('space', function() {
      /* #@range_begin(space) */
      it('空白パーサー space をテストする', function(next) {
        expect(parser.parse(parser.space())("  abc")).to.eql( { value : '', input : 'abc' });
        next();
      });
      /* #@range_end(space) */
    });
    it('string', function(next) {
      /* #@range_begin(string) */
      expect(parser.parse(parser.string("abc"))("abcdef")).to.eql( { value : 'abc', input : 'def' });
      expect(parser.parse(parser.string("abc"))("ab1234")).to.eql( {});
      /* #@range_end(string) */
      next();
    });
    describe('seq', function() {
      it('plusminus', function(next){
        var plus = parser.char('+');
        var minus = parser.char('-');
        var plusminus = parser.seq(plus,function(ans1){
          return parser.seq(minus,function(ans2){
            return parser.succeed([ans1, ans2]);
          });
        });
        expect(parser.parse(plusminus)("+-*")).to.eql( { value : ['+','-'], input : '*' } );
        next();
      });
      it('minusone should be -1', function(next){
        /*
          #@range_begin(minusone)
        */
        var minus = parser.char('-');
        var one = parser.char('1');
        var minusone = parser.seq(minus,function(ans1){
          return parser.seq(one,function(ans2){
            var ans = parseInt(ans1 + ans2);
            return parser.succeed(ans);
          });
        });
        expect(parser.parse(minusone)("-1")).to.eql( { value : -1, input : '' } );
        /*
          #@range_end(minusone)
        */
        next();
      });
      it('oneoneone should be 111', function(next){
        var one = parser.char('1');
        var oneoneone = parser.seq(one,function(ans1){
          return parser.seq(one,function(ans2){
            return parser.seq(one,function(ans3){
              var ans = parseInt(ans1 + ans2 + ans3);
              return parser.succeed(ans);
            });
          });
        });
        expect(parser.parse(oneoneone)("1111")).to.eql( { value : 111, input : '1' } );
        next();
      });
    });
    describe('alt', function() {
      it('one or succeed a',function(next){
        var syntax = parser.alt(parser.char('1'),
                                parser.succeed('a'));
        expect(parser.parse(syntax)("1a")).to.eql( { value : '1', input : 'a' });
        next();
      });
      it('item or succeed',function(next){
        var syntax = parser.alt(parser.item(), parser.succeed('a'));
        expect(parser.parse(syntax)("hello")).to.eql( { value : 'h', input : 'ello' });
        next();
      });
      it('fail or succeed(d) abc',function(next){
        var syntax = parser.alt(parser.fail(), parser.succeed('d'));
        expect(parser.parse(syntax)("abc")).to.eql( { value : 'd', input : 'abc' });
        next();
      });
      it('fail or succeed',function(next){
        var syntax = parser.alt(parser.fail(), parser.succeed('a'));
        expect(parser.parse(syntax)("hello")).to.eql({ value : 'a', input : 'hello' });
        next();
      });
      it('fail or fail',function(next){
        var syntax = parser.alt(parser.fail(), parser.fail());
        expect(parser.parse(syntax)("hello")).to.eql( {});
        next();
      });
    });
    // describe('digit', function() {
    //  it('accepts digits',function(next){
    //    expect(parser.parse(parser.many(parser.digit())(__.op["+"]))("123")).to.eql( { value : '123', input : '' });
    //    expect(parser.parse(parser.many1(parser.digit())(__.op["+"]))("abcdef")).to.eql( {});
    //    next();
    //  });
    // });
    // describe('lower', function() {
    //  it('many lower', function(next){
    //    expect(parser.parse(parser.many(parser.lower())(__.op["+"]))("abc")).to.eql( { value : 'abc', input : '' } );
    //    expect(parser.parse(parser.many(parser.lower())(__.op["+"]))("abcABC")).to.eql( { value : 'abc', input : 'ABC' });
    //    next();
    //  });
    // });
    // describe('ident', function() {
    //  it('ident', function(next){
    //    expect(parser.parse(parser.ident())("hello world")).to.eql( { value : 'hello', input : ' world' });
    //    next();
    //  });
    // });
    // describe('nat', function() {
    //  it('nat', function(next){
    //    var result = parser.parse(parser.nat())("123 pounds")
    //    expect(__.get("value")(result)).to.eql(123);
    //    //expect(parser.parse(parser.nat())("123 pounds")).to.eql( [ { value : 123, input : ' pounds' } ]); // should succeed
    //    next();
    //  });
    // });
    // describe('token', function() {
    //  it('identifier', function(next){
    //    expect(parser.parse(parser.identifier())("  abc   ")).to.eql( { value : 'abc', input : '' });
    //    next();
    //  });
    //  it('natural', function(next){
    //    expect(parser.parse(parser.natural())("  234   ")).to.eql( { value : 234, input : '' } );
    //    next();
    //  });
    //  it('symbol', function(next){
    //    expect(parser.parse(parser.symbol("pi"))("  pi   ")).to.eql(  { value : 'pi', input : '' } );
    //    expect(parser.parse(parser.symbol("pi"))("pi   ")).to.eql(  { value : 'pi', input : '' } );
    //    expect(parser.parse(parser.symbol("pi"))("  pi")).to.eql(  { value : 'pi', input : '' } );
    //    next();
    //  });
    // });
    describe('satisfy', function() {
      it('digit', function(next){
        /*
          #@range_begin(satisfy_digit) */
        var isDigit = function(x){
          if(/\d/.test(x)){
            return true;
          } else {
            return false;
          }
        };
        var digit = parser.satisfy(isDigit);
        expect(parser.parse(digit)("123")).to.eql( { value : '1', input : '23' } );
        expect(parser.parse(digit)("abc")).to.eql( {});
        /* #@range_end(satisfy_digit)
         */
        next();
      });
      it('char', function(next){
        expect(parser.parse(parser.char("a"))("abc")).to.eql( { value : 'a', input : 'bc' });
        expect(parser.parse(parser.char("a"))("123")).to.eql( {});
        next();
      });
    });
    // describe('many', function() {
    //  it('many digit 123abc',function(next){
    //    /*
    //     #@range_begin(many_digit)
    //     */
    //    var isDigit = function(x){
    //      if(/\d/.test(x)){
    //        return true;
    //      } else {
    //        return false;
    //      }
    //    };
    //    var digit = parser.satisfy(isDigit);
    //    expect(parser.parse(parser.many(digit)(__.op["+"]))("123abc")).to.eql({ value : '123', input : 'abc' } );
    //    /*
    //     #@range_end(many_digit)
    //     */
    //    next();
    //  });
    //  it('many digit abcdef',function(next){
    //    expect(parser.parse(parser.many(parser.digit())(__.op["+"]))("abcdef")).to.eql( { value : [  ], input : 'abcdef' });
    //    next();
    //  });
    //  it('many natural',function(next){
    //    var natural = parser.token(parser.nat());
    //    expect(parser.parse(parser.many(natural)(__.cons))("12 23 ")).to.eql( { value : [12, 23], input : '' });
    //    next();
    //  });
    // });
    // /* #@range_begin(natural_array) */
    // describe('自然数の配列を認識する', function() {
    //  var openBracket = parser.token(parser.char("["));
    //  var closeBracket = parser.token(parser.char("]"));
    //  var natural = parser.token(parser.nat());

    //  var arrayParser = parser.seq(openBracket,function(_){
    //    return parser.seq(parser.many(natural)(__.cons),function(naturals){
    //      return parser.seq(closeBracket,function(_){
    //        if(__.isEmpty(naturals))
    //          return parser.succeed([]);
    //        else
    //          return parser.succeed(naturals);
    //      });
    //    });
    //  });
    //  it('[ ]', function(next){
    //    expect(parser.parse(arrayParser)("[   ]")).to.eql(  { value : [ ], input : '' } );
    //    next();
    //  });
    //  it('[ 12 23 45 ]', function(next){
    //    expect(parser.parse(arrayParser)("[ 12 23 45 ]")).to.eql(  { value : [12, 23, 45 ], input : '' } );
    //    next();
    //  });
    // });
    // /* #@range_end(natural_array) */
  });
});
