"use strict";

var expect = require('expect.js');
var Pair = require('../lib/pair.js');
var List = require('../lib/list.js');
var Parser = require('../lib/parser.js'); 
var PP = require('../lib/pprinter.js');

module.exports = {
    // expr ::= term (+ expr | e)
    expr: (_) => {
        var self = this;
        return Parser.flatMap(self.term())((t) => {
            return Parser.alt(Parser.flatMap(Parser.symbol(List.fromString("+")))((_) => {
                return Parser.flatMap(self.expr())((e) => {
                    return Parser.pure(t + e);
                });
            }))(
            Parser.pure(t)
            );
        });
    },   
    // term ::= factor (* term | e)
    term: (_) => {
        var self = this;
        var multiply = Parser.symbol(List.fromString("*"));
        return Parser.flatMap(self.factor())((f) => {
            return Parser.alt(Parser.flatMap(multiply)((_) => {
                return Parser.flatMap(self.expr())((e) => {
                    return Parser.pure(f * e);
                });
            }))(
                Parser.pure(f)
            )
        });
    },   
    // factor ::= (expr) | nat
    factor: (_) => {
        var self = this;
        var openParen = Parser.symbol(List.fromString("("));
        var closeParen = Parser.symbol(List.fromString(")"));
        return Parser.alt(Parser.flatMap(openParen)((_) => {
            return Parser.flatMap(self.expr())((e) => {
                return Parser.flatMap(closeParen)((_) => {
                    return Parser.pure(e);
                })
            });
        }))(
            Parser.natural()
          )
    },
    evaluate: (inputString) => {
        var self = this;
        var parseResult = Parser.parse(self.expr())(List.fromString(inputString));
        return parseResult.match({
            empty: (_) => {
                return "Invalid input";
            },
            cons: (head, tail) => {
                return head.match({
                    cons: (left, remainingInput) => {
                        return remainingInput.match({
                            empty: (_) => {
                                return left;
                            },
                            cons: (head, tail) => {
                                return PP.print(tail);
                            }
                        });
                    }
                })
            }
        });

    }   
};

