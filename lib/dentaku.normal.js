"use strict";

var expect = require('expect.js');
var Pair = require('../lib/pair.js');
var List = require('../lib/list.js');
var Parser = require('../lib/parser.js'); 
var I = require('../lib/evaluator.js').ID; 
var Env = require('../lib/env.js');
var PP = require('../lib/pprinter.js');

module.exports = {
    // expr ::= term (+ expr | e)
    expr: (_) => {
        var self = this;
        var add = List.fromString("+");
        var subtract = List.fromString("-");
        var add_or_subtract_parser = Parser.alt(Parser.symbol(add))(Parser.symbol(subtract));
        return Parser.flatMap(self.term())((t) => {
            return Parser.alt(Parser.flatMap(add_or_subtract_parser)((operator) => {
                if(List.isEqual(operator,add)) {
                    return Parser.flatMap(self.expr())((e) => {
                        return Parser.pure(I.Exp.add(t, e));
                    });
                } else if(List.isEqual(operator, subtract)) {
                    return Parser.flatMap(self.expr())((e) => {
                        return Parser.pure(I.Exp.subtract(t, e));
                    });
                }
            }))(
                Parser.pure(t)
            );
        });
    },   
    // term ::= factor (* term | e)
    term: (_) => {
        var self = this;
        var multiply = List.fromString("*");
        var divide = List.fromString("/");
        var multiply_or_divide_parser = Parser.alt(Parser.symbol(multiply))(Parser.symbol(divide));
        return Parser.flatMap(self.factor())((f) => {
            return Parser.alt(Parser.flatMap(multiply_or_divide_parser)((operator) => {
                if(List.isEqual(operator,multiply)) {
                    return Parser.flatMap(self.expr())((e) => {
                        return Parser.pure(I.Exp.multiply(f, e));
                    });
                } else if(List.isEqual(operator, divide)) {
                    return Parser.flatMap(self.expr())((e) => {
                        return Parser.pure(I.Exp.divide(f, e));
                    });
                }
            }))(
                Parser.pure(f)
            );
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
            Parser.flatMap(Parser.numeric())((numeric) => {
                return Parser.pure(I.Exp.num(numeric));
            })
          )
    },
    evaluate: (inputString) => {
        var self = this;
        var parseResult = Parser.parse(self.expr())(List.fromString(inputString));
        var env = Env.empty;

        return parseResult.match({
            empty: (_) => {
                return "Invalid input";
            },
            cons: (head, tail) => {
                return head.match({
                    cons: (ast, remainingInput) => {
                        return remainingInput.match({
                            empty: (_) => {
                                return I.evaluate(ast,env);
                            },
                            cons: (head, tail) => {
                                return PP.print(tail);
                                // throw("Unused input "  + PP.print(tail));
                            }
                        });
                    }
                })
            }
        });

    }   
};

