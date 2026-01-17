"use strict";

import { fromString as listFromString, isEqual as listIsEqual, List } from './list';
import * as Parser from './parser';
import { IDEvaluator as I } from './evaluator';
import { empty as envEmpty } from './env';
import { print as ppPrint } from './pprinter';

/**
 * Simple calculator parser and evaluator
 */
export const Dentaku = {
  /**
   * expr ::= term (+ expr | - expr | e)
   */
  expr: (): Parser.Parser<any> => {
    const add = listFromString("+");
    const subtract = listFromString("-");
    const add_or_subtract_parser = Parser.alt(Parser.symbol(add))(Parser.symbol(subtract));

    return Parser.flatMap(Dentaku.term())((t: any) => {
      return Parser.alt(Parser.flatMap(add_or_subtract_parser)((operator: List<string>) => {
        if (listIsEqual(operator, add)) {
          return Parser.flatMap(Dentaku.expr())((e: any) => {
            return Parser.pure(I.Exp.add(t, e));
          });
        } else if (listIsEqual(operator, subtract)) {
          return Parser.flatMap(Dentaku.expr())((e: any) => {
            return Parser.pure(I.Exp.subtract(t, e));
          });
        }
        return Parser.empty;
      }))(
        Parser.pure(t)
      );
    });
  },

  /**
   * term ::= factor (* term | / term | e)
   */
  term: (): Parser.Parser<any> => {
    const multiply = listFromString("*");
    const divide = listFromString("/");
    const multiply_or_divide_parser = Parser.alt(Parser.symbol(multiply))(Parser.symbol(divide));

    return Parser.flatMap(Dentaku.factor())((f: any) => {
      return Parser.alt(Parser.flatMap(multiply_or_divide_parser)((operator: List<string>) => {
        if (listIsEqual(operator, multiply)) {
          return Parser.flatMap(Dentaku.expr())((e: any) => {
            return Parser.pure(I.Exp.multiply(f, e));
          });
        } else if (listIsEqual(operator, divide)) {
          return Parser.flatMap(Dentaku.expr())((e: any) => {
            return Parser.pure(I.Exp.divide(f, e));
          });
        }
        return Parser.empty;
      }))(
        Parser.pure(f)
      );
    });
  },

  /**
   * factor ::= (expr) | nat
   */
  factor: (): Parser.Parser<any> => {
    const openParen = Parser.symbol(listFromString("("));
    const closeParen = Parser.symbol(listFromString(")"));

    return Parser.alt(Parser.flatMap(openParen)((_: any) => {
      return Parser.flatMap(Dentaku.expr())((e: any) => {
        return Parser.flatMap(closeParen)((_: any) => {
          return Parser.pure(e);
        });
      });
    }))(
      Parser.flatMap(Parser.numeric())((numeric: number) => {
        return Parser.pure(I.Exp.num(numeric));
      })
    );
  },

  /**
   * Evaluate an input string expression
   */
  evaluate: (inputString: string): any => {
    const parseResult = Parser.parse(Dentaku.expr())(listFromString(inputString));
    const env = envEmpty;

    return parseResult.match({
      empty: () => "Invalid input",
      cons: (head, _tail) => {
        return head.match({
          cons: (ast: any, remainingInput: List<string>) => {
            return remainingInput.match({
              empty: () => I.evaluate(ast, env),
              cons: (_h, tail) => ppPrint(tail)
            });
          }
        });
      }
    });
  }
};

export default Dentaku;
