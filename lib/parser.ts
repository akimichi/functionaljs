"use strict";

import { Pair, cons as pairCons, empty as pairEmpty, EmptyPair } from './pair';
import { List, cons as listCons, empty as listEmpty, foldr } from './list';

// Parser type: String -> [(a, String)]
export type Parser<A> = (input: List<string>) => List<Pair<A, List<string>>>;

/**
 * pure :: a -> Parser a
 */
export function pure<A>(v: A): Parser<A> {
  return (input: List<string>): List<Pair<A, List<string>>> => {
    return listCons(pairCons(v, input), listEmpty());
  };
}

/**
 * parse :: Parser a -> String -> [(a,String)]
 */
export function parse<A>(parser: Parser<A>): (input: List<string>) => List<Pair<A, List<string>>> {
  return (input: List<string>): List<Pair<A, List<string>>> => {
    return parser(input);
  };
}

/**
 * fmap :: (a -> b) -> Parser a -> Parser b
 */
export function fmap<A, B>(f: (a: A) => B): (parserA: Parser<A>) => Parser<B> {
  return (parserA: Parser<A>): Parser<B> => {
    return (input: List<string>): List<Pair<B, List<string>>> => {
      return parse(parserA)(input).match({
        empty: () => listEmpty(),
        cons: (pair, _) => {
          return pair.match({
            cons: (v: A, out: List<string>) => {
              return listCons(pairCons(f(v), out), listEmpty());
            }
          });
        }
      });
    };
  };
}

/**
 * <*> :: Parser (a -> b) -> Parser a -> Parser b
 */
export function apply<A, B>(pg: Parser<(a: A) => B>): (px: Parser<A>) => Parser<B> {
  return (px: Parser<A>): Parser<B> => {
    return (input: List<string>): List<Pair<B, List<string>>> => {
      return parse(pg)(input).match({
        empty: () => listEmpty(),
        cons: (pair, _) => {
          return pair.match({
            cons: (g: (a: A) => B, out: List<string>) => {
              return parse(fmap(g)(px))(out);
            }
          });
        }
      });
    };
  };
}

/**
 * flatMap :: Parser a -> (a -> Parser b) -> Parser b
 */
export function flatMap<A>(parser: Parser<A>): <B>(f: (a: A) => Parser<B>) => Parser<B> {
  return <B>(f: (a: A) => Parser<B>): Parser<B> => {
    return (input: List<string>): List<Pair<B, List<string>>> => {
      return parse(parser)(input).match({
        empty: () => listEmpty(),
        cons: (pair, _) => {
          return pair.match({
            cons: (v: A, out: List<string>) => {
              return parse(f(v))(out);
            }
          });
        }
      });
    };
  };
}

/**
 * empty :: Parser a
 */
export function empty<A>(_input: List<string>): List<Pair<A, List<string>>> {
  return listEmpty();
}

/**
 * alt :: Parser a -> Parser a -> Parser a
 */
export function alt<A>(parserA: Parser<A>): (parserB: Parser<A>) => Parser<A> {
  return (parserB: Parser<A>): Parser<A> => {
    return (input: List<string>): List<Pair<A, List<string>>> => {
      return parse(parserA)(input).match({
        empty: () => parse(parserB)(input),
        cons: (pair, _) => {
          return pair.match({
            cons: (v: A, out: List<string>) => {
              return listCons(pairCons(v, out), listEmpty());
            }
          });
        }
      });
    };
  };
}

/**
 * item :: Parser String
 */
export function item(input: List<string>): List<Pair<string, List<string>>> {
  return input.match({
    empty: () => listEmpty(),
    cons: (head: string, tail: List<string>) => {
      return listCons(pairCons(head, tail), listEmpty());
    }
  });
}

/**
 * sat :: (String -> Bool) -> Parser String
 */
export function sat(predicate: (x: string) => boolean): Parser<string> {
  return flatMap<string>(item)((x: string): Parser<string> => {
    if (predicate(x) === true) {
      return pure(x);
    } else {
      return empty as Parser<string>;
    }
  });
}

/**
 * digit :: Parser String
 */
export function digit(): Parser<string> {
  const isDigit = (x: string): boolean => !isNaN(Number(x));
  return sat(isDigit);
}

/**
 * lower :: Parser String
 */
export function lower(): Parser<string> {
  const isLower = (x: string): boolean => /^[a-z]/.test(x);
  return sat(isLower);
}

/**
 * upper :: Parser String
 */
export function upper(): Parser<string> {
  const isUpper = (x: string): boolean => /^[A-Z]/.test(x);
  return sat(isUpper);
}

/**
 * letter :: Parser Char
 */
export function letter(): Parser<string> {
  const isAlpha = (x: string): boolean => /^[a-zA-Z]/.test(x);
  return sat(isAlpha);
}

/**
 * alphanum :: Parser Char
 */
export function alphanum(): Parser<string> {
  const isAlphaNum = (x: string): boolean => /^[a-zA-Z0-9]/.test(x);
  return sat(isAlphaNum);
}

/**
 * char :: Char -> Parser Char
 */
export function char(x: string): Parser<string> {
  const isX = (input: string): boolean => x === input;
  return sat(isX);
}

/**
 * string :: String -> Parser String
 */
export function string(str: List<string>): Parser<List<string>> {
  return str.match({
    empty: () => pure(listEmpty()),
    cons: (x: string, xs: List<string>) => {
      return flatMap(char(x))((_) => {
        return flatMap(string(xs))((_) => {
          return pure(listCons(x, xs));
        });
      });
    }
  });
}

/**
 * many :: Parser a -> Parser [a]
 */
export function many<A>(x: Parser<A>): Parser<List<A>> {
  return alt(some(x))(pure(listEmpty()));
}

/**
 * some :: Parser a -> Parser [a]
 */
export function some<A>(x: Parser<A>): Parser<List<A>> {
  return flatMap(x)((a: A) => {
    return flatMap(many(x))((b: List<A>) => {
      return pure(listCons(a, b));
    });
  });
}

/**
 * ident :: Parser [Char]
 */
export function ident(): Parser<List<string>> {
  return flatMap(lower())((x: string) => {
    return flatMap(many(alphanum()))((xs: List<string>) => {
      return pure(listCons(x, xs));
    });
  });
}

/**
 * nat :: Parser Int
 */
export function nat(): Parser<number> {
  const read = (xs: List<string>): number => {
    const list2str = (list: List<string>): string => {
      return foldr(list)("")((x: string) => (accumulator: string) => x + accumulator);
    };
    return parseInt(list2str(xs), 10);
  };
  return flatMap(some(digit()))((xs: List<string>) => {
    return pure(read(xs));
  });
}

/**
 * space :: Parser ()
 */
export function space(): Parser<EmptyPair> {
  const isSpace = (x: string): boolean => /^[ \t]/.test(x);
  return flatMap(many(sat(isSpace)))((_) => {
    return pure(pairEmpty());
  });
}

/**
 * int :: Parser Int
 */
export function int(): Parser<number> {
  return alt(
    flatMap(char("-"))((_) => {
      return flatMap(nat())((n: number) => {
        return pure(-n);
      });
    })
  )(nat());
}

/**
 * float :: Parser Float
 */
export function float(): Parser<number> {
  const minus = char("-");
  const dot = char(".");
  return alt(
    flatMap(minus)((_) => {
      return flatMap(nat())((n: number) => {
        return flatMap(dot)((_) => {
          return flatMap(nat())((m: number) => {
            return pure(-n - m * (1 / Math.pow(10, Math.floor(1 + Math.log10(m)))));
          });
        });
      });
    })
  )(
    flatMap(nat())((n: number) => {
      return flatMap(dot)((_) => {
        return flatMap(nat())((m: number) => {
          return pure(n + m * (1 / Math.pow(10, Math.floor(1 + Math.log10(m)))));
        });
      });
    })
  );
}

/**
 * token :: Parser a -> Parser a
 */
export function token<A>(parser: Parser<A>): Parser<A> {
  return flatMap(space())((_) => {
    return flatMap(parser)((v: A) => {
      return flatMap(space())((_) => {
        return pure(v);
      });
    });
  });
}

/**
 * identifier :: Parser [Char]
 */
export function identifier(): Parser<List<string>> {
  return token(ident());
}

/**
 * natural :: Parser Int
 */
export function natural(): Parser<number> {
  return token(nat());
}

/**
 * integer :: Parser Int
 */
export function integer(): Parser<number> {
  return token(int());
}

/**
 * numeric :: Parser Float
 */
export function numeric(): Parser<number> {
  return token(alt(float())(int()));
}

/**
 * symbol :: String -> Parser String
 */
export function symbol(xs: List<string>): Parser<List<string>> {
  return token(string(xs));
}

// Namespace export for module compatibility
export const Parser = {
  pure,
  parse,
  fmap,
  apply,
  flatMap,
  empty,
  alt,
  item,
  sat,
  digit,
  lower,
  upper,
  letter,
  alphanum,
  char,
  string,
  many,
  some,
  ident,
  nat,
  space,
  int,
  float,
  token,
  identifier,
  natural,
  integer,
  numeric,
  symbol
};

export default Parser;
