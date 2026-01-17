"use strict";

import * as fs from 'fs';
import { List, map as listMap, cons as listCons, unit as listUnit } from './list';

// Maybe pattern types
export interface MaybePatterns<T, R> {
  just: (value: T) => R;
  nothing: (value?: undefined) => R;
}

// Church-encoded Maybe type
export type Maybe<T> = <R>(pattern: MaybePatterns<T, R>) => R;

// Either pattern types
export interface EitherPatterns<L, R, Result> {
  left: (value: L) => Result;
  right: (value: R) => Result;
}

// Church-encoded Either type
export type Either<L, R> = (pattern: EitherPatterns<L, R, any>) => any;

// IO type
export type IO<T> = () => T;

// Continuation type
export type Cont<R, A> = (k: (a: A) => R) => R;

/**
 * ID Monad - The simplest monad (identity)
 */
export const ID = {
  unit: <T>(value: T): T => {
    return value;
  },

  flatMap: <T>(instance: T) => <U>(transform: (value: T) => U): U => {
    return transform(instance);
  }
};

/**
 * Maybe Monad - Represents optional values
 */
export const Maybe = {
  match: <T, R>(data: Maybe<T>, pattern: MaybePatterns<T, R>): R => {
    return data(pattern);
  },

  just: <T>(value: T): Maybe<T> => {
    return <R>(pattern: MaybePatterns<T, R>): R => {
      return pattern.just(value);
    };
  },

  nothing: <T>(): Maybe<T> => {
    return <R>(pattern: MaybePatterns<T, R>): R => {
      return pattern.nothing();
    };
  },

  unit: <T>(value: T): Maybe<T> => {
    return Maybe.just(value);
  },

  flatMap: <T>(maybeInstance: Maybe<T>) => <U>(transform: (value: T) => Maybe<U>): Maybe<U> => {
    return Maybe.match(maybeInstance, {
      just: (value) => transform(value),
      nothing: () => Maybe.nothing<U>()
    });
  },

  // MonadPlus instance
  zero: <T>(): Maybe<T> => {
    return Maybe.nothing();
  },

  plus: <T>(x: Maybe<T>) => (y: Maybe<T>): Maybe<T> => {
    return Maybe.match(x, {
      nothing: () => {
        return Maybe.match(y, {
          nothing: () => Maybe.nothing<T>(),
          just: () => y
        });
      },
      just: () => x
    });
  },

  // Functor instance
  map: <T>(maybeInstance: Maybe<T>) => <U>(transform: (value: T) => U): Maybe<U> => {
    return Maybe.match(maybeInstance, {
      nothing: () => Maybe.nothing<U>(),
      just: (value) => Maybe.just(transform(value))
    });
  },

  // liftM :: (a -> b) -> Maybe a -> Maybe b
  liftM: <A, B>(f: (a: A) => B) => (ma: Maybe<A>): Maybe<B> => {
    return Maybe.flatMap(ma)((x) => Maybe.unit(f(x)));
  },

  // apply :: Maybe (a -> b) -> Maybe a -> Maybe b
  apply: <A, B>(mf: Maybe<(a: A) => B>) => (ma: Maybe<A>): Maybe<B> => {
    return Maybe.flatMap(mf)((f) => {
      return Maybe.flatMap(ma)((a) => {
        return Maybe.unit(f(a));
      });
    });
  },

  get: <T>(maybe: Maybe<T>): T | null => {
    return Maybe.getOrElse(maybe)(null);
  },

  getOrElse: <T>(instance: Maybe<T>) => (alternate: T | null): T | null => {
    return Maybe.match(instance, {
      just: (value) => value,
      nothing: () => alternate
    });
  },

  isEqual: <T>(maybeA: Maybe<T>) => (maybeB: Maybe<T>): boolean => {
    return Maybe.match(maybeA, {
      just: (valueA) => {
        return Maybe.match(maybeB, {
          just: (valueB) => valueA === valueB,
          nothing: () => false
        });
      },
      nothing: () => {
        return Maybe.match(maybeB, {
          just: () => false,
          nothing: () => true
        });
      }
    });
  }
};

/**
 * Either Monad - Represents values with two possibilities
 */
export const Either = {
  match: <L, R, Result>(data: Either<L, R>, pattern: EitherPatterns<L, R, Result>): Result => {
    return data(pattern);
  },

  left: <L, R>(value: L): Either<L, R> => {
    return (pattern: EitherPatterns<L, R, any>): any => {
      return pattern.left(value);
    };
  },

  right: <L, R>(value: R): Either<L, R> => {
    return (pattern: EitherPatterns<L, R, any>): any => {
      return pattern.right(value);
    };
  },

  unit: <L, R>(value: R): Either<L, R> => {
    return Either.right(value);
  },

  flatMap: <L, R>(instanceM: Either<L, R>) => <U>(transform: (value: R) => Either<L, U>): Either<L, U> => {
    return Either.match(instanceM, {
      right: (value) => transform(value),
      left: (value) => Either.left<L, U>(value)
    });
  },

  // Functor instance
  map: <L, R>(instanceM: Either<L, R>) => <U>(transform: (value: R) => U): Either<L, U> => {
    return Either.match(instanceM, {
      right: (value) => Either.right<L, U>(transform(value)),
      left: (value) => Either.left<L, U>(value)
    });
  }
};

/**
 * IO Monad - Represents side effects
 */
export const IOMonad = {
  unit: <T>(any: T): IO<T> => {
    return (): T => any;
  },

  flatMap: <T>(instanceA: IO<T>) => <U>(actionAB: (a: T) => IO<U>): IO<U> => {
    return IOMonad.unit(IOMonad.run(actionAB(IOMonad.run(instanceA))));
  },

  done: <T>(): IO<T | undefined> => {
    return IOMonad.unit(undefined);
  },

  run: <T>(instanceM: IO<T>): T => {
    return instanceM();
  },

  readFile: (path: string): IO<IO<string>> => {
    return (): IO<string> => {
      const content = fs.readFileSync(path, 'utf8');
      return IOMonad.unit(content);
    };
  },

  println: (message: string): IO<IO<null>> => {
    return (): IO<null> => {
      console.log(message);
      return IOMonad.unit(null);
    };
  },

  writeFile: (path: string) => (content: string): IO<IO<null>> => {
    return (): IO<null> => {
      fs.writeFileSync(path, content);
      return IOMonad.unit(null);
    };
  },

  seq: <T>(instanceA: IO<T>) => <U>(instanceB: IO<U>): IO<U> => {
    return IOMonad.flatMap(instanceA)((_a) => instanceB);
  },

  seqs: <T>(alist: List<IO<T>>): IO<T | undefined> => {
    return alist.match({
      empty: () => IOMonad.unit(undefined),
      cons: (head, tail) => {
        return IOMonad.flatMap(head)((_) => IOMonad.seqs(tail));
      }
    });
  },

  putc: (character: string): IO<null> => {
    return (_io?: any): null => {
      process.stdout.write(character);
      return null;
    };
  },

  puts: (alist: List<string>): IO<null | undefined> => {
    return IOMonad.seqs(listMap(alist)(IOMonad.putc));
  },

  getc: (): any => {
    const continuation = (): string | null => {
      const chunk = process.stdin.read();
      return chunk;
    };
    process.stdin.setEncoding('utf8');
    return process.stdin.on('readable', continuation);
  },

  getLine: (): IO<List<string>> => {
    return IOMonad.flatMap(IOMonad.getc)((c: string) => {
      if (c === "\n") {
        return IOMonad.unit(listUnit(""));
      } else {
        return IOMonad.flatMap(IOMonad.getLine())((l: List<string>) => {
          return IOMonad.unit(listCons(c, l));
        });
      }
    });
  }
};

/**
 * Continuation Monad
 */
export const ContMonad = {
  unit: <R, A>(a: A): Cont<R, A> => {
    return (k: (a: A) => R): R => {
      return k(a);
    };
  },

  flatMap: <R, A>(m: Cont<R, A>) => <B>(f: (a: A) => Cont<R, B>): Cont<R, B> => {
    return (k: (b: B) => R): R => {
      return m((a: A) => {
        return f(a)(k);
      });
    };
  },

  callCC: <R, A>(f: (exit: (a: A) => Cont<R, never>) => Cont<R, A>): Cont<R, A> => {
    return (k: (a: A) => R): R => {
      return f((a: A) => {
        return (_: any): R => {
          return k(a);
        };
      })(k);
    };
  }
};

// Main Monad namespace for compatibility
export const Monad = {
  ID,
  Maybe,
  Either,
  IO: IOMonad,
  Cont: ContMonad
};

export default Monad;
