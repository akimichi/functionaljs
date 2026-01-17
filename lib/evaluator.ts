"use strict";

import { Env, Environment, lookup, extend } from './env';
import { ID, ContMonad } from './monad';

// Expression patterns for ID evaluator
export interface IDExpPatterns<R> {
  num: (value: number) => R;
  variable: (name: string) => R;
  lambda: (variable: IDExp, body: IDExp) => R;
  app: (lambda: IDExp, arg: IDExp) => R;
  add: (expL: IDExp, expR: IDExp) => R;
  subtract: (expL: IDExp, expR: IDExp) => R;
  divide: (expL: IDExp, expR: IDExp) => R;
  multiply: (expL: IDExp, expR: IDExp) => R;
}

// Church-encoded expression type for ID evaluator
export type IDExp = <R>(pattern: Partial<IDExpPatterns<R>>) => R;

// Expression patterns for Cont evaluator
export interface ContExpPatterns<R> {
  num: (value: number) => R;
  variable: (name: string) => R;
  lambda: (variable: ContExp, body: ContExp) => R;
  app: (lambda: ContExp, arg: ContExp) => R;
  add: (expL: ContExp, expR: ContExp) => R;
  callcc: (name: string, exp: ContExp) => R;
}

// Church-encoded expression type for Cont evaluator
export type ContExp = <R>(pattern: Partial<ContExpPatterns<R>>) => R;

/**
 * ID Monad based evaluator
 */
export const IDEvaluator = {
  Exp: {
    match: <R>(data: IDExp, pattern: IDExpPatterns<R>): R => {
      return data(pattern);
    },

    num: (value: number): IDExp => {
      return <R>(pattern: Partial<IDExpPatterns<R>>): R => {
        return pattern.num!(value);
      };
    },

    variable: (name: string): IDExp => {
      return <R>(pattern: Partial<IDExpPatterns<R>>): R => {
        return pattern.variable!(name);
      };
    },

    lambda: (variable: IDExp, body: IDExp): IDExp => {
      return <R>(pattern: Partial<IDExpPatterns<R>>): R => {
        return pattern.lambda!(variable, body);
      };
    },

    app: (lambda: IDExp, arg: IDExp): IDExp => {
      return <R>(pattern: Partial<IDExpPatterns<R>>): R => {
        return pattern.app!(lambda, arg);
      };
    },

    add: (expL: IDExp, expR: IDExp): IDExp => {
      return <R>(pattern: Partial<IDExpPatterns<R>>): R => {
        return pattern.add!(expL, expR);
      };
    },

    subtract: (expL: IDExp, expR: IDExp): IDExp => {
      return <R>(pattern: Partial<IDExpPatterns<R>>): R => {
        return pattern.subtract!(expL, expR);
      };
    },

    divide: (expL: IDExp, expR: IDExp): IDExp => {
      return <R>(pattern: Partial<IDExpPatterns<R>>): R => {
        return pattern.divide!(expL, expR);
      };
    },

    multiply: (expL: IDExp, expR: IDExp): IDExp => {
      return <R>(pattern: Partial<IDExpPatterns<R>>): R => {
        return pattern.multiply!(expL, expR);
      };
    }
  },

  evaluate: (anExp: IDExp, environment: Environment<any>): any => {
    return IDEvaluator.Exp.match(anExp, {
      num: (numericValue: number) => {
        return ID.unit(numericValue);
      },

      variable: (name: string) => {
        return ID.unit(lookup(name, environment));
      },

      lambda: (variable: IDExp, body: IDExp) => {
        return IDEvaluator.Exp.match(variable, {
          num: () => { throw new Error('Expected variable'); },
          variable: (name: string) => {
            return ID.unit((actualArg: any) => {
              return IDEvaluator.evaluate(body, extend(name, actualArg, environment));
            });
          },
          lambda: () => { throw new Error('Expected variable'); },
          app: () => { throw new Error('Expected variable'); },
          add: () => { throw new Error('Expected variable'); },
          subtract: () => { throw new Error('Expected variable'); },
          divide: () => { throw new Error('Expected variable'); },
          multiply: () => { throw new Error('Expected variable'); }
        });
      },

      app: (lambda: IDExp, arg: IDExp) => {
        return ID.flatMap(IDEvaluator.evaluate(lambda, environment))((closure: any) => {
          return ID.flatMap(IDEvaluator.evaluate(arg, environment))((actualArg: any) => {
            return closure(actualArg);
          });
        });
      },

      add: (expL: IDExp, expR: IDExp) => {
        return ID.flatMap(IDEvaluator.evaluate(expL, environment))((valueL: number) => {
          return ID.flatMap(IDEvaluator.evaluate(expR, environment))((valueR: number) => {
            return ID.unit(valueL + valueR);
          });
        });
      },

      subtract: (expL: IDExp, expR: IDExp) => {
        return ID.flatMap(IDEvaluator.evaluate(expL, environment))((valueL: number) => {
          return ID.flatMap(IDEvaluator.evaluate(expR, environment))((valueR: number) => {
            return ID.unit(valueL - valueR);
          });
        });
      },

      multiply: (expL: IDExp, expR: IDExp) => {
        return ID.flatMap(IDEvaluator.evaluate(expL, environment))((valueL: number) => {
          return ID.flatMap(IDEvaluator.evaluate(expR, environment))((valueR: number) => {
            return ID.unit(valueL * valueR);
          });
        });
      },

      divide: (expL: IDExp, expR: IDExp) => {
        return ID.flatMap(IDEvaluator.evaluate(expL, environment))((valueL: number) => {
          return ID.flatMap(IDEvaluator.evaluate(expR, environment))((valueR: number) => {
            return ID.unit(valueL / valueR);
          });
        });
      }
    });
  }
};

/**
 * Continuation Monad based evaluator
 */
export const ContEvaluator = {
  Exp: {
    match: <R>(data: ContExp, pattern: ContExpPatterns<R>): R => {
      return data(pattern);
    },

    num: (value: number): ContExp => {
      return <R>(pattern: Partial<ContExpPatterns<R>>): R => {
        return pattern.num!(value);
      };
    },

    variable: (name: string): ContExp => {
      return <R>(pattern: Partial<ContExpPatterns<R>>): R => {
        return pattern.variable!(name);
      };
    },

    lambda: (variable: ContExp, body: ContExp): ContExp => {
      return <R>(pattern: Partial<ContExpPatterns<R>>): R => {
        return pattern.lambda!(variable, body);
      };
    },

    app: (lambda: ContExp, arg: ContExp): ContExp => {
      return <R>(pattern: Partial<ContExpPatterns<R>>): R => {
        return pattern.app!(lambda, arg);
      };
    },

    add: (expL: ContExp, expR: ContExp): ContExp => {
      return <R>(pattern: Partial<ContExpPatterns<R>>): R => {
        return pattern.add!(expL, expR);
      };
    },

    callcc: (name: string, exp: ContExp): ContExp => {
      return <R>(pattern: Partial<ContExpPatterns<R>>): R => {
        return pattern.callcc!(name, exp);
      };
    }
  },

  evaluate: (anExp: ContExp, environment: Environment<any>): any => {
    return ContEvaluator.Exp.match(anExp, {
      num: (numericValue: number) => {
        return ContMonad.unit(numericValue);
      },

      variable: (name: string) => {
        return ContMonad.unit(lookup(name, environment));
      },

      add: (expL: ContExp, expR: ContExp) => {
        return ContMonad.flatMap(ContEvaluator.evaluate(expL, environment))((valueL: any) => {
          return ContMonad.flatMap(ContEvaluator.evaluate(expR, environment))((valueR: any) => {
            return ContMonad.unit(valueL + valueR);
          });
        });
      },

      lambda: (variable: ContExp, body: ContExp) => {
        return ContEvaluator.Exp.match(variable, {
          num: () => { throw new Error('Expected variable'); },
          variable: (name: string) => {
            return ContMonad.unit((actualArg: any) => {
              return ContEvaluator.evaluate(body, extend(name, actualArg, environment));
            });
          },
          lambda: () => { throw new Error('Expected variable'); },
          app: () => { throw new Error('Expected variable'); },
          add: () => { throw new Error('Expected variable'); },
          callcc: () => { throw new Error('Expected variable'); }
        });
      },

      app: (lambda: ContExp, arg: ContExp) => {
        return ContMonad.flatMap(ContEvaluator.evaluate(lambda, environment))((closure: any) => {
          return ContMonad.flatMap(ContEvaluator.evaluate(arg, environment))((actualArg: any) => {
            return closure(actualArg);
          });
        });
      },

      callcc: (_name: string, _exp: ContExp) => {
        // TODO: Implement callcc evaluation
        throw new Error('callcc not implemented');
      }
    });
  }
};

// Main export for compatibility
export const Evaluator = {
  ID: IDEvaluator,
  Cont: ContEvaluator
};

export default Evaluator;
