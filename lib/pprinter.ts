"use strict";

import { type as dataType } from './data';
import { List, foldr } from './list';
import { Pair } from './pair';

// Type for printable data (can be List, Pair, number, or string)
type Printable = List<any> | Pair<any, any> | number | string;

/**
 * Pretty prints data structures
 */
export function print(data: Printable): string | number {
  return dataType<any, string | number>(data, {
    pair: () => {
      return (data as Pair<any, any>).match({
        empty: () => "()",
        cons: (l: any, r: any) => {
          const left = print(l);
          const right = print(r);
          return "(" + left + "," + right + ")";
        }
      });
    },
    list: () => {
      return (data as List<any>).match({
        empty: () => "[]",
        cons: (_head: any, _tail: List<any>) => {
          return "[" + foldr(data as List<any>)("nil")((item: any) => {
            return (accumulator: string) => {
              return print(item) + "," + accumulator;
            };
          }) + "]";
        }
      });
    },
    number: (d: number) => d,
    string: (d: string) => d
  });
}

// Namespace export for module compatibility
export const Pprinter = {
  print
};

export default Pprinter;
