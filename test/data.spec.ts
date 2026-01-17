"use strict";

import * as Pair from '../lib/pair';
import * as Data from '../lib/data';

// List is not yet converted to TypeScript, use require
const List = require('../lib/list.js');

describe('Data', () => {
  const tuple = Pair.cons(1, 2);

  it("Data#type", () => {
    Data.type(tuple, {
      pair: () => {
        expect(true).toEqual(true);
      }
    });

    Data.type(1, {
      pair: () => {
        fail('Should not reach pair handler for number');
      },
      number: (value: number) => {
        expect(value).toEqual(1);
      }
    });
  });

  describe('Data#match', () => {
    it("Data#match(pair)", () => {
      Data.match(tuple, {
        cons: (x: number, _y: number) => {
          expect(x).toEqual(1);
        }
      });
    });

    it("Data#match(list)", () => {
      const list = List.cons(1, List.empty());
      Data.match(list, {
        cons: (x: number, _y: any) => {
          expect(x).toEqual(1);
        }
      });
    });
  });
});
