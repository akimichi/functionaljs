"use strict";

import * as Pair from '../lib/pair';

describe('Pair', () => {
  const data = Pair.cons(1, 2);

  it("match", () => {
    data.match({
      cons: (left: number, _right: number) => {
        expect(left).toEqual(1);
      }
    });
  });

  it("left", () => {
    expect(Pair.left(data)).toEqual(1);
  });

  it("right", () => {
    expect(Pair.right(data)).toEqual(2);
  });
});
