"use strict";

import { Maybe, ID } from '../lib/monad';
import { MaybeT } from '../lib/monad_transformer';

describe("MaybeTモナドをテストする", () => {
  it("MaybeT#unitをテストする", () => {
    Maybe.match(MaybeT.unit(1)(ID as any) as any, {
      nothing: () => {
        fail('Expected just, got nothing');
      },
      just: (v) => {
        expect(v).toEqual(1);
      }
    });
  });

  it("MaybeT#flatMapをテストする", () => {
    const instanceM = MaybeT.unit(1)(ID as any);
    const double = (x: number): any => 2 * x;

    expect(MaybeT.flatMap(instanceM)(double as any)(ID as any)).toEqual(2);
  });
});
