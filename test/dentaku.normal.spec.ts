"use strict";

import { Dentaku } from '../lib/dentaku.normal';

describe('dentaku.normal', () => {
  it("evaluate", () => {
    expect(Dentaku.evaluate("123")).toEqual(123);
    expect(Dentaku.evaluate("1 + 2")).toEqual(3);
    expect(Dentaku.evaluate("2 - 1")).toEqual(1);
    expect(Dentaku.evaluate("1.2 * 2")).toEqual(2.4);
    expect(Dentaku.evaluate("3.9 / 3")).toEqual(1.3);
  });
});
