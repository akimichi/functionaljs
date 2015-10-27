"use strict";

var expect = require('expect.js');
var util = require('util');

it('条件式の実装(不完全)', (next) => {
  setTimeout(next, 300);
  var functionalIf = (predicate, thenClause, elseClause) => {
	if(predicate){
      return thenClause;
	} else {
      return elseClause;
	}
  };
  var infiniteLoop = (_) => {
    return infiniteLoop(_);
  };
  expect(
    functionalIf((2 < 3), 2, infiniteLoop())
  ).to.eql(
    2
  );
});


