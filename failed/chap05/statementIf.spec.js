"use strict";

var expect = require('expect.js');
var util = require('util');

it('ifは文である', (next) => {
  /* ##@range_begin(if_statement) */
  var result = if(true) {
    true;
  } else {
    false;
  }
  /* ##@range_end(if_statement) */
  next();
});


