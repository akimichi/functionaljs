"use strict";

var expect = require('expect.js');
var util = require('util');

it('ifは文である', (next) => {
  /* ##@range_begin(if_isnot_first_class_object) */
  var result = if(true) {
    true;
  } else {
    false;
  }
  /* ##@range_end(if_isnot_first_class_object) */
  next();
});


