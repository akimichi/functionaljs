/*

expect = require('expect.js')
util = require('util')

describe '制御構造', -> 
  it 'even', (next) ->
    ##@range_begin(if_as_expression)
    even = (n) ->
      if (n % 2) is 0
        true
      else
        false
    ##@range_end(if_as_expression)
    expect(even(2)).to.be true
    expect(even(3)).to.be false
    next()
