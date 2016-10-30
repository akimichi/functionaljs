"use strict";

var expect = require('expect.js');
var Pair = require('../lib/pair.js');
var List = require('../lib/list.js');
var Data = require('../lib/data.js');


describe('Data', () => {
  var tuple = Pair.cons(1,2);
  it("Data#type", (next) => {
    Data.type(tuple,{
      pair: (_) => {
        expect(true).to.eql(true);
      }
    });
    next();
  });
  describe('Data#match', () => {
    it("Data#match(pair)", (next) => {
      Data.match(tuple,{
        cons: (x,y) => {
          expect(x).to.eql(1);
        }
      });
      next();
    });
    it("Data#match(list)", (next) => {
      var list = List.cons(1,List.empty());
      Data.match(list,{
        cons: (x,y) => {
          expect(x).to.eql(1);
        }
      });
      next();
    });
  });
});
