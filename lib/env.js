"use strict";

var expect = require('expect.js');
// var fs = require('fs');
// var List = require('./list.js');
// var Pair = require('../lib/pair.js');
// var String = require('../lib/string.js');
// var PP = require('../lib/pprinter.js');
// var IO = require('../lib/monad.js').IO;

module.exports = {
  // ## 環境モジュール
  // 空の環境
  empty: (variable) => {
    return undefined;
  },
  /* 変数名に対応する値を環境から取りだす */
  // lookupEnv:: (STRING, ENV) => M[VALUE]
  lookup: (identifier, environment) => {
    return environment(identifier);
  },
  /* 環境を拡張する */
  // extendEnv:: (STRING, VALUE, ENV) => ENV 
  extend: (identifier, value, environment) => {
    var self = this;
    expect(identifier).to.a('string');
    return (queryIdentifier) => {
      expect(queryIdentifier).to.a('string');
      if(identifier === queryIdentifier) {
        return value;
      } else {
        return self.lookup(queryIdentifier, environment);
      }
    };
  }
};
