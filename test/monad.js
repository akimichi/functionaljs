"use strict";

// モナド
// =====

var fs = require('fs');

module.exports = {
  pair: {
    match : (data, pattern) => {
      return data.call(this.pair, pattern);
    },
    cons: (left, right) => {
      return (pattern) => {
        return pattern.cons(left, right);
      };
    },
    right: (tuple) => {
      return this.pair.match(tuple, {
        cons: (left, right) => {
          return right;
        }
      });
    },
    left: (tuple) => {
      return this.pair.match(tuple, {
        cons: (left, right) => {
          return left;
        }
      });
    }
  },
  // ## IDモナド
  ID: {
    /* unit:: T => ID[T] */
    unit: (value) => {  // 単なる identity関数と同じ
      return value;
    },
    /* flatMap:: ID[T] => FUN[T => ID[T]] => ID[T] */
    flatMap: (instanceM) => {
      return (transform) => {
        return transform(instanceM); // 単なる関数適用と同じ
      };
    }
  }
};
