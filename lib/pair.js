"use strict";

module.exports = {
/* #@range_begin(pair_datatype) */
  // pair の代数的データ構造
  cons: (left, right) => {
    return (pattern) => {
      return pattern.cons(left, right);
    };
  },
  match : (data, pattern) => {
    return data( pattern);
  },
  // ペアの右側を取得する
  right: (tuple) => {
    return this.match(tuple, {
      cons: (left, right) => {
        return right;
      }
    });
  },
  // ペアの左側を取得する
  left: (tuple) => {
    return this.match(tuple, {
      cons: (left, right) => {
        return left;
      }
    });
  }
/* #@range_end(pair_datatype) */
};
