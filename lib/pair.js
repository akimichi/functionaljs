"use strict";

module.exports = {
  type : (pattern) => {
    return pattern.pair();    
  },
  // match : (data, pattern) => {
  //   return data( pattern);
  // },
  empty: (_) => {
    return {
      pair: true,
      empty: true,
      type: (pattern) => {
        return pattern.pair();
      },
      match: (pattern) => {
        return pattern.empty();
      }
    };
  },
  // pair の代数的データ構造
  cons: (left, right) => {
    return {
      pair: true,
      type: (pattern) => {
        return pattern.pair();
      }, 
      match: (pattern) => {
        return pattern.cons(left, right);
      }
    };
  },
  // ペアの右側を取得する
  right: (data) => {
    return data.match({
      cons: (left, right) => {
        return right;
      }
    });
  },
  // ペアの左側を取得する
  left: (data) => {
    return data.match({
      cons: (left, right) => {
        return left;
      }
    });
  },
  // isEqual: (pairA) => {
  //   return (pairB) => {
  //   };
  // }
};
