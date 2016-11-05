"use strict";

var expect = require('expect.js');
var fs = require('fs');
var List = require('./list.js');
var Maybe = require('./monad.js').Maybe;

module.exports = {
  // ## MaybeT
  // Maybeモナド変換子
  // ~~~haskell
  // newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}
  // instance Monad m => Monad (MaybeT m) where
  //   return x = MaybeT $ return (Just x)
  //   m >>= k  = MaybeT $ do a <- runMaybeT m
  //                          case a of
  //                            Nothing -> return Nothing
  //                            Just v  -> runMaybeT (k v)
  //   fail _   = MaybeT $ return Nothing
  //
  // instance MonadTrans MaybeT where
  //   lift m = MaybeT $ m >>= (\x -> return (Just x))
  // ~~~
  MaybeT: {
    // run : (m) => {
    //   var self = this;
    //   return (a) => {
    //     return m(Maybe.unit(a));
    //   };
    // },
    fail : (_) => {
      var self = this;
      return {
        run: (m) => {
          return m.unit(Maybe.nothing());
        }
      };
    },
    unit : (x) => {
      return {
        run: (m) => { // runMaybeT :: m (Maybe a) 
          return m.unit(Maybe.unit(x));
        }
      };
    },
    flatMap: (maybeT) => {
      var self = this;
      return (f) => {
        return { 
          run: (m) => {
            return self.flatMap(maybeT.run(m))((maybe) => {
              return maybe.match({
                nothing: (_) => {
                },
                just: (v) => {
                  return f(v).run
                }
              });

        });
      };
    },
  }
};
