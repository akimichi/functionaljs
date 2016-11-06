"use strict";

var expect = require('expect.js');
var fs = require('fs');
var List = require('./list.js');
var Maybe = require('./monad.js').Maybe;
var ID = require('./monad.js').ID;

module.exports = {
  // ## MaybeT
  // Maybeモナド変換子
  // ~~~haskell
  // newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } 
  // instance Monad m => Monad (MaybeT m) where
  //   return = MaybeT . return . Just
  //   x >>= f = MaybeT $ do maybe_value <- runMaybeT x
  //                         case maybe_value of
  //                             Nothing -> return Nothing
  //                             Just value -> runMaybeT $ f value
  //
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
      return (underlyingM) => {
        return underlyingM.unit(Maybe.nothing());
      };
    },
    unit : (x) => {
      return (underlyingM) => {
        return underlyingM.unit(Maybe.unit(x));
      };
    },
    // ~~~haskell
    // (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    // ~~~
    // flatMap: (maybeT) => { // MaybeT m a
    //   var self = this;
    //   return (f) => { // f:: a -> MaybeT m b
    //     return (maybeT).flatMap(maybeT)((maybe) => {
    //       return maybe.match({
    //         nothing: (_) => {
    //           return self.unit(Maybe.nothing());
    //         },
    //         just: (v) => {
    //           return self.unit(f(v));
    //         }
    //       });
    //     });
    //   };
    // }
  }
};
