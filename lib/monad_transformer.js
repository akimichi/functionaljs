"use strict";

var expect = require('expect.js');
var fs = require('fs');
var List = require('./list.js');
var ID = require('./monad.js').ID;
var Maybe = require('./monad.js').Maybe;
var Either = require('./monad.js').Either;

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
      return (Monad) => {
        return Monad.unit(Maybe.nothing());
      };
    },
    unit : (x) => {
      return (Monad) => {
        return Monad.unit(Maybe.unit(x));
        // return Maybe.unit(Monad.unit(x));
      };
    },
    // ~~~haskell
    // (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    // ~~~
    flatMap: (maybeT) => { // MaybeT m a
      var self = this;
      return (f) => { // f:: a -> MaybeT m b
        return (Monad) => {
          return Monad.flatMap(maybeT)((maybeInstance) => {
            return Maybe.match(maybeInstance,{
              nothing: (_) => {
                return self.unit(Maybe.nothing())(Monad);
              },
              just: (v) => {
                return f(v);
                // return self.unit(Maybe.unit(f(v)))(Monad);
              }
            });
          });
        };
      };
    },
    // instance MonadTrans MaybeT where
    //   lift m = MaybeT $ m >>= (\x -> return (Just x))
    lift: (m) => {
      var self = this;
      return m.self().flatMap(m)((x) => {
        return self.unit(Maybe.unit(x));
      });
    }
  },
  ErrorT: {
    unit: (x) => {
      return (Monad) => {
        return Monad.unit(Either.unit(x));
      };
    },
    // ~~~haskell
    // (>>=) :: ErrorT m a -> (a -> ErrorT m b) -> ErrorT m b
    // ~~~
    flatMap: (errorT) => {
      var self = this;
      return (f) => { // f:: a -> MaybeT m b
        return (Monad) => {
          return Monad.flatMap(errorT)((eitherInstance) => {
            return Either.match(eitherInstance,{
              left: (l) => {
                return self.unit(Either.left(l))(Monad);
              },
              right: (r) => {
                return f(r);
              }
            });
          });
        };
      };
    },
    throwError: (x) => {
      var self = this;
      return self.unit(Either.left(x));
    },
    catchError: (m) => {
      var self = this;
      return (f) => {
        return self.flatMap(m)((eitherInstance) => {
          return Either.match(eitherInstance,{
            left: (l) => {
              return f(l); 
            },
            right: (r) => {
              return self.unit(Either.right(r));
            }
          });
        });
      };
    }
  }
};
