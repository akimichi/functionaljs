"use strict";

var expect = require('expect.js');
var fs = require('fs');
var List = require('./list.js');

module.exports = {
  ID: {
    self: () => {
      var self = this;
      return self;
    },
    unit: (value) => {
      return value;
    },
    flatMap: (instance) => {
      return (transform) => {
        expect(transform).to.a('function');
        return transform(instance);
      };
    }
  },
  // ### Maybeモナドの定義
  // ~~~haskell
  // instance Monad Maybe where
  //   Nothing  >>= _ = Nothing
  //   (Just x) >>= f = f x
  // ~~~
  Maybe: {
    match: (data, pattern) => {
      return data(pattern);
    },
    just : (value) => {
      return (pattern) => {
        return pattern.just(value);
      };
    },
    nothing : (_) => {
      return (pattern) => {
        return pattern.nothing(_);
      };
    },
    // **Maybe#unit**
    unit : (value) => {
      var self = this;
      return self.just(value);
    },
    // **Maybe#flatMap**
    flatMap : (maybeInstance) => {
      var self = this;
      return (transform) => {
        expect(transform).to.a('function');
        return self.match(maybeInstance,{
          just: (value) => {
            return transform(value);
          },
          nothing: (_) => {
            return self.nothing(_);
          }
        });
      };
    },
    // instance MonadPlus Maybe where
    //          mzero                   = Nothing
    //          Nothing `mplus` Nothing = Nothing 
    //          Just x  `mplus` Nothing = Just x  
    //          Nothing `mplus` Just x  = Just x 
    //          Just x  `mplus` Just y  = Just x 
    zero: (_) => {
      var self = this;
      return self.nothing();
    },
    plus: (x) => {
      var self = this;
      return (y) => {
        return self.match(x,{
          nothing: (_) => {
            return self.match(y,{
              nothing: (_) => {
                return self.nothing();
              },
              just: (value) => {
                return y; 
              }
            });
          },
          just: (value) => {
            return x; 
          }
        });
      };
    },
    // **Maybe#map**
    // ~~~haskell
    // instance Functor Maybe where
    //    fmap _ Nothing = Nothing
    //    fmap f (Just x) = Just (f x)
    // ~~~
    map : (maybeInstance) => {
      var self = this;
      return (transform) => {
        expect(transform).to.a('function');
        return self.match(maybeInstance,{
          nothing: (_) => {
            return self.nothing(_);
          },
          just: (value) => {
            return self.just(transform(value));
          }
        });
      };
    },
    // -- | Promote a function to a monad.
    // liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
    // liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
    // liftM f m1              = do { x1 <- m1; return (f x1) }
    liftM: (f) => {
      var self = this;
      return (ma) => {
        return self.flatMap(ma)((x) => {
          return self.unit(f(x));
        });
      };
    },
    // (<*>) :: (Monad m) => m (a -> b) -> m a -> m b
    apply: (mf) => {
      var self = this;
      return (ma) => {
        return self.flatMap(mf)((f) => {
          return self.flatMap(ma)((a) => {
            return self.unit(f(a));
          });
        });
      }; 
    },
    get: (maybe) => {
      var self = this;
      return self.getOrElse(maybe)(null);
    },
    getOrElse: (instance) => {
      var self = this;
      return (alternate) => {
        return self.match(instance,{
          just: (value) => {
            return value;
          },
          nothing: (_) => {
            return alternate;
          }
        });
      };
    },
    isEqual : (maybeA) => {
      var self = this;
      return (maybeB) => {
        return self.match(maybeA,{
          just: (valueA) => {
            return self.match(maybeB,{
              just: (valueB) => {
                return (valueA === valueB);
              },
              nothing: (_) => {
                return false;
              }
            });
          },
          nothing: (_) => {
            return self.match(maybeB,{
              just: (_) => {
                return false;
              },
              nothing: (_) => {
                return true;
              }
            });
          }
        });
      };
    }
  },
  Either: {
    match: (data, pattern) => {
      return data.call(data,pattern);
    },
    left : (value) => {
      return (pattern) => {
        return pattern.left(value);
      };
    },
    right : (value) => {
      return (pattern) => {
        return pattern.right(value);
      };
    },
    // ~~~haskell
    // instance Monad (Either a b) where
    //   return x = Right x
    //   Right x >>= f = f x
    //   Left x >>= Left x
    // ~~~
    // **Either#unit**
    unit : (value) => {
      var self = this;
      return self.right(value);
    },
    // **Either#flatMap**
    flatMap : (instanceM) => {
      var self = this;
      return (transform) => {
        expect(transform).to.a('function');
        return self.match(instanceM,{
          right: (value) => {
            return transform(value);
          },
          left: (value) => {
            return self.left(value);
          }
        });
      };
    },
  // **Either#map**
  // ~~~haskell
  // instance Functor (Either a) where
  //   fmap f (Right x) = Right (f x)
  //   fmap f (Left x) = Left x
  // ~~~
  map: (instanceM) => {
    var self = this;
    return (transform) => {
      return self.match(instanceM,{
        right: (value) => {
          return self.right(transform(value));
        },
        left: (value) => {
          return self.left(value);
        }
      });
    };
  }
  },
  IO: {
    /* unit:: T => IO[T] */
    unit : (any) => {
      return (_) =>  { 
        return any;
      };
    },
    /* flatMap:: IO[T] => FUN[T => IO[U]] => IO[U] */
    flatMap : (instanceA) => {
      var self = this;
      return (actionAB) => { // actionAB:: a -> IO[b]
        return self.unit(self.run(actionAB(self.run(instanceA))));
      };
    },
    // **IO#done**関数
    // 
    // > IOアクションを何も実行しない
    /* done:: T => IO[T] */
    done : (any) => {
      var self = this;
      return self.unit();
    },
    // **IO#run**関数
    //
    // > IOアクションを実行する
    /* run:: IO[A] => A */
    run : (instanceM) => {
      return instanceM();
    },
    // **IO#readFile**
    /* readFile:: STRING => IO[STRING] */
    readFile : (path) => {
      var self = this;
      return (_) => {
        var content = fs.readFileSync(path, 'utf8');
        return self.unit(content)(_);
      };
    },
    // **IO#println**
    /* println:: STRING => IO[null] */
    println : (message) => {
      var self = this;
      return (_) => {
        console.log(message);
        return self.unit(null)(_);
      };
    },
    // **IO#writeFile**
    writeFile : (path) => {
      var self = this;
      return (content) => {
        return (_) => {
          fs.writeFileSync(path,content);
          return self.unit(null)(_);
        };
      };
    },
    // **IO#seq**
    /* IO.seq:: IO[a] => IO[b] => IO[b] */
    seq: (instanceA) => {
      var self = this;
      return (instanceB) => {
        return self.flatMap(instanceA)((a) => {
          return instanceB;
        });
      };
    },
    // sequence_        :: [IO ()] -> IO ()
    // sequence_ []     =  return ()
    // sequence_ (a:as) =  do a
    //                        sequence as
    // seqs :: LIST[IO[()]] => IO[()]
    seqs: (alist) => {
      var self = this;
      // return alist.match({
      //   empty: () => {
      //     return self.done();
      //   },
      //   cons: (head, tail) => {
      //     self.run(head);
      //     return self.seqs(tail); 
      //   }
      // });
      return alist.match({
        empty: () => {
          return self.unit();
        },
        cons: (head, tail) => {
          return self.flatMap(head)((_) => {
            return self.seqs(tail);
          });
        }
      });
    },
    // **IO#putc**
    /* IO.putc:: CHAR => IO[] */
    putc: (character) => {
      return (io) => {
        process.stdout.write(character);
        return null;
      };
    },
    // **IO#puts**
    // ~~~haskell
    // putStr                  :: String -> IO ()
    // putStr s                =  sequence_ (map putChar s)
    // puts list = seqs (map putc list)
    // ~~~
    /* IO.puts:: LIST[CHAR] => IO[()] */
    puts: (alist) => {
      var self = this;
      return self.seqs(List.map(alist)(self.putc));
      // return alist.match({
      //   empty: () => {
      //     return self.done();
      //   },
      //   cons: (head, tail) => {
      //     return self.seq(self.putc(head))(self.puts(tail));
      //   }
      // });
    },
    // **IO#getc**
    /* IO.getc :: IO[CHAR] */
    getc: () => {
      var continuation = () => {
        var chunk = process.stdin.read();
        return chunk;
      }; 
      process.stdin.setEncoding('utf8');
      return process.stdin.on('readable', continuation);
    },
    // ~~~haskell
    // getLine     :: IO String
    // getLine     =  do c <- getChar
    //                   if c == '\n'
    //                        then return ""
    //                        else do l <- getLine
    //                                return (c:l)
    // ~~~
    getLine: () => {
      var self = this;
      return self.flatMap(self.getc)((c) => {
        if(c === "\n") {
          return List.unit("");
        } else {
          return self.flatMap(self.getLine)((l) => {
            return self.unit(List.cons(c,l));
          });
        }
      });
    }
  },
  // ~~~haskell
  // newtype Cont r a = Cont { runCont :: ((a -> r) -> r) } -- r は計算全体の最終の型
  // instance Monad (Cont r) where 
  //     return a       = Cont $ \k -> k a                       
  //     -- i.e. return a = \k -> k a 
  //     (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k) 
  //     -- i.e. m >>= f = \k -> m (\a -> f a k) 
  // ~~~
  Cont: {
    unit: (a) => {
      return (k) => {
        return k(a);
      };
    },
    flatMap: (m) => {
      var self = this;
      return (f) => { // f:: a -> Cont r a
        expect(f).to.a('function');
        return (k) => {
          return m((a) => {
            return f(a)(k);
          });
        };
      };
    },
    // ~~~haskell
    // class Monad m => MonadCont m where
    //   callCC :: ((a -> m a) -> m a) -> m a
    // instance MonadCont (Cont r) where
    //   callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
    //   -- i.e.  callCC f = \k -> ((f (\a -> \_ -> k a)) k)
    // ~~~
    callCC: (f) => {
      return (k) => { 
        return f((a) => {
          return (_) => {
            return k(a);
          }; 
        })(k);
      };
    }
  }
};
