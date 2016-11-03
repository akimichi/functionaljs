"use strict";

var expect = require('expect.js');
var fs = require('fs');
var List = require('./list.js');

module.exports = {
  ID: {
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
  }
};
