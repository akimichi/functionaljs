"use strict";

var expect = require('expect.js');
var IO = require('../lib/monad.js').IO;
var List = require('../lib/list.js');
var Pair = require('../lib/pair.js');
var String = require('../lib/string.js');
var PP = require('../lib/pprinter.js');

// ライフ・ゲーム
// ============
// IOモナドを使って、画面出力という副作用とゲームの内部で実行される純粋な処理を分離する。

var height = 10;
var width = 10;

var glider = List.fromArray([Pair.cons(4,2),
                             Pair.cons(2,3),
                             Pair.cons(4,3),
                             Pair.cons(3,4),
                             Pair.cons(4,4)]); 

// ## 純粋な関数群

var isAlive = (board) => {
  return (position) => {
    var isEqual = (x) => {
      return (y) => {
        return (Pair.left(x) === Pair.left(y)) && (Pair.right(x) === Pair.right(y));
      };
    };
    return List.any(board)(isEqual(position));
  };
};

expect(
  isAlive(glider)(Pair.cons(1,1))
).to.eql(
  false
);
expect(
  isAlive(glider)(Pair.cons(4,2))
).to.eql(
  true
);

var isVacant = (board) => {
  return (position) => {
    return ! isAlive(board)(position); 
  };
};

var wrap = (position) => {
  return position.match({
    empty: (_) => {
      return Pair.empty();
    },
    cons: (x,y) => {
      return Pair.cons(((x-1) % width) + 1,
                       ((y-1) % height) + 1);
    }
  });
};

expect(
  PP.print(wrap(Pair.cons(1,1)))
).to.eql(
  "(1,1)"
);
expect(
  PP.print(wrap(Pair.cons(10,10)))
).to.eql(
  "(10,10)"
);

var neighbors = (position) => {
  var x =  Pair.left(position);
  var y =  Pair.right(position);
  return List.map(List.fromArray([Pair.cons(x-1,y-1),
                                  Pair.cons(x,y-1),
                                  Pair.cons(x+1,y-1),
                                  Pair.cons(x-1,y),
                                  Pair.cons(x+1,y),
                                  Pair.cons(x-1,y+1),
                                  Pair.cons(x,y+1),
                                  Pair.cons(x+1,y+1)]))(wrap);
};

expect(
  PP.print(neighbors(Pair.cons(1,1)))
).to.eql(
  "[(0,0),(1,0),(2,0),(0,1),(2,1),(0,2),(1,2),(2,2),nil]"
);
expect(
  PP.print(neighbors(Pair.cons(10,10)))
).to.eql(
  "[(9,9),(10,9),(1,9),(9,10),(1,10),(9,1),(10,1),(1,1),nil]"
);
expect(
  PP.print(neighbors(Pair.cons(10,8)))
).to.eql(
  "[(9,7),(10,7),(1,7),(9,8),(1,8),(9,9),(10,9),(1,9),nil]"
);

// expect(
//   PP.print(neighbors(Pair.cons(1,8)))
// ).to.eql(
//   "[(9,7),(10,7),(1,7),(9,8),(1,8),(9,9),(10,9),(1,9),nil]"
// );

var liveNeighbors = (board) => {
  return (position) => {
    return List.length(
      List.filter(neighbors(position))((neighbor) => {
        return isAlive(board)(neighbor);
      })
    );
  };
};

expect(
  liveNeighbors(List.fromArray([Pair.cons(10,9),Pair.cons(10,8),Pair.cons(10,7)]))(Pair.cons(9,8))
).to.eql(
  3
);
// expect(
//   liveNeighbors(List.fromArray([Pair.cons(10,9),Pair.cons(10,8),Pair.cons(10,7)]))(Pair.cons(1,8))
// ).to.eql(
//   -1
// );

// survivors :: Board -> [Pos]
// survivors board = [pos | pos <- board, elem (liveNeighbors board pos) [2,3]]
var survivors = (board) => {
  /* positionに位置する細胞の周辺に2もしくは3個の他の細胞が生存している */
  return List.filter(board)((position) => {
    return List.elem(List.cons(2,List.cons(3,List.empty())))(liveNeighbors(board)(position));
  });
  // return List.flatMap(board)((position) => {
  //   if(List.elem(List.cons(2,List.cons(3,List.empty())))(liveNeighbors(board)(position))) {
  //     return List.unit(position);
  //   } else {
  //     return List.empty();
  //   };
  // });
};

expect(
  PP.print(survivors(glider))
).to.eql(
  "[(4,3),(3,4),(4,4),nil]"
);
expect(
  PP.print(survivors(List.fromArray([Pair.cons(10,9),Pair.cons(10,8),Pair.cons(10,7)])))
).to.eql(
  "[(10,8),nil]"
);
expect(
  PP.print(survivors(List.fromArray([Pair.cons(9,9),Pair.cons(10,9),Pair.cons(1,9)])))
).to.eql(
  "[(10,9),nil]"
);

// births :: Board -> [Pos]
// births board = [(x,y) | x <- [1..width],
//                         y <- [1..height],
//                         isVacant board (x,y)
//                         liveNeighbors board (x,y) === 3]
var births = (board) => {
  var listFromNtoM = (n,m) => {
    if(n === m) {
      return List.cons(m,List.empty());
    } else {
      return List.cons(n, listFromNtoM(n+1,m));
    }
  };
  var wholeBoard =  List.flatMap(listFromNtoM(1,width))((x) => {
    return List.flatMap(listFromNtoM(1,height))((y) => {
      var position = Pair.cons(x,y);
      return List.unit(position);
    });
  });
  return List.flatMap(listFromNtoM(1,width))((x) => {
    return List.flatMap(listFromNtoM(1,height))((y) => {
      var cell = Pair.cons(x,y);
      if((isVacant(board)(cell) === true) && (liveNeighbors(board)(cell) === 3)) {
        return List.unit(cell);
      } else {
        return List.empty();
      }
    });
  });
  // return List.filter(wholeBoard)((position) => {
  //   // if((isVacant(board)(position) === true) && (liveNeighbors(board)(position) === 3)){
  //   if((isAlive(board)(position) === false) && (liveNeighbors(board)(position) === 3)){
  //     return true; 
  //   } else {
  //     return false;
  //   }
  // });
  // return List.flatMap(listFromNtoM(1,width))((x) => {
  //   return List.flatMap(listFromNtoM(1,height))((y) => {
  //     var cell = Pair.cons(x,y);
  //     if((isVacant(board)(cell) === true) && (liveNeighbors(board)(cell) === 3)){
  //       return List.unit(cell);
  //     } else {
  //       return List.empty();
  //     }
  //   });
  // });
};

expect(
  PP.print(births(glider))
).to.eql(
  "[(3,2),(5,3),nil]"
);

expect(
  PP.print(births(List.fromArray([Pair.cons(9,9),Pair.cons(10,9),Pair.cons(1,9)])))
).to.eql(
  "[(10,8),(10,10),nil]"
);
// expect(
//   PP.print(births(List.fromArray([Pair.cons(10,10),Pair.cons(10,9),Pair.cons(10,8)])))
// ).to.eql(
//   "[(3,2),(5,3),nil]"
// );

var nextGen = (board) => {
  return List.append(survivors(board))(births(board));
};

// expect(
//   PP.print(nextGen(glider))
// ).to.eql(
//   "[(4,3),(3,4),(4,4),(3,2),(5,3),nil]"
// );
// expect(
//   PP.print(nextGen(List.fromArray([Pair.cons(10,10),
//                              Pair.cons(9,10),
//                              Pair.cons(10,9),
//                              Pair.cons(9,9)])))
// ).to.eql(
//   "[(4,3),(3,4),(4,4),(3,2),(5,3),nil]"
// );

// ## IOアクション

// ### writeAt
var writeAt = (position) => {
  return (xs) => {
    return IO.flatMap(goto(position))((_) => {
      return IO.puts(xs);
    });
  };
};
// 
// ### goto 
var goto = (position) => {
  var x =  Pair.left(position);
  var y =  Pair.right(position);
  return IO.puts(String.toList('\u001B[' + y + ";" + x + 'H'));
};

// ### showCells
var showCells = (board) => {
  return () => {
    return IO.seqs(List.flatMap(board)((position) => {
      return List.unit(writeAt(position)(List.unit("O")));
    }));
  };
};

// ### 画面を消去するアクション
// > \u001B[2J は画面を消去するエスケープコード
var cls = () => {
  return IO.puts(String.toList('\u001B[2J'));
};

// life :: Board -> IO()
// life board = do cls
//                 showcells board
//                 wait 50000000
//                 life (nextgen board)
var life = (board) => {
  // return IO.flatMap(cls)((_) => {
  return IO.flatMap(IO.run(cls))((_) => {
    // showCells(board);
    // return life(nextGen(board));
    return IO.flatMap(showCells(board))((action) => {
      IO.run(action);
      return life(nextGen(board));
    });
  });
  // return IO.seqs(List.cons(cls(),
  //                          List.cons(life(nextGen(board)),
  //                                    List.empty())));
};


// main :: IO ()
// main = life glider
var main = life(glider); 

