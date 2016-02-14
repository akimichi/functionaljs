"use strict";

process.stdin.resume();
process.stdin.setEncoding('utf8');

process.stdin.on('data', (chunk) => {
  var maybeInt = parseInt(chunk,10);
  var tarai = (x,y,z) => {
    if(x > y) {
      return tarai(tarai(x - 1, y, z), tarai(y - 1, z, x), tarai(z - 1, x, y));
    } else {
      return y;
    }
  };
  if(isNaN(maybeInt)){
    process.stdout.write('string: ' + chunk.toUpperCase());
    process.stdout.write('\n');
  } else {
    process.stdout.write('number: ' + tarai(maybeInt * 2, maybeInt, 0));
    process.stdout.write('\n');
  }

});

process.stdin.on('end',  () => {
 process.stdout.write('end');
});
