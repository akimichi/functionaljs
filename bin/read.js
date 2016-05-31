/* #@range_begin(file_stream) */
var fs = require('fs');

// 変数fstreamにはファイルの内容がストリームとして格納される  
var fstream = fs.createReadStream('/var/log/syslog', {encoding: 'utf-8'});

// データが入ってきたときに実行される
fstream.on('data', (data) =>  {
  console.log(data);
});
// それ以上読みこむデータがないときに実行される
fstream.on('end', () => {
  console.log("EXIT");
});
/* #@range_end(file_stream) */

// var main = (stream) => {
//   stream.on('data', (data) =>  {
//     console.log(data);
//   });
//   stream.on('end', () => {
//     main(stream);
//   });
// };


// main(fstream);
