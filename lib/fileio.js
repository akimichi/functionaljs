var fs = require('fs');

var read = (path) => {
  return fs.readFileSync(path, 'utf8');
};
var write = (path, content) => {
  return fs.writeFileSync(path, content);
};


