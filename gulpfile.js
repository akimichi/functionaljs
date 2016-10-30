var gulp = require('gulp');
var gutil = require('gulp-util');
var mocha = require('gulp-mocha');
var coffee = require('gulp-coffee');
var foreach = require('gulp-foreach');
var tap = require('gulp-tap');
var docco = require("gulp-docco");
var ghPages = require('gulp-gh-pages');
var exec = require('child_process').exec;

gulp.task('js-test', function () {
    gulp.src('test/*.js')
        .pipe(mocha({
            reporter: 'spec',
            //reporter: 'nyan',
            clearRequireCache: true,
            ignoreLeaks: true
        }));
});

gulp.task('ruby-test', function (cb) {
  exec('rspec test/', function (err, stdout, stderr) {
    console.log(stdout);
    console.log(stderr);
    cb(err);
  });
});

gulp.task('scala-test', function (cb) {
  exec('sbt test', function (err, stdout, stderr) {
    console.log(stdout);
    console.log(stderr);
    cb(err);
  });
});

gulp.task('haskell-test', function (cb) {
  exec('stack test', function (err, stdout, stderr) {
    console.log(stdout);
    exec('stack test', function (err, stdout, stderr) {
      cb(err);
    });
  });
});
// Watch Files For Changes
gulp.task('watch', function() {
    gulp.watch(['test/*.js']);
});


gulp.task('doc', function() {
  var options = {
    layout:     'parallel',
    output:     'docs',
    template:   'docs/template/docco.jst',
    css:        'docs/css/docco.css',
    extension:  null,
    languages:  {},
    marked:     null
  };
  return gulp.src(["./test/*.js","./lib/*.js"])
    .pipe(docco(options))
    .pipe(gulp.dest('./docs'));
});

gulp.task('deploy', function() {
  return gulp.src('./docs/**/*')
    .pipe(ghPages());
});

gulp.task('default', ['test','doc', 'deploy']);
gulp.task('test', ['js-test','scala-test', 'haskell-test','ruby-test']);
