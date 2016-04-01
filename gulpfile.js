var gulp = require('gulp');
var gutil = require('gulp-util');
var mocha = require('gulp-mocha');
var coffee = require('gulp-coffee');
var foreach = require('gulp-foreach');
var tap = require('gulp-tap');
var docco = require("gulp-docco");
var ghPages = require('gulp-gh-pages');
var exec = require('child_process').exec;
// var runSequence = require('run-sequence');

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

// gulp.task('cabal-configure', function (cb) {
//   runSequence('stack build',
//               'cabal configure',
//               cb);
// });

gulp.task('haskell-test', function (cb) {
  exec('stack test', function (err, stdout, stderr) {
    console.log(stdout);
    // console.log(stderr);
    cb(err);
  });
});
// Watch Files For Changes
gulp.task('watch', function() {
    gulp.watch(['test/*.js']);
});


gulp.task('doc', function() {
  return gulp.src("./test/*.js")
    .pipe(docco())
    .pipe(gulp.dest('./docs'));
});

gulp.task('deploy', function() {
  return gulp.src('./docs/**/*')
    .pipe(ghPages());
});

gulp.task('default', ['test','doc', 'deploy']);
gulp.task('test', ['js-test','ruby-test','scala-test', 'haskell-test']);
