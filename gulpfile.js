var gulp = require('gulp');
var gutil = require('gulp-util');
var mocha = require('gulp-mocha');
var coffee = require('gulp-coffee');
var foreach = require('gulp-foreach');
var tap = require('gulp-tap');
var docco = require("gulp-docco");
var ghPages = require('gulp-gh-pages');
 

gulp.task('test', function () {
    gulp.src('test/*.js')
        .pipe(mocha({
            reporter: 'spec',
            //reporter: 'nyan',
            clearRequireCache: true,
            ignoreLeaks: true
        }));
});

// Watch Files For Changes
gulp.task('watch', function() {
    gulp.watch(['test/*.js']);
});

// gulp.task('test', function() {
//   run('mocha --harmony -R spec').exec();
// });

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
