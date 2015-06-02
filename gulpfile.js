var gulp = require('gulp');
var gutil = require('gulp-util');
var mocha = require('gulp-mocha');
var coffee = require('gulp-coffee');
var run = require('gulp-run');
var foreach = require('gulp-foreach');
var tap = require('gulp-tap');


gulp.task('test', function() {
  run('mocha --harmony -R spec').exec();
});

gulp.task('default', ['test']);
