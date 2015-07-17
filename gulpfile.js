var gulp = require('gulp'),
    shell = require('gulp-shell'),
    livereload = require('gulp-livereload');

gulp.task('compile', shell.task([
  'elm-make Main.elm'
]))

gulp.task('reload', function () {
  gulp.src('elm.js')
    .pipe(livereload());
});

gulp.task('default', function () {
  livereload.listen();
  gulp.watch('*.elm', ['compile', 'reload']);
});