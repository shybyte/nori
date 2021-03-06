var gulp = require('gulp'),
    shell = require('gulp-shell'),
    browserSync = require('browser-sync'),
    livereload = require('gulp-livereload');

gulp.task('compile', shell.task([
  'elm-make Nori.elm'
]))

gulp.task('reload', function () {
  gulp.src('elm.js')
    .pipe(livereload());
});

gulp.task('watch', function () {
  livereload.listen();
  browserSync({
    open: false,
    port: 3020,
    ui: {
      port: 3021
    },
    server: {
      baseDir: ["."],
    }
  });
  gulp.watch('*.elm', ['compile', 'reload', browserSync.reload]);
  gulp.watch(['*.css', '*.html', 'js/*.js'], browserSync.reload);
});

gulp.task('default', ['compile','watch']);