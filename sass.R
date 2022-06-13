
sass::sass(
  sass::sass_file("inst/www/theme_sass.scss"),
  options = sass::sass_options(output_style = "compressed"),
  output = "inst/www/from_sass_theme.css"
)
