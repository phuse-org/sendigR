
sass::sass(
  sass::sass_file("www/theme_sass.scss"),
  options = sass::sass_options(output_style = "compressed"),
  output = "www/from_sass_theme.css"
)
