/#/ {
  # Strip comments
  gsub(/#.*$/,"")
}
/maven-version/ {
  # Print a series of "-DfooVersion=xxx" to override the default chisel versions in build.sbt
  printf "%s-D%sVersion=%s", sep, $2, $3; sep = " "
}
