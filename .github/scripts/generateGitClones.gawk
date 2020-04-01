/#/ {
  # Strip comments
  gsub(/#.*$/,"")
}
/git-clone/ {
  # Do a shallow fetch of all branches, checkout the desired ref/sha, and build and publish the jars.
  printf "git clone --no-single-branch --no-checkout --depth 5 %s %s && (cd %s && git checkout %s && sbt +publishLocal)\n",$3,$2,$2,$4
}
