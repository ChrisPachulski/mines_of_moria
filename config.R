if (Sys.info()[["sysname"]] == "Darwin") {
  # macOS
  path_prefix <- file.path(Sys.getenv("HOME"), "Documents")
} else {
  # Linux, Windows, etc.
  path_prefix <- Sys.getenv("HOME")
}
