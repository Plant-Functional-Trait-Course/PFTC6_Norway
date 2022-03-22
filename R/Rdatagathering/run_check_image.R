main <- function(){
  x <- readLines("stdin")
  source("check_image.R")
  cat("\n\n\n")
  x <- strsplit(x, split = " ")[[1]][3]

  check_image(x)

  cat("done")
}
main()