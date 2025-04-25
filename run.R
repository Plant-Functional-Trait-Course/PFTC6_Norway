library(targets)
source("other_code/load_libraries.R")

targets::tar_make()
tar_load_everything()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) tst # nolint

