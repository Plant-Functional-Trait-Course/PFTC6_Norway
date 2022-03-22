#generate_valid filenames
library("tidyr", quietly = TRUE)
library("dplyr", quietly = TRUE)
library("R.utils", quietly = TRUE)

all_codes <- crossing(A = LETTERS, B = LETTERS, C = LETTERS) %>% 
  mutate(code = paste0(A, B, C), 
         hash = (1L:n()) %% 10000L,
         hash = withSeed(sample(hash), seed = 32),
         hash = formatC(hash, width = 4, format = "d", flag = "0"),
         hashcode = paste0(code, hash))

save(all_codes, file = "traits/Rdatagathering/envelope_codes.Rdata")
