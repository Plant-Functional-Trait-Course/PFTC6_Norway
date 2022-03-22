# CHECK IMAGE ID'S



# get all valid IDs
load("traits/Rdatagathering/envelope_codes.Rdata")
all_codes


filter(all_codes, agrepl("AXF5", hashcode, max.distance = .1))

list.files() %>% set_names() %>% map(~agrepl(paste0("hat", ., "$")))