# Check envelopes used
library("tidyverse")
library("readxl")

# get all valid IDs
load("traits/Rdatagathering/envelope_codes.Rdata")
all_codes

# read in Spreadsheet
traits <- read_excel(path = "traits/data/TraitSpreadsheet_Template.xlsx")


# envelopes given out
out <- all_codes[1:10, "hashcode"] %>% 
  mutate(envelopes_out = "out")

breaks <- crossing(LETTERS, LETTERS, "A") %>% apply(1, paste0, collapse = "")


traits %>% 
  right_join(out, by = c("ID" = "hashcode")) %>% 
  mutate(start = substring(ID, 1, 1)) %>% 
  mutate(ID = substring(ID, 1, 3)) %>% 
  mutate(has.trait = !is.na(Genus)) %>% 
  mutate(has.LA = !is.na(Leaf_Area_cm2)) %>% 
  mutate(has = has.trait + has.LA * 2) %>% 
  mutate(has = plyr::mapvalues(has, c(0, 1, 2, 3), c("Missing all", "Missing LA", "Missing trait", "Complete"))) %>% 
  ggplot(aes(x = ID, fill = has)) +
  geom_bar() +
  facet_wrap(~start, ncol = 1, scales = "free_x") +
  scale_x_discrete(breaks = breaks) +
  theme (axis.text.x = element_text(angle = 90))
  
