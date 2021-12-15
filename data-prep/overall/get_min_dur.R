library(phonfieldwork)
library(tidyverse)
names <- list.files(path = "../secure/aclew/raw_eafs", pattern = regex("[0-9][0-9][0-9][0-9].eaf$"))
files <- lapply(paste0("../secure/aclew/raw_eafs/", names), eaf_to_df)

min_dur <- do.call(rbind, files) %>%
  mutate(dur = round((time_end - time_start) * 1000)) %>%
  filter(tier_type == "transcription") %>%
  arrange(dur) %>%
  filter(!str_detect(content, pattern = "0.|kisses|laughs")) %>%
  slice(1) %>%
  pull(dur)