library(tidyverse)
library(data.table)

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

files2collect <- unique(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/links2profiles", full.names = T))
ought <- lapply(files2collect, readLines)
collection <- tibble(profiles = unlist(ought), file = rep(str_extract(files2collect, "(?<=\\/)[a-z]+\\.txt"), lengths(ought)))
profilesCollected <- unique(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/profiles", full.names = T))
profiles <- lapply(profilesCollected, read.table)
all(sapply(profiles, ncol)==11)
profiles <- do.call(rbind, c(profiles, fill = T)) %>% 
  setNames(., c("SRA_ID", "name", "tel", "mail", "roles", "address", "gmap", "status", "aop", "lang", "url")) %>% 
  mutate(profiles = str_extract(url, "\\/person.*"))
df <- left_join(collection, profiles)
df <- df %>% filter(file %in% unique(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/profiles")))
df %>% group_by(file) %>% summarise(N_NA = sum(is.na(SRA_ID)))
