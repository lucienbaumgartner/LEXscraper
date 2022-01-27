library(tidyverse)
library(data.table)
library(pbmcapply)
library(jsonlite)

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

files2collect <- unique(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/links2profiles", full.names = T))
ought <- lapply(files2collect, readLines)
collection <- tibble(profiles = unlist(ought), file = rep(str_extract(files2collect, "(?<=\\/)[a-z]+\\.txt"), lengths(ought)))
profilesCollected <- unique(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/profiles", full.names = T))
profiles <- pbmclapply(profilesCollected, read.table, mc.cores = 4)
all(sapply(profiles, ncol)==11)
profiles <- do.call(rbind, c(profiles, fill = T)) %>% 
  setNames(., c("SRA_ID", "name", "tel", "mail", "roles", "address", "gmap", "status", "aop", "lang", "url")) %>% 
  mutate(profiles = str_extract(url, "\\/person.*"))
df <- left_join(collection, profiles)
df <- df %>% filter(file %in% unique(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/profiles")) & !profiles == "")
df <- unique(df)
#df %>% group_by(file) %>% summarise(N_NA = sum(is.na(SRA_ID)))
#df %>% filter(is.na(SRA_ID))
#head(df)
problms <- df %>% filter(is.na(url))
logged <- readLines("/Volumes/extICY/LEXscraping/law_society_GB/output/logs/server_errors.txt") %>% gsub(".*\\.org\\.uk", "", .)
all(problms$profiles %in% logged)

full <- df %>% filter(!duplicated(profiles) & !is.na(url))
full %>% filter(!is.na(tel) | !is.na(mail)) %>% nrow
full %>% filter(!is.na(tel) | !is.na(mail)) %>% nrow/nrow(full)
full %>% filter(!is.na(mail)) %>% nrow/nrow(full)

write.csv(full, file = "/Volumes/extICY/LEXscraping/law_society_GB/output/collection/law_society_GB_collection.csv", quote = T, row.names = F)
write_json(full, "/Volumes/extICY/LEXscraping/law_society_GB/output/collection/law_society_GB_collection.json")
law_society_GB <- full
save(law_society_GB, file = "/Volumes/extICY/LEXscraping/law_society_GB/output/collection/law_society_GB_collection.RDS")
