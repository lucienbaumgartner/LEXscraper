library(rvest)
library(tidyverse)
library(XML)
library(xml2)
library(data.table)

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

writeLines(capture.output(sessionInfo()), "/Volumes/extICY/LEXscraping/law_society_GB/res/profile_scraper__session_info.txt")
get_node <- function(data, node = NULL, xpath = NULL, logFile = NULL, currentURL = NULL){
  if(!is.null(node)){
    tryCatch({html_nodes(data, css = node)}, error = function(e) {
      if(!is.null(logFile)) write(currentURL, file = logFile, append = T)
      return(read_xml("<p>NA</p>"))
    })
  }else if(!is.null(xpath)){
    tryCatch({html_nodes(data, xpath = xpath)}, error = function(e) {
      if(!is.null(logFile)) write(currentURL, file = logFile, append = T)
      return(read_xml("<p>NA</p>"))
    })
  }
}

files2collect <- unique(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/links2profiles", full.names = T))

last_file <- readLines("/Volumes/extICY/LEXscraping/law_society_GB/output/logs/profiles_last_file.txt")
last_entry <- readLines("/Volumes/extICY/LEXscraping/law_society_GB/output/logs/profiles_last_entry.txt")
if(identical(last_file, character(0))) last_file <- files2collect[1]
if(identical(last_entry, character(0)) | !last_entry %in% readLines(last_file)) last_entry <- readLines(last_file)[1]

for(m in files2collect[grep(last_file, files2collect):length(files2collect)]){
  print(m)
  writeLines(m, "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/profiles_last_file.txt")
  urlList <- paste0("https://solicitors.lawsociety.org.uk", readLines(m))
  if(!last_entry %in% readLines(m)) last_entry <- readLines(m)[1]
  urlList <- urlList[grep(last_entry, urlList):length(urlList)]
  for(i in urlList){
    writeLines(i, "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/profiles_last_entry.txt")
    fileName <- str_extract(m, "(?<=\\/)[a-z]+\\.txt")
    page <- try(read_html(i))
    if('try-error' %in% class(page)){
      write(i, file = "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/server_errors.txt", append = T)
      next
    }
    name <- get_node(data = page, node = "header h1", logFile = "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/get_names.log", currentURL = x) %>% html_text
    status <- get_node(data = page, node = "header p", logFile = "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/get_status.log", currentURL = x) %>% html_text %>% trimws
    contacts <- get_node(data = page, node = "#main-details-accordion .panel-half", logFile = "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/get_contacts.log", currentURL = x)
    SRA_ID <- contacts[1] %>% get_node(xpath = "//dt[contains(text(), 'SRA ID')]/following-sibling::dd") %>% html_text
    tel <- contacts[1] %>% get_node(xpath = "//dt[contains(text(), 'Tel')]/following-sibling::dd") %>% html_text %>% .[grep("[0-9]+", .)]
    mail <- contacts[1] %>% get_node(xpath = "//dt[@id = 'Email']/following-sibling::dd/a[contains(text(), '@')]") %>% html_text() %>% trimws
    roles <- contacts[1] %>% get_node(xpath = "//dt[contains(text(), 'Roles')]/following-sibling::dd") %>% html_text %>% gsub("\\s+", " ", .) %>% trimws %>% paste0(., collapse = ", ")
    address <- contacts[1] %>% 
      get_node(xpath = "//dt[contains(text(), 'Consultant at')]/following-sibling::dd") %>% 
      html_text %>% gsub("(?<=[A-z])\\r\\n", ",", ., perl = T) %>% 
      gsub("View in Google Maps?(\\,)|\\s{2,}", " ", ., perl = T) %>% 
      trimws
    if(length(address)>1) address <- paste0(address, collapse = ", ")
    gmap <- contacts[1] %>% get_node(xpath = "//dt[contains(text(), 'Consultant at')]/following-sibling::dd/a[contains(@href, 'maps')]") %>% html_attr("href")
    aop <- get_node(data = page, node = "#areas-of-practice-accordion", logFile = "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/get_AoP.log", currentURL = x) %>% html_text() %>% trimws %>% gsub("\\s{2,}", ", ", .)
    lang <- get_node(data = page, node = "#languages-spoken-accordion", logFile = "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/get_languages.log", currentURL = x) %>% html_text() %>% trimws %>% gsub("\\s{2,}", ", ", .)
    
    df <- data.table(SRA_ID, name, tel, mail, roles, address, gmap, status, aop, lang, url = i)
    write.table(df, file = paste0("/Volumes/extICY/LEXscraping/law_society_GB/output/profiles/", fileName), append = T, row.names = F, col.names = F)
  }
}


read.table(paste0("/Volumes/extICY/LEXscraping/law_society_GB/output/profiles/", fileName))

