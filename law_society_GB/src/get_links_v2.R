library(rvest)
library(tidyverse)

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

writeLines(capture.output(sessionInfo()), "/Volumes/extICY/LEXscraping/law_society_GB/res/scraper__session_info_v2.txt")

root_url <- function(x, page = 1) paste0("https://solicitors.lawsociety.org.uk/search/results?Type=1&Name=", x,"&IncludeNlsp=True&Pro=True&Page=", page)
get_search_n <- function(x) x %>% html_node("div.row-fluid h1") %>% html_text() %>% gsub(",", "", .) %>% str_extract(., "(?<=Found\\s)[0-9]+") %>% as.numeric
letters <- c(letters, "ö", "ü", "ä", "é", "è", "ë", "ì", "ê", "ï", "î", "ã", "å", "ā", "á", "ē", "í", "ī", 
             "ô", "ò", "ó", "õ", "ø", "ō", "û", "ù", "ú", "ū", "ÿ", "ñ", "ń", "ç", "ć", "č") %>% unique

write_out_links <- function(n, index){
  print(index)
  page <- 1
  for(i in 1:round(n/20, 0)){
    search_page <- read_html(root_url(index, page = i))
    links <- search_page %>% html_nodes("#results > section > header > h2 > a") %>% html_attr("href")
    write(links, file = paste0("/Volumes/extICY/LEXscraping/law_society_GB/output/links2profiles_v2/", index, ".txt"), append=TRUE)
    page <- page + 1
  }
  writeLines(index, "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/last_index.txt") # doesn't need v2
  write(index, file = paste0("/Volumes/extICY/LEXscraping/law_society_GB/output/logs/indices_v2.txt"), append=TRUE)
}

cycler <- function(f1 = 1, f2 = 1, f3 = 1, f4 = 1, f5 = 1){
  for(m in letters[f1:length(letters)]){
    tmp_search_page <- read_html(root_url(paste0(m)))
    current_n <- get_search_n(tmp_search_page)
    if(current_n == 0){
      next
    }else if(current_n/(30*20) <= 1){
      write_out_links(n = current_n, index = m)
    }else if(current_n/(30*20) >= 1){
      for(k in letters[f2:length(letters)]){
        tmp_search_page <- read_html(root_url(paste0(m,k)))
        current_n <- get_search_n(tmp_search_page)
        if(current_n == 0){
          if(k=="z"){
            f2 <- 1
            break
          }else{
            next
          }
        }else if(current_n/(30*20) <= 1){
          write_out_links(n = current_n, index = paste0(m,k))
        }else if(current_n/(30*20) >= 1){
          for(i in letters[f3:length(letters)]){
            tmp_search_page <- read_html(root_url(paste0(m,k,i)))
            current_n <- get_search_n(tmp_search_page)
            if(current_n == 0){
              if(i=="z"){
                f3 <- 1
                break
              }else{
                next
              }
            }else if(current_n/(30*20) <= 1){
              write_out_links(n = current_n, index = paste0(m,k,i))
            }else if(current_n/(30*20) >= 1){
              for(o in letters[f4:length(letters)]){
                tmp_search_page <- read_html(root_url(paste0(m,k,i,o)))
                current_n <- get_search_n(tmp_search_page)
                if(current_n == 0){
                  if(o=="z"){
                    f4 <- 1
                    break
                  }else{
                    next
                  }
                }else if(current_n/(30*20) <= 1){
                  write_out_links(n = current_n, index = paste0(m,k,i,o))
                }else if(current_n/(30*20) >= 1){
                  for(z in letters[f5:length(letters)]){
                    tmp_search_page <- read_html(root_url(paste0(m,k,i,o,z)))
                    current_n <- get_search_n(tmp_search_page)
                    if(current_n == 0){
                      if(z=="z"){
                        f5 <- 1
                        break
                      }else{
                        next
                      }
                    }else{
                      write_out_links(n = current_n, index = paste0(m,k,i,o,z))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

last_index <- readLines("/Volumes/extICY/LEXscraping/law_society_GB/output/logs/last_index.txt")
if(identical(last_index, character(0))){
  cycler()
}else{
  vec <- setNames(1:length(letters), letters)
  last_index <- unlist(strsplit(last_index, ""))[1:5]
  last_index <- vec[last_index]
  last_index[is.na(last_index)] <- 1
  cycler(f1 = last_index[1], f2 = last_index[2], f3 = last_index[3], f4 = last_index[4], f5 = last_index[5])
}

files2collect <- unique(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/links2profiles_v2/", full.names = T))
collection <- lapply(files2collect, readLines)
cvec <- unlist(collection)
cnames <- rep(list.files("/Volumes/extICY/LEXscraping/law_society_GB/output/links2profiles_v2/"), times = lengths(collection))
length(cvec) == length(cnames)
profiles <- tibble(cvec, cnames) %>% 
  filter(!cvec == "") %>% 
  filter(!duplicated(cvec))
checks <- profiles %>% 
  group_by(cnames) %>% 
  summarise(n = n()) %>% 
  mutate(p_maxed_out = n/(30*20))
range(checks$p_maxed_out)
hist(checks$p_maxed_out)
checks %>% 
  filter(p_maxed_out > 0.98)

nrow(profiles)/196148

