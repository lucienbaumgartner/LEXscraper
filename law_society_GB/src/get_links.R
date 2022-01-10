library(rvest)
library(tidyverse)

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

writeLines(capture.output(sessionInfo()), "/Volumes/extICY/LEXscraping/law_society_GB/res/scraper__session_info.txt")

root_url <- function(x, page = 1) paste0("https://solicitors.lawsociety.org.uk/search/results?Type=1&Name=", x,"&IncludeNlsp=True&Pro=True&Page=", page)
get_search_n <- function(x) x %>% html_node("div.row-fluid h1") %>% html_text() %>% gsub(",", "", .) %>% str_extract(., "(?<=Found\\s)[0-9]+") %>% as.numeric

write_out_links <- function(n, index){
  print(index)
  page <- 1
  for(i in 1:round(n/20, 0)){
    search_page <- read_html(root_url(index, page = i))
    links <- search_page %>% html_nodes("#results > section > header > h2 > a") %>% html_attr("href")
    write(links, file = paste0("/Volumes/extICY/LEXscraping/law_society_GB/output/links2profiles/", index, ".txt"), append=TRUE)
    page <- page + 1
  }
  writeLines(index, "/Volumes/extICY/LEXscraping/law_society_GB/output/logs/last_index.txt")
  write(index, file = paste0("/Volumes/extICY/LEXscraping/law_society_GB/output/logs/indices.txt"), append=TRUE)
}

cycler <- function(f1 = 1, f2 = 1, f3 = 1){
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
            }
            write_out_links(n = current_n, index = paste0(m,k,i))
          }
        }
      }
    }
  }
}

last_index <- readLines("/Volumes/extICY/LEXscraping/law_society_GB/output/logs/last_index.txt")
if(last_index == ""){
  cycler()
}else{
  vec <- setNames(1:26, letters)
  last_index <- unlist(strsplit(last_index, ""))[1:3]
  last_index <- vec[last_index]
  last_index[is.na(last_index)] <- 1
  cycler(f1 = last_index[1], f2 = last_index[2], f3 = last_index[3])
}






substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
resolve_index <- function(x, .next = F){
  if(nchar(x) == 1 | (nchar(x) == 2 & !.next)){
    tmp_index <- paste0(x, letters[1])
    #}else if((nchar(x) == 2 | str_sub(x, -1) == "z") & .next){
  }else if(nchar(x) == 3 | .next){
    if(str_sub(x, -2) == "zz"){
      tmp_index <- letters[grep(unlist(strsplit(x, ""))[1], letters) + 1]
    }else if(grepl("^[a-z]z*", str_sub(x, -2))){
      tmp_index <- paste0(strtrim(x, nchar(x) - 2), letters[grep(unlist(strsplit(x, ""))[nchar(x) -1 ], letters) + 1])
    }else{
      tmp_index <- paste0(strtrim(x, nchar(x) - 1), letters[grep(unlist(strsplit(x, ""))[nchar(x)], letters) + 1])
    }
  }
  return(tmp_index)
}
get_index <- function(x){
  last_index <- readLines("../output/logs/last_index.txt")
  return(resolve_index(last_index, .next = T))
}
update_index <- function(x){
  tmp_index <- get_index()
  tmp_search_page <- x
  current_n <- get_search_n(x)
  while(current_n/(30*20) >= 1 | current_n == 0){
    tmp_index <- resolve_index(tmp_index)
    tmp_search_page <- read_html(root_url(tmp_index))
    current_n <- get_search_n(tmp_search_page)
  }
  print(tmp_index)
  return(list(search_page = tmp_search_page, index = tmp_index))
}
while(check){
  page <- 1
  search_page <- read_html(root_url(get_index()))
  task <- update_index(search_page)
  for(i in 1:round(get_search_n(task$search_page)/20, 0)){
    search_page <- read_html(root_url(task$index, page = i))
    links <- search_page %>% html_nodes("#results > section > header > h2 > a") %>% html_attr("href")
    write(links, file = paste0("../output/links2profiles/", task$index, ".txt"), append=TRUE)
    page <- page + 1
  }
  writeLines(task$index, "../output/logs/last_index.txt")
  write(task$index, file = paste0("../output/logs/indices.txt"), append=TRUE)
  check <- !task$index == 'bab'
}