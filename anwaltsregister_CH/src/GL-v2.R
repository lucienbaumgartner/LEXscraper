library(tidyverse)
library(rvest)
rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

writeLines(capture.output(sessionInfo()), "/Volumes/extICY/LEXscraping/anwaltsregister_CH/res/session_infos/GL_profile_scraper__session_info.txt")

page <- read_html("https://www.gl.ch/rechtspflege/anwaltsregister.html/276")
df <- page %>% html_node(xpath = "//main[@id = 'main']") %>% html_table()
js <- page %>% html_node("script")
df <- df %>%
  setNames(., c("address", "tel", "fax", "mail")) %>% 
  mutate(
    name = gsub("\\,.*", "", address),
    web = str_extract(mail, "www.*")
  )
df
mailinglist <- lapply(df$web, function(x){
  #x <- df$web[31]
  if(is.na(x)) return(NA)
  .url <- c(paste0("https://", x, "/kontakt"), paste0("https://", x, "/kontakt.html"),
            paste0("https://", x, "/contact"), paste0("https://", x, "/contact.html"))
  sublist <- sapply(
    .url, function(y){
      cpage <- try(read_html(y))
      if("try-error"%in%class(cpage)){
        return(NULL)
      }else{
        mail <- cpage %>% html_nodes(xpath = "//a[contains(@href, 'mailto')]") %>% html_attr("href") %>% gsub("mailto\\:", "", .) %>% unique
        if(length(mail) == 1){
          if(is.na(mail)|identical(mail, character(0))){
            mail <- try(read_html(paste0("https://", x)) %>% html_nodes(xpath = "//*[contains(text(), '@')]") %>% html_text("href")%>% unique)
            if("try-error"%in%class(mail)) return(NULL)
          }
        }
        return(mail)
      }
    }
  )
  sublist <- unique(unlist(sublist))
  return(sublist)
  }
)

#Loading both the required libraries
library(rvest)
library(V8)
#URL with js-rendered content to be scraped
link <- 'https://food.list.co.uk/place/22191-brewhemia-edinburgh/'
#Read the html page content and extract all javascript codes that are inside a list
emailjs <- read_html(link) %>% html_nodes('li') %>% html_nodes('script') %>% html_text()
# Create a new v8 context
ct <- v8()
#parse the html content from the js output and print it as text
read_html(ct$eval(gsub('document.write','',emailjs))) %>% 
  html_text()