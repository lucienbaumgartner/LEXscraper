library(tidyverse)
library(rvest)
library(RSelenium)
rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

writeLines(capture.output(sessionInfo()), "/Volumes/extICY/LEXscraping/anwaltsregister_CH/res/session_infos/GL_profile_scraper__session_info.txt")

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "firefox"
)

remDr$open()
#remDr$setWindowSize(width = 800, height = 300)
writeLines(capture.output(remDr$getStatus()), "/Volumes/extICY/LEXscraping/anwaltsregister_CH/res/session_infos/GL_selenium__session_info.txt")

remDr$navigate("https://www.gl.ch/rechtspflege/anwaltsregister.html/276")
remDr$getCurrentUrl()
# get table
ftable <- remDr$findElement(using = "xpath", value = "//main[@id = 'main']")
df <- ftable$getElementAttribute("innerHTML")[[1]] %>% read_html() %>% html_table() %>% .[[1]]
df <- df %>%
  setNames(., c("address", "tel", "fax", "mail")) %>% 
  mutate(
    name = gsub("\\,.*", "", address),
    web = str_extract(mail, "www.*")
  )
df
# get mail buttons
pageRefs <- remDr$findElements(using = "xpath", value = "//a[contains(text(), 'www')]")
container <- list()
for(i in 1:length(pageRefs)){
  #length(pageRefs)
  #i =5
  pageRefs[[i]]$clickElement()
  remDr$getCurrentUrl()
  remDr$getWindowHandles()
  if(i == 1){
    init <- remDr$getCurrentWindowHandle()[[1]]
    visited.windows <- init
  }
  next.handle <- unlist(remDr$getWindowHandles())
  next.handle <- next.handle[!next.handle%in%visited.windows]
  remDr$switchToWindow(next.handle) # go to new window
  visited.windows <- c(visited.windows, remDr$getCurrentWindowHandle()[[1]])
  remDr$screenshot(TRUE)
  Sys.sleep(2)
  scroll <- remDr$findElement("css", "body")
  scroll$sendKeysToElement(list(key = "end"))
  Sys.sleep(4)
  cpage <- try(remDr$findElement(using = "xpath", value = "//a[contains(translate(text(), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'kontakt') or contains(translate(text(), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'contact')]"))
  if("try-error"%in%class(cpage)){
    mail <- try(remDr$findElements(using = "xpath", value = "//a[contains(@href, 'mailto')]|//p[contains(text(), '@')]|//span[contains(text(), '@')]")$getElementAttribute("innerHTML")[[1]])
    if("try-error"%in%class(mail)){
      container[[i]] <- NA
    }else{
      multimail <- lapply(mail, function(x) x$getElementAttribute("innerHTML")[[1]]) %>% unlist %>% unique
      container[[i]] <- multimail[grepl("@", multimail)]
    }
  }else{
    cpage$clickElement()
    Sys.sleep(2)
    remDr$screenshot(TRUE)
    mail <- try(remDr$findElements(using = "xpath", value = "//a[contains(@href, 'mailto')]|//p[contains(text(), '@')]|//span[contains(text(), '@')]"))
    if("try-error"%in%class(mail)){
      container[[i]] <- NA
    }else{
      multimail <- lapply(mail, function(x) x$getElementAttribute("innerHTML")[[1]]) %>% unlist %>% unique
      container[[i]] <- multimail[grepl("@", multimail)]
    }
    visited.windows <- c(visited.windows, remDr$getCurrentWindowHandle()[[1]])
  }
  remDr$switchToWindow(init) # go back to initial window
  Sys.sleep(2)
}
remDr$close()


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