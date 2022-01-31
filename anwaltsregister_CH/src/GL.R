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
remDr$navigate()
writeLines(capture.output(remDr$getStatus()), "/Volumes/extICY/LEXscraping/anwaltsregister_CH/res/session_infos/GL_selenium__session_info.txt")

remDr$navigate("https://www.gl.ch/rechtspflege/anwaltsregister.html/276")

remDr$getCurrentUrl()
# get table
ftable <- remDr$findElement(using = "xpath", value = "//main[@id = 'main']")
df <- ftable$getElementAttribute("innerHTML")[[1]] %>% read_html() %>% html_table() %>% .[[1]]
# get mail buttons
pageRefs <- remDr$findElements(using = "xpath", value = "//a[contains(text(), 'www')]")
pageRefs[[1]]$clickElement()
remDr$switchToWindow(remDr$getWindowHandles()[[2]])
remDr$screenshot(TRUE)
remDr$findElement(using = "xpath", value = "//a[contains(text(), 'Kontakt')]")$clickElement()
remDr$screenshot(TRUE)
mail <- remDr$findElement(using = "xpath", value = "//a[contains(@href, 'mailto')]")$getElementAttribute("innerHTML")[[1]]
remDr$closeWindow(remDr$getWindowHandles()[[2]])

str(mailRefs)
mailRefs[[1]]$clickElement()
mailRefs <- remDr$findElements(using = "xpath", value = "//a[contains(text(), 'Mail')]")
mailRefs[[1]]$clickElement()
mail <- remDr$executeScript("arguments[0].click();", list(mailRefs[[1]]))
mail
# get frames
frames <- remDr$findElements(using = "tag name", "frame")
remDr$screenshot(TRUE)

remDr$close()

read_html(webElem$getElementAttribute('name'))
page <- read_html("https://www.gl.ch/rechtspflege/anwaltsregister.html/276")
df <- page %>% html_node(xpath = "//main[@id = 'main']") %>% html_table()
js <- page %>% html_node("script")
emailjs <- page %>% html_node(xpath = "//a[contains(text(), 'Mail')]") %>% html_attr("href")
ct <- v8()
read_html(ct$eval(gsub('document.write','',emailjs))) %>% 
  html_text()
df <- df %>%
  setNames(., c("address", "tel", "fax", "mail")) %>% 
  mutate(
    name = gsub("\\,.*", "", address),
  )
df

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