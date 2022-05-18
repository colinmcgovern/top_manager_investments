library(tidyquant)
library(RCurl)
library(RTidyHTML)
library(XML)
library(readtext)
library(stringi)
library(BBmisc)
library(lubridate)
library(qdapTools)
 
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("AAPL", from = '2017-01-01',
           to = "2018-03-01",warnings = FALSE,
           auto.assign = TRUE)

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

u <- "http://stackoverflow.com/questions/tagged?tagnames=r" 
doc.raw <- getURL(u)
doc <- tidyHTML(doc.raw)
html <- htmlTreeParse(doc, useInternal = TRUE)
txt <- xpathApply(html, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]", xmlValue)
cat(unlist(txt))

setwd("C:/Users/theco/Documents")

manager_df = data.frame(matrix(NA, nrow = 1E6, ncol = 3))
colnames(manager_df) = c("date","company","sum")
iter = 1

a = list.files("manager_history")
for(date_fn in a){
  
  date = substr(date_fn,1,8)
  
  text = readtext(paste0("manager_history/",date_fn,"/www.dataroma.com/m/managers.php"))$text
  company = regmatches(text,regexpr(">[A-Z]+<",text))
  company = sub(">","",company)
  company = sub("<","",company)
  company = regmatches(company,regexpr("[A-Z]+",company))
  
  if(!identical(company, character(0))){
    manager_df[iter,]$date = date
    manager_df[iter,]$company = company 
    iter = iter + 1
  }
  
  
}

manager_df = delete.na(manager_df)

df$Date <- as.Date( manager_df$date, '%m/%d/%Y')
require(ggplot2)
ggplot( data = manager_df, aes( date, company )) + geom_line() 

head(mtabulate(manager_df$company))


# 
# manager_df = apply(manager_df, 1, function(x){any(is.na(x))})
#   

plot(ts(mtabulate(manager_df$company)))
