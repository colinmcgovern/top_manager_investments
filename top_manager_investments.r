library(tidyquant)
library(RCurl)
remotes::install_github("omegahat/RTidyHTML")
library(RTidyHTML)
library(XML)
library(readtext)
library(stringi)
library(BBmisc)
library(lubridate)
library(qdapTools)
library(stringr)
library(dplyr)
 
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("AAPL", from = '2017-01-01',
           to = "2018-03-01",warnings = FALSE,
           auto.assign = TRUE)

setwd("C:/Users/theco/Documents/top_manager_investments")

manager_df = data.frame(matrix(nrow = 0, ncol = 2))
colnames(manager_df) = c("date","company")

a = list.files("manager_history")
for(date_fn in a){
  
  date = substr(date_fn,1,8)
  
  text = readtext(paste0("manager_history/",date_fn,"/www.dataroma.com/m/managers.php"))$text
  companies = stri_match_all_regex(text,">[A-Z]+<")[[1]][,1]
  
  for(company in companies){
    company = sub(">","",company)
    company = sub("<","",company)
    company = regmatches(company,regexpr("[A-Z]+",company))
    
    if(!identical(company, character(0))){
      manager_df[nrow(manager_df) + 1,] = c(date,company)
      print
    }
  }

}

manager_df$year = substr(manager_df$date,1,4)
manager_df$month = substr(manager_df$date,5,6)
manager_df$day = substr(manager_df$date,7,8)

month_years = unique(manager_df[,c("month","year")])
unique_companies = unique(manager_df[,c("company")])

month_year_all = c()
for(year_iter in min(month_years$year):max(month_years$year)){
  for(month_iter in 1:12){
    month_year_all = append(month_year_all,paste(month_iter,year_iter))
  }
}

month_year_count_df = data.frame(matrix(0,nrow = length(month_year_all), ncol = length(unique_companies)))
rownames(month_year_count_df) = month_year_all
colnames(month_year_count_df) = unique_companies

for(i in 1:nrow(month_years)){
  my_month = month_years[i,1]
  my_year = month_years[i,2]
  sub_manager_df = manager_df %>% filter(month==my_month&year==my_year)
  counts = mtabulate(sub_manager_df$company)
  for(c in colnames(counts)){
    month_year_count_df[paste(my_month,my_year),c] = sum(counts[,c])
  }
}


month_year_count_df$GOOG


df$Date <- as.Date( manager_df$date, '%m/%d/%Y')
require(ggplot2)
ggplot( data = month_year_count_df, aes( rownames(month_year_count_df), "GOOG" )) 


ggplot() + geom_line(data = month_year_count_df, aes(x = id, y = value, color = func, group = func), size = 1)

matplot(month_year_count_df, type="l", ylim=c(0,0.3), lwd=4, col=1:5, lty=1)


month_year_count_df

# 
# manager_df = apply(manager_df, 1, function(x){any(is.na(x))})
#   

plot(ts(mtabulate(manager_df$company)))
