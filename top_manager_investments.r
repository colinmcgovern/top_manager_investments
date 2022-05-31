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
library(ggplot2)
 
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

setwd("C:/Users/theco/Documents/top_manager_investments")

manager_df = data.frame(matrix(nrow = 0, ncol = 2))
colnames(manager_df) = c("date","company")

a = list.files("manager_history")
for(date_fn in a){
  
  print(date_fn)
  
  date = substr(date_fn,1,8)
  
  text = readtext(paste0("manager_history/",date_fn,"/www.dataroma.com/m/managers.php"))$text
  companies = stri_match_all_regex(text,">[A-Z]+<")[[1]][,1]
  
  for(company in companies){
    company = sub(">","",company)
    company = sub("<","",company)
    company = regmatches(company,regexpr("[A-Z]+",company))
    
    if(!identical(company, character(0))){
      print(company)
      manager_df[nrow(manager_df) + 1,] = c(date,company)
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

month_year_count_df = data.frame(matrix(0,nrow = length(month_year_all), ncol = length(unique_companies)+1))
rownames(month_year_count_df) = month_year_all
colnames(month_year_count_df) = append(unique_companies,"date")

for(x in month_year_all){
  my_month = str_split(x," ")[[1]][1]
  my_year = str_split(x," ")[[1]][2]
  month_year_count_df[paste(str_remove(my_month, "^0+"),my_year),"date"] =  format(as.Date(paste0(my_year,"-",my_month,"-01")))
}

for(i in 1:length(month_year_all)){
  my_month = str_split(x," ")[[1]][1]
  my_year = str_split(x," ")[[1]][2]
  
  sub_manager_df = manager_df %>% filter(month==my_month&year==my_year)
  sub_manager_df = manager_df %>% filter(day==min(sub_manager_df$day))
  
  if(nrow(sub_manager_df)!=0){
    counts = mtabulate(sub_manager_df$company)
    for(c in unique_companies){
      month_year_count_df[paste(str_remove(my_month, "^0+"),my_year),c] = sum(counts[,c])
    }
  }else{
    for(c in unique_companies){
      month_year_count_df[paste(str_remove(my_month, "^0+"),my_year),c] = -1 #To be interpolated 
    }
  }
}

ggplot(data = month_year_count_df, aes(x = date, y = MSFT, group=1)) + geom_line()


getSymbols("MSFT", from = '2018-01-01',
           to = "2022-12-01",warnings = FALSE,
           auto.assign = TRUE)
MSFT = as.data.frame(MSFT)

ggplot(data = MSFT, aes(x = rownames(MSFT), y = MSFT.Close, group=1)) + geom_line()



for(c in unique_companies){
  getSymbols(c, from = '2017-01-01',
             to = "2018-03-01",warnings = FALSE,
             auto.assign = TRUE)
}




