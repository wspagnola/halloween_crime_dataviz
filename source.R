#Packages
library(tidyverse)
library(lubridate)

#### Functions ####

mutate_cols <- function(x){
  require(dplyr)
  require(lubridate)
  rename <- which(names(x)=="CMPLNT_FR_DT" | names(x)=="OFNS_DESC" )
  colnames(x)[rename] <-  c("date_reported", "crime")
  
  x <- x %>% mutate(date_reported = mdy( date_reported ),
                    month = month(date_reported),
                    day = day(date_reported),
                    year = year(date_reported)
  )
  
  return(x)
}

clean_data <- function(x) {
  require(dplyr)
  x <- x[which(is.na(x$date_reported)==F) , ]
  x <- x %>% filter(year >= 2006)
  return(x)
}


add_halloween <- function(x){
  require(dplyr)
  x <- x %>%  mutate( halloween = case_when(day == 31 & month == 10 ~ 1,
                                            TRUE ~ 0
  )) 
  
  #Check Dimension
  check_dims <-  x %>% filter(day == 31 & month == 10) %>% nrow() ==  
    x %>% filter(halloween == 1) %>% nrow()        &&
    x %>% filter(day != 31 | month != 10) %>% nrow() ==  
    x %>% filter(halloween == 0) %>% nrow()
  
  if(check_dims == T) { 
    return(x)
  } else(
    
    return("Error: Dimensions do not match")
  )
  
}

halloween_compare <- function(x){
  
  
  #Calculate Halloween Crimes by Year 
  halloween <- x %>% filter(halloween == 1) %>% 
    group_by(year) %>%
    summarize(on_halloween = n()) %>%
    as.data.frame()
  
  #Calculate Average Crime per Year Excluding Halloween
  non_halloween <- x %>% filter(halloween ==0) %>% 
    group_by(year) %>%
    summarise(avg_for_year = n() / 364) %>% as.data.frame()
  
  #Adjust for Leap Years
  non_halloween <-  non_halloween %>% 
    mutate(avg_for_year = case_when( 
      year %% 4 == 0 ~ avg_for_year*(364/365),
      TRUE ~ avg_for_year
      
    ))
  
  #Merge Data
  sum_df <- merge(x = halloween, y = non_halloween, by = "year")
  
  #Rename Cols
  colnames(sum_df) <- c("Year", "On_Halloween", "Average_for_Year")
  
  sum_df$Year <- as.integer((sum_df$Year))
  return(sum_df)
  
}

convert_long <- function(x){
  
  require(dplyr)
  
  x <- x %>% gather(key="Halloween", 
                    value = "Number_of_Incidents", 
                    Average_for_Year:On_Halloween) 
  
  x <- x %>% mutate(Halloween = as.factor(Halloween))
  
  levels(x$Halloween) <- c("Average for Year", "On Halloween")
  return(x)
  
}