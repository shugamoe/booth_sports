# My own version of getting the pitch and at bat data (Julian McClellan)
library(pitchRx)
library(tidyverse)
library(stringr)
library(magrittr)
library(lubridate)

START <- ymd("2008-01-01")

OFFSET <- as.list(0:9)

offset_date <- function(offset, date_obj, start_or_end){
  if (start_or_end == "start"){
    as.character(date_obj + years(offset))
  } else if (start_or_end == "end"){
    as.character(date_obj + years(offset + 1) - days(1)) 
  }
}

YEAR_DF <- tibble(start = OFFSET %>% 
                    map_chr(~offset_date(., date_obj = START, 
                                     start_or_end = "start")),
                  end = OFFSET %>%
                    map_chr(~offset_date(., date_obj = START,
                                         start_or_end = "end")),
                  db = as.list(2008:2017) %>%
                          map_chr(~sprintf("pitchfx/pitchfx_%d.sqlite", .)) %>%
                          map(~src_sqlite(., create = T)))

scrape_to_local <- function(db, start, end, func, write){
  END <- end
  start <- ymd(start)
  if (func == "update"){
    update_db(db$con, end = END)    
  } else if (func == "create"){
    scrape_by_day(db, start, END, start + days(1))  
  }
  
  if (write){
   export_csv(db) 
  }
}

scrape_by_day <- function(db, start, final_end, end){
  if (!exists("end")){
    end <- start + days(1)
  }
  
  if (end >= final_end){
    return()
  }
  
  print("Scraping:")
  print(start)
  print(end)
  
  try(
  scrape(start = as.character(start),
         end = as.character(end),
         connect = db$con)
  )
  scrape_by_day(db, start + days(2), final_end, end + days(2))
}

export_csv <- function(db){
  print("Exporting")
  file_name <- str_replace(db$con@dbname, "sqlite", "csv")
  print(file_name)
  t <- inner_join(tbl(db, "pitch"), tbl(db, "atbat"), by = 
                    c("num", "gameday_link")) %>%
    dplyr::select(-contains(".y")) %>%
    collect() %>%
    mutate(season = as.integer(str_extract(gameday_link, "[\\d]{4}"))) %>%
    filter(!str_detect(gameday_link, "aasmlb|nasmlb"))
  if (any(is.na(t$pitcher_name))){
    browser()
  }
  print(unique(t$season))
  names(t) <- str_replace(names(t), "\\.x", "")
  
  write.csv(t, file_name)
}

main <- function(create_or_update){
  as.list(YEAR_DF) %>%
    pwalk(.f = scrape_to_local, func = create_or_update, write = TRUE)
}

main("create")
