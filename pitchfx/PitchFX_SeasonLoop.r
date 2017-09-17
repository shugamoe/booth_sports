library(pitchRx)
library(dplyr)
library(plyr)
setwd("C:/Users/bradl/Documents/R/PitchFX")



start <- as.Date("30-03-14",format="%d-%m-%y")
end   <- as.Date("28-09-14",format="%d-%m-%y")

dat <- scrape(start = paste0(format(start,"%Y"),"-",format(start,"%m"),"-",format(start,"%d")), 
              end = paste0(format(start+6,"%Y"),"-",format(start+6,"%m"),"-",format(start+6,"%d")))
names(dat)
pitch.data <- dat$pitch
atbat.data <- dat$atbat
table <- inner_join(pitch.data, atbat.data, by = c('num', 'gameday_link'))
table <- subset(table,!(table$gameday_link %in% c("gid_2014_07_15_nasmlb_aasmlb_1"
                                                  ,"gid_2014_09_30_oakmlb_kcamlb_1"
                                                  ,"gid_2014_10_01_sfnmlb_pitmlb_1"
                                                  ,"gid_2014_10_02_detmlb_balmlb_1"
                                                  ,"gid_2014_10_02_kcamlb_anamlb_1"
                                                  ,"gid_2014_10_03_detmlb_balmlb_1"
                                                  ,"gid_2014_10_03_sfnmlb_wasmlb_1"
                                                  ,"gid_2014_10_03_slnmlb_lanmlb_1"
                                                  ,"gid_2014_10_03_kcamlb_anamlb_1"
                                                  ,"gid_2014_10_04_sfnmlb_wasmlb_1"
                                                  ,"gid_2014_10_04_slnmlb_lanmlb_1"
                                                  
                                                  
)))

total_2014 <- table

theDate <- start + 7

while (theDate <= end)
{
  
  
  dat <- scrape(start = paste0(format(theDate,"%Y"),"-",format(theDate,"%m"),"-",format(theDate,"%d")), 
                end = paste0(format(theDate+6,"%Y"),"-",format(theDate+6,"%m"),"-",format(theDate+6,"%d")))
  names(dat)
  pitch.data <- dat$pitch
  atbat.data <- dat$atbat
  table <- inner_join(pitch.data, atbat.data, by = c('num', 'gameday_link'))
  table <- subset(table,!(table$gameday_link %in% c("gid_2014_07_15_nasmlb_aasmlb_1"
                                                    ,"gid_2014_09_30_oakmlb_kcamlb_1"
                                                    ,"gid_2014_10_01_sfnmlb_pitmlb_1"
                                                    ,"gid_2014_10_02_detmlb_balmlb_1"
                                                    ,"gid_2014_10_02_kcamlb_anamlb_1"
                                                    ,"gid_2014_10_03_detmlb_balmlb_1"
                                                    ,"gid_2014_10_03_sfnmlb_wasmlb_1"
                                                    ,"gid_2014_10_03_slnmlb_lanmlb_1"
                                                    ,"gid_2014_10_03_kcamlb_anamlb_1"
                                                    ,"gid_2014_10_04_sfnmlb_wasmlb_1"
                                                    ,"gid_2014_10_04_slnmlb_lanmlb_1"
                                                    
                                                    
  )))
  
  total_2014 <- rbind.fill(total_2014, table)
  
  theDate <- theDate + 7 
  
}

write.csv(total_2014, file = paste0("Pitch_Data_",format(theDate, "%Y"),".csv"))







start <- as.Date("05-04-15",format="%d-%m-%y")
end   <- as.Date("04-10-15",format="%d-%m-%y")

dat <- scrape(start = paste0(format(start,"%Y"),"-",format(start,"%m"),"-",format(start,"%d")), 
              end = paste0(format(start+6,"%Y"),"-",format(start+6,"%m"),"-",format(start+6,"%d")))
names(dat)
pitch.data <- dat$pitch
atbat.data <- dat$atbat
table <- inner_join(pitch.data, atbat.data, by = c('num', 'gameday_link'))
table <- subset(table,!(table$gameday_link %in% c("gid_2015_07_14_nasmlb_aasmlb_1"
                                                  ,"gid_2015_10_06_houmlb_nyamlb_1"
                                                  ,"gid_2015_10_07_chnmlb_pitmlb_1"
                                                  ,"gid_2015_10_08_texmlb_tormlb_1"
                                                  ,"gid_2015_10_08_houmlb_kcamlb_1"
                                                  ,"gid_2015_10_09_texmlb_tormlb_1"
                                                  ,"gid_2015_10_09_houmlb_kcamlb_1"
                                                  ,"gid_2015_10_09_chnmlb_slnmlb_1"
                                                  ,"gid_2015_10_09_nynmlb_lanmlb_1"
                                                  ,"gid_2015_10_10_chnmlb_slnmlb_1"
                                                  ,"gid_2015_10_10_nynmlb_lanmlb_1"
                                                  
)))

total_2015 <- table

theDate <- start + 7

while (theDate <= end)
{
  
  
  dat <- scrape(start = paste0(format(theDate,"%Y"),"-",format(theDate,"%m"),"-",format(theDate,"%d")), 
                end = paste0(format(theDate+6,"%Y"),"-",format(theDate+6,"%m"),"-",format(theDate+6,"%d")))
  names(dat)
  pitch.data <- dat$pitch
  atbat.data <- dat$atbat
  table <- inner_join(pitch.data, atbat.data, by = c('num', 'gameday_link'))
  table <- subset(table,!(table$gameday_link %in% c("gid_2015_07_14_nasmlb_aasmlb_1"
                                                    ,"gid_2015_10_06_houmlb_nyamlb_1"
                                                    ,"gid_2015_10_07_chnmlb_pitmlb_1"
                                                    ,"gid_2015_10_08_texmlb_tormlb_1"
                                                    ,"gid_2015_10_08_houmlb_kcamlb_1"
                                                    ,"gid_2015_10_09_texmlb_tormlb_1"
                                                    ,"gid_2015_10_09_houmlb_kcamlb_1"
                                                    ,"gid_2015_10_09_chnmlb_slnmlb_1"
                                                    ,"gid_2015_10_09_nynmlb_lanmlb_1"
                                                    ,"gid_2015_10_10_chnmlb_slnmlb_1"
                                                    ,"gid_2015_10_10_nynmlb_lanmlb_1"
                                                    
  )))

  total_2015 <- rbind.fill(total_2015, table)
  
  theDate <- theDate + 7 
  
}

write.csv(total_2015, file = paste0("Pitch_Data_",format(theDate, "%Y"),".csv"))






#2016 pull

start <- as.Date("03-04-16",format="%d-%m-%y")
end   <- as.Date("02-10-16",format="%d-%m-%y")

dat <- scrape(start = paste0(format(start,"%Y"),"-",format(start,"%m"),"-",format(start,"%d")), 
              end = paste0(format(start+6,"%Y"),"-",format(start+6,"%m"),"-",format(start+6,"%d")))
names(dat)
pitch.data <- dat$pitch
atbat.data <- dat$atbat
table <- inner_join(pitch.data, atbat.data, by = c('num', 'gameday_link'))
table <- subset(table,!(table$gameday_link %in% c("gid_2016_04_03_chnmlb_anamlb_1"
                                                  ,"gid_2016_07_12_nasmlb_aasmlb_1"
                                                  ,"gid_2016_10_04_balmlb_tormlb_1"
                                                  ,"gid_2016_10_05_sfnmlb_nynmlb_1"
                                                  ,"gid_2016_10_06_tormlb_texmlb_1"
                                                  ,"gid_2016_10_06_bosmlb_clemlb_1"
                                                  ,"gid_2016_10_07_tormlb_texmlb_1"
                                                  ,"gid_2016_10_07_bosmlb_clemlb_1"
                                                  ,"gid_2016_10_07_lanmlb_wasmlb_1"
                                                  ,"gid_2016_10_07_sfnmlb_chnmlb_1"
                                                  ,"gid_2016_10_08_sfnmlb_chnmlb_1"
                                                  
)))

total_2016 <- table

theDate <- start + 7

while (theDate <= end)
{
  
  
  dat <- scrape(start = paste0(format(theDate,"%Y"),"-",format(theDate,"%m"),"-",format(theDate,"%d")), 
                end = paste0(format(theDate+6,"%Y"),"-",format(theDate+6,"%m"),"-",format(theDate+6,"%d")))
  names(dat)
  pitch.data <- dat$pitch
  atbat.data <- dat$atbat
  table <- inner_join(pitch.data, atbat.data, by = c('num', 'gameday_link'))
  table <- subset(table,!(table$gameday_link %in% c("gid_2016_04_03_chnmlb_anamlb_1"
                                                    ,"gid_2016_07_12_nasmlb_aasmlb_1"
                                                    ,"gid_2016_10_04_balmlb_tormlb_1"
                                                    ,"gid_2016_10_05_sfnmlb_nynmlb_1"
                                                    ,"gid_2016_10_06_tormlb_texmlb_1"
                                                    ,"gid_2016_10_06_bosmlb_clemlb_1"
                                                    ,"gid_2016_10_07_tormlb_texmlb_1"
                                                    ,"gid_2016_10_07_bosmlb_clemlb_1"
                                                    ,"gid_2016_10_07_lanmlb_wasmlb_1"
                                                    ,"gid_2016_10_07_sfnmlb_chnmlb_1"
                                                    ,"gid_2016_10_08_sfnmlb_chnmlb_1"
                                                    
  )))
  
  total_2016 <- rbind.fill(total_2016, table)
  
  theDate <- theDate + 7 
  
}
write.csv(total_2016, file = paste0("Pitch_Data_",format(theDate, "%Y"),".csv"))

