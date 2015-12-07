---
title: 'Data wrangling with dplyr'
author: 'David Hope'
date: '2015-12-9'
output: html_document




### Script to import all datasets from python output ####
# rm(list = ls())
library(ggplot2)
library(corrgram)
require(plyr)
library(dplyr)
library(ggthemes)
library(gridExtra) # http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization


database.loc <- '~/Dropbox/SFU/ShorebirdSurveyDatabase/'


wesa.counts.cleaned <- read.csv(paste(database.loc, 'Rscripts/wesa_cleaned.txt',sep = ""), sep = '\t', header = T)

wesa.counts.cleaned$count.numeric <- as.numeric(wesa.counts.cleaned$Count_cleaned)

site.codes <- read.csv(paste(database.loc, 'AreaCalculations/AreaComparison.csv', sep = ""),
                       stringsAsFactors = FALSE)

# SUMMARY BY DATE
wesa.summary <-
  wesa.counts.cleaned %>%
  mutate(count.numeric = as.numeric(Count_cleaned)) %>%
  filter(RecordID != 'RecordID' & !is.na(count.numeric)) %>%
  group_by(RecordID) %>%
  dplyr::summarize(max.count = max(count.numeric, na.rm=T), 
            avg.count = mean(count.numeric, na.rm=T),
            sd.count = sd(count.numeric, na.rm = T),
            sum.count = sum(count.numeric, na.rm = T), 
            n.counts = n() )%>%
  mutate(SiteID = substr(RecordID, 1, 4), 
         Year = substr(RecordID, 5, 8), 
         Month = substr(RecordID, 9,9),
         Day = substr(RecordID, 10, 11)) %>%
  left_join(site.codes, by = 'SiteID')


  # Time counts at boundary bay
bb <- 
  wesa.counts.cleaned %>%
  filter(grepl(pattern = '^BB', SiteID )) %>%
  select(ID, RecordID, CountNum,Time, SiteID, count.numeric) %>%
  mutate(SiteID = substr(RecordID, 1, 4), 
         Year = substr(RecordID, 5, 8), 
         Month = substr(RecordID, 9,9),
         Day = substr(RecordID, 10, 11),
         datecode = paste(Year, Month, Day, sep = "")) %>%
  arrange(datecode, CountNum, SiteID) 





bb.final.sum <- 
  bb %>%
  group_by(datecode, CountNum, Year, Month, Day) %>%
  dplyr::summarize(sum.count = sum(count.numeric), 
                 avg.count = mean(count.numeric), 
                 max.count = max(count.numeric),
                 number.sites = n(),
                 SiteID = 'BBAY',
                 SiteName = 'Boundary Bay',
                 sd.count = sd(count.numeric),
                 RecordID = paste(SiteID, datecode, sep = "")) %>%
    group_by(RecordID) %>%
    dplyr::summarize(max.count = max(sum.count, na.rm=T), 
                     avg.count = mean(sum.count, na.rm=T),
                     sd.count = sd(sum.count, na.rm = T),
                     sum.count = sum(sum.count, na.rm = T), 
                     n.counts = n() )%>%
    mutate(SiteID = substr(RecordID, 1, 4), 
           Year = substr(RecordID, 5, 8), 
           Month = substr(RecordID, 9,9),
           Day = substr(RecordID, 10, 11))%>%
  left_join(site.codes, by = 'SiteID')
  



wesa.w.large.sites <- 
  wesa.summary %>%
  filter(!grepl(pattern = '^BB', SiteID )) %>%
  full_join(bb.final.sum)
