rm(list=ls())
library(tidyverse)
library(broom)

fnams <- list.files("CSV Data", "ad", full.names = TRUE) # needed for reading data
subjs <- list.files("CSV Data", "ad") # needed for identifying subject numbers

data <- NULL
for (subj in 1:length(fnams)) { 
  
  pData <- read_csv(fnams[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>% 
    mutate(subj = as.numeric(substr(subjs[subj],1,2))) %>% 
    select(subj,everything())
  
  data <- rbind(data, pData) # combine data array with existing data
  
}

data <- rename(data, trial = X1, TT = X2, tQuad = X3,
               tLoc = X4, tOrient = X5, resp = X6, acc = X7, RT = X8)

# mutate column based on arrow/no-arrow patterns
data <- data %>% 
  mutate(TT = as.factor(case_when(
    subj %% 2 == 1 & TT==1 & tQuad %in% c(1,4) ~ "rep_arrow",
    subj %% 2 == 1 & TT==1 & tQuad %in% c(2,3) ~ "rep_no_arrow",
    subj %% 2 == 0 & TT==1 & tQuad %in% c(1,4) ~ "rep_no_arrow",
    subj %% 2 == 0 & TT==1 & tQuad %in% c(2,3) ~ "rep_arrow",
    TT==3 ~ "random"
  )))

# pairwise t-tests 
data %>% 
  filter(!acc==9999) %>% 
  group_by(subj, TT) %>% 
  summarise(propCor = mean(acc)) %>% 
  {pairwise.t.test(x = .$propCor, g = .$TT, data = .)}

data %>% 
  filter(!acc==9999) %>% 
  group_by(subj, TT) %>% 
  summarise(propCor = mean(acc)) %>% 
  pivot_wider(names_from = TT, values_from = propCor) %>% 
  write_csv("output_awareness_data.csv")
