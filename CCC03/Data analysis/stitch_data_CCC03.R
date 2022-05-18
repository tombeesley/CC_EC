# This script stitches together the data files into one dataframe.
# It then does some basic data cleaning, such as editing column names

library(tidyverse)

# this bit reads in the files and uses part of the filename to make a new "subj" variable
fnams <- list.files("CSV Data", "td", full.names = TRUE) # needed for reading data
subjs <- list.files("CSV Data", "td") # needed for identifying subject numbers
data <- NULL
for (subj in 1:length(fnams)) {
  pData <- read_csv(fnams[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>%
    mutate(subj = substr(subjs[subj],1,str_length(subjs[subj])-7)) %>%
    select(subj,everything())
  data <- rbind(data, pData) # combine data array with existing data
}


# first step of data cleaning to rename variables
data <-
  data %>%
  select(-X1) %>% # removes this variable which didn't code "phase" correctly
  # This renames the columns
  rename(block = X2, trial = X3, TT = X4, patType = X5,
         tQuad = X6, tLoc = X7, tOrient = X8, switched_T = X9,
         resp = X10, acc = X11, RT = X12)

# some more data cleaning to get the final dataframe we will use for analysis
data <- data %>%
  select(-tLoc, -tOrient, -resp, -switched_T) %>% # remove some irrelevant variables
  mutate(epoch = ceiling(block/4)) %>% # this is a recoding of block (epoch 1 = blocks 1-4, epoch 2 = blocks 5-8) - useful for figures.
  mutate(phase = if_else(block <= 20, 1, 2)) %>% # new phase variable - standard = 1, with arrow = 2.
  select(subj, phase, epoch, block:patType, everything()) # reorder variables

save(data, file = "CCC03_data.RData")

