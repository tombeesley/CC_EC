rm(list=ls())
library(tidyverse)
library(broom)

# Compile Experiment 1 data -----------------------------------------------

fnams_exp1 <- list.files(path = "../CCC01/Data analysis/CSV Data/",
                    pattern = "td",
                    full.names = TRUE) # needed for reading data
subjs_exp1 <- list.files(path = "../CCC01/Data analysis/CSV Data/",
                    pattern = "td") # needed for identifying subject numbers

data <- NULL
for (subj in 1:length(fnams_exp1)) {

  pData <- read_csv(fnams_exp1[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>%
    mutate(subj = substr(subjs_exp1[subj],1,str_length(subjs_exp1[subj])-7)) %>%
    select(subj,everything())

  data <- rbind(data, pData) # combine data array with existing data

}

data <-
  data %>%
  select(-X1) %>%
  rename(block = X2, trial = X3, TT = X4, patType = X5,
         tQuad = X6, tLoc = X7, tOrient = X8, switched_T = X9,
         resp = X10, acc = X11, RT = X12)

data <-
  data %>%
  select(-tLoc, -tOrient, -resp) %>% # remove irrelevant variables
  mutate(epoch = ceiling(block/4)) %>%
  mutate(phase = ceiling(block/20)) %>%
  select(subj, phase, epoch, block:patType, everything()) # reorder variables

factorCols <- c("subj","TT","patType", "tQuad")
data[factorCols] <- lapply(data[factorCols], factor)

data <- data %>%
  mutate(TT = case_when(TT == 1 & switched_T == 0 ~ "repeated (C)",
                        TT == 1 & switched_T == 1 ~ "repeated (I)",
                        TT == 2 ~ "random"))

data_exp1 <- data

# Compile Experiment 2 data -----------------------------------------------

fnams_exp2 <- list.files(path = "../CCC02/Data analysis/CSV Data/",
                         pattern = "td",
                         full.names = TRUE) # needed for reading data
subjs_exp2 <- list.files(path = "../CCC02/Data analysis/CSV Data/",
                         pattern = "td") # needed for identifying subject numbers

data <- NULL
for (subj in 1:length(fnams_exp2)) {

  pData <- read_csv(fnams_exp2[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>%
    mutate(subj = substr(subjs_exp2[subj],1,2)) %>%
    select(subj,everything())

  data <- rbind(data, pData) # combine data array with existing data

}

data <-
  rename(data, oldSNum = X1, phase = X2,
         block = X3, trial = X4, TT = X5,
         patType = X6, tQuad = X7, tLoc = X8,
         tOrient = X9, arrow = X10, resp = X11,
         acc = X12, RT = X13)

# mutate column based on blocked and control patterns
data <-
  data %>%
  mutate(patArrowP1 = as.factor(case_when(
    oldSNum %% 2 == 1 & TT==1 & tQuad %in% c(1,4) ~ 1,
    oldSNum %% 2 == 1 & TT==1 & tQuad %in% c(2,3) ~ 2,
    oldSNum %% 2 == 1 & TT==2 & tQuad %in% c(1,4) ~ 2,
    oldSNum %% 2 == 1 & TT==2 & tQuad %in% c(2,3) ~ 1,
    oldSNum %% 2 == 0 & TT==1 & tQuad %in% c(1,4) ~ 2,
    oldSNum %% 2 == 0 & TT==1 & tQuad %in% c(2,3) ~ 1,
    oldSNum %% 2 == 0 & TT==2 & tQuad %in% c(1,4) ~ 1,
    oldSNum %% 2 == 0 & TT==2 & tQuad %in% c(2,3) ~ 2,
  )))

data <-
  data %>%
  select(-oldSNum, -phase, -tLoc, -tOrient, -resp) %>% # remove irrelevant variables
  mutate(epoch = ceiling(block/4)) %>%
  mutate(phase = ceiling(block/20)) %>%
  select(subj, phase, epoch, block:patType, patArrowP1, everything()) # reorder variables

factorCols <- c("subj","TT","patType", "patArrowP1", "tQuad")
data[factorCols] <- lapply(data[factorCols], factor)
data$TT <- recode_factor(data$TT, "1" = "repeated", "2" = "random")
data$patArrowP1 <- recode_factor(data$patArrowP1, "1" = "arrow", "2" = "no arrow")

data_exp2 <- data


# Compile Experiment 3 data -----------------------------------------------

fnams_exp3 <- list.files(path = "../CCC03/Data analysis/CSV Data/",
                         pattern = "td",
                         full.names = TRUE) # needed for reading data
subjs_exp3 <- list.files(path = "../CCC03/Data analysis/CSV Data/",
                         pattern = "td") # needed for identifying subject numbers

data <- NULL
for (subj in 1:length(fnams_exp3)) {

  pData <- read_csv(fnams_exp3[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>%
    mutate(subj = substr(subjs_exp3[subj],1,2)) %>%
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

data_exp3 <- data
