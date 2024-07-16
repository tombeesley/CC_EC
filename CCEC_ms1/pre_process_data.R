rm(list=ls())
library(tidyverse)
library(magrittr)
library(broom)

# Compile Experiment 1 data -----------------------------------------------

fnams_exp1 <- list.files(path = "../CCC01/Data analysis/CSV Data/",
                         pattern = "td",
                         full.names = TRUE) # needed for reading data
subjs_exp1 <- list.files(path = "../CCC01/Data analysis/CSV Data/",
                         pattern = "td") # needed for identifying subject numbers
detnams_exp1 <- list.files(path = "../CCC01/Data analysis/CSV Data/",
                           pattern = "det",
                           full.names = TRUE) # needed for reading data

data <- NULL
for (subj in 1:length(fnams_exp1)) {

  pData <- read_csv(fnams_exp1[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>%
    mutate(subj = substr(subjs_exp1[subj],1,str_length(subjs_exp1[subj])-7)) %>%
    select(subj,everything())

  # get subj details data
  subj_details <- read_csv(detnams_exp1[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  subj_details <- str_split(subj_details, pattern = " ", simplify = TRUE)

  pData %<>%
    mutate(age = subj_details[1],
           gender = subj_details[2])

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
  select(-resp) %>% # remove irrelevant variables
  mutate(epoch = ceiling(block/4),
         phase = ceiling(block/20),
         exp = "CCC01") %>%
  select(exp, subj, age, gender, phase, epoch, block:patType, everything()) # reorder variables

data <- data %>%
  mutate(TT = case_when(TT == 1 & switched_T == 0 ~ "repeated_con",
                        TT == 1 & switched_T == 1 ~ "repeated_incon",
                        TT == 2 & switched_T == 0 ~ "random_con",
                        TT == 2 & switched_T == 1 ~ "random_incon"))

data_exp1 <- data

# Compile Experiment 2 data -----------------------------------------------

fnams_exp2 <- list.files(path = "../CCC02/Data analysis/CSV Data/",
                         pattern = "td",
                         full.names = TRUE) # needed for reading data
subjs_exp2 <- list.files(path = "../CCC02/Data analysis/CSV Data/",
                         pattern = "td") # needed for identifying subject numbers

detnams_exp2 <- list.files(path = "../CCC02/Data analysis/CSV Data/",
                           pattern = "det",
                           full.names = TRUE) # needed for reading data

data <- NULL
for (subj in 1:length(fnams_exp2)) {

  pData <- read_csv(fnams_exp2[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>%
    mutate(subj = substr(subjs_exp2[subj],1,2)) %>%
    select(subj,everything())

  # get subj details data
  subj_details <- read_csv(detnams_exp2[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  subj_details <- str_split(subj_details, pattern = " ", simplify = TRUE)

  pData %<>%
    mutate(age = subj_details[1],
           gender = subj_details[2])

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
  mutate(patArrowP1 = as.factor(
    case_when(
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
  select(-oldSNum, -phase, -resp) %>% # remove irrelevant variables
  mutate(epoch = ceiling(block/4),
         phase = ceiling(block/20),
         exp = "CCC02",
         TT = recode(TT, "1" = "repeated", "2" = "random"),
         patArrowP1 = recode(patArrowP1, "1" = "arrow", "2" = "no arrow")) %>%
  select(exp, subj, age, gender, phase, epoch, block:patType, patArrowP1, everything()) # reorder variables

data_exp2 <- data


# Compile Experiment 3 data -----------------------------------------------

fnams_exp3 <- list.files(path = "../CCC03/Data analysis/CSV Data/",
                         pattern = "td",
                         full.names = TRUE) # needed for reading data
subjs_exp3 <- list.files(path = "../CCC03/Data analysis/CSV Data/",
                         pattern = "td") # needed for identifying subject numbers
detnams_exp3 <- list.files(path = "../CCC03/Data analysis/CSV Data/",
                           pattern = "det",
                           full.names = TRUE) # needed for reading data

data <- NULL
for (subj in 1:length(fnams_exp3)) {

  pData <- read_csv(fnams_exp3[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>%
    mutate(subj = substr(subjs_exp3[subj],1,3)) %>%
    select(subj,everything())

  print(subj)

  # get subj details data
  subj_details <- read_csv(detnams_exp3[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  subj_details <- str_split(subj_details, pattern = " ", simplify = TRUE)

  pData %<>%
    mutate(age = subj_details[1],
           gender = subj_details[2])

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

# remove participant 215 as they failed to complete the experiment

data <-
  data %>%
  filter(subj != "215")


# some more data cleaning to get the final dataframe we will use for analysis

# need to use rep as P1 has 8 trials per block, and P2 16.)
data <-
  data %>%
  group_by(subj) %>%
  mutate(epoch = rep(1:10, each = 32))

data <-
  data %>%
  select(-resp, -switched_T) %>% # remove some irrelevant variables
  mutate(phase = if_else(block <= 20, 1, 2), # new phase variable - standard = 1, with arrow = 2.
         exp = "CCC03",
         TT = recode(TT, "1" = "repeated", "2" = "random",
                     "3" = "global (local random)", "4" = "local (global random)")) %>%
  select(exp, subj, age, gender, phase, epoch, block:patType, everything()) # reorder variables

data_exp3 <- data


# Join experiments together and tidy -----------------------------------------------

all_data <-
  bind_rows(data_exp1, data_exp2, data_exp3)

# arrange columns
all_data %<>%
  select(exp:patType, patArrowP1, arrow, everything())

all_data %<>%
  mutate(timeout = case_when(acc == 9999 ~ 1, # set new timeout column based on accuracy of 9999
                             acc < 9999 ~ 0), .before = acc,
         acc = ifelse(timeout == 1, NA, acc), # use timeout column to set acc and RT to NA
         RT = ifelse(timeout == 1, NA, RT),
         subj = paste0(exp, "_", subj))

# clean up column values
all_data %<>%
  mutate(gender = recode(gender, "F" = "f", "Female" = "f", "female" = "f", "M" = "m", "Male" = "m", "male" = "m"),
         age = as.numeric(age))

# make relevant columns factors
factorCols <- colnames(all_data[c(1,2,4:14)])
all_data[factorCols] <- lapply(all_data[factorCols], factor)

saveRDS(all_data, "combinedData.rds")
