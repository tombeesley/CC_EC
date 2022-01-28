rm(list=ls())
library(tidyverse)
library(broom)

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

data <- data %>% 
  select(-X1) %>% 
  rename(block = X2, trial = X3, TT = X4, patType = X5,
         tQuad = X6, tLoc = X7, tOrient = X8, switched_T = X9, 
         resp = X10, acc = X11, RT = X12)

data <- data %>% 
  select(-tLoc, -tOrient, -resp) %>% # remove irrelevant variables
  mutate(epoch = ceiling(block/4)) %>% 
  mutate(phase = ceiling(block/20)) %>% 
  select(subj, phase, epoch, block:patType, everything()) # reorder variables

# factorCols <- c("subj","TT","patType", "tQuad")
# data[factorCols] <- lapply(data[factorCols], factor)

data <- data %>% 
  mutate(TT = case_when(TT == 1 & switched_T == 0 ~ "repeated (C)",
                        TT == 1 & switched_T == 1 ~ "repeated (I)",
                        TT == 2 ~ "random"))

# get basic stats
basic_stats <- data %>% 
  group_by(subj) %>% 
  summarise(mean_RT = mean(RT),
            mean_Acc = mean(acc!=9999),
            num_TO = sum(acc==9999),
            num_trials = n())

data_NO <- data %>%
  filter(acc==1) %>% 
  group_by(subj) %>% 
  mutate(zRT = scale(RT)) %>% 
  filter(between(zRT,-2.5, 2.5)) %>% 
  select(-zRT)


# graphing effect across epochs, phase 1 and 2
data_NO %>% 
  group_by(TT, epoch) %>% 
  summarise(meanRT = mean(RT)) %>% 
  ggplot(aes(x = epoch, y = meanRT, colour = TT)) +
  theme_classic(base_size = 14) +
  geom_line()+
  geom_point(size = 3) +
  scale_x_continuous(limits = c(1,10), breaks = 1:10)
   
# plot phase 2 as columns
data_NO %>% 
  filter(epoch %in% 6:8) %>% 
  group_by(TT) %>% 
  summarise(meanRT = mean(RT)) %>% 
  ggplot(aes(x = TT, y = meanRT, fill = TT)) +
  theme_classic(base_size = 14) +
  geom_col(position = position_dodge(), colour = "black")
  

# ANOVA PHASE 1
data_NO %>% 
  filter(phase == 1) %>% 
  group_by(subj, TT, epoch) %>% 
  summarise(meanRT = mean(RT)) %>% 
  aov(meanRT ~ (TT*epoch) + Error(subj/(TT*epoch)), data = .) %>% 
  tidy()

# ANOVA PHASE 2
data_NO %>% 
  filter(phase == 2) %>% 
  group_by(subj, TT, epoch) %>% 
  summarise(meanRT = mean(RT)) %>% 
  aov(meanRT ~ (TT*epoch) + Error(subj/(TT*epoch)), data = .) %>% 
  tidy()

# T-TESTS PHASE 2
data_NO %>% 
  filter(phase == 2) %>% 
  group_by(subj, TT) %>% 
  summarise(meanRT = mean(RT)) %>% 
  {tidy(pairwise.t.test(.$meanRT, .$TT, paired = TRUE))}


# check target quadrant frequency
data %>% 
  group_by(phase, TT, tQuad) %>% 
  summarise(n())

                      