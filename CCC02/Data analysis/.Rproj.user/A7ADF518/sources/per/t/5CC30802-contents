rm(list=ls())
library(tidyverse)
library(broom)

fnams <- list.files("CSV Data", "td", full.names = TRUE) # needed for reading data
subjs <- list.files("CSV Data", "td") # needed for identifying subject numbers

data <- NULL
for (subj in 1:length(fnams)) { 
  
  pData <- read_csv(fnams[subj], col_types = cols(), col_names = FALSE) # read the data from csv
  pData <- pData %>% 
    mutate(subj = substr(subjs[subj],1,2)) %>% 
    select(subj,everything())
  
  data <- rbind(data, pData) # combine data array with existing data
  
}

data <- rename(data, oldSNum = X1, phase = X2, block = X3, trial = X4, TT = X5, patType = X6,
               tQuad = X7, tLoc = X8, tOrient = X9, arrow = X10, resp = X11, acc = X12, RT = X13)

# mutate column based on blocked and control patterns
data <- data %>% 
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

data <- data %>% 
  select(-oldSNum, -phase, -tLoc, -tOrient, -resp) %>% # remove irrelevant variables
  mutate(epoch = ceiling(block/4)) %>% 
  mutate(phase = ceiling(block/20)) %>% 
  select(subj, phase, epoch, block:patType, patArrowP1, everything()) # reorder variables

factorCols <- c("subj","TT","patType", "patArrowP1", "tQuad")
data[factorCols] <- lapply(data[factorCols], factor)
data$TT <- recode_factor(data$TT, "1" = "repeated", "2" = "random")
data$patArrowP1 <- recode_factor(data$patArrowP1, "1" = "arrow", "2" = "no arrow")

# get basic stats
basic_stats <- data %>% 
  group_by(subj) %>% 
  summarise(mean_RT = mean(RT),
            mean_Acc = mean(acc!=9999),
            num_TO = sum(acc==9999),
            num_trials = n())

# remove inaccurate trials and outlier RTs
data_NO <- data %>%
  filter(acc==1) %>% 
  group_by(subj) %>% 
  mutate(zRT = scale(RT)) %>% 
  filter(between(zRT,-2.5, 2.5)) %>% 
  select(-zRT)


factorLbls <- c("Repeated: Arrow", "Random: Arrow", "Repeated: No arrow", "Random: No arrow")

# graphing effect across epochs, phase 1 and 2
data_NO %>% 
  group_by(TT, patArrowP1, epoch) %>% 
  summarise(meanRT = mean(RT)) %>% 
  ggplot(aes(x = epoch, y = meanRT, group = interaction(TT, patArrowP1))) +
  theme_classic(base_size = 14) +
  geom_line()+
  geom_point(aes(fill = interaction(TT, patArrowP1), shape = interaction(TT, patArrowP1)), size = 3) +
  scale_fill_manual(name = "",
                    labels = factorLbls,
                    values=c("black", "black", "white", "white")) +
  scale_shape_manual(name = "",
                     labels = factorLbls,
                     values = c(21,22,21,22))+
  scale_x_continuous(limits = c(1,10), breaks = 1:10)
  
# plot phase 2 as columns
data_NO %>% 
  filter(epoch %in% 6:8) %>% 
  group_by(TT, patArrowP1) %>% 
  summarise(meanRT = mean(RT)) %>% 
  ggplot(aes(x = patArrowP1, y = meanRT, fill = TT)) +
  theme_classic(base_size = 14) +
  geom_col(position = position_dodge(), colour = "black") +
  coord_cartesian(ylim = c(1700,2300)) +
  scale_fill_manual(name = "Patern type", values = c("black","white")) +
  xlab("Phase 1 training condition") +
  ylab("RT in ms") +
  ggtitle("Phase 2 RT as a function of Phase 1 training with/without an arrow cue")
  
data_NO %>% 
  filter(phase == 2) %>% 
  group_by(subj,TT, patArrowP1) %>% 
  summarise(meanRT = mean(RT)) %>% 
  pivot_wider(names_from = c(TT, patArrowP1), values_from = meanRT) %>% 
  write_csv("dataForJasp.csv")

# data output for student
data_NO %>% 
  group_by(subj, TT, patArrowP1, epoch) %>% 
  summarise(meanRT = mean(RT)) %>% 
  pivot_wider(names_from = c(TT, patArrowP1, epoch), values_from = meanRT) %>% 
  left_join(basic_stats, by = "subj") %>% 
  select(subj, mean_RT:num_trials, everything()) %>% 
  write_csv("data_output.csv")

# ANOVA
data_NO %>% 
  filter(phase == 2) %>% 
  group_by(subj, TT, patArrowP1, epoch) %>% 
  summarise(meanRT = mean(RT)) %>% 
  aov(meanRT ~ (TT*patArrowP1*epoch) + Error(subj/(TT*patArrowP1*epoch)), data = .) %>% 
  tidy()

                      