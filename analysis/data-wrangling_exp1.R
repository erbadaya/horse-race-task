# data wrangling

set.seed(123)

# libraries

library(tidyverse)
library(lme4)
library(wesanderson)
library(stringr) 
library(here)

# load data
# to run only once
# merges all individual .csv files & anonymises data

# temp <- list.files(path = "./data/", pattern = "*.csv")
# ppt_id <- length(temp)
# dexp1 <- data.frame()
# 
# for(i in 1:ppt_id){
#   FILE <- read_csv(paste("./data/",temp[i], sep = "")) # read in each file, skip the first three lines (information about the session of Testable)
#   FILE$ppt <- i # add participant number, anonymous
#   # check that we have same number of columns (because of participants overbetting)
#   if(any(FILE$overbet=="yes", na.rm = TRUE)){
#     FILE <- FILE %>%
#       select(., c("ppt", "horse", "list", "delivery", "overbet", "speaker", "response", "horse3_fix",
#                   "horse2_fix", "horse1_fix", "horse0_fix", "trial_type", "trial_index"))
#     for (col in 1:ncol(FILE)){
#       colnames(FILE)[col] <-  sub("_fix*", "", colnames(FILE)[col])
#     }
#   } else {
#     FILE <- FILE %>%
#       select(., c("ppt", "horse", "list", "delivery", "overbet", "speaker", "response", "horse3_bet",
#                   "horse2_bet", "horse1_bet", "horse0_bet", "trial_type", "trial_index"))
#     for (col in 1:ncol(FILE)){
#       colnames(FILE)[col] <-  sub("_bet*", "", colnames(FILE)[col])
#   }}
#   FILE <- FILE %>% filter(!trial_index == 2) # anonymise data
#   dexp1 <- rbind(dexp1, FILE)
#   print(i)
# }
# 
# rm(FILE, i, ppt_id, temp)
# 
# write.csv(dexp1, "./data/horse_race_exp1_rawdata.csv")

# load all anonymised data

dexp1 <- read_csv("./data/exp1/horse_race_exp1_rawdata.csv") %>% select(-c(1))

# wrangle separately bet data from questionnaire data

# BETTING BEHAVIOUR
# we work with participants' last bet (regardless of what they put)
# if they do not go over a 100, this information is stored in horseX_bet
# if they go overbet, this information is stored in horseX_fix

dexp1bet <- dexp1 %>%
  filter(!is.na(overbet)) %>%
  group_by(ppt) %>%
  mutate(order_filter = seq_len(n()) - 1) %>% ungroup() %>%
  mutate(order_filter = paste("horse", order_filter, sep = "")) %>%
  pivot_longer(c(8:11), names_to = "order", values_to = "raw_money") %>%
  filter(order == order_filter)

# RSO stats help UGent suggests scaling money bet (previous issue: 0 variance for participant in LME, arguably because everyone is spending 100)

dexp1bet <- dexp1bet %>%
  group_by(ppt) %>%
  mutate(total_spent = sum(as.numeric(raw_money))) %>%
  mutate(scaled_money = ((raw_money * 100)/total_spent))


# LANGUAGE ATTITUDES QUESTIONNAIRE


dexp1lang <- dexp1 %>%
  filter(trial_index != 2) %>%
  filter(trial_type == "survey") %>% 
  filter(!is.na(speaker))

dexp1lang <- dexp1lang %>%
  mutate(response =  gsub('_native','',response)) %>%
  mutate(response =  gsub('_nonnative','',response)) %>%
  mutate(response =  gsub('"','',response)) %>%
  mutate(response =  gsub('}','',response)) %>%
  separate_wider_delim(response, delim = ",", names =  c("P1", "P2", "P3", "P4", "P5", "P6","P7", "P8", "P9", "P10",
                                                         "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20",
                                                         "P21", "P22", "P23", "P24")) %>%
  
  separate_wider_delim(c("P1", "P2", "P3", "P4", "P5", "P6","P7", "P8", "P9", "P10",
                          "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20",
                          "P21", "P22", "P23", "P24"), delim = ":", names = c("question", "answer"), names_sep = "_", too_few = "align_start") %>% 
  select_if(~ !any(is.na(.))) %>%
  pivot_longer(c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49), names_to = 'column', values_to = 'item') %>%
  pivot_longer(c(3:26), names_to = 'blah', values_to = 'score') %>%
  mutate(column = gsub('_question','',column), blah = gsub('_answer','',blah)) %>%
  filter(column == blah) %>% filter(!score== 'null') %>% select(!c(column, blah)) %>%
  pivot_wider(names_from = item, values_from = score) %>%
  mutate(across(c(9:28), as.numeric)) %>%
  mutate(speaker = ifelse(speaker == 'native', 'native', 'nonnative'))

# reverse score negative affect (i.e., annoyed, irritated, frustrated)

dexp1lang <- dexp1lang %>%
  mutate(
    annoyed = 10 - annoyed,
    irritated = 10 - irritated, 
    frustrated = 10 - frustrated
  )

# calculate means of lang attitudes

dexp1lang <- dexp1lang %>%
  mutate(
    affect = (annoyed + irritated + frustrated + interested + happy + enthusiastic) / 6,
    status = (intelligent + educated + smart + competent + successful) / 5,
    solidarity = (friendly + nice + pleasant + honest + sociable) / 5
  )

dexp1lang <- dexp1lang %>% 
  select(!c(horse0, horse1, horse2, horse3, trial_type, trial_index))

# ppt 257 filled the questionnaire the other way around (native for non-native)
# ppt 550 answered all questions as if for non-native (remove their data)

dexp1lang <- dexp1lang %>%
  mutate(
    speaker = case_when(
      ppt == 257 & speaker == 'native' ~ "nonnative",
      ppt == 257 & speaker == 'nonnative' ~ "native",
      TRUE  ~ speaker
    )
  ) %>%
  mutate(
    across(2:24, ~ ifelse(ppt == 550, NA, .))
  )

# CHECKS AND OPEN QUESTIONS

dexp1survey <- dexp1 %>%
  filter(trial_index != 2) %>%
  filter(trial_type == "survey") %>% 
  filter(is.na(speaker))


dexp1survey <- dexp1survey %>%
  mutate(response =  gsub('}','',response)) %>%
  separate_wider_delim(response, delim = ',"', names = c("P1", "P2", "P3", "P4", "P5", "P6","P7", "P8", "P9", "P10",
                                                         "P11", "P12", "P13", "P14", "P15", "P16")) %>%
  separate_wider_delim(c("P1", "P2", "P3", "P4", "P5", "P6","P7", "P8", "P9", "P10",
                         "P11", "P12", "P13", "P14", "P15", "P16"), delim = ":", names = c("question", "answer"), names_sep = "_", too_few = "align_start") %>% 
  select_if(~ !any(is.na(.))) %>%
  pivot_longer(c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32), names_to = 'column', values_to = 'item') %>%
  pivot_longer(c(2:17), names_to = 'blah', values_to = 'score') %>%
  mutate(column = gsub('_question','',column), blah = gsub('_answer','',blah)) %>%
  filter(column == blah) %>% filter(!score== 'null') %>% select(!c(column, blah)) %>%
  mutate(item =  gsub('"','',item)) %>%
  mutate(score =  gsub('"','',score)) %>%
  pivot_wider(names_from = item, values_from = score) %>%
  mutate(across(c(11, 12, 15, 16), as.numeric)) %>%
  mutate(
    bet_before = P1_Q4,
    expertise_betting = P1_Q5,
    IN_ANALYSIS_AUDIO = ifelse(naturalness_native < 4 | naturalness_nonnative < 4, "No", "Yes"),
    IN_ANALYSIS_EXPERTISE = ifelse(expertise_betting > 3, "No", "Yes")
  ) %>%
  select(!c(horse0, horse1, horse2, horse3, trial_type, trial_index)) %>%
  pivot_longer(c(9,10), names_to = 'question', values_to = 'naturalness') %>%
  separate_wider_delim(question, delim = '_', names = c("rly", "speaker")) %>%
  pivot_longer(c(5,6), names_to = 'question', values_to = 'exposure') %>%
  separate_wider_delim(question, delim = '_', names = c("opsi", "sp")) %>%
  mutate(sp = ifelse(sp == 'nn', 'nonnative', 'native')) %>%
  filter(sp == speaker) %>%
  select(!c(sp, rly, opsi))

# PUT ALL INFO IN ONE DF
# for filtering & analyses

dexp1lang <- left_join(dexp1lang, dexp1survey, by = 'ppt')
dexp1lang <- dexp1lang %>%  filter(speaker.x == speaker.y) %>% mutate(speaker = speaker.x) %>% select(!c(speaker.x, speaker.y))
dexp1bet <- left_join(dexp1bet, dexp1lang, by = c('ppt'))
dexp1bet <- dexp1bet %>% filter(speaker.x == speaker.y) %>% mutate(speaker = speaker.x) %>% select(!c(speaker.x, speaker.y))


# filter participants

dexp1bet_prereg <- dexp1bet %>%
  filter(IN_ANALYSIS_AUDIO == "Yes" & IN_ANALYSIS_EXPERTISE == "Yes")

dexp1lang_prereg <- dexp1lang %>%
  filter(IN_ANALYSIS_AUDIO == "Yes" & IN_ANALYSIS_EXPERTISE == "Yes")

# LAST CHECKS

# dexp1lang_prereg %>%
#   filter(!duplicated(ppt)) %>%
#   write.csv(., "./analysis/horse_race_exp1_surveydata.csv")

# additional: participants that need to be removed due to their post-experimental questionnaire answers (e.g., reported noticing the manipulation and the like)
# source: dexp1survey

# ppt 12: accent and clarity of speech
# ppt 23: fluency of language
# ppt 40: importance of what someone is saying v how they're saying it
# ppt 46: difference between confident speech and um-ing speech
# ppt 279: ums were removed from audio
# ppt 334: effect of fluency
# ppt 505: cuts on the audio
# ppt 586: there are cuts in the audio
# ppt 617: fluency and trust
# log: 24/10

dexp1bet <- dexp1bet %>%
  mutate(
    IN_ANALYSIS_RESPONSE = case_when(
      ppt == 12 ~ "No", 
      ppt == 23 ~ "No",
      ppt == 40 ~ "No", 
      ppt == 46 ~ "No", 
      ppt == 279 ~ "No", 
      ppt == 334 ~ "No", 
      ppt == 505 ~ "No", 
      ppt == 586 ~ "No", 
      ppt == 617 ~ "No",  
      TRUE  ~ "Yes"
    )
  )

dexp1lang <- dexp1lang %>%
  mutate(
    IN_ANALYSIS_RESPONSE = case_when(
      ppt == 12 ~ "No", 
      ppt == 23 ~ "No",
      ppt == 40 ~ "No", 
      ppt == 46 ~ "No", 
      ppt == 279 ~ "No", 
      ppt == 334 ~ "No", 
      ppt == 505 ~ "No", 
      ppt == 586 ~ "No", 
      ppt == 617 ~ "No",  
      TRUE  ~ "Yes"
    )
  )

# filter participants

dexp1bet_prereg <- dexp1bet %>%
  filter(IN_ANALYSIS_AUDIO == "Yes" & IN_ANALYSIS_EXPERTISE == "Yes" & IN_ANALYSIS_RESPONSE == "Yes")

dexp1lang_prereg <- dexp1lang %>%
  filter(IN_ANALYSIS_AUDIO == "Yes" & IN_ANALYSIS_EXPERTISE == "Yes" & IN_ANALYSIS_RESPONSE == "Yes")


# check list

# Contrast coding

dexp1bet_prereg <- dexp1bet_prereg %>%
  mutate(
    delivery = factor(delivery, levels = c("fluent", "disfluent")),
    speaker = factor(speaker, levels = c("native", "nonnative")),
    delivery_cont = factor(delivery, levels = c("fluent", "disfluent")),
    speaker_cont = factor(speaker, levels = c("native", "nonnative"))
  )

dexp1bet <- dexp1bet %>%
  mutate(
    delivery = factor(delivery, levels = c("fluent", "disfluent")),
    speaker = factor(speaker, levels = c("native", "nonnative")),
    delivery_cont = factor(delivery, levels = c("fluent", "disfluent")),
    speaker_cont = factor(speaker, levels = c("native", "nonnative"))
  )

contrasts(dexp1bet_prereg$delivery_cont) <- c(-0.5, +0.5)
contrasts(dexp1bet_prereg$speaker_cont) <- c(-0.5, +0.5)

contrasts(dexp1bet$delivery_cont) <- c(-0.5, +0.5)
contrasts(dexp1bet$speaker_cont) <- c(-0.5, +0.5)
