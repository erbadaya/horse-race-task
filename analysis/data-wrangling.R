# data wrangling

set.seed(123)

# libraries

library(tidyverse)
library(lme4)
library(wesanderson)
library(stringr) 

# load data
# to run only once
# merges all individual .csv files & anonymises data

temp <- list.files(pattern = "*.csv")
ppt_id <- length(temp)
dexp1 <- data.frame()

for(i in 1:ppt_id){
  FILE <- read_csv(temp[i], skip = 3) # read in each file, skip the first three lines (information about the session of Testable)
  FILE$ppt <- i # add participant number, anonymous
  dexp1 <- rbind(dexp1, FILE)
  print(i)
}

rm(FILE, i, ppt_id, temp)
write.csv(dexp1, "horse_race_exp1_rawdata.csv")

# rename variables and keep only those that are relevant

dexp1 <- dexp1 %>% #  new name = old name
  rename(
    "trial_type" = "condition1",
    "description" = "condition2",
    "speaker" = "condition3",
    "delivery" = "condition4",
    "trial_status" = "condition5", # takes up values preview, audio, bet, move-on for the experiment (i.e., trial_type == 'critical') & attention-check, dimension_affect/status/solidarity/rest, bet-experience-binary, bet-knowledge, naturalness, and open-question for our questionnaires.
    "list" = "subjectGroup",
    'question' = 'responseRows_actual'
  )  %>%
  select(., c("trial_type", "description", "speaker", "delivery", "trial_status", "response", "ppt", "list", "correct", 'question'))

# mark those who correctly remembered how many speakers there were and their country of origin (might still need visual inspection in case someone made a typo we did not anticipate)
# likewise, we exclude participants who consider themselves experts in betting

dexp1 <- dexp1 %>%
  group_by(ppt) %>%
  mutate(
    expertise_betting = case_when(
      trial_status == "bet-knowledge" & response == "Strongly disagree" ~ 1,
      trial_status == "bet-knowledge" & response == "Somewhat disagree" ~ 2,
      trial_status == "bet-knowledge" & response == "Neither agree nor disagree" ~ 3,
      trial_status == "bet-knowledge" & response == "Somewhat agree" ~ 4,
      trial_status == "bet-knowledge" & response == "Strongly agree" ~ 5,
      TRUE ~ NA_real_
    )
  ) %>% fill(expertise_betting, .direction = 'updown')%>%
  mutate(
    IN_ANALYSIS = case_when(
      trial_status == "attention-check-numberspeakers" & correct == "1" ~ "YES",
      trial_status == "attention-check-coo-italy" & correct == "1" ~ "YES",
      trial_status == "attention-check-coo-uk"& correct == "1" ~ "YES",
      expertise_betting < 3 ~ "YES",
      TRUE ~ "NO",
    )
  )

# separate into bets & questionnaire

dexp1bet <- dexp1 %>% filter(trial_status == 'bet')
dexp1quest <- dexp1 %>% filter(trial_type == 'form') %>%
  filter(trial_status %in% c('dimension_solidarity_nonnative', 'dimension_solidarity_native',
                             'dimension_status_nonnative', 'dimension_status_native',
                             'dimension_affect_nonnative', 'dimension_affect_native',
                             'dimension_rest_nonnative', 'dimension_rest_native'))

dexp1checks <- dexp1 %>% filter(trial_type == 'form') %>%
  filter(trial_status %in% c('exposure-nn', 'exposure-n',
                             'bet-experience-binary', 'bet-knowledge',
                             'naturalness-native', 'naturalness-nonnative'))


# BETS

dexp1bet <- select(dexp1bet, -c('correct', 'question'))


# LANGUAGE ATTITUDES QUESTIONNAIRE

# need to separate the questions for language attitudes
# note that the order of presentation of items per dimension is randomised by ppt
# the key word per item is the second word in each question

dexp1quest <- dexp1quest %>%
  separate_wider_delim(question, delim = ";", names = c("a","b","c","d","e","f"), too_few = "align_start") %>% # separate questions
  separate_wider_delim(response, delim = "_", names = c("a_ans","b_ans","c_ans","d_ans","e_ans","f_ans"), too_few = "align_start") %>% # separate answers
  mutate(
    a = ifelse(is.na(a) == FALSE, word(a, 2), NA), 
    b = ifelse(is.na(b) == FALSE, word(b, 2), NA),
    c = ifelse(is.na(c) == FALSE, word(c, 2), NA),
    d = ifelse(is.na(d) == FALSE, word(d, 2), NA),
    e = ifelse(is.na(e) == FALSE, word(e, 2), NA),
    f = ifelse(is.na(f) == FALSE, word(f, 2), NA)
  ) %>% 
  pivot_longer(cols = c("a","b","c","d","e","f"), names_to = 'ques', values_to = 'item') %>%
  pivot_longer(cols = c("a_ans","b_ans","c_ans","d_ans","e_ans","f_ans"), names_to = 'ans', values_to = 'score') %>%
  mutate(ans = gsub('_ans', '', ans), item = paste(item, speaker, sep = "_")) %>% mutate(iden = ifelse(ques == ans, 1, 0)) %>% filter(iden == 1) %>% drop_na(score) %>%
  # make it into wider
  pivot_wider(names_from = item, values_from = score) %>%
  group_by(ppt) %>% fill(-c(1:13), .direction = 'updown') %>% filter(!duplicated(ppt)) %>%
  select(-c('description', 'delivery', 'list', 'correct', 'expertise_betting', 'IN_ANALYSIS', 'ans', 'ques', 'iden', 'trial_type', 'trial_status', 'speaker'))

# final df: one row per ppt, with all the scores

# LAST CHECKS

# from long to wide

dexp1checks <- dexp1checks %>%
  select(c('ppt', 'trial_status', 'response')) %>%
  pivot_wider(names_from = trial_status, values_from = response)

# MERGE ALL INTO ONE

dexp1 <- left_join(dexp1bet, dexp1quest)
dexp1 <- left_join(dexp1, dexp1checks)

