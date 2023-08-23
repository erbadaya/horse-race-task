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

temp <- list.files(path = "./data/", pattern = "*.csv")
ppt_id <- length(temp)
dexp1 <- data.frame()

for(i in 1:ppt_id){
  FILE <- read_csv(paste("./data/",temp[i], sep = ""), skip = 3) # read in each file, skip the first three lines (information about the session of Testable)
  FILE <- FILE %>% filter(!condition5 == "id") # anonymise data
  FILE$ppt <- i # add participant number, anonymous
  dexp1 <- rbind(dexp1, FILE)
  print(i)
}

rm(FILE, i, ppt_id, temp)
#write.csv(dexp1, "./data/horse_race_exp1_rawdata.csv")

#dexp1 <- read_csv("./data/horse_race_exp1_rawdata.csv")

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
    ),
    trial_status = case_when(
      trial_status == "attention-check-numberspeakers"  ~ "attention_check_numberspeakers",
      TRUE ~ trial_status
    )) %>%
  fill(expertise_betting, .direction = 'updown')

# separate into bets & questionnaire

dexp1bet <- dexp1 %>% filter(trial_status == 'bet' | trial_status == "finalbet" | trial_status == "finalbet_error")
dexp1quest <- dexp1 %>% filter(trial_type == 'form') %>%
  filter(trial_status %in% c('dimension_solidarity_nonnative', 'dimension_solidarity_native',
                             'dimension_status_nonnative', 'dimension_status_native',
                             'dimension_affect_nonnative', 'dimension_affect_native',
                             'dimension_rest_nonnative', 'dimension_rest_native',
                             'exposure', 'naturalness'))

dexp1checks <- dexp1 %>% filter(trial_type == 'form') %>%
  filter(trial_status %in% c('bet-experience-binary', 'bet-knowledge',
                             'naturalness',
                             'expertise_betting', "attention_check_numberspeakers"))


# BETS

dexp1bet <- select(dexp1bet, -c('correct', 'question'))

# nb participants are given the chance to modify their bets in the fifth trial, once they have heard all horses descriptions
# in final-bet, order of responses is apocalypse | blackblade | firewalker | silversky
# if in final-bet they had bet more than 200 (which they shouldn't), they are sent to the same window so they can fix it
# therefore, we first need to check whether participants did this or not

dexp1bet <- dexp1bet %>% group_by(ppt) %>% mutate(error = ifelse(trial_status == 'finalbet_error', 1, NA)) %>% fill(error, .direction = 'updown') %>% 
  mutate(response = case_when(error == 1 & trial_status == "finalbet" ~ NA, TRUE ~ response)) %>% ungroup() %>% drop_na(response)

dexp1bet_final <- dexp1bet %>% filter(trial_status == 'finalbet' | trial_status == 'finalbet_error') %>% select(-c("description")) %>%
  separate_wider_delim(response, delim = "|", names = c("apocalypse","blackblade","firewalker","silversky"), too_few = "align_start") %>%
  pivot_longer(c("apocalypse","blackblade","firewalker","silversky"), names_to = "description", values_to = "response") %>%
  mutate(response_final = response) %>% select(c(ppt, description, response_final))

dexp1bet <- dexp1bet %>% filter(trial_status == 'bet') %>% mutate(response_bet = response) %>% select(-c(response))

dexp1bet <- left_join(dexp1bet, dexp1bet_final)

# check whether they bet the same the last time and in their first bet, and keep always the last bet if there is a mismatch
# technically we asked them to re-write their bets in final bet even if they wanted to bet the same, so we could just use 'finalbet'


dexp1bet <- dexp1bet %>%
  mutate(change = ifelse(response_bet == response_final, 'same', 'diff')) %>%
  mutate(money = ifelse(change == 'same', response_final, response_final))
  

# LANGUAGE ATTITUDES QUESTIONNAIRE

# need to separate the questions for language attitudes
# note that the order of presentation of items per dimension is randomised by ppt
# the key word per item is the second word in each question

dexp1quest <- dexp1quest %>%
  separate_wider_delim(question, delim = ";", names = c("a","b","c","d","e","f", "exposure", "naturalness"), too_few = "align_start") %>% # separate questions
# separate answers
  mutate(
    a = ifelse(is.na(a) == FALSE, word(a, 2), NA), 
    b = ifelse(is.na(b) == FALSE, word(b, 2), NA),
    c = ifelse(is.na(c) == FALSE, word(c, 2), NA),
    d = ifelse(is.na(d) == FALSE, word(d, 2), NA),
    e = ifelse(is.na(e) == FALSE, word(e, 2), NA),
    f = ifelse(is.na(f) == FALSE, word(f, 2), NA),
    exposure = "exposure",
    naturalness = "naturalness"
  ) %>% 
  pivot_longer(cols = c("a","b","c","d","e","f", "exposure", "naturalness"), names_to = 'ques', values_to = 'item') %>% mutate(item = gsub('<b>', '',  item)) %>% mutate(item = gsub('</b>', '',  item)) %>%
  mutate(
    exposure = ifelse(trial_status=="exposure", response, NA),
    naturalness = ifelse(trial_status == "naturalness", response, NA)
  ) %>%
  separate_wider_delim(response, delim = "_", names = c("a","b","c","d","e","f"), too_few = "align_start") %>%
  pivot_longer(cols =  c("a","b","c","d","e","f", "exposure", "naturalness"), names_to = 'ans', values_to = 'score') %>%
  filter(ques == ans) %>% drop_na(score) %>% drop_na(item) %>%
  # make it into wider
  pivot_wider(names_from = item, values_from = score) %>%
  group_by(ppt, speaker) %>% fill(-c(1:10), .direction = 'updown') %>% filter(!duplicated(ppt)) %>%
  select(-c('description', 'delivery', 'list', 'correct', 'ans', 'ques', 'trial_type', 'trial_status', 'speaker')) %>% ungroup()

# Dragojevic & Giles (2016) and Foucart & Hartsuiker (2021) averaged to get scores
# Therefore, average score by participant and by speaker
# affect = annoyed + interested + irritated + enthusiastic + frustrated + happy / 6 (3 negative affect, 3 positive affect)
# status = intelligent + educated + smart + successful + competent / 5
# solidarity =  sociable + friendly + pleasant + nice + honest / 5

dexp1quest <- dexp1quest %>%
  mutate(across(c(3:24), as.numeric)) %>%
  mutate(
    affect = (annoyed + interested + irritated + enthusiastic + frustrated + happy) / 6,
    status = (intelligent + educated + smart + successful + competent) / 5,
    solidarity =  (sociable + friendly + pleasant + nice + honest) / 5
  )
  

# final df: one row per ppt, with all the scores

# LAST CHECKS

# from long to wide

dexp1checks 

tt <- dexp1checks %>%
  select(c('ppt', 'trial_status', 'response','expertise_betting')) %>%
  pivot_wider(names_from = trial_status, values_from = response) %>% 
  mutate(
    IN_ANALYSIS_ATTENTION = case_when(
      attention_check_numberspeakers == "2" ~ "YES",
      TRUE ~ "NO"
    ),
    IN_ANALYSIS_NATURALNESS = case_when(
      trial_status == "naturalness" & response > 3  ~ "YES",
      TRUE ~ "NO"
    ),
    IN_ANALYSIS_BET = case_when(
      expertise_betting < 3 ~ "YES",
      TRUE ~ "NO"
    )
  ) %>% mutate(
    IN_ANALYSIS = case_when(
      IN_ANALYSIS_ATTENTION == "NO" | IN_ANALYSIS_NATURALNESS == "NO" | IN_ANALYSIS_BET == "NO" ~ "NO",
      TRUE ~ "YES"
    )
  ) %>% select(c(ppt, IN_ANALYSIS))


# MERGE ALL INTO ONE

dexp1 <- left_join(dexp1bet, dexp1quest)
dexp1 <- left_join(dexp1, dexp1checks)

# Filter ppts

#dexp1 <- dexp1 %>% filter(IN_ANALYSIS == "YES")

# Contrast coding

dexp1 <- dexp1 %>%
  mutate(
    delivery = factor(delivery, levels = c("fluent", "disfluent")),
    speaker = factor(speaker, levels = c("native", "non-native")),
    money = as.numeric(money),
    across(c(13:33), as.numeric)
  )

contrasts(dexp1$delivery) <- c(-0.5, +0.5)
contrasts(dexp1$speaker) <- c(-0.5, +0.5)