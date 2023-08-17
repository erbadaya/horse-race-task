library(tidyverse)
library(MASS)
library(lme4)
library(psych)

# import data
# all saved in the same csv file

dval_descriptions <- read_csv('./stimuli/horse-descriptions/validation/data-validation-butterworth1.csv')

# Information about variables
# ID: Prolific ID
# H1_independent rate_4 and so on: likelihood for each horse of winning the race, on a 10-point scale
# Horse1_Position and so on: position of each horse (relative to the others) of winning a race (i.e., assuming the four of them are competing against one another)
# Bet: Whether participants had bet on horse races before (1 = No, 2 = Yes)
# Expertise: How much participants agree with the statement 'I am an expert on horse races'  on a 5-point scale (1: Strongly disagree, 5: Strongly agree)

# Questions to answer
# H1: Is there a difference in the likelihood of each horse to win individually depending on the description?
# H2: Is there a difference in rankings attributable to horses' description?

# H1 to answer = score_winning ~ horse 
# H2 to answer = position ~ horse (ordinal regression)

# Data wrangling

# we to change columns to rows for horses and their position

dval_descriptionsrank <- dval_descriptions %>% pivot_longer(c(Horse1_Position, Horse2_Position, Horse3_Position, Horse4_Position),
                                                    names_to = "Horse", values_to = "Position") %>%
  mutate(
    Horse = case_when(Horse == "Horse1_Position" ~ "Horse1",
                      Horse == "Horse2_Position" ~ "Horse2",
                      Horse == "Horse3_Position" ~ "Horse3",
                      Horse == "Horse4_Position" ~ "Horse4")
  ) %>%
  dplyr::select(ID, Horse, Position, Bet, Expertise)


# we also want a column for score and another one for horse

dval_descriptionsscore <- dval_descriptions %>% pivot_longer(c(H1_independent, H2_independent, H3_independent, H4_independent),
                                                    names_to = "Horse", values_to = "Score_winning") %>%
  mutate(
    Horse = case_when(Horse == "H1_independent" ~ "Horse1",
                      Horse == "H2_independent" ~ "Horse2",
                      Horse == "H3_independent" ~ "Horse3",
                      Horse == "H4_independent" ~ "Horse4")
  ) %>%
  dplyr::select(ID, Horse, Score_winning, Bet, Expertise)

# merge

dval_descriptions <- left_join(dval_descriptionsrank, dval_descriptionsscore)

# wrang data

dval_descriptions <- dval_descriptions %>%
  mutate(
    Position = as.factor(Position),
    Horse = as.factor(Horse),
    ID = as.factor(ID),
    #Bet = ifelse(pilot_materials_ug$Bet == 1, 'No', 'Yes'),
    Expertise = case_when(
      dval_descriptions$Expertise == 1 ~ 'Strongly disagree',
      dval_descriptions$Expertise == 2 ~ 'Somewhat disagree',
      dval_descriptions$Expertise == 3 ~ 'Neither',
      dval_descriptions$Expertise == 4 ~ 'Somewhat agree',
      dval_descriptions$Expertise == 5 ~ 'Strongly agree'
    )
  ) 

# summary statistics

table(dval_descriptions$Score_winning, dval_descriptions$Horse)


# let's see how many people are into betting

dval_descriptions  %>%
  filter(!duplicated(ID)) %>%
  count(Bet)

dval_descriptions  %>%
  filter(!duplicated(ID)) %>%
  count(Expertise)

psych::describeBy(dval_descriptions$Score_winning, dval_descriptions$Horse)


# Individual likelihood of winning

summary(aov(Score_winning ~ Horse, data = dval_descriptionsscore))


# Ranking data

model_rank <- polr(as.factor(Position) ~ Horse, data = dval_descriptionsrank)
summary(model_rank)

newdata <- data.frame(Horse = rep(c("Horse1", "Horse2", "Horse3", "Horse4"), 1)
                     # Bet = rep(c("Yes", "No"), 6),
                      #Expertise = rep(c("Strongly disagree", "Somewhat disagree", "Somewhat agree"),4)
                     )

(phat <- predict(object = model_rank, newdata, type="p"))


