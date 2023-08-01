library(tidyverse)
library(MASS)
library(lme4)
library(psych)

# import data
# all saved in the same csv file

pilot_materials_ug <- read_csv('data-validation-butterworth1.csv')

# Information about variables
# ID: Prolific ID
# H1_independent rate_4 and so on: likelihood for each horse of winning the race, on a 10-point scale
# Horse1_Position and so on: position of each horse (relative to the others) of winning a race (i.e., assuming the four of them are competing against one another)
# Bet: Whether participants had bet on horse races before (1 = No, 2 = Yes)
# Expertise: How much participants agree with the statement 'I am an expert on horse races'  on a 5-point scale (1: Strongly disagree, 5: Strongly agree)

# Questions to answer
# H1: Is there a difference in the likelihood of each horse to win individually depending on the description?
# H2: Is there a difference in rankings attributable to horses' description?

# H1 to answer = score_winning ~ horse (lm), random intercept by participant, add betting behaviour + expertise to control for confounds (i.e., a max model and without these two variables)
# H2 to answer = position ~ horse (ordinal regression), random intercept by participant, add betting behaviour + expertise to control for confounds (i.e., a max model and without these two variables)

# Data wrangling

# we to change columns to rows for horses and their position

pilot_materials_ug_rank <- pilot_materials_ug %>% pivot_longer(c(Horse1_Position, Horse2_Position, Horse3_Position, Horse4_Position),
                                                    names_to = "Horse", values_to = "Position") %>%
  mutate(
    Horse = case_when(Horse == "Horse1_Position" ~ "Horse1",
                      Horse == "Horse2_Position" ~ "Horse2",
                      Horse == "Horse3_Position" ~ "Horse3",
                      Horse == "Horse4_Position" ~ "Horse4")
  ) %>%
  dplyr::select(ID, Horse, Position, Bet, Expertise)


# we also want a column for score and another one for horse

pilot_materials_ug_score <- pilot_materials_ug %>% pivot_longer(c(H1_independent, H2_independent, H3_independent, H4_independent),
                                                    names_to = "Horse", values_to = "Score_winning") %>%
  mutate(
    Horse = case_when(Horse == "H1_independent" ~ "Horse1",
                      Horse == "H2_independent" ~ "Horse2",
                      Horse == "H3_independent" ~ "Horse3",
                      Horse == "H4_independent" ~ "Horse4")
  ) %>%
  dplyr::select(ID, Horse, Score_winning, Bet, Expertise)

# merge

pilot_materials_ug <- left_join(pilot_materials_ug_rank, pilot_materials_ug_score)

# wrang data

pilot_materials_ug <- pilot_materials_ug %>%
  mutate(
    Position = as.factor(Position),
    Horse = as.factor(Horse),
    ID = as.factor(ID),
    #Bet = ifelse(pilot_materials_ug$Bet == 1, 'No', 'Yes'),
    Expertise = case_when(
      pilot_materials_ug$Expertise == 1 ~ 'Strongly disagree',
      pilot_materials_ug$Expertise == 2 ~ 'Somewhat disagree',
      pilot_materials_ug$Expertise == 3 ~ 'Neither',
      pilot_materials_ug$Expertise == 4 ~ 'Somewhat agree',
      pilot_materials_ug$Expertise == 5 ~ 'Strongly agree',
      TRUE ~ 'Boh'
    )
  ) 

# summary statistics

table(pilot_materials_ug$Score_winning, pilot_materials_ug$Horse)


# let's see how many people are into betting

pilot_materials_ug  %>%
  filter(!duplicated(ID)) %>%
  count(Bet)

pilot_materials_ug  %>%
  filter(!duplicated(ID)) %>%
  count(Expertise)

psych::describeBy(pilot_materials_ug$Score_winning, pilot_materials_ug$Horse)


# H1

score_model <- lmer(Score_winning ~ Horse + Bet + Expertise +
       (1 | ID),
     data = pilot_materials_ug_score)

summary(score_model)

anova(score_model)

# H2

model_rank <- polr(as.factor(Position) ~ Horse, data = pilot_materials_ug_rank)
summary(model_rank)

newdata <- data.frame(Horse = rep(c("Horse1", "Horse2", "Horse3", "Horse4"), 1)
                     # Bet = rep(c("Yes", "No"), 6),
                      #Expertise = rep(c("Strongly disagree", "Somewhat disagree", "Somewhat agree"),4)
                     )

(phat <- predict(object = model_rank, newdata, type="p"))

# one of the issues is that, interestingly, horse 1 tends to rank 1, and horse 4 tends to rank 4... meaning that maybe people aren't moving the horses as much
# how can we control for this?

# one way is checking whether individual rankings correlate with the comparison
# however nb that in the individual scoring there are 10 levels, but we only have 4 ranks
# un poco chapucero, but we could aggregare scores into ranks (e.g., 1-3 equals the 4th position)
# I would still wonder whether one of the categories should be more permissible than the other

pilot_materials_ug_agg <- pilot_materials_ug %>%
  mutate(
    agg_rank = case_when(
      Score_winning <= 3 ~ 4,
      Score_winning >= 4 & Score_winning < 7 ~ 3,
      Score_winning > 6 & Score_winning < 9 ~ 2,
      Score_winning > 8 ~ 1
    )
  )

test <- polr(as.factor(agg_rank) ~ Horse, data = pilot_materials_ug_agg)
