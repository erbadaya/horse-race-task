library(tidyverse)
library(MASS)
library(lme4)
library(psych)

# import data
# all saved in the same csv file

pilot_materials_ug <- read_csv('data-validation-butterworth2.csv')

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

pilot_materials_ug_rank <- pilot_materials_ug %>% pivot_longer(c(SS_Position, FW_Position, BB_Position, A_Position),
                                                    names_to = "Horse", values_to = "Position") %>%
  mutate(
    Horse = case_when(Horse == "SS_Position" ~ "Silver Sky",
                      Horse == "FW_Position" ~ "Fire Walker",
                      Horse == "BB_Position" ~ "Black Blade",
                      Horse == "A_Position" ~ "Apocalypse")
  ) %>%
  dplyr::select(ID, Horse, Position, Bet, Expertise)


# we also want a column for score and another one for horse

pilot_materials_ug_score <- pilot_materials_ug %>% pivot_longer(c(SS_independent, FW_independent, BB_independent, A_independent),
                                                    names_to = "Horse", values_to = "Score_winning") %>%
  mutate(
    Horse = case_when(Horse == "SS_independent" ~ "Silver Sky",
                      Horse == "FW_independent" ~ "Fire Walker",
                      Horse == "BB_independent" ~ "Black Blade",
                      Horse == "A_independent" ~ "Apocalypse")
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
    # Expertise = case_when(
    #   pilot_materials_ug$Expertise == 1 ~ 'Strongly disagree',
    #   pilot_materials_ug$Expertise == 2 ~ 'Somewhat disagree',
    #   pilot_materials_ug$Expertise == 3 ~ 'Neither',
    #   pilot_materials_ug$Expertise == 4 ~ 'Somewhat agree',
    #   pilot_materials_ug$Expertise == 5 ~ 'Strongly agree',
    #   TRUE ~ 'Boh'
    # )
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

score_model <- lmer(Score_winning ~ Horse +
       (1 | ID),
     data = pilot_materials_ug_score)

summary(score_model)

anova(score_model)

summary(aov(Score_winning ~ Horse,
      data = pilot_materials_ug_score))
# H2

model_rank <- polr(as.factor(Position) ~ Horse, data = pilot_materials_ug_rank)
summary(model_rank)

newdata <- data.frame(Horse = rep(c("Silver Sky", "Fire Walker", "Black Blade", "Apocalypse"), 1)
                     # Bet = rep(c("Yes", "No"), 6),
                      #Expertise = rep(c("Strongly disagree", "Somewhat disagree", "Somewhat agree"),4)
                     )

(phat <- predict(object = model_rank, newdata, type="p"))

