library(tidyverse)
library(MASS)

# import data
# all saved in the same csv file

pilot_materials <- read_csv('data-validation-attheraces.csv')

# we to change columns to rows for horses and their position

pilot_materials <- pilot_materials %>% pivot_longer(c(Horse1_Position, Horse2_Position, Horse3_Position, Horse4_Position),
                                                    names_to = "Horse", values_to = "Position")

pilot_materials <- pilot_materials %>%
  mutate(
    Position = as.factor(Position),
    Horse = as.factor(Horse),
    Bet = ifelse(pilot_materials$`Have you bet on horse races before?` == 1, 'No', 'Yes'),
    Knowledge = case_when(
      pilot_materials$`How much do you agree with the statement 'I am an expert in horse races'?` == 1 ~ 'Strongly disagree',
      pilot_materials$`How much do you agree with the statement 'I am an expert in horse races'?` == 2 ~ 'Somewhat disagree',
      pilot_materials$`How much do you agree with the statement 'I am an expert in horse races'?` == 3 ~ 'Neither',
      pilot_materials$`How much do you agree with the statement 'I am an expert in horse races'?` == 4 ~ 'Somewhat agree',
      pilot_materials$`How much do you agree with the statement 'I am an expert in horse races'?` == 5 ~ 'Strongly agree',
      TRUE ~ 'Boh'
    )
  ) 

# summary statistics

table(pilot_materials$Position, pilot_materials$Horse)

# let's see how many people are into betting

pilot_materials  %>%
  filter(!duplicated(ID)) %>%
  count(Bet)

pilot_materials  %>%
  filter(!duplicated(ID)) %>%
  count(Knowledge)


# analyse ranks
# ordered logistic regression


model_rank <- polr(Position ~ Horse, data = pilot_materials)
summary(model_rank)

newdata <- data.frame(Horse = c("Horse1_Position", "Horse2_Position", "Horse3_Position", "Horse4_Position"))

(phat <- predict(object = model_rank, newdata, type="p"))

# it looks like horse 3 is more likely to be ranked 4 (prob = 0.64), horse 1 is more likely to be ranked as 1 or 2, same goes for horse 2, horse 4 is the only one with somehow spread distributions

### NEW TEST HORSES

pilot_materials_2 <- read_csv('second-pilot-materials.csv')

# we to change columns to rows for horses and their position

pilot_materials_2 <- pilot_materials_2 %>% pivot_longer(c(Horse1_Position, Horse2_Position, Horse3_Position, Horse4_Position),
                                                    names_to = "Horse", values_to = "Position")

pilot_materials_2 <- pilot_materials_2 %>%
  mutate(
    Position = as.factor(Position),
    Horse = as.factor(Horse),
    Bet = ifelse(pilot_materials_2$`Have you bet on horse races before?` == 1, 'No', 'Yes'),
    Knowledge = case_when(
      pilot_materials_2$`How much do you agree with the statement 'I am an expert in horse races'?` == 1 ~ 'Strongly disagree',
      pilot_materials_2$`How much do you agree with the statement 'I am an expert in horse races'?` == 2 ~ 'Somewhat disagree',
      pilot_materials_2$`How much do you agree with the statement 'I am an expert in horse races'?` == 3 ~ 'Neither',
      pilot_materials_2$`How much do you agree with the statement 'I am an expert in horse races'?` == 4 ~ 'Somewhat agree',
      pilot_materials_2$`How much do you agree with the statement 'I am an expert in horse races'?` == 5 ~ 'Strongly agree',
      TRUE ~ 'Boh'
    )
  ) 

# summary statistics

table(pilot_materials_2$Position, pilot_materials_2$Horse)

# let's see how many people are into betting

pilot_materials_2  %>%
  filter(!duplicated(ID)) %>%
  count(Bet)

pilot_materials_2  %>%
  filter(!duplicated(ID)) %>%
  count(Knowledge)


# analyse ranks
# ordered logistic regression


model_rank2 <- polr(Position ~ Horse + Bet + Knowledge, data = pilot_materials_2)
summary(model_rank2)

newdata2 <- data.frame(Horse = rep(c("Horse1_Position", "Horse2_Position", "Horse3_Position", "Horse4_Position"), 3),
                       Bet = rep(c("Yes", "No"), 6),
                       Knowledge = rep(c("Strongly disagree", "Somewhat disagree", "Neither"),4))


(phat <- predict(object = model_rank2, newdata2, type="p"))


