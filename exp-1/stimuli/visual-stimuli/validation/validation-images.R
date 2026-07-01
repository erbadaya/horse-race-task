# we showed 10 participants (who did not take part in experiment 1) eight horse pictures on Qualtrics
# participants' task was to rate the likelihood of each individual horse picture to win a race
# and then they had to rank them in the order they thought they would cross the finish line if they all participated in the same race

set.seed(123)

library(tidyverse)
library(MASS)
library(car)
library(lme4)
library(polr)

visual <- read_csv('./stimuli/visual-stimuli/validation/validation-images.csv')

# move ranking so it's one column the position the other is the horse

visual_pos <- visual %>%
  pivot_longer(c(2:9), names_to = "Horse", values_to = "Position")

visual_rank <- visual %>%
  pivot_longer(c(10:17), names_to = "Horse", values_to = "Rank")

# Individual likelihood to win a race

summary(aov(Position ~ Horse, data = visual_pos))

# Ranking data 

visual_rank <- visual_rank %>%
  mutate(
    Horse = as.factor(Horse),
    Rank = as.factor(Rank)
  )

model_rank <- polr(Rank ~ Horse, data = visual_rank)
summary(model_rank)
newdata <- data.frame(Horse =levels(visual_rank$Horse))

(phat <- predict(object = model_rank, newdata, type="p"))

# based on these results, we selected those horses for which their individual average likelihood was similar
