set.seed(123)

library(tidyverse)
library(lme4)

# load in data

# basically, load in all files and then merge into one
# run only once!
# and delete all the individual .csvs (they are not anonymised)
# temp <- list.files(pattern = "./stimuli/auditory-stimuli/validation/*.csv")
# 
# # put all the dfs in one and do some work
# # create an additional df for metainformation
# 
# dval <- data.frame()
# pptdsid <- length(temp)
# 
# for(i in temp){
#   FILE <- read_csv(i, skip = 3) # read in each file
#   FILE$participant_ID <- i
#   dval <- rbind(dval, FILE)
#   print(i)
# }
# 
# rm(FILE, i, pptdsid, temp)
# 
# names(dval)
# 
# # rename columns
# 
# dval <- dval %>% #  new name = old name
#   rename(
#     "trial_type" = "condition1",
#     "description" = "condition2",
#     "speaker" = "condition3",
#     "delivery" = "condition4",
#     "trial_status" = "condition5", # takes up values audio (learning phase), fluency (rate fluency), naturalness (rate naturalness), accentedness, and country of origin of the speaker
#     "list" = "subjectGroup"
#   ) %>%
#   mutate(
#     horse_speaker = paste(description, speaker) # this is a little trick for later
#   ) %>%
#   select(., c("trial_type", "description", "speaker", "delivery", "trial_status", "response", "participant_ID", "horse_speaker", "list")) # we do not need anything else for now
# 
# write.csv(dval, './stimuli/auditory-stimuli/validation/dval_audio.csv')

dval <- read_csv('./stimuli/auditory-stimuli/validation/dval_audio.csv')

# keep audio trials

dval <- dval %>%
  filter(trial_type %in% c("filler", "critical"))

# FLUENCY ANALYSIS

# descriptive stats
# ignoring horse description for now (because we are including it as random effect in our model anyways)

dval %>%
  filter(trial_status == "fluency") %>%
  group_by(speaker, delivery) %>%
  summarise(
    obvs = n(),
    avg_resp = mean(as.numeric(response)),
    sd_resp = sd(response)
  )

# is fluency affected by manner of delivery and speaker?

mdl_validation <- dval %>%
  filter(trial_status == "fluency") %>%
  lmer(
    as.numeric(response) ~ delivery * speaker +
      (1 | participant_ID) + (1 | description),
    data = .
  )

summary(mdl_validation)

# disfluent utterances are judged lower than fluent ones
# the non-native speaker is judged as more disfluent (which partially aligns with our experiment)
# no interaction between the two


# NATURALNESS ANALYSIS

# descriptive stats
# ignoring horse description for now (because we are including it as random effect in our model anyways)

dval %>%
  filter(trial_status == "naturalness") %>%
  group_by(speaker, delivery) %>%
  summarise(
    obvs = n(),
    avg_resp = mean(as.numeric(response)),
    sd_resp = sd(response)
  )

# is naturalness affected by manner of delivery and speaker?

mdl_validation_nat <- dval %>%
  filter(trial_status == "naturalness") %>%
  lmer(
    as.numeric(response) ~ delivery * speaker +
      (1 | participant_ID) + (1 | description),
    data = .
  )

summary(mdl_validation_nat)

# disfluent utterances are judged lower than fluent ones
# the non-native speaker is judged as more disfluent (which partially aligns with our experiment)
# no interaction between the two

