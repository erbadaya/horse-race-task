# Pre-processing of norming of horses descriptions
# Author: Esperanza Badaya
# Date: 23/04/2026
# Note: If running this script on its own, reading in files needs to only have one period instead of two
# Two periods are only needed for running the .ms 

# Data collection start: 04/05/2026
# Data collection end: 05/05/2026
# Until we reached N = 30 of people who met the criteria (we recruited 11 more as 11 participants did not meet the criteria)

library(tidyverse)
library(here)
library(ordinal)
library(ggplot2)
library(MASS)
library(jsonlite)
library(PlackettLuce)
library(proxy)
library(gtools)

# Function from: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

ModeFunc <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# load in data
# comment out once all data is recruited & create a single .csv with all the raw, anonymised data
# 
# vdatDesc_SingleHorse <- data.frame()
# vdatDesc_Ranks <- data.frame()
# surveyResponses <- data.frame()
# temp <- list.files(path = "./exp-2/data/norming/descriptions", pattern = "*.csv")
# ppt_id <- length(temp)
# filesRemove <- c() # Files to remove because ppts do not pass the attention check (do not remove those that do not meet the criteria)
# 
# for(i in 1:ppt_id){
#   
#   print(paste0("Processing file ", temp[i]))
#   print(paste0("File nr ", i, " out of ", ppt_id))
#   
#   # Bring in the file
#   
#   FILE <- read_csv(paste("./exp-2/data/norming/descriptions/",temp[i], sep = ""), show_col_types = FALSE) %>%
#     filter(!trialtype == 'prolificID') # anonymise data
#   
#   FILE$ppt <- i # Create participant nr
#   
#   SH <- FILE %>%
#     filter(trialtype == 'rate_single_horse') %>% # trials with single horses
#     dplyr::select(ppt, horse_name, length_description, origin, response, description) # keep only relevant columns
#     
#   RH <- FILE %>%
#     filter(trialtype == 'rank_horses') %>% # trials with ranking
#     dplyr::select(ppt, original_order, response, first_place, second_place, third_place, forth_place) %>% # keep only relevant columns
#     mutate(race_nr = seq(nrow(.))) %>%
#     pivot_longer(ends_with('_place'),
#                  names_to = 'ranking',
#                  values_to = 'horse_name')
#   
#   
#   # Does this participant pass the attention check
#   
#   passOrNot <- FILE %>% filter(trialtype == 'attention_check') %>%
#     dplyr::select(accuracy) %>% mutate(accuracy = ifelse(accuracy == 'correct', 1, 0)) %>%
#     summarise(perc = sum(accuracy) / nrow(.))
#   
#   out = 1
#   
#   if(passOrNot > 0.33){
#     print(paste0('File ', temp[i], ' passed the attention check'))
#     out = out + 1
#   } else {
#     print(paste0('File ', temp[i], ' did not the attention check. DO NOT REIMBURSE, REMOVE FILE FROM DIRECTORY, DO NOT INCLUDE THEIR DATA'))
#     out = 0
#     filesRemove <- c(i, filesRemove)
#     
#   }
#   
#   # Does this participant meet our criteria?
#   
#   tt <- FILE %>% filter(trialtype == 'survey_end') %>%
#     dplyr::select(response) 
#   criteria <- fromJSON(as.character(tt))
#   
#   if(criteria$englishL1 == 'TRUE' & criteria$UKCoB == 'TRUE' & criteria$betBefore == 'FALSE' &
#      criteria$considerExpert < 4 & criteria$evaluateKnowledge < 4){
#     out = out + 1
#     SH$`in_analysis` <- 'Yes'
#     RH$`in_analysis` <- 'Yes'
#   } else {
#     SH$`in_analysis` <- 'No'
#     RH$`in_analysis` <- 'No'
#   }
#   
#   # Either store the file if the participant passed the attention check + meet our criteria, or discard them
# 
#   if(out != 0){
#     vdatDesc_SingleHorse <- rbind(SH, vdatDesc_SingleHorse)
#     vdatDesc_Ranks <- rbind(RH, vdatDesc_Ranks)
#     criteria[sapply(criteria, length) == 0] <- NA
#     criteria <- as.data.frame(criteria)
#     criteria$ppt <- i
#     surveyResponses <- rbind(criteria, surveyResponses)
#   } 
#   
# }
# 
# rm(FILE, i, out, ppt_id, temp, SH, RH, passOrNot, tt, criteria)
# 
# # Log 04/05/2026
# # 1 ppt reported in the survey responses that they work with horses as they are studying veterinary. they are already marked as 'no' in_analysis, as they reported having taken part in a similar study before
# # Store the responses to survey for other people to explore them
# # ppt 39 reported having knowledge on horse races (even if they didnt rate themselves as expert)
# 
# vdatDesc_SingleHorse <- vdatDesc_SingleHorse %>%
#   mutate(in_analysis = ifelse(ppt == 4| ppt == 39, 'No', in_analysis))
# 
# vdatDesc_Ranks <- vdatDesc_Ranks %>%
#   mutate(in_analysis = ifelse(ppt == 4 | ppt == 39, 'No', in_analysis))
# 
# # write unprocessed data
# write.csv(vdatDesc_SingleHorse, './exp-2/data/norming/vdatDesc_SingleHorse.csv')
# write.csv(vdatDesc_Ranks, './exp-2/data/norming/vdatDesc_Ranks.csv')
# write.csv(responsesSurvey, './exp-2/data/norming/vdat_ResponsesSurvey.csv')

# read in all data files if data collection is done

vdatDesc_SingleHorse <- read.csv('../exp-2/data/norming/vdatDesc_SingleHorse.csv')
vdatDesc_Ranks <- read.csv('../exp-2/data/norming/vdatDesc_Ranks.csv')

# How can we determine what horses are not different from each other?
# We have two points of information
# Single horse rating -> how that horse is perceived on its own
# Ranking horses -> how horses would be placed in a race. For this ranking, we need to note two things
# 1: Groups were done at random
# 2: An ordinal model (like we did in Exp 1) "ignores" the competition within a set (but this is also partially what we want?)

# We need to get horses that sound similar in terms of winning (not necessarily that they always end up in 1st position)
# A better approach is a Placket-Luce model
# Rob suggested getting a competition score, something like a cosine similarity
# So what we could do is to get the predicted PL score
# And then do a composite score

## PlackettLuce requires a specfic input

dRank_PL <- vdatDesc_Ranks %>%
  filter(in_analysis == 'Yes') %>%
  dplyr::select(ppt, race_nr, ranking, horse_name) %>%
  mutate(ppt_trial = paste(ppt, race_nr)) %>%
  group_by(ppt_trial) %>%
  pivot_wider(
    names_from = ranking,
    values_from = horse_name
  ) %>%
  rename(
    rank1 = first_place,
    rank2 = second_place,
    rank3 = third_place,
    rank4 = forth_place
  )

# Get horse names

horses <- vdatDesc_SingleHorse  %>% filter(ppt == '1') %>% dplyr::pull(horse_name)
horse_ids <- setNames(1:length(horses), horses)

rankings_df_num <- dRank_PL %>%
  mutate(across(rank1:rank4, ~ horse_ids[.x]))

rank_matrix <- as.matrix(rankings_df_num[, c("rank1","rank2","rank3","rank4")])
attr(rank_matrix, "items") <- horses

pl_rankings <- as.rankings(rank_matrix, input = "orderings",
                           items = attr(rank_matrix, "items"))

pl_model <- PlackettLuce(pl_rankings)

pl_scores <- coef(pl_model)

pl_df <- data.frame(
  horse = names(pl_scores),
  pl_score = as.numeric(pl_scores)
)

print(pl_df)

# pl_df contains the 'worth' of the horses, given who beat them and whom they beat
# the score is how strong the horse is compared to other horses

# Next step: Compute pairwise similarities (one per horse, against the other horses)

ability <- as.numeric(pl_scores)
names(ability) <- names(pl_scores)

pairwise_prob <- function(a, b) {
    1 / (1 + exp(-(a - b)))
  }
  
prob_matrix <- outer(
  ability,
  ability,
  FUN = pairwise_prob
)

rownames(prob_matrix) <- names(ability)
colnames(prob_matrix) <- names(ability)

diag(prob_matrix) <- NA

# prob_matrix now shows how one horse wins or loses against another horse
# we can use these vectors to compute cosine similarity

cosine_sim <- proxy::simil(
  prob_matrix,
  method = "cosine",
  use = "pairwise.complete.obs"
)

heatmap(as.matrix(cosine_sim))

# Now the question is: randomly present the selected horses, or ensure that horses are similar within a race?
# The latter is more common in psyling (match target - distractors within a trial, e.g.)

cosine_sim <- as.matrix(cosine_sim)
horses <- rownames(cosine_sim)
groups <- combn(horses, 4, simplify = FALSE)

score_group <- function(g) {
  mean(cosine_sim[g, g][upper.tri(diag(length(g)))])
}

groups <- combn(horses, 4, simplify = FALSE)

scores <- vapply(groups, score_group, numeric(1))

best_groups <- groups[order(scores, decreasing = TRUE)][1:160]
best_scores <- sort(scores, decreasing = TRUE)[1:160]

best160 <- data.frame(
  group = vapply(best_groups, paste, collapse = ", ", character(1)),
  score = best_scores
)

# We need to get 8 groups with not overlapping horses
# Within these groups, the horses cannot differ in their individual rating

best160_individual <- best160 %>%
  separate(group, into = c("horse1", "horse2", "horse3", "horse4"), sep = ",\\s*") %>%
  mutate(group = row_number()) %>%
  pivot_longer(
    cols = starts_with("horse"),
    names_to = "position",
    values_to = "horse"
  ) %>%
  dplyr::select(group, horse)%>%
  rename(horse_name = 'horse') %>%
  left_join(., vdatDesc_SingleHorse) %>% filter(in_analysis == 'Yes') 

results <- best160_individual %>%
  group_by(group) %>%
  group_modify(~{
    m <- lmerTest::lmer(as.numeric(response) ~ horse_name + (1 | ppt), data = .x)
    p <- anova(m)$`Pr(>F)`[1]
    data.frame(p_x = p)
  }) %>%
  ungroup() %>%
  mutate(significant = p_x < 0.05) %>%
  rename(race = 'group')

best160 <- best160 %>%
  mutate(race = row_number()) %>%
  left_join(., results, by = "race") %>%
  filter(significant == FALSE)

# Get have selected 8 groups, get their probabilities
# groups are:
# Bold Pride	Lightning Monarch	Urban Legend	Saffron Crest	0.9999997	1
# Crystal Frontier	Nightfall	Swift Valor	Echo Fury	0.9999982	2
# Diamond Hood	Vanya	Flint Sonata	Pale Corsair	0.9999808	31
# Star Dust	Onyx Rune	Air Ace	Brave Horizon	0.9999785	35
# Flashfire	Karenin	Dark Mirage	Steady Fighter	0.9999775	37
# Bramble Halo	Black Ember	Ruby Ash	Blue Horizon	0.9999746	55
# Vladimir, Crimson Eclipse, Worthing, Silent Eclipse 0.9999682 103
# Scarlet Racer, Nimbus Crest, Windchaser, Emerald 0.9999550 157


# Moss Relic	Red Meridian	Worthing	Silent Eclipse	0.9999908	8
# Nimbus Crest	Winchaser	Lovelace	Emerald	0.9999865	20

selected_groups <- c(1,2,31,35,37,55,103,157)

predicted_probs <- data.frame()

pl_prob <- function(order, worth) {
  remaining <- order
  prob <- 1
  
  for (i in order) {
    prob <- prob * worth[i] / sum(worth[remaining])
    remaining <- remaining[remaining != i]
  }
  
  prob
}

position_probs <- function(group, worth) {
  perms <- permutations(length(group), length(group), group)
  
  perms %>%
    as.data.frame() %>%
    split(seq(nrow(perms))) %>%
    map_dfr(~{
      ord <- unlist(.x)
      tibble(
        horse = ord,
        position = seq_along(ord),
        prob = pl_prob(ord, worth)
      )
    }) |>
    group_by(horse, position) %>%
    summarise(prob = sum(prob), .groups = "drop") %>%
    pivot_wider(names_from = position, values_from = prob)
}


for(race in 1:length(selected_groups)){
  
  gg <-selected_groups[race]
  
  print(paste0("Processing race ", selected_groups[race]))
  
  group <- best_groups[[selected_groups[race]]]

  tt <- position_probs(group, worth = exp(pl_scores))
  tt$race_group <- race
  comparison <- results %>% filter(race == gg) %>% pull(p_x)
  tt$comp <- comparison
  predicted_probs <- rbind(predicted_probs, tt)
  
}

rm(race, tt, gg, comparison)

## For sanity check, compare our selected groups with an ordinal model per group

predicted_probs_ordinal <- data.frame()

for(race in 1:length(selected_groups)){
  print(paste0("Processing race ", selected_groups[race]))
  
  # Filter the large dataset
  
  filterGroup <- predicted_probs %>% filter(race_group == race) 
  forModel <- vdatDesc_Ranks %>% filter(
    horse_name %in% filterGroup$horse
  ) %>% filter(in_analysis == 'Yes') %>%
    mutate(ranking = factor(ranking, ordered = TRUE,
                            levels = c(
                              "first_place",
                              "second_place",
                              "third_place",
                              "forth_place"
                            )))
  
  ordinal_model <-ordinal::clm(ranking ~ horse_name ,
                       data = forModel)
  
  newdatRank <- data.frame(
    horse_name = unique(forModel$horse_name)
  )
  
  predRank <- predict(ordinal_model, newdata = newdatRank, type = "prob")
  
  anova_check <- aov(ranking ~ horse_name ,
                     data = forModel)
  
  predRank_df <- cbind(newdatRank, predRank) %>%
    rename('horse' = 'horse_name')
  predRank_df$race_group <- race
  predicted_probs_ordinal <- rbind(predicted_probs_ordinal, predRank_df) 
  
}

# Compare & also attach group similarity for table in paper

compare_df <- left_join(predicted_probs, predicted_probs_ordinal, by = c("horse", "race_group"))
for(race_g in selected_groups){
  g_s <- best160 %>% filter(race == race_g) %>% pull(score)
  compare_df$group_similarity <- g_s
}

# We may also want to add the individual comparison of ratings?

valuesSingle <- vdatDesc_SingleHorse %>%
  filter(horse_name %in% compare_df$horse) %>%
  group_by(horse_name) %>%
  summarise(
    mean_score = mean(response, na.rm = TRUE),
    sd_score = sd(response, na.rm = TRUE)
  ) %>%
  rename(horse = 'horse_name')

# Format table for paper
# Selected critical horses

criticalHorses <- c('Saffron Crest', 'Nightfall', 'Pale Corsair', 'Air Ace',
                    'Karerin', 'Blue Horizon', 'Silent Eclipse', 'Scarlet Racer')

tab1 <- compare_df %>%
  left_join(., valuesSingle) %>%
  mutate(
    Role = ifelse(horse %in% criticalHorses, 'Critical', 'Filler')
  ) %>%
  mutate(
    `1` = round(`1`, 2),
    `2` = round(`2`, 2),
    `3` = round(`3`, 2),
    `4` = round(`4`, 2),
    fit.first_place = round(fit.first_place, 2),
    fit.second_place = round(fit.second_place, 2),
    fit.third_place = round(fit.third_place, 2),
    fit.forth_place = round(fit.forth_place, 2),
    values = paste0(round(mean_score, 2), " (", round(sd_score, 2), ")", sep = "")
  ) %>%
  dplyr::select(-c(mean_score, sd_score)) %>%
  relocate(race_group, group_similarity, horse, Role, `1`, `2`, `3`, `4`, 
           fit.first_place, fit.second_place, fit.third_place, fit.forth_place, 
           values, comp) %>%
  group_by(race_group) %>%
  arrange(Role,.by_group = TRUE) %>%
  rename(
    `Horse name` = 'horse',
    `PL Score Rank 1` = `1` ,
    `PL Score Rank 2` = `2` ,
    `PL Score Rank 3` = `3` ,
    `PL Score Rank 4` = `4` ,
    `Predicted probability Rank 1` = `fit.first_place`,
    `Predicted probability Rank 2` = `fit.second_place`,
    `Predicted probability Rank 3` = `fit.third_place`,
    `Predicted probability Rank 4` = `fit.forth_place`,
    `Group` = 'race_group',
    `Group Similarity Score` = 'group_similarity',
    `Mean (SD)` = 'values',
    `Pr(>F)` =  'comp'
  )


# Table for the written descriptions

tabDescriptions <- compare_df %>%
  dplyr::select(horse, race_group) %>%
  rename( 'horse_name' = 'horse') %>%
  left_join(., vdatDesc_SingleHorse) %>%
  filter(!duplicated(horse_name)) %>%
  mutate(
    Role = ifelse(horse_name %in% criticalHorses, 'Critical', 'Filler')
  ) %>%
  group_by(race_group) %>%
  arrange(Role,.by_group = TRUE) %>%
  dplyr::select(horse_name, Role, race_group, description) %>%
  relocate(race_group, horse_name, Role, description) %>%
  rename(
    `Group` =  'race_group',
    `Horse name` = 'horse_name',
    `Description` = 'description'
  )

# Write the descriptions into a .csv

#write.csv(tabDescriptions, './exp-2/analysis/norming/descriptions/descriptions.csv')

# ## OLD CODES
# 
# ######## ANALYSE THE RESULTS
# 
# # Single horse rating
# # Note that here at face value we are comparing against a reference horse
# # Ideally, what we want, is horses that do not differ greatly from the average (or median)
# 
# mdSingleHorse <- vdatDesc_SingleHorse %>%
#   filter(in_analysis == 'Yes') %>%
#   lmer(
#   as.numeric(response) ~ horse_name + (1 | ppt),
#   data = .
# )
# 
# summary(mdSingleHorse)
# 
# newdatSingle <- data.frame(
#   horse_name = unique(vdatDesc_SingleHorse$horse_name)
# )
# 
# predSingle <- predict(mdSingleHorse, newdata = newdatSingle, re.form = NA)
# 
# predSingle_df <- cbind(newdatSingle, predSingle)
# 
# 
# # Ranking the horses
# # Needs to be clm to used pred() later to see the distribution across ranks
# 
# mdRank <- vdatDesc_Ranks %>%
#   filter(in_analysis == 'Yes') %>%
#   mutate(ranking = ordered(ranking,
#                            levels = c('first_place', 'second_place', 'third_place', 'forth_place'))) %>%
#   ordinal::clm(ranking ~ horse_name,
#                        data = .)
# 
# summary(mdRank)
# # 
# # library(emmeans)
# # emm <- emmeans(mdRank, ~ horse_name)
# # pairs <- pairs(emm, adjust = "tukey")
# # pairs_df <- as.data.frame(pairs)
# # 
# # indist_pairs <- pairs_df %>%
# #   dplyr::filter(p.value > 0.05)
# # 
# # probs <- emmeans(mdRank, ~ horse_name, type = "link")
# # probs_df <- as.data.frame(probs)
# 
# 
# newdatRank <- data.frame(
#   horse_name = unique(vdatDesc_Ranks$horse_name)
# )
# 
# predRank <- predict(mdRank, newdata = newdatRank)
# 
# predRank_df <- cbind(newdatRank, predRank)
# 
# # What horses pass?
# # We need to provide the summaries for supplementary materials!
# 
# predictionsHorses <- left_join(predRank_df, predSingle_df, by = 'horse_name')
# 
# # write the dfs for transparency
# 
# write.csv(predictionsHorses, './exp-2/analysis/norming/descriptions/output_Predictions.csv')
# 
# predictionsHorses <- read.csv('./exp-2/analysis/norming/descriptions/output_Predictions.csv')
# fullSet <- predictionsHorses
# 
# # First, create a list of all the horses that will not be part of our study at the end
# # We have piloted 64 horses, we need 32 horses for the critical trials
# # We should discard 32 horses (can be used for fillers, for example)
# 
# # Criteria to select horses
# # Balance between single horse and ranking
# # Note that single rating is less representative of the actual experiment
# # In ranking, they're all compared against a reference horse in the model
# # It's better to start by looking at the predicted probabilities of each rank (predRank_df)
# 
# # Definitely weak and very strong horses
# # Largely based on predRank_df
# 
# horsesOutRank <- data.frame('horse_name' =  c("Silversky", "Wave Rider","Green Dusk","Neon Charger","Velvet Thunder","Copper Legacy", "Royal Rebel",
#                "Comet Stride","Golden Tempest","Black Ember", "Solar Flash", # very weak horses
#                "Ticker Pike", "Gilded Lily", "Port Louis", "Estragon", "Celestial Queen", "Blue Blaze", "Star Dust",
#                "Onyx Rune", "Brave Horizon", "Air Ace", "Lunar Charger", "Windchaser", "Lovelace", "Stockman",
#                "Nimbus Crest" # very strong horses
# ))
# 
# # Filter those out
# 
# fullSet <- fullSet %>%
#   filter(!horse_name %in% horsesOutRank$horse_name)
# 
# # We are left with 38
# 
# # horsesKeepRank <- subset(fullSet,
# #        fit.first_place > 0.15 &
# #          fit.forth_place < 0.30) # Leaves us missing 4!
# 
# # Remove Bramble Halo, Leeloo, Diamond Hood, Pale Corsair, Desert Flower (based on rank)
# # Remove Gallant Storm (based on single)
# 
# fullSet <- fullSet %>%
#   filter(!horse_name %in% c( 'Bramble Halo', 'Leeloo', 'Diamond Hood', 'Pale Corsair', 'Desert Flower', 'Gallant Storm'))
# 
# # Sanity check
# 
# mdlSingle_FullSet <- vdatDesc_SingleHorse %>%
#   filter(in_analysis == 'Yes') %>%
#   filter(horse_name %in% fullSet$horse_name) %>%
#   lmer(
#     as.numeric(response) ~ horse_name + (1 | ppt),
#     data = .
#   )
# 
# mdlRank_FullSet <- vdatDesc_Ranks %>%
#   filter(in_analysis == 'Yes') %>%
#   filter(horse_name %in% fullSet$horse_name) %>%
#   mutate(ranking = ordered(ranking,
#                            levels = c('first_place', 'second_place', 'third_place', 'forth_place'))) %>%
#   ordinal::clm(ranking ~ horse_name,
#                data = .)
# 
# # Is there any difference between our critical and our filler stims?
# 
# horsesCritical <- data.frame(horse_name = c("Emerald", "Swift Valor", "Electric Stride", "Flint Sonata",
#                                             "Bold Pride", "Crimson Eclipse", "Midnight Inferno", "Worthing"))
# 
# fullSet <- fullSet %>%
#   mutate(
#     trial_type = ifelse(horse_name %in% horsesCritical$horse_name, "critical", "filler")
#   )
# 
# 
# mdlSingle_FullSetbyType <- vdatDesc_SingleHorse %>%
#   filter(in_analysis == 'Yes') %>%
#   filter(horse_name %in% fullSet$horse_name) %>%
#   mutate(
#     trial_type = ifelse(horse_name %in% horsesCritical$horse_name, "critical", "filler")
#   ) %>%
#   lmer(
#     as.numeric(response) ~ trial_type + (1 | ppt),
#     data = .
#   )
# 
# mdlRank_FullSetbyType <- vdatDesc_Ranks %>%
#   filter(in_analysis == 'Yes') %>%
#   filter(horse_name %in% fullSet$horse_name) %>%
#   mutate(
#     trial_type = ifelse(horse_name %in% horsesCritical$horse_name, "critical", "filler")
#   ) %>%
#   mutate(ranking = ordered(ranking,
#                            levels = c('first_place', 'second_place', 'third_place', 'forth_place'))) %>%
#   ordinal::clm(ranking ~ trial_type,
#                data = .)
# 
# ## Single horses
# 
# tabSingle <- vdatDesc_SingleHorse %>%
#   filter(in_analysis == 'Yes') %>%
#   filter(horse_name %in% fullSet$horse_name) %>%
#   mutate(
#     trial_type = ifelse(horse_name %in% horsesCritical$horse_name, "critical", "filler")
#   ) %>% 
#   group_by(horse_name, description, trial_type) %>%
#   summarise(
#     mean_response = mean(as.numeric(response)),
#     sd_response = sd(as.numeric(response))
#   )
# 
# ## Rank horses
# # What actually makes sense if to print all the information in one
# 
# tabRank <- vdatDesc_Ranks %>%
#   filter(in_analysis == 'Yes') %>%
#   filter(horse_name %in% fullSet$horse_name) %>%
#   mutate(
#     trial_type = ifelse(horse_name %in% horsesCritical$horse_name, "critical", "filler")
#   ) %>% 
#   group_by(horse_name) %>%
#   summarise(
#     mode_ranking = ModeFunc(ranking)
#   )
# 
# tabSummaryDescriptions <- left_join(tabSingle, tabRank, by = 'horse_name') %>%
#   mutate(
#     status = ifelse(trial_type == "critical", 'Critical item', 'Filler item'),
#     mean_sd = paste0(round(mean_response, 2), " (", round(sd_response, 2), ")", sep = "")
#   ) %>%
#   rename(
#     'Status' = 'status',
#     'Horse' = 'horse_name',
#     `Mean (SD)` = 'mean_sd',
#     'Mode' = 'mode_ranking',
#     'Description' = 'description'
#   ) %>%
#   dplyr::select(Horse, Description, Status, `Mean (SD)`, Mode) %>%
#   relocate(Horse, Description, Status, `Mean (SD)`, Mode) %>%
#   mutate(
#     Mode = case_when(
#       Mode == 'first_place' ~ 'First place',
#       Mode == 'second_place' ~ 'Second place',
#       Mode == 'third_place' ~ 'Third place',
#       Mode == 'forth_place' ~ 'Fourth place',
#       TRUE ~ Mode
#     )
#   )
#   
