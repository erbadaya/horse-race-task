# Note: if running on its own, remove one period
# Otherwise, it is set for running on the manuscript
source('../analysis/data-wrangling.R')


library(psych)
library(optimx)
library(gt)
library(gtsummary)
library(webshot2)


###### PRE-REGISTERED ANALYSIS ######

# 12/09/2024
# Change in code: R bug whereby using formula in t.test forces you to drop paired
# Formula is for unpaired t-test, S3 method is for paired
# See here: https://github.com/insightsengineering/cardx/issues/56

# 1st: Explore and report behavioural data that is not directly related to our question 
# Dimensions to compare: naturalness, accentedness, trustworthy, fluency, comprehensibility, exposure to non-native speaker

langatt_native <- dexp1lang_prereg %>% filter(speaker == "native")
langatt_nonnative <-dexp1lang_prereg %>% filter(speaker == "nonnative")

t.test(langatt_native$trustworthy, langatt_nonnative$trustworthy, paired = TRUE) # trustworthiness
t.test(langatt_native$easy, langatt_nonnative$easy, paired = TRUE) # comprehensibility, 'how easy is to comprehend this speaker'
t.test(langatt_native$naturalness, langatt_nonnative$naturalness, paired = TRUE) # naturalness of the audio
t.test(langatt_native$strong, langatt_nonnative$strong, paired = TRUE) # accent, 'how strong was this speaker's accent'
t.test(langatt_native$fluent, langatt_nonnative$fluent, paired = TRUE) # fluency
t.test(langatt_native$exposure, langatt_nonnative$exposure, paired = TRUE) # exposure to native and non-native speakers in daily life

# RQ: Does money bet differ depending on manner of delivery and speaker's linguistic background?
## exp1_mdlbet runs fine, but NB it misses a random intercept by-participant, which should be included
## log: 07/11/2023
## not modelling random intercept-by participant (previous model run with variance 0) because everyone is betting the 100
## so the average is 25
## fix: do not include it
## additionally: we want to model nonetheless the variance by effect
## lme4 does not work well with sum coding, RSO stats UGent offers the following alternative:


# dexp1bet_prereg$flue_nat <- 1 * (dexp1bet_prereg$delivery == "fluent" & 
#                                       dexp1bet_prereg$speaker == "native")
# dexp1bet_prereg$flue_non <- 1 * (dexp1bet_prereg$delivery == "fluent" & 
#                                       dexp1bet_prereg$speaker == "nonnative")
# dexp1bet_prereg$dis_nat <- 1 * (dexp1bet_prereg$delivery != "fluent" & 
#                                       dexp1bet_prereg$speaker == "native")
# 
# exp1_mdlbet_max <- lmer(
#   raw_money ~ delivery_cont * speaker_cont +
#     (-1 + flue_nat + flue_non + dis_nat | ppt) +
#     (1 | horse),
#   data = dexp1bet_prereg, 
#   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))
# )
# 
# summary(exp1_mdlbet_max)
# hist(resid(exp1_mdlbet_max))

# singular fit

mdlbet_m1 <- lmerTest::lmer(
  raw_money ~ delivery_cont * speaker_cont +
    (1 | horse),
  data = dexp1bet_prereg, 
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))
)


# for manuscript

mdlbet_m1 <- apa_print(lmerTest::lmer(
  raw_money ~ delivery_cont * speaker_cont +
    (1 | horse),
  data = dexp1bet_prereg, 
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))
))

# table for report
# gt and papaja don't work well
# tab_exp1_mdlbet <- tbl_regression(exp1_mdlbet_m1, exponentiate = TRUE,
#                                show_single_row = c(delivery, speaker,`delivery:speaker`),
#                                #tidy_fun = broom.mixed::tidy,
#                                # adding labels to table
#                                label = list(
#                                  fluency ~ "Manner of delivery",
#                                  nativeness ~ "Speaker's linguistic background",
#                                  `fluency:nativeness` ~ 'Interaction'
#                                )) %>% as_gt()
# 
# gtsave(tab_exp1_mdlbet, 'exp1-bet_results.png', path = './analysis/tables')

# let's check with all participants to ensure it is not a power issue

mdlbet_m1_all <- lmerTest::lmer(
  raw_money ~ delivery_cont * speaker_cont +
    (1 | horse),
  data = dexp1bet, 
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))
)


# for manuscript

mdlbet_m1_all <- apa_print(lmerTest::lmer(
  raw_money ~ delivery_cont * speaker_cont +
    (1 | horse),
  data = dexp1bet, 
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))
))


# RQ: Do language attitudes affect money bet?

# 1. Alpha
# 
affect_alpha = round(as.numeric(alpha(dexp1lang_prereg[,13:18])$total[1]), 2)
status_alpha = round(as.numeric(alpha(dexp1lang_prereg[,3:7])$total[1]), 2)
solidarity_alpha = round(as.numeric(alpha(dexp1lang_prereg[,8:12])$total[1]), 2)
# 
# # 2. Compare the three dimensions (affect, status, solidarity) and three dimensions (comprehension, accent, trustworthiness) between speakers
# Pass data from wide to long

dexp1lang_prereg<- dexp1lang_prereg %>%
  pivot_longer(c(easy, strong, affect, status, solidarity, trustworthy, fluent), names_to = "dimension", values_to = "score")


# # Bonferroni correction 0.05/6 = 0.01
# # via t.tests 
# 
tests_questionnaires <- list()

tests_questionnaires[[1]] <- t.test(langatt_native$easy, langatt_nonnative$easy, paired = TRUE)
tests_questionnaires[[2]] <- t.test(langatt_native$strong, langatt_nonnative$strong, paired = TRUE)
tests_questionnaires[[3]] <- t.test(langatt_native$fluent, langatt_nonnative$fluent, paired = TRUE)
tests_questionnaires[[4]] <- t.test(langatt_native$affect, langatt_nonnative$affect, paired = TRUE)
tests_questionnaires[[5]] <- t.test(langatt_native$status, langatt_nonnative$status, paired = TRUE)
tests_questionnaires[[6]] <- t.test(langatt_native$solidarity, langatt_nonnative$solidarity, paired = TRUE)
tests_questionnaires[[7]] <- t.test(langatt_native$trustworthy, langatt_nonnative$trustworthy, paired = TRUE)
names(tests_questionnaires) <- c("Comprehensibility", "Accent", "Fluency", "Affect", "Status", "Solidarity", "Trustworthy")

# # table for report
# # idea from https://stackoverflow.com/questions/21840021/grabbing-certain-results-out-of-multiple-t-test-outputs-to-create-a-table
# 
tab6 <- sapply(tests_questionnaires, function(x) {
  c(x$statistic,
    x$parameter,
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value,
    x$estimate)
}) %>% t() %>% cbind(tests = dimnames(.)[[1]]) %>% as_tibble() %>%
  mutate(across(1:6,as.numeric)) %>% mutate(across(1:6,round, 2)) %>% mutate(p.value = "< .001") %>%
  mutate(
    col_name = paste("t(", df, ")", sep = ""),
    `95% CI` = paste("[", ci.lower, ", ", ci.upper, "]", sep = "")
  ) %>%
  pivot_wider(names_from = col_name, values_from = t) %>%
  select(-c(df, ci.lower, ci.upper)) %>% gt() %>%
  cols_move(
    columns = c(p.value, `95% CI`, `mean difference`),
    after = starts_with("t(")
  ) 

tab6 <- tab6[[1]]

tab6 <- tab6[,c(3, 5, 4, 2)]

tab6 <- tab6 %>%
  rename(d = `mean difference`,
         Dimension = tests)

#   tab_options(
#     table.border.top.color = "white",
#     heading.title.font.size = px(16),
#     column_labels.border.top.width = 3,
#     column_labels.border.top.color = "black",
#     column_labels.border.bottom.width = 3,
#     column_labels.border.bottom.color = "black",
#     table_body.border.bottom.color = "black",
#     table.border.bottom.color = "white",
#     table.width = pct(100),
#     table.background.color = "white"
#   ) %>%
#   cols_align(align="center") %>%
#   tab_style(
#     style = list(
#       cell_borders(
#         sides = c("top", "bottom"),
#         color = "white",
#         weight = px(1)
#       ),
#       cell_text(
#         align="center"
#       ),
#       cell_fill(color = "white", alpha = NULL)
#     ),
#     locations = cells_body(
#       columns = everything(),
#       rows = everything()
#     )
#   ) %>%
#   opt_align_table_header(align = "left") %>%
#   cols_label(
#     p.value = "p",
#     tests = "",
#     `mean difference` =  "d"
#   )
#   
# gtsave(tab_ttestsexp1, 'exp1-ttests_results.png', path = './analysis/tables')
# 
# 
# # 3. Explore whether model fit improves by including the variable with significant differences.
# 
mdlbet_attitudes <- lmerTest::lmer(
  raw_money ~ delivery_cont * speaker_cont +easy + strong + status + solidarity + affect + trustworthy + fluent+
    (1 | horse),
  data = dexp1bet_prereg,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)),
  REML = "FALSE"
)

# anova(mdlbet_m1, mdlbet_attitudes) papaja doesn't like anovas

# for manuscript

mdlbet_attitudes <- apa_print(lmerTest::lmer(
  raw_money ~ delivery_cont * speaker_cont +easy + strong + status + solidarity + affect + trustworthy + fluent +
    (1 | horse),
  data = dexp1bet_prereg,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)),
  REML = "FALSE"
))

# 
# summary(exp1_mdlbet_attitudes)
# 
# ##### EXPLORATORY ANALYSIS ######
# 
# # 4. Preference to learn
# ## Do people prefer to learn from the native or from the non-native speaker?
# ## chi goodness of fit
# 
dexp1lang_prereg_tab <- dexp1lang_prereg %>%
  filter(!duplicated(ppt)) %>%
  mutate(learn_future = as.factor(learn_future)) %>%
  group_by(learn_future) %>%
  summarise(obvs = n()) %>%
  mutate(expected_count = rep(360 * (1/2), 2)) %>%
  mutate(expected_prop = rep(1/2,2))

mdl_learnpreference <- chisq.test(x = dexp1lang_prereg_tab$obvs,
                                  p = dexp1lang_prereg_tab$expected_prop)

mdl_learnpreference
# 
# mdl_learneasy <- dexp1lang_prereg %>%
#   filter(!duplicated(ppt)) %>%
#   mutate(learn_bin = 1 * (learn_future == "Native speaker")) %>%
#   glm(
#     learn_bin~easy,
#     data = .,
#     family = 'binomial'
#   )
# 
# summary(mdl_learneasy)

# 5. Correlation linguistic and social factors
# Do participants' exposure affect their betting behaviour?

mdlbet_m1_exposure <- lmerTest::lmer(
  raw_money ~ delivery_cont * speaker_cont + exposure +
    (1 | horse),
  data = dexp1bet_prereg, 
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))
)

# for manuscript

mdlbet_m1_exposure <- apa_print(lmerTest::lmer(
  raw_money ~ delivery_cont * speaker_cont + exposure +
    (1 | horse),
  data = dexp1bet_prereg, 
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))
))