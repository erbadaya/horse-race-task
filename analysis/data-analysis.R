source('./analysis/data-wrangling.R')

library(psych)
library(optimx)
library(gt)
library(gtsummary)
library(webshot2)

# 1st: Explore and report behavioural data that is not directly related to our question 
# Dimensions to compare: naturalness, accentedness, trustworthy, fluency, comprehensibility, exposure to non-native speaker

t.test(trustworthy ~ speaker, data = dexp1lang_prereg, paired = TRUE) # trustworthiness
t.test(easy ~ speaker, data = dexp1lang_prereg, paired = TRUE) # comprehensibility, 'how easy is to comprehend this speaker'
t.test(naturalness ~ speaker, data = dexp1lang_prereg, paired = TRUE) # naturalness of the audio
t.test(strong ~ speaker, data = dexp1lang_prereg, paired = TRUE) # accent, 'how strong was this speaker's accent'
t.test(fluent ~ speaker, data = dexp1lang_prereg, paired = TRUE) # fluency
t.test(exposure ~ speaker, data = dexp1lang_prereg, paired = TRUE) # exposure to native and non-native speakers in daily life

# RQ: Does money bet differ depending on manner of delivery and speaker's linguistic background?

exp1_mdlbet <- lmer(
  money ~ delivery * speaker +
    (1 | horse),
  data = dexp1bet, 
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))
)

summary(exp1_mdlbet)

# table for report

tab_exp1_mdlbet <- tbl_regression(exp1_mdlbet, exponentiate = TRUE,
                               show_single_row = c(delivery, speaker,`delivery:speaker`),
                               #tidy_fun = broom.mixed::tidy,
                               # adding labels to table
                               label = list(
                                 fluency ~ "Manner of delivery",
                                 nativeness ~ "Speaker's linguistic background",
                                 `fluency:nativeness` ~ 'Interaction'
                               )) %>% as_gt()

gtsave(tab_exp1_mdlbet, 'exp1-bet_results.png', path = './analysis/tables')

# RQ: Do language attitudes affect money bet?

# 1. Alpha

affect_alpha = round(as.numeric(alpha(dexp1lang_prereg[,13:18])$total[1]), 2)
status_alpha = round(as.numeric(alpha(dexp1lang_prereg[,3:7])$total[1]), 2)
solidarity_alpha = round(as.numeric(alpha(dexp1lang_prereg[,8:12])$total[1]), 2)

# 2. Compare the three dimensions (affect, status, solidarity) and three dimensions (comprehension, accent, trustworthiness) between speakers
# Bonferroni correction 0.05/6 = 0.01
# via t.tests 

tests_questionnaires <- list()

tests_questionnaires[[1]] <- t.test(affect ~ speaker, data = dexp1lang_prereg, paired = TRUE)
tests_questionnaires[[2]] <- t.test(status ~ speaker, data = dexp1lang_prereg, paired = TRUE)
tests_questionnaires[[3]] <- t.test(solidarity ~ speaker, data = dexp1lang_prereg, paired = TRUE)
tests_questionnaires[[4]] <- t.test(easy ~ speaker, data = dexp1lang_prereg, paired = TRUE)
tests_questionnaires[[5]] <- t.test(strong ~ speaker, data = dexp1lang_prereg, paired = TRUE)
tests_questionnaires[[6]] <- t.test(trustworthy ~ speaker, data = dexp1lang_prereg, paired = TRUE)
names(tests_questionnaires) <- c("Affect", "Status", "Solidarity", "Comprehensibility", "Accent", "Trustworthy")

# table for report
# idea from https://stackoverflow.com/questions/21840021/grabbing-certain-results-out-of-multiple-t-test-outputs-to-create-a-table

tab_ttestsexp1 <- sapply(tests_questionnaires, function(x) {
  c(x$statistic,
    x$parameter,
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
}) %>% t() %>% cbind(tests = dimnames(.)[[1]]) %>% as_tibble() %>%
  mutate(across(1:5,as.numeric)) %>% mutate(across(1:5,round, 2)) %>%
  mutate(
    col_name = paste("t(", df, ")", sep = ""),
    `95% CI` = paste("[", ci.lower, ", ", ci.upper, "]", sep = "")
  ) %>%
  pivot_wider(names_from = col_name, values_from = t) %>%
  select(-c(df, ci.lower, ci.upper)) %>% gt() %>%
  cols_move(
    columns = c(p.value, `95% CI`),
    after = starts_with("t(")
  ) %>%
  tab_options(
    table.border.top.color = "white",
    heading.title.font.size = px(16),
    column_labels.border.top.width = 3,
    column_labels.border.top.color = "black",
    column_labels.border.bottom.width = 3,
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table.border.bottom.color = "white",
    table.width = pct(100),
    table.background.color = "white"
  ) %>%
  cols_align(align="center") %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "white",
        weight = px(1)
      ),
      cell_text(
        align="center"
      ),
      cell_fill(color = "white", alpha = NULL)
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  opt_align_table_header(align = "left") %>%
  cols_label(
    p.value = "p",
    tests = ""
  )
  
gtsave(tab_ttestsexp1, 'exp1-ttests_results.png', path = './analysis/tables')


# 3. Explore whether model fit improves by including the variable with significant differences.


