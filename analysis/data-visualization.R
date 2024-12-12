# Note: if running on its own, remove one period
# Otherwise, it is set for running on the manuscript
source('../analysis/data-wrangling.R')
source('../analysis/utils.R') # for violin plot graphs, from Glasgow TeachR team

library(psych)
library(optimx)
library(ggdist)
library(gt)
library(gtsummary)
library(webshot2)
library(plotrix)
library(MetBrewer) # for colors
library(wesanderson)

# Descriptive statistics

# questionnaire data
# avg and sd of language attitudes dimensions, naturalness, accentedness, fluency, trustworthiness, ease of understanding, exposure to native and non-native speakers
# papaja and gt are not friends
# re doing this code to be able to use apa_table and also to refer to it in-text

tab5 <- dexp1lang_prereg %>%
  dplyr::select(c(18:24,38,39)) %>% mutate(speaker = ifelse(speaker=='native', 'Native Speaker', 'Non-native Speaker')) %>%
  rename(trustworthiness = trustworthy) %>%
  rename_with(str_to_title) %>%
  dplyr::select(c("Speaker", "Easy", "Strong", "Affect", "Status", "Solidarity", "Trustworthiness", "Fluent")) %>%
  rename(Comprehensibility = Easy,
         Accentedness = Strong,
         Fluency = Fluent
         ) %>%
  group_by(Speaker) %>%
  summarise(
    avg_easy = mean(as.numeric(Comprehensibility)),
    sd_easy = sd(as.numeric(Comprehensibility)),
    avg_strong = mean(as.numeric(Accentedness)),
    sd_strong = sd(as.numeric(Accentedness)),
    avg_flu = mean(as.numeric(Fluency)),
    sd_flu = sd(as.numeric(Fluency)),
    avg_aff = mean(as.numeric(Affect)),
    sd_aff = sd(as.numeric(Affect)),
    avg_stat = mean(as.numeric(Status)),
    sd_stat = sd(as.numeric(Status)),
    avg_sol = mean(as.numeric(Solidarity)),
    sd_sol = sd(as.numeric(Solidarity)),
    avg_trust = mean(as.numeric(Trustworthiness)),
    sd_trust = sd(as.numeric(Trustworthiness))
  ) %>% 
  dplyr::mutate(
    Comprehensibility = paste(round(avg_easy, 2), paste("(", round(sd_easy,2), ")", sep = ""), sep = " "),
    Accentedness = paste(round(avg_strong, 2), paste("(", round(sd_strong,2), ")", sep = ""), sep = " "),
    Fluency = paste(round(avg_flu, 2), paste("(", round(sd_flu,2), ")", sep = ""), sep = " "),
    Affect = paste(round(avg_aff, 2), paste("(", round(sd_aff,2), ")", sep = ""), sep = " "),
    Status = paste(round(avg_stat, 2), paste("(", round(sd_stat,2), ")", sep = ""), sep = " "),
    Solidarity = paste(round(avg_sol, 2), paste("(", round(sd_sol,2), ")", sep = ""), sep = " "),
    Trustworthiness = paste(format(round(avg_trust, 2),nsmall=2), paste("(", round(sd_trust,2), ")", sep = ""), sep = " ")) %>%
  dplyr::select(Speaker, Comprehensibility, Accentedness, Fluency, Affect, Status, Solidarity, Trustworthiness) %>%
  pivot_longer(c(2:8), names_to = "Dimension", values_to = "Score") %>%
  pivot_wider(values_from = Score, names_from = Speaker)
  #   ) %>%
  # tbl_summary(
  #   by = Speaker,
  #   statistic = list(
  #     all_continuous() ~ "{mean} ({sd})"
  #   ),
  #   digits = all_continuous() ~ 2,
  #   type = c(Comprehensibility = "continuous", Accentedness = "continuous", 
  #            Affect = "continuous", Status = "continuous", Solidarity = "continuous", Trustworthy = "continuous") # just for testing the table
  # )%>% modify_header(label = "**Dimension**") %>% as_gt() %>%
  # tab_options(
  #   table.border.top.color = "white",
  #   heading.title.font.size = px(16),
  #   column_labels.border.top.width = 3,
  #   column_labels.border.top.color = "black",
  #   column_labels.border.bottom.width = 3,
  #   column_labels.border.bottom.color = "black",
  #   table_body.border.bottom.color = "black",
  #   table.border.bottom.color = "white",
  #   table.width = pct(100),
  #   table.background.color = "white",
  #   column_labels.text_transform = 'capitalize'
  # ) %>%
  # cols_align(align="center") %>%
  # opt_align_table_header(align = "left") 
# 
# gtsave(tab_exp1descriptive, 'exp1-descriptive.png', path = './analysis/tables')

# figure for the descriptive statistics

dexp1lang_prereg %>%
  dplyr::select(c(18:24,38,39)) %>% mutate(speaker = ifelse(speaker=='native', 'Native Speaker', 'Non-native Speaker')) %>%
rename(trustworthiness = trustworthy) %>%
  rename_with(str_to_title) %>%
  dplyr::select(c("Speaker", "Easy", "Strong", "Affect", "Status", "Solidarity", "Trustworthiness")) %>%
  rename(Comprehensibility = Easy,
         Accentedness = Strong
  ) %>%
  pivot_longer(!Speaker, names_to = "dimension", values_to = "score") %>%
  group_by(Speaker, dimension) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    sd_score =  sd(score, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(speaker = ifelse(Speaker == 'native', 'Native Speaker', 'Non-native Speaker'))%>%
  ggplot(., aes(x=factor(dimension,levels = c("Comprehensibility", "Accentedness", "Affect",
                                              "Status", "Solidarity", "Trustworthy")), y=as.numeric(mean_score), fill = Speaker)) +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_score-sd_score, ymax=mean_score+sd_score), width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(name = "Rating") + # need to decide color palette based on poster palette
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = c("Native Speaker" = "#00A08A",
                               "Non-native Speaker" = "#F2AD00"), name = "Speaker's linguistic background") + theme(legend.position="top")

# 
# ggsave('exp1-scores-barchart.png', path = './analysis/figures')

# from whom they'd like to learn in the future

dexp1lang_prereg %>% 
  filter(!duplicated(ppt)) %>%
  mutate(learn_future = as.factor(learn_future)) %>%
  group_by(learn_future) %>%
  summarise(no_rows = length(learn_future))

# proportion money distributed

tab4 <- dexp1bet_prereg %>%
  dplyr::select(c(4,11,51)) %>%
  rename_with(str_to_title) %>%
  rename(Money = Raw_money) %>%
  mutate(Delivery = factor(Delivery, levels = c("fluent", "disfluent"))) %>%
  group_by(Speaker, Delivery) %>%
  dplyr::summarise(
    avg_money = mean(as.numeric(Money), na.rm = TRUE),
    sd_money = sd(as.numeric(Money), na.rm = TRUE)
  ) %>% 
  dplyr::mutate(
    distribution = paste(round(avg_money, 2), paste("(", round(sd_money,2), ")", sep = ""), sep = " ")) %>%
  mutate(Delivery = ifelse(Delivery == 'fluent', 'Fluent', 'Disfluent'), Speaker = ifelse(Speaker == 'native', 'Native', 'Non-native')) %>%
  dplyr::select(c(Speaker, Delivery, distribution)) %>%
  rename("Money bet" = distribution)

tab4[c(2,4), 1] <- ""

# papaja and gt() don't work very well
#   #pivot_wider(names_from = Speaker, values_from = distribution) %>%
#   gt(., groupname_col = 'Delivery', rowname_col = 'Speaker') %>%
#     tab_options(
#       table.border.top.color = "white",
#       heading.title.font.size = px(16),
#       column_labels.border.top.width = 3,
#       column_labels.border.top.color = "black",
#       column_labels.border.bottom.width = 3,
#       column_labels.border.bottom.color = "black",
#       table_body.border.bottom.color = "black",
#       table.border.bottom.color = "white",
#       table.width = pct(100),
#       table.background.color = "white"
#     ) %>%
#     cols_align(align="center") %>%
#     tab_style(
#       style = list(
#         cell_borders(
#           sides = c("top", "bottom"),
#           color = "white",
#           weight = px(1)
#         ),
#         cell_text(
#           align="center"
#         ),
#         cell_fill(color = "white", alpha = NULL)
#       ),
#       locations = cells_body(
#         columns = everything(),
#         rows = everything()
#       )
#     ) %>%
#     opt_align_table_header(align = "left") %>%
#   cols_label(
#     distribution = "Money bet"
#   )
# gtsave(tab_exp1moneydist, 'exp1-moneydist.png', path = './analysis/tables')

# viz for poster presentations

# fig1 <- dexp1bet_prereg %>%
#   mutate(delivery = ifelse(delivery == 'fluent', 'Fluent', 'Disfluent'), speaker = ifelse(speaker == 'native', 'Native', 'Non-native'))%>%
#   ggplot(., aes (x = delivery, y = as.numeric(scaled_money), fill = speaker)) +
#   geom_split_violin(alpha = .4, trim = FALSE)  +
#   geom_boxplot(width = .2, alpha = .6, fatten = NULL, show.legend = FALSE) +
#   stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
#                position = position_dodge(.175))  +
#   scale_x_discrete(name = "Manner of Delivery", labels = c("Disfluent", "Fluent")) +
#   scale_y_continuous(name = "Money bet",
#                      breaks = seq(0, 100, 20), 
#                      limits = c(0, 100)) +
#   scale_fill_brewer(palette = "Dark2", name = "Speaker's linguistic background") + # need to decide color palette based on poster palette
#   theme_minimal()

# 
# ggsave('exp1-money-violin.png', path = './analysis/figures')

# bar chart

# dexp1bet_prereg%>%
#   group_by(delivery, speaker)%>%
#   summarise(mean_money = mean(raw_money, na.rm = TRUE), sd_money = sd(raw_money)) %>% ungroup() %>%
#   mutate(delivery = ifelse(delivery == 'fluent', 'Fluent', 'Disfluent'), speaker = ifelse(speaker == 'native', 'Native Speaker', 'Non-native Speaker'))%>%
#   mutate(delivery = factor(delivery, levels = c("Fluent", "Disfluent"))) %>%
#   ggplot(., aes(x=delivery,y=as.numeric(mean_money), fill = speaker)) +
#   geom_bar(stat="identity", color="black", 
#            position=position_dodge()) +
#   geom_errorbar(aes(ymin=mean_money-sd_money, ymax=mean_money+sd_money), width=.2,
#                 position=position_dodge(.9)) +
#   scale_x_discrete(name = "Manner of Delivery", labels = c("Fluent", "Disfluent")) +
#   scale_y_continuous(name = "Money bet") + # need to decide color palette based on poster palette
#   theme_minimal() +
#   scale_fill_manual(values = c("Native Speaker" = "#00A08A",
#                                "Non-native Speaker" = "#F2AD00"), name = "Speaker's linguistic background") + theme(legend.position="top")
# 
# ggsave('exp1-money-barchart.png', path = './analysis/figures')
