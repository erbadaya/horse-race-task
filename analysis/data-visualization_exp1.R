source('./analysis/data-wrangling.R')
source('./analysis/utils.R') # for violin plot graphs

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

tab_exp1descriptive <- dexp1lang_prereg %>%
  select(c(18:24,38,39)) %>% mutate(speaker = ifelse(speaker=='native', 'Native Speaker', 'Non-native Speaker')) %>%
  rename_with(str_to_title) %>%
  select(c("Speaker", "Easy", "Strong", "Affect", "Status", "Solidarity", "Trustworthy")) %>%
  rename(Comprehensibility = Easy,
         Accentedness = Strong
         ) %>%
  tbl_summary(
    by = Speaker,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = all_continuous() ~ 2,
    type = c(Comprehensibility = "continuous", Accentedness = "continuous", 
             Affect = "continuous", Status = "continuous", Solidarity = "continuous", Trustworthy = "continuous") # just for testing the table
  )%>% modify_header(label = "**Dimension**") %>% as_gt() %>%
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
    table.background.color = "white",
    column_labels.text_transform = 'capitalize'
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
  opt_align_table_header(align = "left") 

gtsave(tab_exp1descriptive, 'exp1-descriptive.png', path = './analysis/tables')

# figure for the descriptive statistics

dexp1lang_prereg %>%
  select(c(18:24,38,39)) %>% mutate(speaker = ifelse(speaker=='native', 'Native Speaker', 'Non-native Speaker')) %>%
  rename_with(str_to_title) %>%
  select(c("Speaker", "Easy", "Strong", "Affect", "Status", "Solidarity", "Trustworthy")) %>%
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


ggsave('exp1-scores-barchart.png', path = './analysis/figures')

# from whom they'd like to learn in the future

dexp1lang_prereg %>% 
  filter(!duplicated(ppt)) %>%
  mutate(learn_future = as.factor(learn_future)) %>%
  group_by(learn_future) %>%
  summarise(no_rows = length(learn_future))

# proportion money distributed

tab_exp1moneydist <- dexp1bet_prereg %>%
  select(c(4,11,51)) %>%
  rename_with(str_to_title) %>%
  rename(Money = Raw_money) %>%
  group_by(Delivery, Speaker) %>%
  dplyr::summarise(
    avg_money = mean(as.numeric(Money), na.rm = TRUE),
    sd_money = sd(as.numeric(Money), na.rm = TRUE)
  ) %>% 
  dplyr::mutate(
    distribution = paste(round(avg_money, 2), paste("(", round(sd_money,2), ")", sep = ""), sep = " ")) %>%
  mutate(Delivery = ifelse(Delivery == 'fluent', 'Fluent', 'Disfluent'), Speaker = ifelse(Speaker == 'native', 'Native', 'Non-native')) %>%
  select(c(Delivery, Speaker, distribution)) %>%
  #pivot_wider(names_from = Speaker, values_from = distribution) %>%
  gt(., groupname_col = 'Delivery', rowname_col = 'Speaker') %>%
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
    distribution = "Money bet"
  )
gtsave(tab_exp1moneydist, 'exp1-moneydist.png', path = './analysis/tables')

# viz for poster presentations

dexp1bet_prereg %>%
  mutate(delivery = ifelse(delivery == 'fluent', 'Fluent', 'Disfluent'), speaker = ifelse(speaker == 'native', 'Native', 'Non-native'))%>%
  ggplot(., aes (x = delivery, y = as.numeric(scaled_money), fill = speaker)) +
  geom_split_violin(alpha = .4, trim = FALSE)  +
  geom_boxplot(width = .2, alpha = .6, fatten = NULL, show.legend = FALSE) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175))  +
  scale_x_discrete(name = "Manner of Delivery", labels = c("Disfluent", "Fluent")) +
  scale_y_continuous(name = "Money bet",
                     breaks = seq(0, 100, 20), 
                     limits = c(0, 100)) +
  scale_fill_brewer(palette = "Dark2", name = "Speaker's linguistic background") + # need to decide color palette based on poster palette
  theme_minimal()


ggsave('exp1-money-violin.png', path = './analysis/figures')

# bar chart

dexp1bet_prereg%>%
  group_by(delivery, speaker)%>%
  summarise(mean_money = mean(raw_money, na.rm = TRUE), sd_money = sd(raw_money)) %>% ungroup() %>%
  mutate(delivery = ifelse(delivery == 'fluent', 'Fluent', 'Disfluent'), speaker = ifelse(speaker == 'native', 'Native Speaker', 'Non-native Speaker'))%>%
  mutate(delivery = factor(delivery, levels = c("Fluent", "Disfluent"))) %>%
  ggplot(., aes(x=delivery,y=as.numeric(mean_money), fill = speaker)) +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_money-sd_money, ymax=mean_money+sd_money), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name = "Manner of Delivery", labels = c("Fluent", "Disfluent")) +
  scale_y_continuous(name = "Money bet") + # need to decide color palette based on poster palette
  theme_minimal() +
  scale_fill_manual(values = c("Native Speaker" = "#00A08A",
                               "Non-native Speaker" = "#F2AD00"), name = "Speaker's linguistic background") + theme(legend.position="top")

ggsave('exp1-money-barchart.png', path = './analysis/figures')


dexp1bet_prereg%>%
  #group_by(delivery, speaker, status)%>%
  #summarise(mean_money = mean(raw_money, na.rm = TRUE), sd_money = std.error(raw_money)) %>% ungroup() %>%
  mutate(delivery = ifelse(delivery == 'fluent', 'Fluent', 'Disfluent'), speaker = ifelse(speaker == 'native', 'Native Speaker', 'Non-native Speaker'))%>%
  mutate(delivery = factor(delivery, levels = c("Fluent", "Disfluent"))) %>%
  ggplot(., aes(x=status,y=as.numeric(raw_money), fill = delivery)) + facet_grid(~speaker) +
  geom_smooth() +
  #geom_errorbar(aes(ymin=mean_money-sd_money, ymax=mean_money+sd_money), width=.2,
  #              position=position_dodge(.9)) +
  #scale_x_discrete(name = "Manner of Delivery", labels = c("Fluent", "Disfluent")) +
  scale_y_continuous(name = "Money bet") + # need to decide color palette based on poster palette
  theme_minimal() +
  scale_fill_manual(values = c("Fluent" = "#00A08A",
                               "Disfluent" = "#F2AD00"), name = "Speaker's linguistic background") + theme(legend.position="top")
