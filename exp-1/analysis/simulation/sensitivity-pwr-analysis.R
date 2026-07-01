# sensitivity power analysis of exp 1
# following DeBruine & Barr (2021)

set.seed(123)
library(tidyverse)
library(lme4)
library(MASS)
library(broom.mixed)
library(faux)
library(simr)
library(lmerTest)
library(parapurrr) # to speed up computation by parallelising it
options(pa_cores = 10)

# Final model takes the form:
# money_bet ∼ delivery * speaker + (1 + delivery + speaker | participant) +  (1 delivery + speaker| horse), family = "gaussian"
# in this case, both factors are within participants and within items (i.e., horse descriptions), because we have done a Latin Square design.

# motivation
# since horse description is counterbalanced, although we could model it, presentation in all categories should kinda diminish it
# however, participants may really differ in these things, specially depending on their familiarity and interaction with non-native speaker
# however NB this interaction might increase dramatically our N (because we only have four obvs per participant)

# there is no direct comparison to this study in order to obtain effect sizes
# we are expecting a medium effect size

# data structure

# specify fixed effects parameters

# we predict more money on fluent than on disfluent
# we predict same money on native and non-native (when fluent)
# we predict more money on non-native disfluent than on native disfluent

# grand intercept = 100 (avg bet)
# effect of delivery = 40
# effect of speaker = 0 (because we expect no effect in itself)
# interaction = 50 (the 40 of delivery will be 25 more for non-native, 25 less for native)

# test sigma, beta_ds, and number of participants

my_sim_data <- function(
    n_subj = 10, # number of participants per list, this means 240 participants 
    n_items = 4, # number of items per list
    beta_0 = 100, # intercept
    beta_d = 40, # effect of manner of delivery
    beta_s = 0, # effect of speaker linguistic background
    beta_ds = 50, # interaction
    subj_0 = 30, # random intercept by-subject
    subj_d = 20, # random slope for delivery by-subject
    subj_s = 10, # random slope for speaker by-subject
    subj_ds = 25, # random slope for interaction delivery and speaker by-subject
    subj_rho = c( # correlation between random effects by-subject
      .1, .1, 
      .1),
    item_0 = 40, # random intercept by-item
    item_d = 35, # random slope for delivery by-item
    item_s = 45, # random slope for speaker by-item
    item_ds = 45, # ranodm slope for interaction delivery and speaker by-item
    item_rho = c(.3, .3, .3, # correlation between random effects by-item
                 .1, .1, 
                 .1),
    sigma = 40 ){ # residual error
  
  # simulate items
  
  items <- faux::rnorm_multi(
    n = n_items, 
    mu = 0,
    sd = c(item_0),
    #r = item_rho,
    varnames = c("I_0")
  ) %>%
    mutate(item_id = rep(c('benito', 'kelso', 'arcandy', 'starlight'))
    ) 
  # simulate subjects
  
  subjects <- faux::rnorm_multi(
    n = n_subj * 24,
    mu = 0,
    sd = c(subj_0, subj_d, subj_s),
    r = subj_rho,
    varnames = c("S_0", "S_d", "S_s")
  ) %>%
    mutate(subj_id = faux::make_id(nrow(.), "S"), cb = rep(LETTERS[1:24], n_subj))
  
  # simulate trials
  
  alltrials <- crossing(subjects, items,
                        delivery = factor(c("fluent", "disfluent")),
                        speaker = factor(c("native", "nonnative")))
  
  # keep only correct combinations
  # yup, this is a long code so fixing it via excel
  
  conditions <- read.csv('test_filter.csv')
  
  trials <- semi_join(alltrials, conditions, by = c("speaker", "delivery",  "cb", 'item_id'))
  
  trials %>%
    mutate(
      X_d = recode(delivery, "disfluent" = -0.5, "fluent" = 0.5),
      X_s = recode(speaker, "native" = 0.5, "nonnative" = -0.5),
      B_0 = beta_0 + S_0 + I_0,
      B_d = beta_d + S_d,
      B_s = beta_s + S_s,
      B_ds = beta_ds,
      e_si = rnorm(nrow(.), mean = 0, sd = sigma),
      money = B_0 + (B_d * X_d) + (B_s * X_s) + (B_ds * X_d * X_s) + e_si
    ) %>%
    dplyr::select(subj_id, item_id, delivery, speaker, X_d, X_s, money)
}

dat_sim <- my_sim_data()

# explore data and check if it makes sense

ggplot(dat_sim, aes(delivery, money, color = speaker)) +
  geom_boxplot(width = 0.25, position = position_dodge(width = 0.9))


single_run <- function(filename = NULL, ...) {
  dat_sim <- my_sim_data(...)
  
  
  # run lmer and capture any warnings
  ww <- ""
  suppressMessages(suppressWarnings(
    mod_sim <- 
      mod_sim <- withCallingHandlers({
        lmer(money ~ 1 + X_d*X_s + 
               (1 + X_d+X_s| item_id) + 
               (1 + X_d+X_s | subj_id),
             data = dat_sim,
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))},
        warning = function(w) { ww <<- w$message }
      )
  ))
  
  # get results table and add rep number and any warnings
  sim_results <- broom.mixed::tidy(mod_sim) %>%
    mutate(warnings = ww)
  
  # add columns for the specified parameters
  params <- list(...)
  for (name in names(params)) {
    sim_results[name] <- params[name]
  }
  
  # append the results to a file if filename is set
  if (!is.null(filename)) {
    append <- file.exists(filename) # append if the file exists
    write_csv(sim_results, filename, append = append)
  }
  
  sim_results
  summary(mod_sim)
}

# EXPLORE CHANGING BTA_DS AND PARTICIPANTS

alpha <- 0.05
ct <- cols(warnings = col_character(),
           # makes sure plots display in this order
           group = col_factor(ordered = TRUE),
           term = col_factor(ordered = TRUE))

# varying sigma, beta_ds and n_subj
# we may have underspecified the sigma

filename2 <- 'sims_diffsigmapptsds.csv'
nreps <- 1000 # change to 1000 for stable estimation BUT will take ages :)

params <- crossing(
  rep = 1:nreps,
  n_subj =  seq(10, 30, by = 5), # we truly do not have funding for more
  beta_ds = seq(10, 60, by = 10),
  sigma = seq(40, 120, by = 10)
) %>% dplyr::select(-rep)


# if (!file.exists(filename2)) {
#   # run simulations and save to a file
#   reps <- 1000 # 1000 provides stable estimations
#   sims <- purrr::pmap_df(params, single_run, filename = filename2)
# } # the output file is now in the appropiate folder

ct <- cols(warnings = col_character(),
           # makes sure plots display in this order
           group = col_factor(ordered = TRUE),
           term = col_factor(ordered = TRUE))

sims2 <- read_csv(filename2, col_types = ct)

# how many times did the model not converge

failure <- sims2 %>% filter(term == '(Intercept)')

prop_failure <- failure %>%
  mutate(
    non_converge = ifelse(is.na(warnings) == FALSE, 1, 0)
  ) %>%
  group_by(n_subj, beta_ds) %>%
  summarise(
    n_fails = sum(non_converge)
  ) %>% group_by(n_subj, beta_ds) %>%
  summarise(prop_failure = n_fails/1000)

power2<- sims2 %>% 
  filter(effect == "fixed", term == 'X_d:X_s') %>%
  group_by(term, sigma) %>%
  summarise(
    mean_estimate = mean(estimate),
    mean_se = mean(std.error),
    power = mean(p.value < alpha),
    .groups = "drop"
  ) 

power2 %>%
  ggplot(aes(y = power, x = sigma)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = round(power, 2)), 
            color = "black", size = 6) +
  scale_x_continuous(name = "Number of subjects",
                     breaks = seq(40, 100, 10)) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  ggtitle("Power for designs varying in number of subjects by list")
