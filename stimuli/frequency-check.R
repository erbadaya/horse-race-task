# checks for our stimuli created

stimuli <- read_excel('./stimuli/stimuli-creation.xlsx', sheet = 'ENG-version')
stimuli <- stimuli %>%
  filter(language == 'DUT') %>%
  select(., c(`noun-predictable`)) %>%
  mutate(Word = gsub("een ", "", `noun-predictable`))

stimuli_freqs <- left_join(stimuli, subtlex, by = 'Word') %>% select(., c(Word, Zipf))

stimuli_freqs %>%
  summarise(
    avg = mean(Zipf, na.rm = TRUE),
    maximum = max(Zipf, na.rm = TRUE),
    minimum = min(Zipf, na.rm = TRUE),
    sd = sd(Zipf, na.rm = TRUE)
  )

plot(density(stimuli_freqs$Zipf, na.rm = TRUE))
  