# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)



# Data --------------------------------------------------------------------
peermentor <- read.csv(here::here("data/peermentor.csv"))

# Data Processing ---------------------------------------------------------
peermentor <- peermentor %>%
  mutate(gender_factor = ifelse(gender == "male", 0, 1)) # Male = 0, Female = 1

peermentor <- peermentor %>%
  mutate(region_level = as.factor(region))

peermentor <- peermentor %>%
  mutate(rehab_success_factor = ifelse(rehab_success == "No", 0, 1)) # No = 0, Yes = 1


