# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)



# Data --------------------------------------------------------------------
peermentor <- read.csv(here::here("data/peermentor.csv"))

# Data Processing ---------------------------------------------------------

peermentor <- peermentor %>%
  mutate(gender_factor = ifelse(gender == "male", 0, 1)) # Male = 0, Female = 1

peermentor <- peermentor %>%
  mutate(region_factor = as.factor(region))

peermentor <- peermentor %>%
  mutate(rehab_success_factor = ifelse(rehab_success == "No", 0, 1)) # No = 0, Yes = 1
  # mutate(rehab_success_factor = as.factor(rehab_success))


# Analysis ----------------------------------------------------------------
## Aim 1: Chi-square testing
# Injecting Status and rehab_success
# Removing missing in injecting status
peermentor_2 <- peermentor %>%
  filter(injecting_status != "Missing")

peermentor_2 <- peermentor_2 %>%
  mutate(injecting_status_factor = as.factor(injecting_status))

chisq.test(table(peermentor_2$rehab_success_factor, 
                 peermentor_2$injecting_status_factor))


