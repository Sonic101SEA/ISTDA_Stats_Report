# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)



# Data --------------------------------------------------------------------
peermentor <- read.csv(here::here("data/peermentor.csv"))

# Data Processing ---------------------------------------------------------

peermentor <- peermentor %>%
  # mutate(gender_factor = ifelse(gender == "male", 0, 1)) # Male = 0, Female = 1
  mutate(gender_factor = as.factor(gender))

peermentor <- peermentor %>%
  mutate(region_factor = as.factor(region))

peermentor <- peermentor %>%
  # mutate(rehab_success_factor = ifelse(rehab_success == "No", 0, 1)) # No = 0, Yes = 1
  mutate(rehab_success_factor = as.factor(rehab_success))

peermentor <- peermentor %>%
  mutate(intervention_factor = as.factor(intervention))


# Analysis Aim 1: Chi-square testing ----------------------------------------------------------------
# Injecting Status and rehab_success
# Removing missing in injecting status
peermentor_2 <- peermentor %>%
  filter(injecting_status != "Missing")

peermentor_2 <- peermentor_2 %>%
  mutate(injecting_status_factor = as.factor(injecting_status))


# Chi-square test for rehab_success and injecting_status
chisq.test(table(peermentor_2$rehab_success_factor, 
                 peermentor_2$injecting_status_factor))

# Selecting duration use for successful
duration_use_success <- peermentor %>%
  filter(rehab_success_factor == "Yes") %>%
  select(duration_use)

# Selecting duration use for unsuccessful
duration_use_failure <- peermentor %>%
  filter(rehab_success_factor == "No") %>%
  select(duration_use)

# Boxplot for rehab success and failure duration use
peermentor %>% ggplot(aes(x = rehab_success_factor, y = duration_use)) +
  geom_boxplot(fill = c("red", "blue")) +
  theme_minimal() + ggtitle("Boxplot showing duration use between rehab success and failure")


# 2 sample t-test to see difference
# Unequal variance
t.test(duration_use_success, duration_use_failure)
# Equal variance
t.test(duration_use_success, duration_use_failure, var.equal = TRUE)


# Analysis Aim 2: Logistic Regression -------------------------------------
peermentor_3 <- peermentor_2 %>%
  filter(last_obs_outcome == "Study completed")

peermentor_3 <- peermentor_3 %>%
  mutate(relapse_yes_no = ifelse(is.na(relapse_days), 0, 1)) # 0 = No relapse, 1 = relapse

peermentor_3 <- peermentor_3 %>%
  filter(housing_status != "Other") %>%
  mutate(housing_status_factor = as.factor(housing_status))

# Releveling
peermentor_3 <- peermentor_3 %>%
  mutate(intervention_factor = relevel(intervention_factor, "standard of care"))

peermentor_3 <- peermentor_3 %>%
  mutate(injecting_status_factor = relevel(injecting_status_factor, "Never injected"))

peermentor_3 <- peermentor_3 %>%
  mutate(housing_status_factor = relevel(housing_status_factor, "No problem"))

log_mod1 <- glm(data = peermentor_3, family = binomial(link = logit),
                relapse_yes_no ~ intervention_factor + age + 
                gender_factor + duration_use + 
                injecting_status_factor +
               housing_status_factor + rehab_success_factor)
summary(log_mod1)


# Analysis Aim 3: Linear Regression ---------------------------------------
# Filtering out NA in wellbeing1yr
peermentor_4 <- peermentor_2 %>%
  filter(!is.na(wellbeing1yr)) 

peermentor_4 <- peermentor_4 %>%
  filter(housing_status != "Other") %>%
  mutate(housing_status_factor = as.factor(housing_status))

# Releveling
peermentor_4 <- peermentor_4 %>%
  mutate(intervention_factor = relevel(intervention_factor, "standard of care"))

peermentor_4 <- peermentor_4 %>%
  mutate(injecting_status_factor = relevel(injecting_status_factor, "Never injected"))

peermentor_4 <- peermentor_4 %>%
  mutate(housing_status_factor = relevel(housing_status_factor, "No problem"))

linear_model1 <- lm(data = peermentor_3, wellbeing1yr ~ intervention_factor + 
                      age + gender_factor + duration_use + 
                      injecting_status_factor +
                      housing_status_factor + rehab_success_factor)
summary(linear_model1)
