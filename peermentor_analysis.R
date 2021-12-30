# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tableone)

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
# Filtering missing data
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

# Logistic Regression Model
log_mod1 <- glm(data = peermentor_3, family = binomial(link = logit),
                relapse_yes_no ~ intervention_factor + age + 
                gender_factor + duration_use + 
                injecting_status_factor +
               housing_status_factor + rehab_success_factor)

# Does not include rehab_success
log_mod2 <- glm(data = peermentor_3, family = binomial(link = logit),
                relapse_yes_no ~ intervention_factor + age + 
                  gender_factor + duration_use + 
                  injecting_status_factor +
                  housing_status_factor)

# Does not include injecting_status and duration_use
log_mod3 <- glm(data = peermentor_3, family = binomial(link = logit),
                relapse_yes_no ~ intervention_factor + age + 
                  gender_factor + 
                  rehab_success_factor + housing_status_factor)

# Including interaction term
log_mod4 <- glm(data = peermentor_3, family = binomial(link = logit),
                relapse_yes_no ~age + duration_use +
                  housing_status_factor + rehab_success_factor + 
                  intervention_factor + gender_factor + injecting_status_factor +
                intervention_factor : gender_factor + intervention_factor : injecting_status_factor)

summary(log_mod1)
summary(log_mod2)
summary(log_mod3)
summary(log_mod4)
exp(confint(log_mod1))
exp(coef(log_mod1)) # For odds ratio of model

# Creating forest plot for odds ratio
dat <- data.frame(
  Index = c(1,2,3,4,5,6,7,8,9),
  label = c("Intervention: Peer Mentoring vs standard",
            "Age(in years)",
            "Gender: Male vs Female",
            "Duration of opiate use prior to study (in months)",
            "Injecting status: Currently injecting prior to study vs not injecting",
            "Injecting status: Previously injected prior to study vs not injecting",
            "Housing status: Housing problem vs No problem",
            "Housing status: Urgent housing problem vs No problem",
            "Rehabilitation therapy outcome: Success vs Failure"),
  OR = c(0.58, 1.00, 1.06, 0.99, 1.50, 1.66, 1.22, 1.27, 1.22),
  LL = c(0.42, 0.98, 0.68, 0.98, 1.00, 1.14, 0.80, 0.85, 0.87),
  UL = c(0.79, 1.02, 1.64, 1.00, 2.26, 2.41, 1.86, 1.91, 1.71),
  CI = c("0.42, 0.79", "0.98, 1.02", 
         "0.68, 1.64", "0.98, 1.00", "1.00, 2.26", 
         "1.14, 2.41", "0.80, 1.86", "0.85. 1.91", "0.87. 1.71"),
  p = c("< 0.001 ***", "0.89", "0.81", "0.17", "0.05", "< 0.01 **", "0.36", "0.25", "0.25")
)

forest1 <- ggplot(dat, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:9, labels = dat$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

## Create the table-base pallete
table_base <- ggplot(dat, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.02f", round(OR, digits = 3))), size = 4) + ## decimal places
  ggtitle("OR")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size = 4) + 
  ggtitle("95% CI")

tab3 <- table_base + 
  geom_text(aes(y = rev(Index), x = 1, label = p), size = 4) + 
  ggtitle("p-value")

lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3,4,4), nrow = 1)
grid.arrange(forest1, tab1, tab2, tab3, layout_matrix = lay)

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

# Linear model
linear_model1 <- lm(data = peermentor_4, wellbeing1yr ~ intervention_factor + 
                      age + gender_factor + duration_use + 
                      injecting_status_factor +
                      housing_status_factor + rehab_success_factor)

# Does not include rehab_success
linear_model2 <- lm(data = peermentor_4, wellbeing1yr ~ intervention_factor + 
                      age + gender_factor + duration_use + 
                      injecting_status_factor +
                      housing_status_factor)

# Does not include injecting_status and duration_use
linear_model3 <- lm(data = peermentor_4, wellbeing1yr ~ intervention_factor + 
                      age + gender_factor + 
                      housing_status_factor + rehab_success_factor)

# With interaction
linear_model4 <- lm(data = peermentor_4, wellbeing1yr ~ intervention_factor + 
                      age + gender_factor + duration_use * 
                      injecting_status_factor +
                      housing_status_factor + rehab_success_factor)

# Interaction with intervention and gender/injecting status without triple interaction
linear_model5 <- lm(data = peermentor_4, wellbeing1yr ~  age + duration_use +
                      housing_status_factor + rehab_success_factor + 
                    intervention_factor + gender_factor + injecting_status_factor +
                      intervention_factor : gender_factor + intervention_factor : injecting_status_factor)

summary(linear_model1)
summary(linear_model2)
summary(linear_model3)
summary(linear_model4)
summary(linear_model5)
car::vif(linear_model1) # Checking for collinearity
confint(linear_model1)
jtools :: summ(linear_model1, confint = TRUE, digits = 5)

# t-test for 

# Checking linear model assumptions
par(mfrow = c(2,2))
plot(linear_model1)

# Descriptive Statistics --------------------------------------------------
tableone <- CreateTableOne(data = peermentor)
tab3Mat <- print(tableone, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab3Mat, file = "myTable.csv")


# Rectified analysis ---------------------------------------------------------

# Simple logistic regression for aim 2 (include all NAs)
peermentor_allNAs <- peermentor %>%
  mutate(relapse_yes_no = ifelse(is.na(relapse_days), 0, 1)) # 0 = No relapse, 1 = relapse

# Releveling
peermentor_allNAs <- peermentor_allNAs %>%
  mutate(intervention_factor = relevel(intervention_factor, "standard of care"))

peermentor_allNAs <- peermentor_allNAs %>%
  mutate(injecting_status_factor = relevel(injecting_status_factor, "Never injected"))

peermentor_allNAs <- peermentor_allNAs %>%
  mutate(housing_status_factor = relevel(housing_status_factor, "No problem"))

# For multiple regression
peermentor_allNAs_multiple <- peermentor_2 %>%
  mutate(relapse_yes_no = ifelse(is.na(relapse_days), 0, 1)) # 0 = No relapse, 1 = relapse

peermentor_allNAs_multiple <- peermentor_allNAs_multiple %>%
  filter(housing_status != "Other") %>%
  mutate(housing_status_factor = as.factor(housing_status))

# Releveling
peermentor_allNAs_multiple <- peermentor_allNAs_multiple %>%
  mutate(intervention_factor = relevel(intervention_factor, "standard of care"))

peermentor_allNAs_multiple <- peermentor_allNAs_multiple %>%
  mutate(injecting_status_factor = relevel(injecting_status_factor, "Never injected"))

peermentor_allNAs_multiple <- peermentor_allNAs_multiple %>%
  mutate(housing_status_factor = relevel(housing_status_factor, "No problem"))

# Regression
log_modallNAs_simple <- glm(data = peermentor_allNAs, family = binomial(link = logit),
                relapse_yes_no ~ intervention_factor)

log_modallNAs_multiple <- glm(data = peermentor_allNAs_multiple, family = binomial(link = logit),
                relapse_yes_no ~ intervention_factor + age + 
                  gender_factor + duration_use + 
                  injecting_status_factor +
                  housing_status_factor + rehab_success_factor)

summary(log_modallNAs_simple)
summary(log_modallNAs_multiple)
exp(confint(log_modallNAs_simple))
exp(coef(log_modallNAs_simple)) # For odds ratio of model

exp(confint(log_modallNAs_multiple))
exp(coef(log_modallNAs_multiple)) # For odds ratio of model

# Creating forest plot for odds ratio
dat2 <- data.frame(
  Index = c(1,2,3,4,5,6,7,8,9),
  label = c("Intervention: Peer Mentoring vs standard",
            "Age(in years)",
            "Gender: Male vs Female",
            "Duration of opiate use prior to study (in months)",
            "Injecting status: Currently injecting prior to study vs not injecting",
            "Injecting status: Previously injected prior to study vs not injecting",
            "Housing status: Housing problem vs No problem",
            "Housing status: Urgent housing problem vs No problem",
            "Rehabilitation therapy outcome: Success vs Failure"),
  OR = c(0.60, 1.00, 1.13, 0.99, 1.53, 1.56, 1.04, 1.15, 1.78),
  LL = c(0.45, 0.99, 0.77, 0.99, 1.07, 1.12, 0.72, 0.81, 1.34),
  UL = c(0.79, 1.02, 1.67, 1.00, 2.18, 2.17, 1.50, 1.64, 2.39),
  CI = c("0.45, 0.79", "0.99, 1.02", 
         "0.77, 1.67", "0.99, 1.00", "1.07, 2.18", 
         "1.12, 2.17", "0.72, 1.50", "0.81, 1.64", "1.34, 2.39"),
  p = c("< 0.001 ***", "0.53", "0.52", "0.14", "0.02 *", "< 0.01 **", "0.84", "0.42", " < 0.001 ***")
)

forest1 <- ggplot(dat2, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:9, labels = dat2$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

## Create the table-base pallete
table_base <- ggplot(dat2, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.02f", round(OR, digits = 3))), size = 4) + ## decimal places
  ggtitle("OR")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size = 4) + 
  ggtitle("95% CI")

tab3 <- table_base + 
  geom_text(aes(y = rev(Index), x = 1, label = p), size = 4) + 
  ggtitle("p-value")

lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3,4,4), nrow = 1)
grid.arrange(forest1, tab1, tab2, tab3, layout_matrix = lay)


# Simple linear regression for aim 3
peermentor_simple <- peermentor %>%
  mutate(intervention_factor = relevel(intervention_factor, "standard of care"))
linear_model_simple <- lm(data = peermentor_simple, wellbeing1yr ~ intervention_factor)
summary(linear_model_simple)

# Comparing peer vs normal group
# Selecting duration use for successful
intervention_peer <- peermentor %>%
  filter(intervention_factor == "peer mentoring")

# Selecting duration use for unsuccessful
intervention_normal <- peermentor %>%
  filter(intervention_factor == "standard of care")

CreateTableOne(data = intervention_normal)
CreateTableOne(data = intervention_peer)
