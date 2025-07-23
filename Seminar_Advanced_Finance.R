# Set working directory
setwd("C:/Users/ehsan/Desktop/seminar/Code")

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(broom)
library(stargazer)
library(e1071)

# Start from raw pl again
load("pl.RData")

# Clean pl
pl <- pl %>%
  select(pid, hid, syear, plh0212:plh0226, plh0255, pla0009_h, plh0034, plh0198, plh0204_v2)

pl[sapply(pl, is.numeric)] <- lapply(pl[sapply(pl, is.numeric)], function(x) ifelse(x < 0, NA, x))
pl <- pl[!apply(is.na(pl), 1, all), ]

# Calculate household size
hh_sizes <- pl %>% group_by(hid, syear) %>% summarise(hh_size = n_distinct(pid), .groups = "drop")

# Merge HH size into pl
pl <- merge(pl, hh_sizes, by = c("hid", "syear"))




# Define reverse-code function
reverse_7pt <- function(x) ifelse(is.na(x), NA, 8 - x)

# Reverse code personality traits
pl <- pl %>%
  mutate(
    plh0214_r = reverse_7pt(plh0214),
    plh0218_r = reverse_7pt(plh0218),
    plh0223_r = reverse_7pt(plh0223),
    plh0226_r = reverse_7pt(plh0226)
  )

# Compute Big Five averages
pl <- pl %>%
  mutate(
    openness_raw = rowMeans(select(., plh0215, plh0220, plh0225, plh0255), na.rm = TRUE),
    conscientiousness_raw = rowMeans(select(., plh0212, plh0218_r, plh0222), na.rm = TRUE),
    extraversion_raw = rowMeans(select(., plh0213, plh0219, plh0223_r), na.rm = TRUE),
    agreeableness_raw = rowMeans(select(., plh0214_r, plh0217, plh0224), na.rm = TRUE),
    neuroticism_raw = rowMeans(select(., plh0216, plh0221, plh0226_r), na.rm = TRUE)
  )




# Standardize Big Five scores
pl <- pl %>%
  mutate(
    openness_standardized = scale(openness_raw)[,1],
    conscientiousness_standardized = scale(conscientiousness_raw)[,1],
    extraversion_standardized = scale(extraversion_raw)[,1],
    agreeableness_standardized = scale(agreeableness_raw)[,1],
    neuroticism_standardized = scale(neuroticism_raw)[,1]
  )


# Load hlc0107_v2 (stock market participation)
# Load hl and use hid for merging instead of pid
hl <- read.csv("hl.csv") %>% select(hid, syear, hlc0107_v2)

# Merge on hid and syear instead of pid
pl <- merge(pl, hl, by = c("hid", "syear"), all.x = TRUE)

# Recode hlc0107_v2 into binary stock market participation variable
pl <- pl %>%
  mutate(stock_participation = ifelse(hlc0107_v2 == 1, 1,
                                      ifelse(!is.na(hlc0107_v2), 0, NA)))

# Restrict to waves used in Jiang et al. (2005, 2009, 2013, 2017)
valid_years <- c(2005, 2009, 2013, 2017)
pl <- pl %>% filter(syear %in% valid_years)

# Filter out missing participation data
pl <- pl %>% filter(!is.na(stock_participation))


# Scale the binary DV to 100
pl$stock_participation <- pl$stock_participation * 100


# Load birth year and sex
biobirth <- read.csv("biobirth.csv") %>% select(pid, gebjahr, sex)

# Keep only valid sex codes (1 = male, 2 = female)
biobirth$sex[!biobirth$sex %in% c(1, 2)] <- NA

# Create female dummy: 1 = female, 0 = male
biobirth <- biobirth %>%
  mutate(sex_female = ifelse(sex == 2, 1,
                             ifelse(sex == 1, 0, NA)))

# Merge into main panel
pl <- merge(pl, biobirth, by = "pid", all.x = TRUE)

# Compute age
pl$age <- pl$syear - pl$gebjahr


# Load controls: income & education
pgen <- read.csv("pgen.csv") %>%
  select(pid, syear, pglabnet, pgbilzeit) %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, NA, .)))

# Load wealth and debt variables, compute averages
pwealth <- read.csv("pwealth.csv") %>%
  select(pid, syear, w0111a:w0111e, w0011a:w0011e)
pwealth[pwealth < 0] <- NA
pwealth <- pwealth %>%
  rowwise() %>%
  mutate(
    net_wealth = mean(c_across(c(w0111a, w0111b, w0111c, w0111d, w0111e)), na.rm = TRUE),
    net_debt   = mean(c_across(c(w0011a, w0011b, w0011c, w0011d, w0011e)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(pid, syear, net_wealth, net_debt)

# Merge income and education controls
pl <- merge(pl, pgen, by = c("pid", "syear"), all.x = TRUE)

# -----------------------------------------------
# Fix `syear` to be numeric before merging/imputation
pl$syear <- as.integer(as.character(pl$syear))
pwealth$syear <- as.integer(as.character(pwealth$syear))
pgen$syear <- as.integer(as.character(pgen$syear))
# -----------------------------------------------

# Impute 2005, 2009, 2013 from 2002 and 2007
for (target_year in c(2005, 2009, 2013)) {
  donor_year <- if (target_year %in% c(2005, 2009)) 2002 else 2007
  
  donor_data <- pwealth %>%
    filter(syear == donor_year) %>%
    select(pid, net_wealth, net_debt) %>%
    mutate(syear = target_year)
  
  pwealth <- bind_rows(pwealth, donor_data)
}

# Merge all controls into pl
pl <- merge(pl, pwealth, by = c("pid", "syear"), all.x = TRUE)

# Restore syear as factor (after all merging/imputation is done)
# Convert syear to dummy variables (14 dummies for last 15 years)
# pl$syear <- as.factor(pl$syear)
# syear_dummies <- model.matrix(~ syear, data = pl)[, -1]
# pl <- cbind(pl, syear_dummies)
pl$syear <- as.factor(pl$syear)


# Rescaling pglabnet, net_wealth and net_debt 
pl <- pl %>%
  mutate(
    pglabnet_k = pglabnet / 1000,        # income in €1,000s
    net_wealth_k = net_wealth / 1000,    # wealth in €1,000s
    net_debt_k = net_debt / 1000         # debt in €1,000s
  )

# Create year dummy variables
pl <- pl %>%
  mutate(
    year_2005 = ifelse(syear == 2005, 1, 0),
    year_2009 = ifelse(syear == 2009, 1, 0),
    year_2013 = ifelse(syear == 2013, 1, 0)
  )


# Create separate copies *after* all processing is complete:
pl_oneperson <- pl %>% filter(hh_size == 1)
pl_multiperson <- pl %>% filter(hh_size > 1)



#--------------------------------#

# I
# Effect of Big 5 on Participants's beliefs about the economy
# DV: plh0034: worried about stability of financial markets in pl dataset
# RESTRICT TO MUTUAL YEARS FOR BELIEFS & PERSONALITY TRAITS
#---------------------------------#

# Restrict to years where both beliefs and personality traits are available
pl_beliefs <- pl %>% 
  filter(syear %in% c(2009, 2013)) %>%
  filter(!is.na(plh0034)) %>%
  mutate(
    worried_finmarkets = plh0034,
    year_2009 = ifelse(syear == 2009, 1, 0)
  )

# Drop rows with NA in any model variable
model_vars <- c(
  "worried_finmarkets",
  "openness_standardized", "conscientiousness_standardized", "extraversion_standardized",
  "agreeableness_standardized", "neuroticism_standardized",
  "age", "sex_female", "pglabnet_k", "pgbilzeit" ,"year_2009"
)

pl_beliefs_clean <- pl_beliefs %>% filter(complete.cases(across(all_of(model_vars))))

# Fit model
model_beliefs <- lm(
  worried_finmarkets ~ 
    openness_standardized + conscientiousness_standardized + extraversion_standardized +
    agreeableness_standardized + neuroticism_standardized +
    age + sex_female + pglabnet_k + pgbilzeit + year_2009,
  data = pl_beliefs_clean
)

# Robust SE
robust_se_beliefs <- vcovHC(model_beliefs, type = "HC1")
print(tidy(coeftest(model_beliefs, vcov. = robust_se_beliefs)), n = Inf)

# Export to LaTeX
stargazer(
  model_beliefs,
  se = list(robust_se_beliefs),
  type = "latex",
  title = "Effect of Big Five Traits on Worry About Financial Markets",
  label = "tab:beliefs_regression",
  column.labels = "Robust Standard Errors",
  dep.var.labels = "Worried About Financial Markets",
  covariate.labels = c(
    "Openness", "Conscientiousness", "Extraversion",
    "Agreeableness", "Neuroticism", "Age", "Female", 
    "Labor Income (k EUR)", "Years of Education", "Year: 2009"
  ),
  omit.stat = c("f", "ser"),
  out = "beliefs_regression.tex"
)


# ---------------------------------------------
# II. Effect of Big Five on Risk Aversion
# DV: plh0198 - Willingness to Take Risks in Financial Matters (contains data only in 2009),
#and  plh0204_v2: Personal willingness to take risks (contains data in 2009, 2013 and 2017)
# ---------------------------------------------

## ---------------------------------------------
# II. Effect of Big Five on Risk Aversion
# ---------------------------------------------

# Create year dummies (baseline = 2017)
pl <- pl %>%
  mutate(
    year_2009 = ifelse(syear == 2009, 1, 0),
    year_2013 = ifelse(syear == 2013, 1, 0)
  )

#-------------------#
# IIa. 2009 Only: Comparison of Two Risk Items
#-------------------#

pl_risk_2009 <- pl %>%
  filter(syear == 2009) %>%
  mutate(
    risk_financial = ifelse(plh0198 < 0, NA, plh0198),
    risk_general   = ifelse(plh0204_v2 < 0, NA, plh0204_v2)
  )

model_vars_2009 <- c(
  "risk_financial", "risk_general",
  "openness_standardized", "conscientiousness_standardized", "extraversion_standardized",
  "agreeableness_standardized", "neuroticism_standardized",
  "age", "sex_female", "pglabnet_k", "pgbilzeit", "net_wealth_k", "net_debt_k"
)

pl_risk_clean_2009 <- pl_risk_2009 %>% filter(complete.cases(across(all_of(model_vars_2009))))

model_fin <- lm(
  risk_financial ~ openness_standardized + conscientiousness_standardized + extraversion_standardized +
    agreeableness_standardized + neuroticism_standardized +
    age + sex_female + pglabnet_k + pgbilzeit + net_wealth_k + net_debt_k,
  data = pl_risk_clean_2009
)

model_gen <- lm(
  risk_general ~ openness_standardized + conscientiousness_standardized + extraversion_standardized +
    agreeableness_standardized + neuroticism_standardized +
    age + sex_female + pglabnet_k + pgbilzeit + net_wealth_k + net_debt_k,
  data = pl_risk_clean_2009
)

# Robust SEs
se_fin <- sqrt(diag(vcovHC(model_fin, type = "HC1")))
se_gen <- sqrt(diag(vcovHC(model_gen, type = "HC1")))

# LaTeX Output: Table of Financial vs General Risk (2009)
stargazer(
  model_fin, model_gen,
  se = list(se_fin, se_gen),
  type = "latex",
  title = "Big Five Personality Traits and Risk Attitudes (2009)",
  label = "tab:risk_side_by_side_2009",
  column.labels = c("Financial Risk", "General Risk"),
  dep.var.labels.include = TRUE,
  covariate.labels = c(
    "Openness", "Conscientiousness", "Extraversion", 
    "Agreeableness", "Neuroticism", "Age", "Female", 
    "Labor Income (k EUR)", "Years of Education", "Net Wealth", "Net Debt"
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  digits = 3
)

#-------------------#
# IIb. Extended Model: Risk General (2009, 2013, 2017)
#-------------------#

pl_genrisk <- pl %>%
  filter(syear %in% c(2009, 2013, 2017)) %>%
  mutate(risk_general = ifelse(plh0204_v2 < 0, NA, plh0204_v2))

model_vars_gen <- c(
  "risk_general",
  "openness_standardized", "conscientiousness_standardized", "extraversion_standardized",
  "agreeableness_standardized", "neuroticism_standardized",
  "age", "sex_female", "pglabnet_k", "pgbilzeit",
  "year_2009", "year_2013"
)

pl_genrisk_clean <- pl_genrisk %>% filter(complete.cases(across(all_of(model_vars_gen))))

model_general_extended <- lm(
  risk_general ~ openness_standardized + conscientiousness_standardized + extraversion_standardized +
    agreeableness_standardized + neuroticism_standardized +
    age + sex_female + pglabnet_k + pgbilzeit + year_2009 + year_2013,
  data = pl_genrisk_clean
)

# Robust SE
se_general_ext <- sqrt(diag(vcovHC(model_general_extended, type = "HC1")))

# LaTeX Output: Risk General Extended
stargazer(
  model_general_extended,
  se = list(se_general_ext),
  type = "latex",
  title = "General Risk Attitudes and Personality Traits (2009, 2013, 2017)",
  label = "tab:risk_general_extended",
  dep.var.labels = "General Risk Willingness",
  covariate.labels = c(
    "Openness", "Conscientiousness", "Extraversion", 
    "Agreeableness", "Neuroticism", "Age", "Female", 
    "Labor Income (k EUR)", "Years of Education", 
    "Year: 2009", "Year: 2013"
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  digits = 3
)


#--------------------------------------------------------------------------------------------#

#--------------III: Effect of Big 5 on Stock Participation-------------#
############ One-Person Household Model ######
#----------------------------------------------------------------------#
model_oneperson <- lm(
  stock_participation ~ 
    openness_standardized + conscientiousness_standardized + extraversion_standardized +
    agreeableness_standardized + neuroticism_standardized +
    age + pglabnet_k + net_wealth_k + net_debt_k + sex_female + pgbilzeit +
    year_2005 + year_2009 + year_2013, 
  data = pl_oneperson   # <- use pl_oneperson here
)

# Robust SE and results
robust_se <- vcovHC(model_oneperson, type = "HC1")
print(tidy(coeftest(model_oneperson, vcov. = robust_se)), n = Inf)


### ------------------------------
### Heads of Households (HH > 1 person)
### ------------------------------

# For pl_multiperson:
# Merge pbrutto to identify heads of household
pbrutto <- read.csv("pbrutto.csv") %>%
  select(pid, syear, stell_h) %>%
  mutate(
    stell_h = ifelse(stell_h < 0, NA, stell_h),
    head_of_household = ifelse(stell_h == 0, 1, 0)
  )

# --- FIX THE FACTOR PROBLEM HERE ---
pl_multiperson$syear <- as.numeric(as.character(pl_multiperson$syear))
pbrutto$syear <- as.numeric(as.character(pbrutto$syear))

# Merge
pl_multiperson <- merge(pl_multiperson, pbrutto, by = c("pid", "syear"), all.x = TRUE)


# Filter to heads only
pl_heads <- pl_multiperson %>% filter(head_of_household == 1)

model_heads <- lm(
  stock_participation ~ 
    openness_standardized + conscientiousness_standardized + extraversion_standardized +
    agreeableness_standardized + neuroticism_standardized +
    age + pglabnet_k + net_wealth_k + net_debt_k + sex_female + pgbilzeit +
    year_2005 + year_2009 + year_2013, 
  data = pl_heads
)

# Compute robust standard errors
robust_se_heads <- vcovHC(model_heads, type = "HC1")
# Output regression results
print(tidy(coeftest(model_heads, vcov. = robust_se_heads)), n = Inf)

# --------------------------------------------------
# Comparison of one-person households and head of the household
# Side-by-side regression comparison for thesis
stargazer(
  model_oneperson, model_heads,
  type = "text",  # use "latex" if you're writing in LaTeX
  title = "Comparison of Big Five Effects on Stock Market Participation",
  column.labels = c("One-Person HH", "Heads in Multi-Person HHs"),
  dep.var.labels = "Stock Market Participation (%)",
  covariate.labels = c(
    "Openness", "Conscientiousness", "Extraversion", 
    "Agreeableness", "Neuroticism", "Age", 
    "Net Monthly Income (€1,000)", 
    "Net Wealth (€1,000)", "Net Debt (€1,000)", 
    "Female", "Years of Education" , "Year 2005", "Year 2009", "Year 2013"
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  digits = 3
)


#---------------------------------------------------



#------------------------
# Refinement
############ Refined Model with Openness_raw squared  #####################

# Openness do not have linear Rel but Quadratic Rel
# Add openness_sq to both datasets
pl_oneperson <- pl_oneperson %>%
  mutate(openness_sq = openness_raw^2)

pl_heads <- pl_heads %>%
  mutate(openness_sq = openness_raw^2)

model_oneperson_quad <- lm(
  stock_participation ~ 
    openness_raw + openness_sq +
    conscientiousness_raw + extraversion_raw +
    agreeableness_raw + neuroticism_raw +
    age + pglabnet_k + net_wealth_k + net_debt_k + sex_female + pgbilzeit +
    year_2005 + year_2009 + year_2013, 
  data = pl_oneperson   # <- use pl_oneperson here
)

# Robust SE and results
robust_se <- vcovHC(model_oneperson_quad, type = "HC1")
print(tidy(coeftest(model_oneperson_quad, vcov. = robust_se)), n = Inf)


model_heads_quad <- lm(
  stock_participation ~ 
    openness_raw + openness_sq +
    conscientiousness_raw + extraversion_raw +
    agreeableness_raw + neuroticism_raw +
    age + pglabnet_k + net_wealth_k + net_debt_k + sex_female + pgbilzeit +
    year_2005 + year_2009 + year_2013, 
  data = pl_heads
)

# Compute robust standard errors
robust_se_heads <- vcovHC(model_heads_quad, type = "HC1")
# Output regression results
print(tidy(coeftest(model_heads_quad, vcov. = robust_se_heads)), n = Inf)


# Side-by-side regression comparison for thesis
stargazer(
  model_oneperson_quad, model_heads_quad,
  type = "text",
  se = list(sqrt(diag(robust_se)), sqrt(diag(robust_se_heads))),  # <- Add this
  title = "Comparison of Big Five Effects on Stock Market Participation",
  column.labels = c("One-Person HH", "Heads in Multi-Person HHs"),
  dep.var.labels = "Stock Market Participation (%)",
  covariate.labels = c(
    "Openness", "Openness²",
    "Conscientiousness", "Extraversion", 
    "Agreeableness", "Neuroticism", "Age", 
    "Net Monthly Income (€1,000)", 
    "Net Wealth (€1,000)", "Net Debt (€1,000)", 
    "Female", "Years of Education" , "Year 2005", "Year 2009", "Year 2013"
  ),
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  digits = 3
)

# Plot
library(ggplot2)

# Create prediction data for plotting
plot_data <- data.frame(openness_raw = seq(min(pl_oneperson$openness_raw, na.rm = TRUE),
                                           max(pl_oneperson$openness_raw, na.rm = TRUE),
                                           length.out = 100))

# Add quadratic term
plot_data$openness_sq <- plot_data$openness_raw^2

# Predict holding other variables at mean
predictors <- pl_oneperson %>%
  summarise(across(c(conscientiousness_raw, extraversion_raw, agreeableness_raw, neuroticism_raw,
                     age, pglabnet_k, net_wealth_k, net_debt_k, sex_female, pgbilzeit, 
                     year_2005, year_2009, year_2013), ~mean(., na.rm = TRUE)))

plot_data <- cbind(plot_data, predictors[rep(1, 100), ])

# Predict values
plot_data$predicted <- predict(model_oneperson_quad, newdata = plot_data)

# Plot
ggplot(plot_data, aes(x = openness_raw, y = predicted)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Predicted Stock Market Participation by Openness",
    x = "Openness (Raw Score)",
    y = "Predicted Stock Participation (%)"
  ) +
  theme_minimal(base_size = 14)

# Likelihood Ratio Test to see whether using Openness Squared is justified

library(lmtest)

# Compare linear vs. quadratic model
lrtest(model_oneperson, model_oneperson_quad)
lrtest(model_heads,model_heads_quad)

#------- IV: a. Running a pooled baseline regression (Big Five + controls, no mediators).----#

# b. Comparing it to your mediation-style regression (Big Five + mediators + controls).

# c. Looking at coefficient shrinkage and significance changes → evidence of mediation.

# d. Formal Mediation Test

# THE GOAL OF THE SECTION WHICH IS THE MAIN ONE:
#“To check if each personality trait has a significant effect on stock participation through these two channels:

#Worried About Financial Markets (plh0034)

#General Risk Willingness (plh0204_v2)”

# Base Model Without Mediatiors

pl_base <- pl %>%
  filter(syear %in% c(2009, 2013)) %>%
  filter(!is.na(stock_participation))

model_vars_base <- c(
  "stock_participation",
  "openness_standardized", "conscientiousness_standardized", "extraversion_standardized",
  "agreeableness_standardized", "neuroticism_standardized",
  "age", "sex_female", "pglabnet_k", "pgbilzeit",
  "net_wealth_k", "net_debt_k", "year_2009"
)

pl_base_clean <- pl_base %>%
  filter(complete.cases(across(all_of(model_vars_base))))

model_participation_base <- lm(
  stock_participation ~ 
    openness_standardized + conscientiousness_standardized + extraversion_standardized +
    agreeableness_standardized + neuroticism_standardized +
    age + sex_female + pglabnet_k + pgbilzeit +
    net_wealth_k + net_debt_k + year_2009,
  data = pl_base_clean
)

# Robust SE
library(sandwich)
library(lmtest)
robust_se_base <- vcovHC(model_participation_base, type = "HC1")
base_results <- coeftest(model_participation_base, vcov. = robust_se_base)
print(base_results)



# Model (Belief + General Risk)
# Model code for stock participation, using plh0034 and plh0204_v2 as mediators

pl_mechanisms <- pl %>%
  filter(syear %in% c(2009, 2013)) %>%
  filter(!is.na(stock_participation)) %>%
  mutate(
    risk_general = ifelse(plh0204_v2 < 0, NA, plh0204_v2),
    worried_finmarkets = ifelse(plh0034 < 0, NA, plh0034)
  )

model_vars <- c(
  "stock_participation",
  "openness_standardized", "conscientiousness_standardized", "extraversion_standardized",
  "agreeableness_standardized", "neuroticism_standardized",
  "risk_general", "worried_finmarkets",
  "age", "sex_female", "pglabnet_k", "pgbilzeit",
  "net_wealth_k", "net_debt_k",
  "year_2009"  # reference category is 2013
)

pl_clean <- pl_mechanisms %>%
  filter(complete.cases(across(all_of(model_vars))))

model_participation_mechanisms <- lm(
  stock_participation ~ 
    openness_standardized + conscientiousness_standardized + extraversion_standardized +
    agreeableness_standardized + neuroticism_standardized +
    worried_finmarkets + risk_general +
    age + sex_female + pglabnet_k + pgbilzeit +
    net_wealth_k + net_debt_k + year_2009,
  data = pl_clean
)

# Robust SE
library(sandwich)
library(lmtest)
robust_se_mechanism <- vcovHC(model_participation_mechanisms, type = "HC1")
coeftest(model_participation_mechanisms, vcov. = robust_se_mechanism)


# Output Table

# Run stargazer with robust SEs
stargazer(model_participation_base, model_participation_mechanisms,
          type = "text",  # use "latex" if you want direct LaTeX output
          se = list(
            sqrt(diag(robust_se_base)),
            sqrt(diag(robust_se_mechanism))
          ),
          title = "Big Five Traits and Stock Market Participation: Baseline vs. Mediated Model",
          dep.var.labels = "Stock Market Participation (%)",
          column.labels = c("Baseline Model", "With Mediators"),
          covariate.labels = c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism",
                               "Worried About Markets", "Risk Willingness",
                               "Age", "Female", "Net Labor Income", "Education",
                               "Net Wealth", "Net Debt", "Year 2009"),
          omit.stat = c("f"),  # remove F-statistic
          digits = 3,
          no.space = TRUE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01)
)



#A formal mediation test statistically quantifies how much of an independent variable’s effect
#(e.g., Neuroticism) on a dependent variable (e.g., stock participation) is indirectly transmitted through a mediator (e.g., beliefs or risk).

#The key outputs are:
#ACME (Average Causal Mediation Effect): the indirect effect
#ADE (Average Direct Effect): the part not explained by the mediator
#Total effect: ACME + ADE
#Proportion Mediated: how much of the total effect flows through the mediator

library(mediation)
# MODEL 1: Mediator model (beliefs ~ Neuroticism)
model_mediator <- lm(
  worried_finmarkets ~ neuroticism_standardized + age + sex_female +
    pglabnet_k + pgbilzeit + year_2009,
  data = pl_clean
)

# MODEL 2: Outcome model (participation ~ Neuroticism + Beliefs)
model_outcome <- lm(
  stock_participation ~ neuroticism_standardized + worried_finmarkets +
    age + sex_female + pglabnet_k + pgbilzeit + net_wealth_k + net_debt_k + year_2009,
  data = pl_clean
)

# Run formal mediation test
mediation_result <- mediate(
  model.m = model_mediator,
  model.y = model_outcome,
  treat = "neuroticism_standardized",
  mediator = "worried_finmarkets",
  boot = TRUE,
  sims = 1000
)

summary(mediation_result)



# Openness → Risk → Participation

library(mediation)

# Subset for relevant years
med_data <- pl %>%
  filter(syear %in% c(2009, 2013)) %>%
  mutate(
    risk_general = ifelse(plh0204_v2 < 0, NA, plh0204_v2)
  ) %>%
  filter(complete.cases(
    openness_standardized, risk_general, stock_participation,
    age, sex_female, pglabnet_k, pgbilzeit,
    net_wealth_k, net_debt_k, year_2009
  ))

model_mediator <- lm(
  risk_general ~ openness_standardized + age + sex_female + pglabnet_k + 
    pgbilzeit + year_2009,
  data = med_data
)

model_outcome <- lm(
  stock_participation ~ openness_standardized + risk_general + age + sex_female + 
    pglabnet_k + pgbilzeit + net_wealth_k + net_debt_k + year_2009,
  data = med_data
)

med_result <- mediate(
  model.m = model_mediator,
  model.y = model_outcome,
  treat = "openness_standardized",
  mediator = "risk_general",
  boot = TRUE,
  sims = 1000
)

summary(med_result)




#---------------- PCA Analysis----------------#
# Create a data frame with just the personality items (with reversed ones included)
pca_items <- dplyr::select(pl,
                           pid, syear, plh0212, plh0213, plh0214_r, plh0215, plh0216,
                           plh0217, plh0218_r, plh0219, plh0220, plh0221, plh0222,
                           plh0223_r, plh0224, plh0225, plh0226_r
)

pca_items_clean <- pca_items %>% filter(complete.cases(.))
write.csv(pca_items_clean, "pca_input.csv", row.names = FALSE)
#-----------------------------------------------#


#--------Robustness Check: Probit/Logit--------------------#
model_logit <- glm(
  stock_participation / 100 ~ openness_standardized + conscientiousness_standardized + 
    extraversion_standardized + agreeableness_standardized + neuroticism_standardized +
    age + sex_female + pglabnet_k + pgbilzeit + net_wealth_k + net_debt_k + 
    year_2009 + year_2013,
  data = pl_clean, family = binomial(link = "logit")
)
# Summary output
summary(model_logit)

# Probit model using same specification as logit
model_probit <- glm(
  stock_participation / 100 ~ openness_standardized + conscientiousness_standardized + 
    extraversion_standardized + agreeableness_standardized + neuroticism_standardized +
    age + sex_female + pglabnet_k + pgbilzeit + net_wealth_k + net_debt_k + 
    year_2009 + year_2013,
  data = pl_clean,
  family = binomial(link = "probit")
)

# Summary output
summary(model_probit)


######################################################



#------------------------ Visualizations -------------------------- #
#-----Table 1: Summary Statistics and Personality Models -----#

library(dplyr)
library(tidyr)
library(e1071)
library(knitr)

# Select and rename variables for summary
panel_a_vars <- pl %>%
  transmute(
    Female = sex_female,
    Age = age,
    `Current Net Labor Income (k EUR)` = pglabnet_k,
    `Net Overall Wealth (k EUR)` = net_wealth_k,
    `Overall Debts (k EUR)` = net_debt_k,
    `Years of Education` = pgbilzeit,
    Agreeableness = agreeableness_raw,
    Conscientiousness = conscientiousness_raw,
    Neuroticism = neuroticism_raw,
    Extraversion = extraversion_raw,
    Openness = openness_raw
  )

# Compute summary statistics and reshape into tidy format
panel_a_tidy <- panel_a_vars %>%
  summarise(across(everything(), list(
    Mean = ~mean(., na.rm = TRUE),
    SD   = ~sd(., na.rm = TRUE),
    P10  = ~quantile(., 0.10, na.rm = TRUE),
    P50  = ~quantile(., 0.50, na.rm = TRUE),
    P90  = ~quantile(., 0.90, na.rm = TRUE),
    Skew = ~skewness(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = value)

# Show the table
kable(panel_a_tidy, digits = 2, caption = "Panel A: Summary Statistics of Demographics and Personality Traits")

# ---- Panel B: Correlation Matrix (Big Five only) ----
panel_b_vars <- pl %>%
  select(
    agreeableness_raw, conscientiousness_raw, neuroticism_raw,
    extraversion_raw, openness_raw
  )

panel_b <- cor(panel_b_vars, use = "pairwise.complete.obs")
round(panel_b, 2)  # For display

# Optional: LaTeX export
stargazer(panel_b, type = "latex", title = "Panel B: Correlation matrix of Big Five traits")

# ---- Panel C: Beliefs and Risk Preferences ----
panel_c <- pl %>%
  select(plh0034, plh0198) %>%
  summarise_all(list(
    Mean = ~mean(., na.rm = TRUE),
    SD   = ~sd(., na.rm = TRUE),
    P10  = ~quantile(., 0.1, na.rm = TRUE),
    P50  = ~quantile(., 0.5, na.rm = TRUE),
    P90  = ~quantile(., 0.9, na.rm = TRUE),
    Skew = ~skewness(., na.rm = TRUE)
  ))

kable(panel_c, digits = 2, caption = "Panel C: Beliefs and risk preference variables")




# ------------------ Table 2: Personality Traits and Demographic Controls -----------------#
# This version treats 'pgbilzeit' (years of education) as numeric
# and includes year fixed effects via factor(syear)

models <- list(
  lm(agreeableness_standardized ~ sex_female + age + pglabnet_k +
       net_wealth_k + net_debt_k + pgbilzeit + year_2005 + year_2009 + year_2013, data = pl),
  
  lm(conscientiousness_standardized ~ sex_female + age + pglabnet_k +
       net_wealth_k + net_debt_k + pgbilzeit + year_2005 + year_2009 + year_2013, data = pl),
  
  lm(neuroticism_standardized ~ sex_female + age + pglabnet_k +
       net_wealth_k + net_debt_k + pgbilzeit + year_2005 + year_2009 + year_2013, data = pl),
  
  lm(extraversion_standardized ~ sex_female + age + pglabnet_k +
       net_wealth_k + net_debt_k + pgbilzeit + year_2005 + year_2009 + year_2013, data = pl),
  
  lm(openness_standardized ~ sex_female + age + pglabnet_k +
       net_wealth_k + net_debt_k + pgbilzeit + year_2005 + year_2009 + year_2013, data = pl)
)

# Output LaTeX-formatted summary table
stargazer(models,
          type = "latex",
          column.labels = c("Agreeableness", "Conscientiousness", "Neuroticism", "Extraversion", "Openness"),
          title = "Table 2: Personality Traits and Demographic Controls",
          dep.var.labels.include = FALSE,
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          digits = 3
)


#----------------Figure 1: Demographics Distribution (Revised)--------------------#
library(ggplot2)
library(gridExtra)  # For arranging plots
library(grid)       # For textGrob
library(dplyr)

# Step 1: Create demographic groups
pl <- pl %>%
  mutate(
    gender = factor(ifelse(sex_female == 1, "Female", "Male")),
    age_group = cut(age, breaks = c(0, 30, 39, 49, 59, 69, Inf), 
                    labels = c("Below 30", "30–39", "40–49", "50–59", "60–69", "70+"), right = FALSE),
    education = cut(pgbilzeit, breaks = c(0, 9, 11, 13, 15, 17, Inf),
                    labels = c("Less than HS", "HS Grad", "Some College", "Bachelor's", "Master's", "Doctoral"), right = FALSE),
    income = cut(pglabnet_k, breaks = c(0, 1, 2, 5, 10, Inf),
                 labels = c("<€1k", "€1k–2k", "€2k–5k", "€5k–10k", ">€10k"), right = FALSE),
    wealth = cut(net_wealth_k, breaks = c(0, 50, 100, 250, 500, 1000, Inf),
                 labels = c("<€50k", "€50–100k", "€100–250k", "€250–500k", "€500k–1M", ">€1M"), right = FALSE),
    debt = cut(net_debt_k, breaks = c(0, 10, 25, 50, 100, 250, Inf),
               labels = c("<€10k", "€10–25k", "€25–50k", "€50–100k", "€100–250k", ">€250k"), right = FALSE)
  )

# Step 2: Filter datasets for missing values where needed
pl_filtered_income <- pl %>% filter(!is.na(income))
pl_filtered_debt <- pl %>% filter(!is.na(debt))

# Step 3: Generate individual plots
p1 <- ggplot(pl, aes(x = gender)) + geom_bar(fill = "white", color = "black") +
  labs(title = "Gender", x = "Gender", y = "Count") + theme_minimal()

p2 <- ggplot(pl, aes(x = age_group)) + geom_bar(fill = "white", color = "black") +
  labs(title = "Age", x = "Age Group", y = "Count") + theme_minimal()

p3 <- ggplot(pl, aes(x = education)) + geom_bar(fill = "white", color = "black") +
  labs(title = "Education", x = "Education Level", y = "Count") + theme_minimal()

p4 <- ggplot(pl_filtered_income, aes(x = income)) + geom_bar(fill = "white", color = "black") +
  labs(title = "Income", x = "Monthly Net Labor Income", y = "Count") + theme_minimal()

p5 <- ggplot(pl %>% filter(!is.na(wealth)), aes(x = wealth)) + 
  geom_bar(fill = "white", color = "black") +
  labs(title = "Wealth", x = "Net Wealth", y = "Count") + 
  theme_minimal()

p6 <- ggplot(pl_filtered_debt, aes(x = debt)) + 
  geom_bar(fill = "white", color = "black") +
  labs(title = "Debt", x = "Net Debt", y = "Count") + 
  theme_minimal()

# Step 4: Save the multi-panel figure with an annotation on imputation
png("demographics_distribution.png", width = 1200, height = 1200)
grid.arrange(
  p1, p2, p3, p4, p5, p6,
  ncol = 2,
  top = textGrob("Figure 1: Distribution of Demographic Variables in the SOEP Sample", 
                 gp = gpar(fontsize = 14, fontface = "bold")),
  bottom = textGrob("Note: Wealth and debt in 2005, 2009, 2013 are imputed from earlier waves (2002, 2007).", 
                    gp = gpar(fontsize = 10), hjust = 1, x = 1)
)
dev.off()


#----------Figure 2: Personality Traits Visualization --------------------------#

library(ggplot2)
library(tidyr)
library(dplyr)

# Reshape data from wide to long format for plotting
pl_long <- pl %>%
  select(openness_raw, conscientiousness_raw, extraversion_raw,
         agreeableness_raw, neuroticism_raw) %>%
  pivot_longer(cols = everything(),
               names_to = "trait", values_to = "score")

# Make trait names prettier
pl_long$trait <- factor(pl_long$trait,
                        levels = c("agreeableness_raw", "conscientiousness_raw",
                                   "neuroticism_raw", "extraversion_raw", "openness_raw"),
                        labels = c("Agreeableness", "Conscientiousness", "Neuroticism",
                                   "Extraversion", "Openness"))

# Save plot to file for LaTeX inclusion
png("personality_traits_distribution.png", width = 1400, height = 400)
ggplot(pl_long, aes(x = score)) +
  geom_histogram(bins = 20, fill = "gray90", color = "black") +
  facet_wrap(~ trait, scales = "free", nrow = 1) +
  labs(x = "Personality Trait Score", y = "Count",
       title = "Distribution of Big Five Personality Traits") +
  theme_minimal(base_size = 14)
dev.off()


########################

#----------Correlation matrix with all variables at hand------------#

# Select relevant variables
corr_vars <- pl %>%
  select(
    agreeableness_standardized,
    conscientiousness_standardized,
    neuroticism_standardized,
    extraversion_standardized,
    openness_standardized,
    sex_female,
    age,
    pgbilzeit,
    pglabnet_k,
    net_wealth_k,
    net_debt_k
  )

# Compute correlation matrix with pairwise deletion
corr_matrix <- cor(corr_vars, use = "pairwise.complete.obs")

# Round for neatness
corr_matrix_rounded <- round(corr_matrix, 2)

# Print to console (nice view)
print(corr_matrix_rounded)

# Optional: export to LaTeX
stargazer(corr_matrix_rounded, type = "latex", title = "Correlation Matrix of Key Variables")



