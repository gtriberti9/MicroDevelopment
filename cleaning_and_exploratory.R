# Zixuan Zhou, Giuliana Triberti

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(reshape2)
library(tidyverse)
library(broom)
library(sjPlot)
library(sandwich)
library(lmtest)
library(lme4)
library(purrr)
library(stringr)


## -------------- IMPORTING THE DATA ------------------##

# This table contains the result of the interview, the household 
# factor and other household characteristics.
household_data <- read.csv(unz("DATABASE.zip", "CS_HOGARES.csv"))

# This table contains the socio-demographic characteristics of the 
# household members, health status and utilization of health services, among others.
individual_data <- read.csv(unz("DATABASE.zip", "CS_RESIDENTES.csv"))

# The table includes information on variables related to the health 
# status of adults, such as obesity, hypertension, depressive symptoms, 
# diabetes mellitus, reproductive health, among others, which were 
# collected in the health questionnaire for adults (20 years and older). 
adult_diseases_data <- read.csv(unz("DATABASE.zip","CS_ADULTOS.csv"))

# The table includes variables from section IV of the household 
# questionnaire and those corresponding to the questionnaire of health 
# service users, these variables capture situations regarding the use of 
# ambulatory services, accessibility and quality of health services, 
# medicines, among others.
health_quality_data <- read.csv(unz("DATABASE.zip","CS_SERV_SALUD.csv"))


# --------------- DESCRIPTIVE STATISTICS -------------------

state_names <- c(
  "1" = "Aguascalientes", "2" = "Baja California", "3" = "Baja California Sur",
  "4" = "Campeche", "5" = "Coahuila", "6" = "Colima", "7" = "Chiapas",
  "8" = "Chihuahua", "9" = "CDMX", "10" = "Durango", "11" = "Guanajuato",
  "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco", "15" = "México",
  "16" = "Michoacán", "17" = "Morelos", "18" = "Nayarit", "19" = "Nuevo León",
  "20" = "Oaxaca", "21" = "Puebla", "22" = "Querétaro", "23" = "Quintana Roo",
  "24" = "San Luis Potosí", "25" = "Sinaloa", "26" = "Sonora", "27" = "Tabasco",
  "28" = "Tamaulipas", "29" = "Tlaxcala", "30" = "Veracruz", "31" = "Yucatán",
  "32" = "Zacatecas"
)

# Count households by state and exclude NA values
household_counts <- household_data %>%
  filter(!is.na(ENT)) %>%
  count(ENT, name = "num_households") %>%
  mutate(state_name = state_names[as.character(ENT)])

# Plot
ggplot(household_counts, aes(x = reorder(state_name, -num_households), y = num_households)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Households by State",
       x = "State",
       y = "Number of Households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ------------ UPDATING THE DATABASES WITHOUT TOP 5 STATES ------------

household_data <- household_data %>%
  filter(ENT != 2 & ENT != 9 & ENT != 14 & ENT != 19 & ENT != 22)

individual_data <- individual_data %>%
  filter(ENT != 2 & ENT != 9 & ENT != 14 & ENT != 19 & ENT != 22)

adult_diseases_data <- adult_diseases_data %>%
  filter(ENT != 2 & ENT != 9 & ENT != 14 & ENT != 19 & ENT != 22)

health_quality_data <- health_quality_data %>%
  filter(ENT != 2 & ENT != 9 & ENT != 14 & ENT != 19 & ENT != 22)



## --------------- DESCRIPTIVE STATISTICS -------------------
## From individual_data, "do you have health problems, where do you usually 
## go for care?"

labels <- c(
  "P3_9_01" = "Social Security (IMSS)",
  "P3_9_02" = "ISSSTE",
  "P3_9_03" = "State-level ISSSTE",
  "P3_9_04" = "Pemex",
  "P3_9_05" = "Defensa",
  "P3_9_06" = "Navy",
  "P3_9_07" = "Seguro Popular",
  "P3_9_08" = "IMSS PROSPERA",
  "P3_9_09" = "Private insurance",
  "P3_9_10" = "Other institution",
  "P3_9_11" = "Not affiliated",
  "P3_9_77" = "Other",
  "P3_9_12" = "None",
  "P3_9_99" = "Doesn't know"
)

# Select the columns of interest
binary_vars <- c("P3_9_01", "P3_9_02", "P3_9_03", "P3_9_04", "P3_9_05",
                 "P3_9_06", "P3_9_07", "P3_9_08", "P3_9_09", "P3_9_10",
                 "P3_9_11", "P3_9_77", "P3_9_12", "P3_9_99")

# Summarize counts of 1s
counts_df <- individual_data %>%
  select(all_of(binary_vars)) %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "code", values_to = "count") %>%
  mutate(label = labels[code]) %>%
  arrange(desc(count))

ggplot(counts_df, aes(x = reorder(label, count), y = count)) +
  geom_col(fill = "skyblue") +
  coord_flip() +  # horizontal for long labels
  labs(
    title = "Where do you usually go for care",
    x = "Service",
    y = "Number of People"
  ) +
  theme_minimal(base_size = 12)


## People that is entitled to or has access to these medical services

labels <- c(
  "1" = "Social Security (IMSS)",
  "2" = "ISSSTE",
  "3" = "State-level ISSSTE",
  "4" = "Pemex",
  "5" = "Defensa",
  "6" = "Navy",
  "7" = "Seguro Popular",
  "8" = "IMSS PROSPERA",
  "9" = "Private insurance",
  "10" = "Other institution",
  "11" = "Not affiliated",
  "99" = "Doesn't know"
)

# Create data frame with counts
access_df <- individual_data %>%
  count(P3_10_OPC1) %>%
  filter(P3_10_OPC1 %in% names(labels)) %>%
  mutate(service = labels[as.character(P3_10_OPC1)]) %>%
  arrange(desc(n))  # sort by frequency

# Plot
ggplot(access_df, aes(x = reorder(service, n), y = n)) +
  geom_col(fill = "skyblue") +
  coord_flip() +  # horizontal bars = better for long labels
  labs(
    title = "Access to Medical Services",
    x = "Healthcare Service",
    y = "Number of People"
  ) +
  theme_minimal(base_size = 12)


## --------- PROPORTION OF PEOPLE BY STRATA -----------
## Here we can see that the highest strata also goes to 
## the highest quality health providers


# Create proportional data
proportional_data <- individual_data %>%
  filter(!is.na(P3_10_OPC1) & P3_10_OPC1 != 99) %>%
  group_by(ESTRATO, P3_10_OPC1) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(service = labels[as.character(P3_10_OPC1)]) %>%
  filter(!is.na(service)) %>%
  group_by(ESTRATO) %>%
  mutate(percentage = count / sum(count) * 100)

# Create 100% stacked bar chart
ggplot(proportional_data, aes(x = factor(ESTRATO), y = percentage, fill = service)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Proportional Distribution of Health Services by Economic Strata",
    x = "Economic Strata (ESTRATO)",
    y = "Percentage",
    fill = "Health Service"
  ) +
  theme_minimal() +
  theme(legend.position = "right")




## --------- CREATING QUALITY INDEX BY PROVIDER ----------

# Reverse code negatively oriented Likert variables
health_quality_data <- health_quality_data %>%
  mutate(
    P4_6_rc = ifelse(P4_6 %in% 1:5, 6 - as.numeric(as.character(P4_6)), NA),
    P7_7_rc = ifelse(P7_7 %in% 1:5, 6 - as.numeric(as.character(P7_7)), NA)
  )

# Recode Yes/No variables: 1 = Yes = 1, 2 = No = 0, others = NA
yes_no_vars <- c("P7_2", "P8_8_1", "P8_8_2", "P8_8_3")
health_quality_data <- health_quality_data %>%
  mutate(across(all_of(yes_no_vars), ~ case_when(
    . == 1 ~ 1,
    . == 2 ~ 0,
    TRUE ~ NA_real_
  )))

# Convert Likert variables to numeric (1 to 5)
likert_vars <- c(
  "P3_5", "P8_2", "P8_3", "P8_4",
  "P8_5_1", "P8_5_2", "P8_5_3", "P8_5_4", "P8_5_5", "P8_5_6", "P8_6"
)
health_quality_data <- health_quality_data %>%
  mutate(across(all_of(likert_vars), ~ ifelse(. %in% 1:5, as.numeric(as.character(.)), NA_real_)))

# Define scoring rules
likert_5_vars <- c(
  "P3_5", "P4_6_rc", "P7_7_rc", "P8_2", "P8_3", "P8_4",
  "P8_5_1", "P8_5_2", "P8_5_3", "P8_5_4", "P8_5_5", "P8_5_6", "P8_6"
)
binary_vars <- c("P7_2", "P8_8_1", "P8_8_2", "P8_8_3")

# Calculate raw score, max possible score, and normalized index
health_quality_data <- health_quality_data %>%
  mutate(
    quality_score_raw = rowSums(across(all_of(c(likert_5_vars, binary_vars))), na.rm = TRUE),
    quality_score_max = rowSums(across(all_of(likert_5_vars), ~ ifelse(!is.na(.), 5, 0))) +
      rowSums(across(all_of(binary_vars), ~ ifelse(!is.na(.), 1, 0))),
    quality_score = ifelse(quality_score_max > 0, quality_score_raw / quality_score_max, NA_real_)
  )


# Compare quality_score across providers

# Recode provider labels
labels <- c(
  "1" = "Social Security (IMSS)",
  "2" = "ISSSTE",
  "3" = "State-level ISSSTE",
  "4" = "Pemex",
  "5" = "Defensa",
  "6" = "Navy",
  "7" = "Seguro Popular",
  "8" = "IMSS PROSPERA",
  "9" = "Private insurance",
  "10" = "Other institution",
  "11" = "Not affiliated",
  "99" = "Doesn't know"
)

health_quality_data <- health_quality_data %>%
  mutate(P3_7_label = recode(as.character(P3_7), !!!labels))

# Summarize normalized quality_score by provider
provider_summary <- health_quality_data %>%
  filter(!is.na(quality_score), !is.na(P3_7_label)) %>%
  group_by(P3_7_label) %>%
  summarise(
    avg_quality = mean(quality_score, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(avg_quality))

# Plot average quality score by provider
ggplot(provider_summary, aes(x = fct_reorder(P3_7_label, avg_quality), y = avg_quality)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Healthcare Provider",
    y = "Average Quality Score (0–1)",
    title = "Average Patient-Reported Quality by Healthcare Provider"
  ) +
  theme_minimal()


## HEATMAP
# Labels for heatmap dimensions
dimension_labels <- c(
  P3_5     = "Quality of service",
  P4_6_rc  = "Waiting time",
  P7_7_rc  = "Facility conditions",
  P8_2     = "Ease of getting an appointment",
  P8_3     = "Access outside regular hours",
  P8_4     = "Phone access to doctor",
  P8_5_1   = "Knows your history",
  P8_5_2   = "Lets you ask questions",
  P8_5_3   = "Spends enough time",
  P8_5_4   = "Involves you in decisions",
  P8_5_5   = "Explains clearly",
  P8_5_6   = "Coordinates specialist care",
  P8_6     = "Overall care rating"
)

# Transform to long format and label dimensions
long_data <- health_quality_data %>%
  select(P3_7_label, all_of(names(dimension_labels))) %>%
  pivot_longer(-P3_7_label, names_to = "dimension", values_to = "value") %>%
  mutate(dimension = recode(dimension, !!!dimension_labels)) %>%
  group_by(P3_7_label, dimension) %>%
  summarise(avg_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
  drop_na()

# Plot heatmap with labels
ggplot(long_data, aes(x = dimension, y = fct_rev(P3_7_label), fill = avg_val)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(limits = c(0, 5)) +
  labs(
    x = "Quality Dimension",
    y = "Healthcare Provider",
    fill = "Avg. Score",
    title = "Patient Experience Scores by Provider and Dimension"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    plot.title = element_text(face = "bold")
  )


# ----------- RETURN PROPENSITY ---------------------
# Calculate return proportions
return_prop <- health_quality_data %>%
  filter(!is.na(P7_2), !is.na(P3_7_label)) %>%
  group_by(P3_7_label, P7_2) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(P3_7_label) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Extract % No (P7_2 == 0) per provider
no_order <- return_prop %>%
  filter(P7_2 == 0) %>%
  select(P3_7_label, no_prop = prop)

# Reorder factor levels based on descending % No
ordered_labels <- no_order %>%
  arrange(desc(no_prop)) %>%
  pull(P3_7_label)

# Apply factor levels to full data
return_prop <- return_prop %>%
  mutate(P3_7_label = factor(P3_7_label, levels = ordered_labels))

# Plot
ggplot(return_prop, aes(x = P3_7_label, y = prop, fill = factor(P7_2, levels = c(1, 0)))) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_fill_manual(values = c("1" = "seagreen3", "0" = "firebrick3"), labels = c("1" = "Yes", "0" = "No")) +
  labs(
    x = "Healthcare Provider",
    y = "Proportion of Respondents",
    fill = "Would Return",
    title = "Willingness to Return by Provider (Ordered by % No Responses)"
  ) +
  theme_minimal()



# Reasons
# Create named labels for reasons
yes_labels <- c(
  P7_3_1 = "No other option",
  P7_3_2 = "Treated well",
  P7_3_3 = "Good care",
  P7_3_4 = "No payment",
  P7_3_5 = "Close to home",
  P7_3_6 = "Affordable",
  P7_3_7 = "Quick attention",
  P7_3_8 = "Received meds",
  P7_3_9 = "Short wait",
  P7_3_10 = "Clean facility",
  P7_3_11 = "Doctor explained",
  P7_3_12 = "Other",
  P7_3_13 = "Doesn’t know"
)

no_labels <- c(
  P7_4_1 = "Treated badly",
  P7_4_2 = "Disagreed with diagnosis",
  P7_4_3 = "Disagreed with treatment",
  P7_4_4 = "Did not improve",
  P7_4_5 = "Expensive",
  P7_4_6 = "Far from home",
  P7_4_7 = "No meds",
  P7_4_8 = "No materials",
  P7_4_9 = "Family not allowed",
  P7_4_10 = "Long wait",
  P7_4_11 = "Dirty facility",
  P7_4_12 = "Doctor didn’t explain",
  P7_4_13 = "Other",
  P7_4_14 = "Doesn’t know"
)

# Pivot long and tag reasons
reasons_long <- health_quality_data %>%
  select(starts_with("P7_3_"), starts_with("P7_4_")) %>%
  pivot_longer(
    everything(),
    names_to = "reason_code",
    values_to = "response"
  ) %>%
  filter(response == 1) %>%  # Only keep selected reasons
  mutate(
    type = case_when(
      str_detect(reason_code, "P7_3") ~ "Yes",
      str_detect(reason_code, "P7_4") ~ "No"
    ),
    reason = case_when(
      reason_code %in% names(yes_labels) ~ yes_labels[reason_code],
      reason_code %in% names(no_labels) ~ no_labels[reason_code],
      TRUE ~ "Unknown"
    )
  )

# Summarize counts
reason_summary <- reasons_long %>%
  group_by(type, reason) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(type) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Plot
ggplot(reason_summary, aes(x = reorder(reason, prop), y = prop, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, scales = "free_y") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Reason",
    y = "Proportion",
    title = "Reasons for Returning or Not Returning to Provider",
    subtitle = "Based on respondents' selected answers"
  ) +
  theme_minimal(base_size = 12)


## ---------- BEFORE STARTING: HEALTH CONDITIONS ------------

# Create condition mapping
condition_labels <- c(
  "1" = "Respiratory infections",
  "2" = "Pneumonia",
  "3" = "COPD",
  "4" = "Cough/Sore throat",
  "5" = "Tuberculosis",
  "6" = "Ear infection",
  "7" = "Conjunctivitis",
  "8" = "Asthma",
  "9" = "Allergies",
  "10" = "Heart disease",
  "11" = "Rheumatic fever",
  "12" = "Diabetes",
  "13" = "Hypertension",
  "14" = "Stroke",
  "15" = "Obesity",
  "16" = "Arthritis",
  "17" = "Diarrhea",
  "18" = "Gastritis/Ulcer",
  "19" = "Colitis",
  "20" = "Parasites",
  "21" = "Hepatitis",
  "22" = "Kidney disease",
  "23" = "UTI",
  "24" = "Exanthematous disease",
  "25" = "STI",
  "26" = "HIV/AIDS",
  "28" = "Dengue",
  "29" = "Poisoning",
  "30" = "Alcoholism",
  "32" = "Drug-related illness",
  "33" = "Accidental injury",
  "34" = "Injury from aggression",
  "36" = "Stress",
  "37" = "Depression",
  "38" = "Skin issues",
  "39" = "Oral disease",
  "40" = "Headache",
  "41" = "Fever (unspecified)",
  "42" = "Folk illnesses",
  "43" = "Pregnancy",
  "44" = "Cancer",
  "45" = "Other",
  "99" = "Don't know"
)

# Prepare the data - convert condition codes to factors with labels
health_quality_data <- health_quality_data %>%
  mutate(
    primary_condition_code = as.character(P1_2),
    primary_condition = factor(primary_condition_code, 
                               levels = names(condition_labels),
                               labels = condition_labels)
  )

# Providers
health_quality_data <- health_quality_data %>%
  mutate(
    provider = factor(P3_7, levels = c(1,2,3,4,5,6,7,8,9, 10, 11),  
                      labels = c("IMSS", "ISSSTE", "State-level ISSSTE",
                                 "Pemex", "Defensa", "Navy", "Seguro Popular", 
                                 "IMSS PROSPERA", "Other", "Private", "Other"))
  )

# Step 1: Chi-square test to see if conditions differ by provider
chi_result <- chisq.test(table(health_quality_data$primary_condition, health_quality_data$provider))
print(paste("Chi-square test result: X² =", round(chi_result$statistic, 2), 
            ", df =", chi_result$parameter, 
            ", p-value =", format.pval(chi_result$p.value, digits = 3)))

# Step 2: Calculate condition frequencies by provider
condition_by_provider <- health_quality_data %>%
  group_by(provider, primary_condition) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(provider) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  ) %>%
  arrange(provider, desc(percentage))

# Step 3: Get the top 5 conditions for each provider
top_conditions_by_provider <- condition_by_provider %>%
  group_by(provider) %>%
  slice_max(order_by = percentage, n = 5) %>%
  ungroup()

# Step 4: Find conditions with the biggest variation across providers
condition_variation <- condition_by_provider %>%
  group_by(primary_condition) %>%
  summarize(
    max_pct = max(percentage, na.rm = TRUE),
    min_pct = min(percentage, na.rm = TRUE),
    variation = max_pct - min_pct,
    avg_pct = mean(percentage, na.rm = TRUE)
  ) %>%
  filter(!is.na(primary_condition)) %>%
  arrange(desc(variation))

# Step 1: Get the top 5 conditions with highest variation
top5_conditions <- condition_variation %>%
  slice_max(order_by = variation, n = 5) %>%
  pull(primary_condition)

# Step 2: Filter the original data for those 5 conditions
top5_data <- condition_by_provider %>%
  filter(primary_condition %in% top5_conditions)

# Step 3: Plot the percentage by provider for each condition
ggplot(top5_data, aes(x = reorder(provider, percentage), y = percentage)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  facet_wrap(~ primary_condition, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Variation in Reported Percentage by Provider for Top 5 Conditions",
    x = "Provider ID (ordered within each condition)",
    y = "Percentage of Patients",
    caption = "Source: Authors calculations with data from ELSANUT, 2018"
  ) +
  theme(axis.text.x = element_blank())  # Optional: hides crowded x-axis labels


## ------------------ HYPOTHESIS 1: --------------------
## Without capitation or fee-for-service incentives and with low 
## monitoring, providers may induce less services. 
 

# Label providers
health_quality_data <- health_quality_data %>%
  mutate(
    provider = factor(P3_7, levels = c(1,2,3,4,5,6,7,8,9, 10, 11),  
                      labels = c("IMSS", "ISSSTE", "State-level ISSSTE",
                                 "Pemex", "Defensa", "Navy", "Seguro Popular", 
                                 "IMSS PROSPERA", "Other", "Private", "Other")),
    test_requested = ifelse(P6_1 == 1, 1, 0),
    sex = factor(SEXO, labels = c("Male", "Female")),
    medications = as.numeric(as.character(P5_1)),
    condition = as.factor(P1_2),  # self-reported condition
    age = as.numeric(EDAD)  
  )

health_quality_data$provider <- relevel(health_quality_data$provider, ref = "Private")

# Diagnostic tests requested
model_tests <- lm(test_requested ~ provider + age + sex + condition,
                   data = health_quality_data)

# Number of medications prescribed
model_meds <- lm(medications ~ provider + age + sex + condition,
                 data = health_quality_data)

# Summary with robust standard errors
coeftest(model_tests, vcov = vcovHC(model_tests, type = "HC1"))
coeftest(model_meds, vcov = vcovHC(model_meds, type = "HC1"))

##############################
#######  PLOTS FOR MODEL_TESTS
##############################
# -- FOR HEALTH CONDITIONS --
condition_map <- data.frame(
  term = paste0("condition", c(
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
    12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 28, 29, 30, 32, 33,
    34, 36, 37, 38, 39, 40, 41, 42, 43, 44,
    45, 99)
  ),
  condition = c(
    "Pneumonia", "COPD", "Cold/Sore throat", "Tuberculosis", "Ear infection", "Conjunctivitis", "Asthma", "Allergies", "Heart disease",
    "Rheumatic fever", "Diabetes", "Hypertension", "Stroke", "Obesity", "Arthritis", "Diarrhea", "Gastritis/Ulcer", "Colitis", "Parasites",
    "Hepatitis", "Kidney disease", "UTI", "Exanthematous disease", "STI", "HIV/AIDS", "Dengue", "Poisoning", "Alcoholism", "Drug-related illness",
    "Accidental injury", "Injury from aggression", "Stress", "Depression", "Skin issues", "Oral disease", "Headache", "Fever (unspecified)",
    "Folk illnesses", "Pregnancy", "Cancer", "Other", "Don't know"
  )
)

tidy_model <- tidy(model_tests, conf.int = TRUE)

# Filter only condition variables
tidy_conditions <- tidy_model %>%
  filter(grepl("^condition\\d+", term)) %>%
  left_join(condition_map, by = "term") %>%
  arrange(estimate)

# Create factor with ordering for plotting
tidy_conditions$condition <- factor(tidy_conditions$condition, levels = tidy_conditions$condition)


ggplot(tidy_conditions, aes(x = estimate, y = condition)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Effect of Health Condition on Diagnostic Test",
    x = "Coefficient Estimate (with 95% CI)",
    y = "Condition"
  ) +
  theme_minimal(base_size = 13)


# -- NOW FOR PROVIDERS--
provider_map <- data.frame(
  term = c(
    "providerIMSS", "providerISSSTE", "providerState-level ISSSTE", 
    "providerPemex", "providerDefensa",
    "providerNavy", "providerSeguro Popular", "providerIMSS PROSPERA",
    "providerOther"
  ),
  provider = c(
    "Social Security (IMSS)", "ISSSTE", "State-level ISSSTE", "Pemex", "Defensa",
    "Navy", "Seguro Popular", "IMSS PROSPERA", "Other institution"
  )
)

# Tidy your model
tidy_model <- tidy(model_tests, conf.int = TRUE)

# Filter for provider terms only
tidy_providers <- tidy_model %>%
  filter(term %in% provider_map$term) %>%
  left_join(provider_map, by = "term") %>%
  arrange(estimate)

# Join the results on quality by provider
provider_summary <- provider_summary %>%
  rename(provider = P3_7_label)

tidy_providers <- tidy_providers %>%
  left_join(provider_summary, by = "provider")

# 1. Add significance stars
tidy_providers <- tidy_providers %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    coef_label = paste0(sprintf("%.2f", estimate), stars)
  )

# 2. Order by average quality (ensure provider is factor)
tidy_providers <- tidy_providers %>%
  mutate(provider = fct_reorder(provider, avg_quality))

# 3. Plot
ggplot(tidy_providers, aes(x = provider, y = estimate)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_errorbar(aes(ymin = estimate - std.error,
                    ymax = estimate + std.error), width = 0.2) +
  geom_text(aes(label = coef_label), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Effect of Healthcare Provider on Diagnostic Test Likelihood",
    x = "Provider",
    y = "Coefficient (with significance)"
  ) +
  theme_minimal()

plot_model(model_tests, show.values = TRUE, value.offset = 0.3) +
  coord_flip()


ggplot(tidy_providers, aes(x = estimate, y = provider)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Effect of Provider Type on Number of Diagnostic Tests",
    subtitle = "Ordered by Mean Quality of Care",
    x = "Coefficient Estimate (with 95% CI)",
    y = "Provider (Highest to Lowest Mean Quality)"
  ) +
  theme_minimal(base_size = 13)

#############################
#######  PLOTS FOR MODEL_MEDS
#############################

# -- FOR HEALTH CONDITIONS --
condition_map <- data.frame(
  term = paste0("condition", c(
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
    12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 28, 29, 30, 32, 33,
    34, 36, 37, 38, 39, 40, 41, 42, 43, 44,
    45, 99)
  ),
  condition = c(
    "Pneumonia", "COPD", "Cold/Sore throat", "Tuberculosis", "Ear infection", "Conjunctivitis", "Asthma", "Allergies", "Heart disease",
    "Rheumatic fever", "Diabetes", "Hypertension", "Stroke", "Obesity", "Arthritis", "Diarrhea", "Gastritis/Ulcer", "Colitis", "Parasites",
    "Hepatitis", "Kidney disease", "UTI", "Exanthematous disease", "STI", "HIV/AIDS", "Dengue", "Poisoning", "Alcoholism", "Drug-related illness",
    "Accidental injury", "Injury from aggression", "Stress", "Depression", "Skin issues", "Oral disease", "Headache", "Fever (unspecified)",
    "Folk illnesses", "Pregnancy", "Cancer", "Other", "Don't know"
  )
)

tidy_model_meds <- tidy(model_meds, conf.int = TRUE)

# Filter only condition variables
tidy_conditions_meds <- tidy_model_meds %>%
  filter(grepl("^condition\\d+", term)) %>%
  left_join(condition_map, by = "term") %>%
  arrange(estimate)

# Create factor with ordering for plotting
tidy_conditions_meds$condition <- factor(tidy_conditions_meds$condition, levels = tidy_conditions_meds$condition)


ggplot(tidy_conditions_meds, aes(x = estimate, y = condition)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Effect of Health Condition on Meds Prescription",
    x = "Coefficient Estimate (with 95% CI)",
    y = "Condition"
  ) +
  theme_minimal(base_size = 13)


# -- NOW FOR PROVIDERS--
provider_map <- data.frame(
  term = c(
    "providerIMSS", "providerISSSTE", "providerState-level ISSSTE", 
    "providerPemex", "providerDefensa",
    "providerNavy", "providerSeguro Popular", "providerIMSS PROSPERA",
    "providerOther"
  ),
  provider = c(
    "Social Security (IMSS)", "ISSSTE", "State-level ISSSTE", "Pemex", "Defensa",
    "Navy", "Seguro Popular", "IMSS PROSPERA", "Other institution"
  )
)

# Tidy your model
tidy_model_meds <- tidy(model_meds, conf.int = TRUE)

# Filter for provider terms only
tidy_providers_meds <- tidy_model_meds %>%
  filter(term %in% provider_map$term) %>%
  left_join(provider_map, by = "term") %>%
  arrange(estimate)

# Join the results on quality by provider
tidy_providers_meds <- tidy_providers_meds %>%
  left_join(provider_summary, by = "provider")

# 1. Add significance stars
tidy_providers_meds <- tidy_providers_meds %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    coef_label = paste0(sprintf("%.2f", estimate), stars)
  )

# 2. Order by average quality (ensure provider is factor)
tidy_providers_meds <- tidy_providers_meds %>%
  mutate(provider = fct_reorder(provider, avg_quality))

# 3. Plot
ggplot(tidy_providers_meds, aes(x = provider, y = estimate)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_errorbar(aes(ymin = estimate - std.error,
                    ymax = estimate + std.error), width = 0.2) +
  geom_text(aes(label = coef_label), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Effect of Healthcare Provider on # of Med Prescription",
    x = "Provider",
    y = "Coefficient (with significance)"
  ) +
  theme_minimal()

plot_model(model_tests, show.values = TRUE, value.offset = 0.3) +
  coord_flip()


ggplot(tidy_providers, aes(x = estimate, y = provider)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Effect of Provider Type on Number of Med Prescription",
    subtitle = "Ordered by Mean Quality of Care",
    x = "Coefficient Estimate (with 95% CI)",
    y = "Provider (Highest to Lowest Mean Quality)"
  ) +
  theme_minimal(base_size = 13)


## ------------------ HYPOTHESIS 2: --------------------
## Without capitation or fee-for-service incentives and with low 
## monitoring, providers put less effort per patient.

health_quality_data <- health_quality_data %>%
  # Consultation time in minutes
  mutate(consult_time = as.numeric(P4_7H) * 60 + as.numeric(P4_7M),
         
         # Effort perception variables (reverse coded so higher = more effort)
         ask_questions = ifelse(P8_5_2 %in% 1:4, 5 - as.numeric(P8_5_2), NA),
         enough_time = ifelse(P8_5_3 %in% 1:4, 5 - as.numeric(P8_5_3), NA),
         shared_decision = ifelse(P8_5_4 %in% 1:4, 5 - as.numeric(P8_5_4), NA),
         explain_clear = ifelse(P8_5_5 %in% 1:4, 5 - as.numeric(P8_5_5), NA),
         coordinate_specialist = ifelse(P8_5_6 %in% 1:4, 5 - as.numeric(P8_5_6), NA),
         
         # Other effort-relevant measures
         review_meds = ifelse(P8_8_1 == 1, 1, ifelse(P8_8_1 == 2, 0, NA)),
         explain_sidefx = ifelse(P8_8_2 == 1, 1, ifelse(P8_8_2 == 2, 0, NA)),
         
         # Provider recoding (if not yet done)
         provider = factor(P3_7, levels = c(1,2,3,4,5,6,7,8,9,10,11),
                           labels = c("IMSS", "ISSSTE", "State-level ISSSTE", "Pemex", "Defensa", "Navy", "Seguro Popular", 
                                      "IMSS PROSPERA", "Other", "Private", "Other")),
         
         sex = factor(SEXO, labels = c("Male", "Female")),
         age = as.numeric(EDAD)
  )

# Standardize variables
effort_vars <- c("consult_time", "ask_questions", "enough_time", "shared_decision",
                 "explain_clear", "coordinate_specialist", "review_meds", "explain_sidefx")

health_quality_data <- health_quality_data %>%
  mutate(across(all_of(effort_vars), ~ scale(.)[,1], .names = "z_{col}")) %>%
  rowwise() %>%
  mutate(effort_index = mean(c_across(starts_with("z_")), na.rm = TRUE)) %>%
  ungroup()

# Define the base comparison for the Private
health_quality_data$provider <- relevel(health_quality_data$provider, ref = "Private")

# Define the model
effort_model <- lm(effort_index ~ provider + age + sex, data = health_quality_data)
effort_model_conditions <- lm(effort_index ~ provider + age + sex + condition, data = health_quality_data)


# Print the summary
summary(effort_model)
summary(effort_model_conditions)


## -- FOR PROVIDERS --

pred_df <- health_quality_data %>%
  group_by(provider) %>%
  summarise(mean_effort = mean(effort_index, na.rm = TRUE),
            se = sd(effort_index, na.rm = TRUE)/sqrt(n()))

# First get the original plot with sorting
plot <- plot_model(effort_model, 
                   type = "est", 
                   sort.est = TRUE,  # Sort by effect size
                   title = "Effects on Provider Effort Index",
                   show.values = TRUE,
                   show.p = TRUE,
                   value.offset = 0.3,
                   colors = c("firebrick", "steelblue"))  # Red for negative, blue for positive

# Extract and modify the data while preserving order
plot_data <- plot$data
# Store original order
plot_data$original_order <- 1:nrow(plot_data)
# Remove "provider" from labels
plot_data$term <- gsub("provider", "", plot_data$term)

# Rebuild plot with modified labels but preserved order
ggplot(plot_data, aes(x = reorder(term, original_order), y = estimate, color = estimate > 0)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), fatten = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("firebrick", "steelblue")) +
  geom_text(aes(label = paste0(sprintf("%.2f", estimate), 
                               ifelse(p.value < 0.001, "***", 
                                      ifelse(p.value < 0.01, "**",
                                             ifelse(p.value < 0.05, "*", ""))))),
            vjust = -0.5, size = 3.5) +
  labs(title = "Effects on Provider Effort Index",
       y = "Estimate",
       x = "") +
  theme_sjplot() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "none") +
  coord_flip()


# -- FOR HEALTH CONDITIONS --
condition_map <- data.frame(
  term = paste0("condition", c(
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
    12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 28, 29, 30, 32, 33,
    34, 36, 37, 38, 39, 40, 41, 42, 43, 44,
    45, 99)
  ),
  condition = c(
    "Pneumonia", "COPD", "Cold/Sore throat", "Tuberculosis", "Ear infection", "Conjunctivitis", "Asthma", "Allergies", "Heart disease",
    "Rheumatic fever", "Diabetes", "Hypertension", "Stroke", "Obesity", "Arthritis", "Diarrhea", "Gastritis/Ulcer", "Colitis", "Parasites",
    "Hepatitis", "Kidney disease", "UTI", "Exanthematous disease", "STI", "HIV/AIDS", "Dengue", "Poisoning", "Alcoholism", "Drug-related illness",
    "Accidental injury", "Injury from aggression", "Stress", "Depression", "Skin issues", "Oral disease", "Headache", "Fever (unspecified)",
    "Folk illnesses", "Pregnancy", "Cancer", "Other", "Don't know"
  )
)

tidy_model_effort <- tidy(effort_model_conditions, conf.int = TRUE)

# Filter only condition variables
tidy_model_effort <- tidy_model_effort %>%
  filter(grepl("^condition\\d+", term)) %>%
  left_join(condition_map, by = "term") %>%
  arrange(estimate)

# Create factor with ordering for plotting
tidy_model_effort$condition <- factor(tidy_model_effort$condition, levels = tidy_model_effort$condition)


ggplot(tidy_model_effort, aes(x = estimate, y = condition)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Effect of Health Condition on Effort",
    x = "Coefficient Estimate (with 95% CI)",
    y = "Condition"
  ) +
  theme_minimal(base_size = 13)



## ----------------- HYPOTHESIS 3: (PENDING) --------------------
## Moral hazard leads to inefficient care, not better care.

# Outcome: would return (Yes = 1)
health_quality_data <- health_quality_data %>%
  mutate(
    health_outcome = ifelse(P7_1 %in% c(1,2), 1, 0)
  )

# Regression: got better ~ overtreatment
summary(lm(health_outcome ~ num_medications + test_requested + factor(P3_7), data = health_quality_data))


## ----------------- HYPOTHESIS 3: --------------------
## Moral hazard leads to higher costs for the system and patient.

# Total medical spending
health_quality_data <- health_quality_data %>%
  mutate(
    lab_cost = as.numeric(P6_4),
    med_cost = as.numeric(P5_6),
    other_cost = as.numeric(P6_6_1) + as.numeric(P6_6_2) + as.numeric(P6_6_3),
    total_cost = rowSums(across(c(lab_cost, med_cost, other_cost)), na.rm = TRUE)
  )

# Regression: Costs ~ Treatment Intensity
summary(lm(total_cost ~ num_medications + test_requested + factor(P3_7), data = health_quality_data))


## ----------------- HYPOTHESIS 4: --------------------
## Preventive care is lower in high-resource providers.

# Preventive proxy: Whether patient was asked for tests
preventive_by_provider <- health_quality_data %>%
  group_by(P3_7_label) %>%
  summarise(preventive_rate = mean(test_requested, na.rm = TRUE)) %>%
  arrange(desc(preventive_rate))

ggplot(preventive_by_provider, aes(x = fct_reorder(P3_7_label, preventive_rate), y = preventive_rate)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(x = "Provider", y = "Proportion of Preventive Testing", title = "Preventive Care by Provider")


## ----------------- HYPOTHESIS 5: --------------------
## Similar patient profiles receive different treatment 
## intensities across providers.


# Add basic patient health controls if available
# Example: age, gender, diagnosis code (placeholder)
health_quality_data <- health_quality_data %>%
  mutate(age = as.numeric(P2_1),  # if you have age variable
         sex = factor(P2_2))      # replace with correct var names

# Regression controlling for patient profile
summary(lm(num_medications ~ age + sex + factor(P3_7), data = health_quality_data))
summary(lm(test_requested ~ age + sex + factor(P3_7), data = health_quality_data, family = binomial))




## ------ FIRST BUBBLE GRAPH (NUMBER OF PEOPLE BY STRATA) ------
## Joint plot to check economic strata
table(individual_data$ESTRATO)

joint_data <- individual_data %>%
  filter(!is.na(P3_10_OPC1) & P3_10_OPC1 != 99) %>% # Remove NA and unknown values
  group_by(ESTRATO, P3_10_OPC1) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(service = labels[as.character(P3_10_OPC1)]) %>%
  filter(!is.na(service)) # Remove any services without labels

# Now create the bubble plot
ggplot(joint_data, aes(x = ESTRATO, y = service, size = count, color = service)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 20)) +
  labs(
    title = "Distribution of Health Services by Economic Strata",
    x = "Economic Strata (ESTRATO)",
    y = "Health Service",
    size = "Number of People"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) 




## ------ EXPENSES IN THE LAST 3 MONTHS ---------

# First, let's analyze healthcare expenses by strata at the household level
household_expense_by_strata <- household_data %>%
  filter(!is.na(P7_2_1) & !is.na(ESTRATO)) %>%  # Remove NA values
  mutate(
    household_size = P2_4,  # Number of people in household
    per_capita_expense = P7_2_1 / household_size  # Calculate per-capita expense
  ) %>%
  group_by(ESTRATO) %>%
  summarise(
    mean_expense = mean(P7_2_1, na.rm = TRUE),
    median_expense = median(P7_2_1, na.rm = TRUE),
    total_expense = sum(P7_2_1, na.rm = TRUE),
    mean_per_capita = mean(per_capita_expense, na.rm = TRUE),
    households = n(),
    total_people = sum(household_size, na.rm = TRUE),
    .groups = "drop"
  )

# View the result
print(household_expense_by_strata)

# Now let's create visualizations

# 1. Bar chart of mean household expenses by strata
ggplot(household_expense_by_strata, aes(x = factor(ESTRATO), y = mean_expense, fill = factor(ESTRATO))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_expense, 1)), vjust = -0.5) +
  labs(
    title = "Mean Household Healthcare Expenses by Economic Strata",
    x = "Economic Strata (ESTRATO)",
    y = "Mean Expense (P7_2_1)",
    fill = "Economic Strata"
  ) +
  theme_minimal()

# 2. Bar chart of per-capita expenses by strata
ggplot(household_expense_by_strata, aes(x = factor(ESTRATO), y = mean_per_capita, fill = factor(ESTRATO))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_per_capita, 1)), vjust = -0.5) +
  labs(
    title = "Mean Per-Capita Healthcare Expenses by Economic Strata",
    x = "Economic Strata (ESTRATO)",
    y = "Mean Per-Capita Expense",
    fill = "Economic Strata"
  ) +
  theme_minimal()

# 4. Violin plot with jittered points to show distribution
ggplot(household_data %>% filter(!is.na(P7_2_1) & !is.na(ESTRATO)), 
       aes(x = factor(ESTRATO), y = P7_2_1, fill = factor(ESTRATO))) +
  geom_violin(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
  scale_y_continuous(limits = quantile(household_data$P7_2_1, c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    title = "Distribution of Household Healthcare Expenses by Economic Strata",
    x = "Economic Strata (ESTRATO)",
    y = "Expense (P7_2_1)",
    fill = "Economic Strata"
  ) +
  theme_minimal()

# 5. Total healthcare expenditure by strata
ggplot(household_expense_by_strata, aes(x = factor(ESTRATO), y = total_expense, fill = factor(ESTRATO))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(total_expense)), vjust = -0.5) +
  labs(
    title = "Total Healthcare Expenditure by Economic Strata",
    x = "Economic Strata (ESTRATO)",
    y = "Total Expenditure",
    fill = "Economic Strata"
  ) +
  theme_minimal()

# 6. Scatter plot of expense vs household size, colored by strata
ggplot(household_data %>% filter(!is.na(P7_2_1) & !is.na(ESTRATO) & !is.na(P2_4)), 
       aes(x = P2_4, y = P7_2_1, color = factor(ESTRATO))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = quantile(household_data$P7_2_1, c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    title = "Healthcare Expenses vs Household Size by Economic Strata",
    x = "Household Size (P2_4)",
    y = "Expense (P7_2_1)",
    color = "Economic Strata"
  ) +
  theme_minimal()

# 7. Bar chart showing mean household expense and household size by strata
expense_size_combined <- household_expense_by_strata %>%
  select(ESTRATO, mean_expense, households) %>%
  gather(key = "metric", value = "value", -ESTRATO) %>%
  mutate(metric = factor(metric, levels = c("mean_expense", "households")))

ggplot(expense_size_combined, aes(x = factor(ESTRATO), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~metric, scales = "free_y", labeller = labeller(
    metric = c("mean_expense" = "Mean Expense", "households" = "Number of Households")
  )) +
  labs(
    title = "Healthcare Expenses and Household Count by Economic Strata",
    x = "Economic Strata (ESTRATO)",
    y = "Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 8. Create a visualization of expense density distribution by strata
ggplot(household_data %>% filter(!is.na(P7_2_1) & !is.na(ESTRATO)), 
       aes(x = P7_2_1, fill = factor(ESTRATO))) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = quantile(household_data$P7_2_1, c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    title = "Density Distribution of Healthcare Expenses by Economic Strata",
    x = "Expense (P7_2_1)",
    y = "Density",
    fill = "Economic Strata"
  ) +
  theme_minimal()


## ---- EXPENSES BY EACH HEALTHCARE PROVIDER ----

# First, let's analyze general healthcare expenses distribution by strata
strata_expense <- household_data %>%
  filter(!is.na(P7_2_1) & !is.na(ESTRATO)) %>%
  group_by(ESTRATO) %>%
  summarise(
    mean_expense = mean(P7_2_1, na.rm = TRUE),
    median_expense = median(P7_2_1, na.rm = TRUE),
    total_expense = sum(P7_2_1, na.rm = TRUE),
    households = n(),
    .groups = "drop"
  )

# Now, let's look at the provider distribution by strata
provider_by_strata <- individual_data %>%
  filter(!is.na(P3_10_OPC1) & !is.na(ESTRATO)) %>%
  mutate(service = labels[as.character(P3_10_OPC1)]) %>%
  filter(!is.na(service)) %>%
  group_by(ESTRATO, service) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ESTRATO) %>%
  mutate(
    strata_total = sum(count),
    proportion = count / strata_total
  )

# Now combine the datasets for visualization
# We'll join provider distribution with expense data
combined_data <- provider_by_strata %>%
  left_join(strata_expense, by = "ESTRATO") %>%
  mutate(
    # Calculate estimated expense per provider based on proportion
    estimated_expense = proportion * total_expense,
    estimated_per_capita = estimated_expense / count
  )

# Visualizations
# 1. Bar chart showing estimated expenses by provider and strata
ggplot(combined_data, aes(x = factor(ESTRATO), y = estimated_expense, fill = service)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Estimated Healthcare Expenses by Economic Strata and Provider",
    subtitle = "Based on provider distribution within each stratum",
    x = "Economic Strata (ESTRATO)",
    y = "Estimated Expense",
    fill = "Healthcare Provider"
  ) +
  theme_minimal()

# 3. Heatmap showing the estimated expenses
ggplot(combined_data, aes(x = factor(ESTRATO), y = service)) +
  geom_tile(aes(fill = estimated_expense), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = scales::comma(round(estimated_expense))), color = "white", size = 3) +
  labs(
    title = "Estimated Healthcare Expenses by Economic Strata and Provider",
    x = "Economic Strata (ESTRATO)",
    y = "Healthcare Provider",
    fill = "Estimated Expense"
  ) +
  theme_minimal()

# 4. Bubble chart showing estimated per-capita expenses
ggplot(combined_data, aes(x = factor(ESTRATO), y = service, size = estimated_per_capita)) +
  geom_point(aes(color = service), alpha = 0.7) +
  scale_size_continuous(range = c(3, 15)) +
  labs(
    title = "Estimated Per-Capita Healthcare Expenses by Strata and Provider",
    x = "Economic Strata (ESTRATO)",
    y = "Healthcare Provider",
    size = "Est. Per-Capita Expense"
  ) +
  theme_minimal()

# 5. Faceted histogram showing expense distribution by strata with provider breakdown
# First, create a summary for providers by strata
provider_summary <- provider_by_strata %>%
  group_by(ESTRATO) %>%
  mutate(prop = count/sum(count)) %>%
  arrange(desc(prop)) %>%
  slice(1:3) %>%  # Top 3 providers per stratum
  summarise(
    top_providers = paste(service, collapse = ", "),
    .groups = "drop"
  )


