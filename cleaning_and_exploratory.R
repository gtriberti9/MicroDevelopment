# Zixuan Zhou, Giuliana Triberti

library(ggplot2)
library(dplyr)
library(tidyr)

# Data_Cleaning

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

## From individual_data, "do you have health problems, where do you usually 
## go for care?"

labels <- c(
  "P3_9_01" = "Social Security (IMSS)",
  "P3_9_02" = "ISSSTE",
  "P3_9_03" = "State-level ISSSTE",
  "P3_9_04" = "Pemex",
  "P3_9_05" = "Defense",
  "P3_9_06" = "Navy",
  "P3_9_07" = "Seguro Popular / Siglo XXI",
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
  "5" = "Defense",
  "6" = "Navy",
  "7" = "Seguro Popular / Siglo XXI",
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


## 


