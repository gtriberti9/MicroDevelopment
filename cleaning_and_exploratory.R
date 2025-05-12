# Zixuan Zhou, Giuliana Triberti

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(reshape2)

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


## ------ QUALITY BY PROVIDER (PENDING) ----------
# Check structure
str(health_quality_data)

# Convert categorical vars to factors
health_quality_data <- health_quality_data %>%
  mutate(across(c(P1_1, P3_1, P3_5, P3_7, P5_3, P7_2, P7_5, P8_6), as.factor))

# Create quality score
health_quality_data <- health_quality_data %>%
  mutate(
    quality_score = rowSums(across(c(P3_5, P7_2, P7_5, P8_6), ~ as.numeric(as.character(.))), na.rm = TRUE)
  )

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

health_quality_data <- health_quality_data %>%
  mutate(P3_7_label = recode(as.character(P3_7), !!!labels))

# # Summarize by healthcare provider
# provider_summary <- health_quality_data %>%
#   group_by(P3_7_label) %>%
#   summarise(
#     avg_quality = mean(quality_score, na.rm = TRUE),
#     n = n()
#   ) %>%
#   arrange(desc(avg_quality))
# 
# ggplot(provider_summary, aes(x = fct_reorder(P3_7_label, avg_quality), y = avg_quality)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(x = "Healthcare Provider", y = "Average Quality Score", title = "Average Quality by Provider")
# 
# 
# # Heathmap of quality dimensions
# long_data <- health_quality_data %>%
#   select(P3_7_label, P3_5, P4_4, P4_6, P4_9, P7_2, P7_5, P8_6) %>%
#   pivot_longer(-P3_7_label, names_to = "dimension", values_to = "value") %>%
#   group_by(P3_7_label, dimension) %>%
#   summarise(avg_val = mean(as.numeric(as.character(value)), na.rm = TRUE)) %>%
#   drop_na()
# 
# ggplot(long_data, aes(x = dimension, y = fct_rev(P3_7_label), fill = avg_val)) +
#   geom_tile(color = "white") +
#   scale_fill_viridis_c() +
#   labs(x = "Quality Dimension", y = "Provider", fill = "Avg. Score", title = "Provider Quality Heatmap") +
#   theme_minimal()
# 
# # Respondents willing to return
# return_prop <- health_quality_data %>%
#   group_by(P3_7_label, P7_2) %>%
#   summarise(n = n()) %>%
#   group_by(P3_7_label) %>%
#   mutate(prop = n / sum(n))
# 
# ggplot(return_prop, aes(x = fct_reorder(P3_7_label, prop), y = prop, fill = P7_2)) +
#   geom_col(position = "fill") +
#   coord_flip() +
#   labs(x = "Provider", y = "Proportion", fill = "Would Return", title = "Willingness to Return by Provider")


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

## ----- PROPORTION OF PEOPLE BY STRATA -----------
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


