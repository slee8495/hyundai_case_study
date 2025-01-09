library(kableExtra)
library(tidyverse)

# Exclude date-related variables
coeff_data <- coeff_data[!grepl("date", coeff_data$Category), ]

# Bar chart
ggplot(coeff_data, aes(x = reorder(Category, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), aes(color = Brand)) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  labs(
    title = "Coefficient Bar Chart: Hyundai vs. Tesla",
    x = "Variables",
    y = "Coefficient Value",
    fill = "Coefficient Sign"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Brand, scales = "free_x")




#############

# Load necessary library
library(kableExtra)

# Prepare coefficient table
coeff_table <- coeff_data[order(-abs(coeff_data$Coefficient)), ]  # Sort by absolute value
coeff_table$Coefficient <- round(coeff_table$Coefficient, 2)  # Round values for readability

# Display table
kable(coeff_table, format = "html", caption = "Coefficient Table: Hyundai vs. Tesla") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



#############

# Prepare the coefficient data (assumes `coeff_data` has columns: Category, Coefficient, Brand)
coeff_data <- coeff_data %>%
  mutate(Group = case_when(
    grepl("region", Category, ignore.case = TRUE) ~ "Region",
    grepl("model", Category, ignore.case = TRUE) ~ "Model",
    grepl("segment", Category, ignore.case = TRUE) ~ "Segmentation",
    TRUE ~ "Other"
  )) %>%
  arrange(Group, Category)

# Create heatmap
ggplot(coeff_data, aes(x = Brand, y = reorder(Category, Group), fill = Coefficient)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "tomato", mid = "lightyellow", high = "steelblue", midpoint = 0) +
  labs(
    title = "Coefficient Heatmap: Hyundai vs. Tesla",
    x = "Brand",
    y = "Variable (Grouped)",
    fill = "Coefficient"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Prepare the coefficient data (assumes `coeff_data` has columns: Category, Coefficient, Brand)
coeff_data <- coeff_data %>%
  mutate(Group = case_when(
    grepl("region", Category, ignore.case = TRUE) ~ "Region",
    grepl("model", Category, ignore.case = TRUE) ~ "Model",
    grepl("segment", Category, ignore.case = TRUE) ~ "Segmentation",
    TRUE ~ "Other"
  )) %>%
  arrange(Group, Category)

# Create dot plot
ggplot(coeff_data, aes(x = Coefficient, y = reorder(Category, Group), color = Brand)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Coefficient Dot Plot: Hyundai vs. Tesla",
    x = "Coefficient",
    y = "Variable (Grouped)"
  ) +
  scale_color_manual(values = c("Hyundai" = "steelblue", "Tesla" = "tomato")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )



########## Hyundai coefficient chart

# Fit the model (replace 'data' with your dataset and formula)
model <- lm(sales_volume ~ fuel_type + segmentations + region + models, data = hyundai_data)

# Create the coefficient summary table
coeff_summary <- summary(model)$coefficients

# Round and format the result to 2 decimal points
formatted_summary <- data.frame(
  Variable = rownames(coeff_summary),
  Estimate = round(coeff_summary[, "Estimate"], 2),
  `Std. Error` = round(coeff_summary[, "Std. Error"], 2),
  `t value` = round(coeff_summary[, "t value"], 2),
  `Pr(>|t|)` = format(round(coeff_summary[, "Pr(>|t|)"], 2), scientific = FALSE, nsmall = 2)
)

# Print the formatted summary
formatted_summary



########## Tesla coefficient chart

# Fit the model (replace 'data' with your dataset and formula)
model <- lm(sales_volume ~ segmentations + region + models, data = tesla_data)

# Create the coefficient summary table
coeff_summary <- summary(model)$coefficients

# Round and format the result to 2 decimal points
formatted_summary <- data.frame(
  Variable = rownames(coeff_summary),
  Estimate = round(coeff_summary[, "Estimate"], 2),
  `Std. Error` = round(coeff_summary[, "Std. Error"], 2),
  `t value` = round(coeff_summary[, "t value"], 2),
  `Pr(>|t|)` = format(round(coeff_summary[, "Pr(>|t|)"], 2), scientific = FALSE, nsmall = 2)
)

# Print the formatted summary
formatted_summary
