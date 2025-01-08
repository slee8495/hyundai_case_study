library(nloptr)
library(dplyr)
library(tidyr)

options(scipen = 999)  # Prevent scientific notation

# Input Data
region_data <- data.frame(
  Region = c("Central", "Eastern", "Mid-Atlantic", "Mountain States", "South Central", "Southern", "Western"),
  Unbuilt_Units_Planned = c(1743, 2707, 676, 599, 616, 1538, 1110)
)

trim_data <- data.frame(
  Trim = c("SE", "SEL", "SEL Plus", "Limited Night", "Ultimate"),
  Unbuilt_Units_Planned = c(2183, 2865, 1581, 991 + 282, 1087)
)

trim_region_mos <- data.frame(
  Region = c("Central", "Eastern", "Mid-Atlantic", "Mountain States", "South Central", "Southern", "Western"),
  SE = c(2.2, 2.5, 2.7, 2.2, 1.5, 2.2, 4.3),
  SEL = c(3.5, 2.2, 2.9, 4.4, 5.2, 2.6, 3.9),
  SEL_Plus = c(4.0, 3.5, 4.9, 5.5, 5.0, 2.3, 4.0),
  Limited_Night = c(8.9, 5.4, 10.0, 8.6, 11.0, 5.6, 5.0),
  Ultimate = c(7.8, 5.4, 7.3, 6.3, 8.8, 5.1, 4.8)
)

# Reshape the MOS data into long format
trim_region_mos_long <- expand.grid(
  Region = unique(region_data$Region),
  Trim = unique(trim_data$Trim)
) %>%
  left_join(trim_region_mos %>%
              pivot_longer(cols = -Region, names_to = "Trim", values_to = "MOS") %>%
              mutate(Trim = gsub("_", " ", Trim)), by = c("Region", "Trim")) %>%
  mutate(MOS = ifelse(is.na(MOS), 0, MOS))

# Cross Join to Create Region-Trim Allocation Matrix
allocation_matrix <- expand.grid(Region = region_data$Region, Trim = trim_data$Trim) %>%
  left_join(region_data %>% select(Region, Unbuilt_Units_Planned), by = "Region") %>%
  left_join(trim_data %>% select(Trim, Trim_Unbuilt_Units_Planned = Unbuilt_Units_Planned), by = "Trim") %>%
  left_join(trim_region_mos_long, by = c("Region", "Trim")) %>%
  mutate(Initial_Allocation = 0)

# Handle Missing and Zero Values in Allocation Matrix
allocation_matrix <- allocation_matrix %>%
  mutate(
    MOS = ifelse(is.na(MOS), 0, MOS),
    Trim_Unbuilt_Units_Planned = ifelse(is.na(Trim_Unbuilt_Units_Planned) | Trim_Unbuilt_Units_Planned == 0, 1, Trim_Unbuilt_Units_Planned),
    Trim_Weight = case_when(
      Trim == "SE" ~ 2.0 / (MOS + 0.01),
      Trim == "SEL" ~ 1.8 / (MOS + 0.01),
      Trim == "SEL Plus" ~ 1.2 / (MOS + 0.01),
      Trim == "Limited + Night" ~ 0.5 / (MOS + 0.01),
      Trim == "Ultimate" ~ 0.8 / (MOS + 0.01),
      TRUE ~ 1 / (MOS + 0.01)
    )  # Assign higher weights to lower MOS trims
  )

# Objective Function
objective_function <- function(x, allocation_matrix) {
  allocation_matrix <- allocation_matrix %>%
    mutate(Allocated = x, 
           New_MOS = MOS + Allocated / Trim_Unbuilt_Units_Planned)
  
  if (any(is.na(allocation_matrix$New_MOS))) {
    stop("Error: New_MOS contains NA values. Check Trim_Unbuilt_Units_Planned or MOS.")
  }
  
  # Penalize allocations to trims with higher MOS and reward trims with lower MOS
  penalties <- allocation_matrix$Trim_Weight * allocation_matrix$Allocated
  weighted_mos <- sum(penalties * allocation_matrix$New_MOS) / sum(penalties)
  
  return(weighted_mos)
}

# Constraints Function
constraint_function <- function(x, allocation_matrix) {
  allocation_matrix <- allocation_matrix %>%
    mutate(Allocated = x)
  
  # Regional constraints: Total allocation per region must match Unbuilt_Units_Planned
  regional_constraints <- allocation_matrix %>%
    group_by(Region) %>%
    summarize(Total_Allocated = sum(Allocated)) %>%
    mutate(Difference = Total_Allocated - region_data$Unbuilt_Units_Planned) %>%
    pull(Difference)
  
  # Global constraint: Total allocation must equal 8989
  global_constraint <- sum(x) - 8989
  
  # Combine constraints
  constraints <- c(regional_constraints, global_constraint)
  return(constraints)
}

# Initial Guess
init_guess <- allocation_matrix %>%
  mutate(Init_Guess = (Unbuilt_Units_Planned / sum(Unbuilt_Units_Planned)) * 8989 * Trim_Weight) %>%
  pull(Init_Guess)

# Optimization
result <- nloptr::nloptr(
  x0 = init_guess,
  eval_f = function(x) objective_function(x, allocation_matrix),
  eval_g_eq = function(x) constraint_function(x, allocation_matrix), # Equality constraints
  lb = rep(0, nrow(allocation_matrix)),
  ub = rep(Inf, nrow(allocation_matrix)),
  opts = list("algorithm" = "NLOPT_LN_AUGLAG_EQ", "xtol_rel" = 1e-6,
              "local_opts" = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6))
)

# Apply Results
optimized_allocation <- allocation_matrix %>%
  mutate(Allocated = result$solution) %>%
  group_by(Region, Trim) %>%
  summarize(
    Allocated = round(sum(Allocated), 0),
    Planned_MOS = unique(MOS),
    New_MOS = round(Planned_MOS + Allocated / unique(Trim_Unbuilt_Units_Planned), 2)
  )

# Verify Regional and Global Totals
optimized_allocation %>%
  group_by(Region) %>%
  summarize(Total_Allocated = sum(Allocated)) %>%
  left_join(region_data, by = "Region") %>%
  mutate(Difference = Total_Allocated - Unbuilt_Units_Planned) %>%
  print()

total_allocated <- sum(result$solution)
print(paste("Total Allocated Units:", round(total_allocated, 0)))




################
library(kableExtra)
library(dplyr)


# Split the data by region
split_tables <- optimized_allocation %>%
  select(Region, Trim, Allocated, Planned_MOS) %>%
  rename(MOS = Planned_MOS) %>% 
  group_split(Region)

# Create polished tables for each region
polished_tables <- lapply(split_tables, function(region_table) {
  region_name <- unique(region_table$Region)
  region_table %>%
    kbl(
      caption = paste("Optimized Allocation for Region:", region_name),
      col.names = c("Region", "Trim", "Allocated Units", "Planned MOS", "New MOS"),
      align = c("l", "l", "c", "c", "c")
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    row_spec(0, bold = TRUE, font_size = 12) %>%
    column_spec(2, bold = TRUE, width = "5cm")
})

# Display all tables in RStudio
polished_tables
