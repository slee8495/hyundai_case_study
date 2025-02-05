library(ggplot2)
library(dplyr)
library(kableExtra)
library(fmsb)
library(nloptr)

######################################################################################################################################################
####################################################################### Part 2 #######################################################################
######################################################################################################################################################


# consolidated data
consolidated_data <- data.frame(
  Month = c("2020-12", "2021-01"),
  Retail_Share = c(7.5, 6.9),  
  Incentive_Spend = c(3386, 3443),  
  CFTP = c(22111, 22338),  
  Lease_Penetration = c(25.5, 25),  
  Dealer_Profitability = c(3, 144), 
  Entry_CUV_Share = c(5.5, 6.3), 
  Kona_Sales = c(5817, 4094)  
)


consolidated_data %>%
  kbl(
    caption = "Corrected Key Metrics for Kona (Dec 2020 - Jan 2021)",
    col.names = c(
      "Month", "Retail Share (%)", "Incentive Spend ($)", "CFTP ($)",
      "Lease Penetration (%)", "Dealer Profitability ($)", 
      "Entry CUV Share (%)", "Kona Sales (Units)"
    ),
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2c3e50") %>%
  column_spec(2:8, width = "3cm") %>%  
  add_header_above(c(" " = 1, "Kona Metrics" = 7)) 





####################


# Bar chart for MOS by region
regional_data <- data.frame(
  Region = c("Southern", "Eastern", "Western", "Central", "Mid-Atlantic", "Mountain States", "South Central"),
  MOS = c(3.1, 3.4, 4.3, 4.6, 4.9, 5.0, 5.5)
)

ggplot(regional_data, aes(x = reorder(Region, MOS), y = MOS, fill = MOS)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(MOS, 1)), vjust = -0.5, size = 8) + 
  scale_fill_gradient(low = "lightgreen", high = "red") +
  labs(
    title = "Months of Stock (MOS) by Region",
    y = "Months of Stock"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title.y = element_text(size = 20, face = "bold"),           
    axis.text.x = element_text(size = 18, face = "bold"),            
    axis.text.y = element_text(size = 18),                         
    axis.title.x = element_blank(),                                 
    legend.position = "none"                                       
  )


########################


# Heatmap for MOS by trim and region
heatmap_data <- data.frame(
  Trim = rep(c("SE", "SEL", "SEL Plus", "Limited + Night", "Ultimate"), each = 7),
  Region = rep(c("Southern", "Eastern", "Western", "Central", "Mid-Atlantic", "Mountain States", "South Central"), 5),
  MOS = c(2.2, 2.5, 4.3, 2.2, 2.7, 2.2, 1.5,  # SE
          2.6, 2.2, 3.9, 3.5, 2.9, 4.4, 5.2,  # SEL
          2.3, 3.5, 4.0, 4.0, 4.9, 5.5, 5.0,  # SEL Plus
          5.6, 5.4, 5.0, 8.9, 10.0, 8.6, 11.0, # Limited + Night
          5.1, 5.4, 4.8, 7.8, 7.3, 6.3, 8.8)  # Ultimate
)

ggplot(heatmap_data, aes(x = Region, y = Trim, fill = MOS)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightgreen", high = "red", name = "MOS") +
  labs(
    title = "Months of Stock (MOS) by Trim and Region"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title = element_blank(),                                     
    axis.text = element_text(size = 18, face = "bold"),               
    legend.position = "none"                                          
  )


###################
stacked_data <- data.frame(
  Month = as.Date(c("2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01")),
  MY_2020 = c(69, 38, 24, 15, 10, 6, 4),
  MY_2021 = c(31, 62, 76, 85, 90, 94, 96)
) %>%
  tidyr::pivot_longer(cols = -Month, names_to = "Model_Year", values_to = "Percentage")

ggplot2::ggplot(stacked_data, ggplot2::aes(x = Month, y = Percentage, fill = Model_Year)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(ggplot2::aes(label = Percentage), position = ggplot2::position_stack(vjust = 0.5), 
                     size = 4, fontface = "bold", color = "black") +
  ggplot2::scale_fill_manual(values = c("MY_2020" = "pink", "MY_2021" = "lightblue")) +
  ggplot2::labs(
    title = "Dealer Stock Mix Transition: MY 2020 vs. MY 2021",
    y = "Percentage of Dealer Stock",
    fill = "Model Year",
    x = " "
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.y = ggplot2::element_text(size = 18),
    axis.text = ggplot2::element_text(size = 16),
    legend.position = c(0.8, 0.8),
    legend.background = ggplot2::element_rect(fill = "white", color = "black"),
    legend.key.size = ggplot2::unit(0.8, "cm")
  )


##############

# Data for radar chart
radar_data <- data.frame(
  Retail_Share = c(7.5, 6.9),       
  Incentive_Spend = c(3386, 3443),   
  CFTP = c(22111, 22338),            
  Lease_Penetration = c(25.5, 25.0), 
  Kona_Sales = c(5817, 4094)   
)
row.names(radar_data) <- c("Dec_2020", "Jan_2021")

# Add max and min values for scaling
max_min <- data.frame(
  Retail_Share = c(9.2, 5.2),        # Max and Min for Retail Share
  Incentive_Spend = c(3900, 2800),# Max and Min for Incentive Spend
  CFTP = c(23000, 22000),         # Max and Min for CFTP
  Lease_Penetration = c(28, 22),  # Max and Min for Lease Penetration
  Kona_Sales = c(6300, 3500) # Max and Min for Dealer Profitability
)
row.names(max_min) <- c("Max", "Min")

# Combine data for radar chart
radar_data_scaled <- rbind(max_min, radar_data)

# Create radar chart without percentage labels
radarchart(
  radar_data_scaled,
  axistype = 0,                   
  pcol = c("blue", "red"),        
  pfcol = c(rgb(0.1, 0.2, 0.5, 0.3), rgb(0.8, 0.2, 0.2, 0.3)),
  plwd = 2,                      
  title = "Kona ICE Performance Overview (Dec 2020 vs. Jan 2021)",
  vlcex = 1.1                     
)

legend(
  "topright", legend = c("Dec 2020", "Jan 2021"),
  col = c("blue", "red"), lty = 1, lwd = 2, bty = "n"
)



############################
# Create the data frame with percentage decrease calculated
competitor_data <- data.frame(
  Month = c("Dec 2020", "Jan 2021", "Change (%)"),
  Seltos = c(6107, 4992, round(((4992 - 6107) / 6107) * 100, 1)),
  Crosstrek = c(14957, 10431, round(((10431 - 14957) / 14957) * 100, 1)),
  HR_V = c(8428, 6369, round(((6369 - 8428) / 8428) * 100, 1))
)

# Format the table with source and change percentage
competitor_data %>%
  kbl(
    caption = "Competitor Sales: Dec 2020 vs. Jan 2021",
    col.names = c("Month", "Seltos (Units)", "Crosstrek (Units)", "HR-V (Units)"),
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2c3e50") %>%
  column_spec(2:4, width = "4cm") %>%
  footnote(
    general = "Source: https://www.conceptcarz.com/monthly-sales",
    general_title = " ",
    footnote_as_chunk = TRUE
  )



############################
# Create the pipeline data frame
pipeline_data <- data.frame(
  Category = c("Dealer Stock", "In-Transit Vehicles", "Port Inventory", "Unbuilt Units"),
  Units = c(19893, 1326, 1157, 8989)
)

# Generate the table using kableExtra
pipeline_data %>%
  kbl(
    caption = "Pipeline Overview",
    col.names = c("Category", "Units"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  column_spec(1, bold = TRUE, width = "5cm") %>%
  column_spec(2, width = "3cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2c3e50")




############################

# Baseline KPIs
kpi_data <- data.frame(
  Region = c("Southern", "Eastern", "Western", "Central", "Mid-Atlantic", "Mountain States", "South Central"),
  Dealer_Stock = c(19893, 1157, 5000, 3000, 4000, 2500, 2000),
  MOS = c(3.1, 3.4, 4.3, 4.6, 4.9, 5.0, 5.5),
  Port_Inventory = c(1157, 0, 0, 0, 0, 0, 0),  # Placeholder
  Unbuilt_Units = c(8989, 0, 0, 0, 0, 0, 0)   # Placeholder
)

# Reallocation scenarios
scenario_1 <- kpi_data %>%
  mutate(
    Port_Inventory = ifelse(Region %in% c("Southern", "Eastern"), Port_Inventory + 300, Port_Inventory - 300),
    Unbuilt_Units = ifelse(Region %in% c("Southern", "Eastern"), Unbuilt_Units + 500, Unbuilt_Units - 500)
  )

# Predicting impact (example calculation: MOS adjustment)
scenario_1 <- scenario_1 %>%
  mutate(
    New_MOS = MOS + (Port_Inventory + Unbuilt_Units) / Dealer_Stock
  )

# Create a comparison table
comparison <- scenario_1 %>%
  select(Region, Dealer_Stock, MOS, New_MOS, Port_Inventory, Unbuilt_Units) %>%
  kbl(
    caption = "What-If Analysis: Reallocation of Inventory and Impact on MOS",
    col.names = c("Region", "Dealer Stock", "Current MOS", "New MOS", "Port Inventory", "Unbuilt Units"),
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

# View table
comparison


############################


library(nloptr)
library(dplyr)
library(kableExtra)

# Baseline KPIs
kpi_data <- data.frame(
  Region = c("Southern", "Eastern", "Western", "Central", "Mid-Atlantic", "Mountain States", "South Central"),
  Dealer_Stock = c(3321, 3892, 2757, 3861, 1726, 1666, 2670),
  MOS = c(3.1, 3.4, 4.3, 4.6, 4.9, 5.0, 5.5),
  Port_Inventory = 1157 * c(3321, 3892, 2757, 3861, 1726, 1666, 2670) / sum(c(3321, 3892, 2757, 3861, 1726, 1666, 2670)),
  Unbuilt_Units = 8989 * c(3321, 3892, 2757, 3861, 1726, 1666, 2670) / sum(c(3321, 3892, 2757, 3861, 1726, 1666, 2670))
)

# Objective Function: Minimize average New_MOS
objective_function <- function(x, kpi_data) {
  # x[1:7] are Port_Inventory adjustments
  # x[8:14] are Unbuilt_Units adjustments
  kpi_data <- kpi_data %>%
    mutate(
      Port_Inventory = Port_Inventory + x[1:7],
      Unbuilt_Units = Unbuilt_Units + x[8:14],
      New_MOS = MOS + (Port_Inventory + Unbuilt_Units) / Dealer_Stock
    )
  
  # Return average New_MOS (lower is better)
  return(mean(kpi_data$New_MOS))
}

# Constraints Function
constraint_function <- function(x, kpi_data) {
  # Total allocation must match the initial totals
  total_port_inventory <- sum(x[1:7]) - 1157
  total_unbuilt_units <- sum(x[8:14]) - 8989
  
  # Ensure no negative allocations
  kpi_data <- kpi_data %>%
    mutate(
      Port_Inventory = Port_Inventory + x[1:7],
      Unbuilt_Units = Unbuilt_Units + x[8:14]
    )
  
  constraints <- c(
    total_port_inventory,   # Total Port Inventory constraint
    total_unbuilt_units,    # Total Unbuilt Units constraint
    kpi_data$Port_Inventory, # No negative Port Inventory
    kpi_data$Unbuilt_Units  # No negative Unbuilt Units
  )
  return(constraints)
}

# Initial Guesses
init_guess <- rep(0, 14)  # Start with no adjustments

# Optimization using NLOPT_LN_COBYLA
result <- nloptr::nloptr(
  x0 = init_guess,
  eval_f = function(x) objective_function(x, kpi_data),
  eval_g_ineq = function(x) constraint_function(x, kpi_data),
  lb = rep(0, 14),  # Lower bound: No negative allocation
  ub = rep(Inf, 14), # Upper bound: No restriction
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

# Updated KPI data with optimized adjustments
optimized_kpi <- kpi_data %>%
  mutate(
    Port_Inventory = Port_Inventory + result$solution[1:7],
    Unbuilt_Units = Unbuilt_Units + result$solution[8:14],
    New_MOS = MOS + (Port_Inventory + Unbuilt_Units) / Dealer_Stock
  )

# Adjustment Function to Match Totals
adjust_to_match_total <- function(values, desired_total) {
  adjustment_factor <- desired_total / sum(values)
  adjusted_values <- values * adjustment_factor
  return(adjusted_values)
}

# Adjust Port_Inventory and Unbuilt_Units to Match Totals
optimized_kpi <- optimized_kpi %>%
  mutate(
    Port_Inventory = adjust_to_match_total(Port_Inventory, 1157),  # Match original total Port Inventory
    Unbuilt_Units = adjust_to_match_total(Unbuilt_Units, 8989),   # Match original total Unbuilt Units
    New_MOS = MOS + (Port_Inventory + Unbuilt_Units) / Dealer_Stock
  )

# Display Final Adjusted Results
optimized_kpi %>%
  select(Region, Dealer_Stock, MOS, New_MOS, Port_Inventory, Unbuilt_Units) %>%
  kbl(
    caption = "Optimized What-If Analysis: Reallocation of Inventory and Impact on MOS",
    col.names = c("Region", "Dealer Stock", "Current MOS", "Adjusted New MOS", "Adjusted Port Inventory", "Adjusted Unbuilt Units"),
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)



##############################


region_data <- data.frame(
  Region = c("Central", "Eastern", "Mid-Atlantic", "Mountain States", "South Central", "Southern", "Western"),
  Unbuilt_Units_Planned = c(1743, 2707, 676, 599, 616, 1538, 1110)
)

region_data %>%
  kbl(
    caption = "Planned Unbuilt Units by Region",
    col.names = c("Region", "Unbuilt Units Planned"),
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, 
                position = "center") %>%
  row_spec(0, bold = TRUE, background = "#f7f7f7") %>%
  column_spec(2, width = "10em")




##############################



# First Table Data
region_data <- data.frame(
  Region = c("Central (4.6)", "Eastern (3.4)", "Mid-Atlantic (4.9)", 
             "Mountain States (5.0)", "South Central (5.5)", 
             "Southern (3.1)", "Western (4.3)"),
  Original = c(1743, 2707, 676, 599, 616, 1538, 1110),
  Suggested = c(1890, 2930, 583, 330, 609, 1702, 830),
  Variance = c(147, 223, -93, -269, -7, 164, -280)
)

# Polishing First Table
region_table <- region_data %>%
  kbl(caption = "Optimized Allocation by Region",
      col.names = c("Region (MOS)", "Original Plan", "Optimized Suggestion", "Variance"),
      align = c("l", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  row_spec(0, bold = T, font_size = 12) %>%
  column_spec(4, bold = T, background = ifelse(region_data$Variance < 0, "#FFCCCB", "#DFFFD6")) %>%
  column_spec(2:3, width = "3cm") %>%
  column_spec(1, bold = T, width = "5cm") %>%
  add_header_above(c(" " = 1, "Unbuilt Units" = 2, " " = 1))

# Second Table Data
trim_data <- data.frame(
  Trim = c("Limited + Night", "SE", "SEL", "SEL Plus", "Ultimate"),
  Original = c(1273, 2183, 2865, 1581, 1087),
  Suggested = c(447, 3525, 3319, 1133, 450),
  Variance = c(-826, 1342, 454, -448, -637)
)

# Polishing Second Table
trim_table <- trim_data %>%
  kbl(caption = "Optimized Allocation by Trim",
      col.names = c("Trim", "Original Plan", "Optimized Suggestion", "Variance"),
      align = c("l", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  row_spec(0, bold = T, font_size = 12) %>%
  column_spec(4, bold = T, background = ifelse(trim_data$Variance < 0, "#FFCCCB", "#DFFFD6")) %>%
  column_spec(2:3, width = "3cm") %>%
  column_spec(1, bold = T, width = "5cm") %>%
  add_header_above(c(" " = 1, "Unbuilt Units" = 2, " " = 1))



# Display tables
region_table
trim_table



