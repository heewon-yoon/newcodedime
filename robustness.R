########################################
##### SHARE
########################################

# DiD

# Define the components of your predictor set
thresholds <- c(10, 20, 30)
types <- c("both", "death", "affect")
outcomes <- c("pct.amt.out", "pct.out")

# Create a grid of all combinations
model_grid <- expand.grid(type = types, threshold = thresholds, yname = outcomes, stringsAsFactors = FALSE)

# Run the 2-stage DiD for all combinations
all_results <- model_grid %>%
  transpose() %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    # Run did2s
    model <- did2s(
      data = agg,
      yname = .x$yname,
      first_stage = ~ 1 | district^party + cycle^party,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    # Clean up results for the master list
    tidy(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        threshold = .x$threshold,
        outcome = ifelse(.x$yname == "pct.amt.out", "% Out (Amount)", "% Out (Number)")
      )
  })


# 1. Prepare and Reorder the Data
att_10_df <- all_results %>%
  filter(threshold == 10) %>%
  mutate(
    # Clean labels for the plot
    type_label = case_when(
      type == "both"   ~ "Deaths + Affected",
      type == "death"  ~ "Deaths Only",
      type == "affect" ~ "Affected Only"
    )
  )

# 1. Prepare and Reorder the Data
plot_10_clean <- att_10_df %>%
  mutate(
    # Set the order and the simplified labels simultaneously
    # 'Affected' will be at the bottom, 'Both' at the top
    type_clean = factor(type, 
                        levels = c("both", "death", "affect"),
                        labels = c("Both", "Deaths", "Affected")),
    # Ensure outcome labels match your panel preferences
    outcome_label = ifelse(outcome == "% Out (Amount)", "Amount", "Count")
  )

# 2. Generate the Two-Panel Aesthetic Plot
ggplot(plot_10_clean, aes(x = estimate, y = type_clean, color = type_clean)) +
  # Reference line for null hypothesis
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") + 
  # Points with specific size and dodge
  geom_point(position = position_dodge(width = 0.7), size = 2.6) + 
  # Error bars with height = 0 for the clean horizontal look
  geom_errorbarh(
    aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error),
    height = 0, 
    position = position_dodge(width = 0.7)
  ) + 
  # Facet by outcome (Amount vs Number)
  facet_wrap(~ outcome_label, scales = "free_x") +
  # Apply your specific color palette with simplified labels
  scale_color_manual(values = c("Deaths" = "#d73027", "Affected" = "#4575b4", "Both" = "#7b3294")) + 
  labs(
    x = "Coefficient estimate", 
    y = NULL,
    title = ""
  ) + 
  theme_minimal(base_size = 13) + 
  theme(
    legend.position = "none",        
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12, color = "black")
  )



# TWFE

# 1. Define Model Sets
# Note: You can add 20/30 back here if you want the full sensitivity matrix
preds <- c("nd_death10", "nd_affect10", "nd_both10") 
outcomes <- c("pct.amt.out", "pct.out")

# 2. Run models + extract coefficients
coef_df <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(feols(as.formula(paste0(outcome, " ~ ", predictor, " | district^party + cycle^party")), data = agg)),
    tidy = list(broom::tidy(model, conf.int = TRUE)) # Ensure conf.int is TRUE
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine for Professional Aesthetic
plot_data_final <- coef_df %>%
  mutate(
    # Fix NA Panels: Map outcome names to clean labels
    panel_group = factor(outcome, 
                         levels = c("pct.amt.out", "pct.out"), 
                         labels = c("Amount", "Count")),
    
    # Identify disaster type
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    # Set vertical category order
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Extract threshold and set vertical dodge order (10 on top)
    threshold = str_extract(predictor, "\\d+"),
    threshold_fac = factor(threshold, levels = c("30", "20", "10"))
  )

# 4. Generate Visualization
ggplot(plot_data_final, aes(x = estimate, y = clean_type, color = threshold_fac)) +
  # Reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Professional pointrange aesthetic
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.8, fatten = 4) +
  
  # Facet using the cleaned labels to remove "NA"
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Apply the updated threshold color palette
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30")
  ) +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL,
    color = "Threshold (%)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )


############
# robustness
############

library(dplyr)
library(ggplot2)
library(stringr)

# 1. Prepare TWFE Data
twfe_clean <- plot_data_final %>%
  mutate(threshold_val = as.numeric(str_extract(predictor, "\\d+"))) %>%
  filter(threshold_val == 10) %>%
  mutate(
    method = "TWFE",
    outcome_label = ifelse(outcome == "pct.amt.out", "Amount", "Count"),
    type_clean = case_when(
      str_detect(predictor, "both") ~ "Both",
      str_detect(predictor, "death") ~ "Deaths",
      str_detect(predictor, "affect") ~ "Affected"
    ),
    type_clean = factor(type_clean, levels = c("Both", "Deaths", "Affected"))
  ) %>%
  select(estimate, conf.low, conf.high, type_clean, outcome_label, method)

# 2. Prepare DiD Data
did_clean <- all_results %>%
  filter(threshold == 10) %>%
  mutate(
    method = "DiD", # Changed from did2s to DiD
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    outcome_label = ifelse(outcome == "% Out (Amount)", "Amount", "Count"),
    type_clean = case_when(
      type == "both" ~ "Both",
      type == "death" ~ "Deaths",
      type == "affect" ~ "Affected"
    ),
    type_clean = factor(type_clean, levels = c("Both", "Deaths", "Affected"))
  ) %>%
  select(estimate, conf.low, conf.high, type_clean, outcome_label, method)

# 3. Merge and Plot
robustness_data <- bind_rows(twfe_clean, did_clean)

ggplot(robustness_data, aes(x = estimate, y = type_clean, color = method, shape = method)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with dodged estimators
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.8, fatten = 4) +
  
  # Panel by Outcome
  facet_wrap(~ outcome_label, scales = "free_x") +
  
  # Update manual scales with the new "DiD" label
  scale_color_manual(values = c("TWFE" = "#293352", "DiD" = "#cb6a33")) +
  scale_shape_manual(values = c("TWFE" = 16, "DiD" = 17)) +
  
  labs(
    x = "Coefficient Estimate",
    y = NULL,
    color = "Estimator",
    shape = "Estimator",
    # title = "Robustness Comparison: TWFE vs. DiD",
    # subtitle = "Effect of Disasters on Out-of-District Share (10% Severity Threshold)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )

# 4. Save
ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/robustness_share.png", 
  width = 10, height = 7, dpi = 300
)


########################################
##### COUNT
########################################

# twfe

# 1. Define sets and run models for 10%
types      <- c("affect", "death", "both")
outcomes   <- c("ln_n_in", "ln_n_out", "ln_amt_in", "ln_amt_out")

coef_df_10 <- expand.grid(
  outcome = outcomes,
  type = types,
  threshold = "10",
  stringsAsFactors = FALSE
) %>%
  mutate(predictor = paste0("nd_", type, threshold)) %>%
  rowwise() %>%
  mutate(
    model = list(feols(as.formula(paste0(outcome, " ~ ", predictor, " | district^party + cycle^party")), data = agg)),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 2. Refine formatting
plot_data_10 <- coef_df_10 %>%
  mutate(
    # Fix NA Headers
    panel_group = ifelse(str_detect(outcome, "amt"), "Log Amount", "Log Count"),
    panel_group = factor(panel_group, levels = c("Log Amount", "Log Count")),
    
    # Location mapping
    location = ifelse(str_detect(outcome, "_in"), "In-District", "Out-of-District"),
    location = factor(location, levels = c("In-District", "Out-of-District")),
    
    # Category mapping
    clean_type = case_when(
      type == "affect" ~ "Affected",
      type == "death"  ~ "Deaths",
      type == "both"   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both")))
  )

# 3. Final Plot with dodging to prevent overlap
ggplot(plot_data_10, aes(x = estimate, y = clean_type, color = location, shape = location)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Added position_dodge to separate In-District and Out-of-District vertically
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.8, fatten = 4) +
  
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Your established Green/Orange palette
  scale_color_manual(values = c("In-District" = "#53a483", "Out-of-District" = "#cb6a33")) +
  scale_shape_manual(values = c("In-District" = 16, "Out-of-District" = 17)) +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL,
    color = "Donor Location",
    shape = "Donor Location"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )


# DiD

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
thresholds <- c(10)
types      <- c("both", "death", "affect")
outcomes   <- c("ln_amt_in", "ln_amt_out", "ln_n_in", "ln_n_out")

model_grid_10 <- expand.grid(type = types, threshold = thresholds, yname = outcomes, stringsAsFactors = FALSE)

# 2. Run models [cite: 2026-02-03]
results_10 <- model_grid_10 %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    model <- did2s(
      data = agg,
      yname = .x$yname,
      first_stage = ~ 1 | district^party + cycle^party,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        yname = .x$yname,
        panel_group = ifelse(str_detect(.x$yname, "ln_amt"), "Log Amount", "Log Count"),
        location = ifelse(str_detect(.x$yname, "_in"), "In-District", "Out-of-District")
      )
  })

# 3. Refine formatting [cite: 2026-02-10]
plot_data_10 <- results_10 %>%
  mutate(
    # Row Variable: Disaster Type on Left
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Column Variable: Amount on Left, Count on Right
    panel_group = factor(panel_group, levels = c("Log Amount", "Log Count")),
    
    # Location for shape mapping
    location = factor(location, levels = c("In-District", "Out-of-District"))
  )

# 4. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_10, aes(x = estimate, y = type_fac, color = location, shape = location)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Pointrange with small dodge to separate In/Out shapes [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.9, fatten = 5) +
  
  # Two panels: Amount (Left), Count (Right)
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Using the In/Out Green/Orange palette for clarity within the 10% subset
  scale_color_manual(values = c("In-District" = "#53a483", "Out-of-District" = "#cb6a33")) +
  scale_shape_manual(values = c("In-District" = 16, "Out-of-District" = 17)) +
  
  labs(
    x = "Coefficient Estimate (ATT)", 
    y = NULL,
    color = "Donor Location",
    shape = "Donor Location"
  ) +
  
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )


############
# robustness
############

library(dplyr)
library(ggplot2)
library(stringr)

# 1. Standardize and Clean TWFE Data
twfe_clean <- plot_data_10 %>%
  mutate(
    method = "TWFE",
    # Ensure disaster type labels are clean and ordered
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    # Simplify location for color mapping
    loc_short = ifelse(location == "In-District", "In", "Out")
  ) %>%
  select(estimate, conf.low, conf.high, clean_type, panel_group, location, loc_short, method)

# 2. Standardize and Clean DiD Data
did_clean <- results_10 %>%
  mutate(
    method = "DiD",
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    clean_type = case_when(
      type == "both" ~ "Both",
      type == "death" ~ "Deaths",
      type == "affect" ~ "Affected"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    panel_group = factor(panel_group, levels = c("Log Amount", "Log Count")),
    location = factor(location, levels = c("In-District", "Out-of-District")),
    loc_short = ifelse(location == "In-District", "In", "Out")
  ) %>%
  select(estimate, conf.low, conf.high, clean_type, panel_group, location, loc_short, method)

# 3. Combine Datasets
combined_data <- bind_rows(twfe_clean, did_clean) %>%
  mutate(
    # Create a grouping variable for the interaction of location and method
    # This helps R handle the dodge properly
    grp = paste(location, method)
  )

# 4. Generate the Visualization
ggplot(combined_data, aes(x = estimate, y = clean_type, color = location, shape = method, group = grp)) +
  # Null reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with wide dodge to prevent overlapping of the four points per row
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.8),
                  size = 0.7, fatten = 3.5) +
  
  # Two panels: Log Amount and Log Count
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Professional Green/Orange Location Palette
  scale_color_manual(values = c("In-District" = "#53a483", "Out-of-District" = "#cb6a33")) +
  
  # Distinct shapes: TWFE (Circle) vs DiD (Triangle)
  scale_shape_manual(values = c("TWFE" = 16, "DiD" = 17)) +
  
  labs(
    x = "Coefficient Estimate",
    y = NULL,
    color = "Donor Location",
    shape = "Estimator",
    # title = "Comparison of TWFE and DiD Estimators",
    # subtitle = "Log Outcomes by Donor Location (10% Threshold)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

# 5. Save output
ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/robustness_log.png", 
  width = 12, height = 7, dpi = 300
)


########################################
##### WIN
########################################

# twfe

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
preds <- c("nd_death10", "nd_affect10", "nd_both10")
outcomes <- c("dwdime")
parties <- c(100, 200)

# 2. Run models and extract coefficients [cite: 2026-02-03]
party_coef_10 <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  party_code = parties,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(feols(as.formula(paste0(outcome, " ~ ", predictor, " | district + cycle")), 
                       data = winner %>% filter(party == party_code))),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine for 10% Results Aesthetic [cite: 2026-02-10]
plot_party_10 <- party_coef_10 %>%
  mutate(
    # Disaster Category for Y-axis [cite: 2026-02-03]
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Party labels for columns [cite: 2026-02-10]
    party_label = ifelse(party_code == 100, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican")),
    
    # Fixed Label for Outcome
    outcome_label = ""
  )

# 4. Generate Final Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_party_10, aes(x = estimate, y = clean_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Professional Result Aesthetic: Green points for 10% results [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#53a483", # Consistent 10% Green [cite: 2026-02-10]
                  size = 0.9, fatten = 5) +
  
  # Rows = Ideology Measure, Cols = Party [cite: 2026-02-10]
  facet_grid(outcome_label ~ party_label, scales = "free_x") +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(1.5, "lines")
  )


# DiD

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
thresholds <- c(10)
types      <- c("both", "death", "affect")
parties    <- c(100, 200)

# 2. Run models for DW-DIME separated by party [cite: 2026-02-03]
ideo_10_results <- expand.grid(
  type = types, 
  threshold = thresholds, 
  party_code = parties, 
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    model <- did2s(
      data = winner %>% filter(party == .x$party_code),
      yname = "dwdime",
      first_stage = ~ 1 | district + cycle,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type, 
        party_code = .x$party_code
      )
  })

# 3. Refine for Professional Result Aesthetic [cite: 2026-02-10]
plot_data_10 <- ideo_10_results %>%
  mutate(
    # Row Variable: Disaster Type on Left
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Column Variable: Party labels
    party_label = ifelse(party_code == 100, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  )

# 4. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_10, aes(x = estimate, y = type_fac)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Professional 10% Green Pointrange [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#53a483", 
                  size = 0.9, fatten = 5) +
  
  # Partisan Panels (Left: Dem, Right: Rep)
  facet_wrap(~ party_label, scales = "free_x") +
  
  labs(
    x = "Coefficient Estimate (ATT)", 
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) + 
  theme(
    strip.text = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )


############
# robustness
############

library(dplyr)
library(ggplot2)
library(stringr)

# 1. Clean TWFE Data
twfe_win_clean <- party_coef_10 %>%
  mutate(
    method = "TWFE",
    # Standardize Disaster Types
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    # Standardize Party Labels
    party_label = ifelse(party_code == 100, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  ) %>%
  select(estimate, conf.low, conf.high, clean_type, party_label, method)

# 2. Clean DiD Data
did_win_clean <- ideo_10_results %>%
  mutate(
    method = "DiD",
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    # Standardize Disaster Types
    clean_type = factor(type, 
                        levels = c("affect", "death", "both"),
                        labels = c("Affected", "Deaths", "Both")),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    # Standardize Party Labels
    party_label = ifelse(party_code == 100, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  ) %>%
  select(estimate, conf.low, conf.high, clean_type, party_label, method)

# 3. Merge Datasets
plot_win_robust <- bind_rows(twfe_win_clean, did_win_clean)

# 4. Generate Visualization
ggplot(plot_win_robust, aes(x = estimate, y = clean_type, color = method, shape = method)) +
  # Null reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Professional Result Aesthetic: Dodged pointranges for comparison
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.8, fatten = 4) +
  
  # Facet by Party (Left: Dem, Right: Rep)
  facet_wrap(~ party_label, scales = "free_x") +
  
  # Use your established robustness color palette
  scale_color_manual(values = c("TWFE" = "#293352", "DiD" = "#cb6a33")) +
  scale_shape_manual(values = c("TWFE" = 16, "DiD" = 17)) +
  
  labs(
    x = "Coefficient Estimate",
    y = NULL,
    color = "Estimator",
    shape = "Estimator",
    # title = "Ideology Robustness: TWFE vs. DiD",
    # subtitle = "Outcome: Winner's DW-DIME (10% Severity Threshold)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

# 5. Save Output
ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/robustness_winner.png", 
  width = 10, height = 7, dpi = 300
)



########################################
##### PROB
########################################

# twfe

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
preds <- c("nd_death10", "nd_affect10", "nd_both10")
outcomes <- c("prob_dd") # Focusing on DW-DIME as requested
parties  <- c(100, 200)

# 2. Run models and extract coefficients [cite: 2026-02-03]
prob_party_10 <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  party_code = parties,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(
      feols(
        as.formula(paste0(outcome, " ~ ", predictor, " | district + cycle")), 
        data = prob_anl_dan %>% filter(party == party_code)
      )
    ),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine Formatting [cite: 2026-02-10]
plot_data_10 <- prob_party_10 %>%
  mutate(
    # Disaster Category for Y-axis
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Party labels for columns
    party_label = ifelse(party_code == 200, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  )

# 4. Generate Final Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_10, aes(x = estimate, y = clean_type)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange: Professional 10% Green Aesthetic [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#53a483", 
                  size = 0.9, fatten = 5) +
  
  # Facet columns by Party (removes the right-side Y label) [cite: 2026-02-10]
  facet_wrap(~ party_label, scales = "free_x") +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL, # Removes the Y-axis title ("clean_type")
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )



# DiD

# 1. Define Model Sets (Filtered for 10s)
thresholds <- c(10)
types      <- c("both", "death", "affect")
parties    <- c(100, 200) 

# 2. Run models for prob_dd separated by party
prob_10_results <- expand.grid(
  type = types, 
  threshold = thresholds, 
  party_code = parties, 
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    model <- did2s(
      data = prob_anl_dan %>% filter(party == .x$party_code),
      yname = "prob_dd",
      first_stage = ~ 1 | district + cycle,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type, 
        party_label = ifelse(.x$party_code == 200, "Democrat", "Republican")
      )
  })

# 3. Refine Formatting
plot_prob_10 <- prob_10_results %>%
  mutate(
    # Disaster Category on Left (Y-axis)
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Party order (Dem on left, Rep on right)
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  )

# 4. Generate Final Visualization
ggplot(plot_prob_10, aes(x = estimate, y = type_fac)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Professional 10% Green Pointrange
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#53a483", 
                  size = 0.9, fatten = 5) +
  
  # Partisan Panels (Left: Dem, Right: Rep)
  facet_wrap(~ party_label, scales = "free_x") +
  
  labs(
    x = "Coefficient Estimate (ATT)", 
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) + 
  theme(
    strip.text = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )


############
# robustness
############

library(dplyr)
library(ggplot2)
library(stringr)

# 1. Prepare TWFE Data (from your prob_party_10 results)
twfe_prob_clean <- prob_party_10 %>%
  mutate(
    method = "TWFE",
    # Standardize Disaster Types
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    # The TWFE party_code is 100/200; ensure labels match your DiD logic
    # Note: Your TWFE code had 200 = Democrat, 100 = Republican
    party_label = ifelse(party_code == 200, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  ) %>%
  select(estimate, conf.low, conf.high, clean_type, party_label, method)

# 2. Prepare DiD Data (from your prob_10_results)
did_prob_clean <- prob_10_results %>%
  mutate(
    method = "DiD",
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    # Standardize Disaster Types
    clean_type = case_when(
      type == "both" ~ "Both",
      type == "death" ~ "Deaths",
      type == "affect" ~ "Affected"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    # Ensure levels match TWFE for merging
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  ) %>%
  select(estimate, conf.low, conf.high, clean_type, party_label, method)

# 3. Merge Datasets
plot_prob_robust <- bind_rows(twfe_prob_clean, did_prob_clean)

# 4. Final Visualization
ggplot(plot_prob_robust, aes(x = estimate, y = clean_type, color = method, shape = method)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Dodged pointranges for comparison
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.8, fatten = 4) +
  
  # Facet by Party
  facet_wrap(~ party_label, scales = "free_x") +
  
  # Professional estimator colors
  scale_color_manual(values = c("TWFE" = "#293352", "DiD" = "#cb6a33")) +
  scale_shape_manual(values = c("TWFE" = 16, "DiD" = 17)) +
  
  labs(
    x = "Coefficient Estimate",
    y = NULL,
    color = "Estimator",
    shape = "Estimator",
    # title = "Selection Probability Robustness: TWFE vs. DiD",
    # subtitle = "Outcome: Prob. Extreme (DW-DIME) | 10% Severity Threshold"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

# 5. Save output
ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/robustness_prob.png", 
  width = 10, height = 7, dpi = 300
)





########################################
##### REGRESSION TABLES
########################################


library(did2s)
library(fixest)
library(dplyr)

# Standardized LaTeX style function based on your image
custom_style <- style.tex(main = "base", tabular = "normal")
custom_postprocess <- function(x) {
  x = x[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption", x)]
  x
}

# Master Dictionary for all tables
dict_master <- c(
  "nd_affect10::1" = "Affected", "nd_affect10" = "Affected",
  "nd_death10::1"  = "Deaths",   "nd_death10"  = "Deaths",
  "nd_both10::1"   = "Both",     "nd_both10"   = "Both"
)

#################################################################
# 1. SHARE TABLE (Amount vs. Count)
#################################################################

# Set 1: Share of Amount
m_share_amt <- list(
  did2s(data = agg, yname = "pct.amt.out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = agg, yname = "pct.amt.out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = agg, yname = "pct.amt.out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

# Set 2: Share of Count
m_share_cnt <- list(
  did2s(data = agg, yname = "pct.out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = agg, yname = "pct.out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = agg, yname = "pct.out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

m_share_final <- c(m_share_amt, m_share_cnt)

etable(
  m_share_final,
  tex = TRUE, style.tex = custom_style, postprocess.tex = custom_postprocess,
  se = "cluster", cluster = ~ district, drop = "Intercept",
  dict = dict_master,
  depvar = FALSE, 
  headers = list("Dependent Variables:" = c("Amount" = 3, "Count" = 3)),
  fitstat = ~ n,
  file = "/Users/hyoon/Desktop/dissertation/did_share.tex",
  replace = TRUE
)


#################################################################
# 2. LOG COUNT TABLE (In-District vs. Out-of-District)
#################################################################

# Set 1: Log Count In-District
m_log_in <- list(
  did2s(data = agg, yname = "ln_n_in", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = agg, yname = "ln_n_in", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = agg, yname = "ln_n_in", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

# Set 2: Log Count Out-of-District
m_log_out <- list(
  did2s(data = agg, yname = "ln_n_out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = agg, yname = "ln_n_out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = agg, yname = "ln_n_out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

m_log_final <- c(m_log_in, m_log_out)

etable(
  m_log_final,
  tex = TRUE, style.tex = custom_style, postprocess.tex = custom_postprocess,
  se = "cluster", cluster = ~ district, drop = "Intercept",
  dict = dict_master,
  depvar = FALSE, 
  headers = list("Dependent Variables:" = c("In-District" = 3, "Out-of-District" = 3)),
  fitstat = ~ n,
  file = "/Users/hyoon/Desktop/dissertation/did_log_count.tex",
  replace = TRUE
)

#################################################################
# LOG AMOUNT TABLE (In-District vs. Out-of-District)
#################################################################

# Set 1: Log Amount In-District
m_log_amt_in <- list(
  did2s(data = agg, yname = "ln_amt_in", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = agg, yname = "ln_amt_in", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = agg, yname = "ln_amt_in", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

# Set 2: Log Amount Out-of-District
m_log_amt_out <- list(
  did2s(data = agg, yname = "ln_amt_out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = agg, yname = "ln_amt_out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = agg, yname = "ln_amt_out", first_stage = ~ 1 | district^party + cycle^party, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

# Combine into master list
m_log_amt_final <- c(m_log_amt_in, m_log_amt_out)

# Export to LaTeX
etable(
  m_log_amt_final,
  tex = TRUE, 
  style.tex = custom_style, 
  postprocess.tex = custom_postprocess,
  se = "cluster", 
  cluster = ~ district, 
  drop = "Intercept",
  dict = dict_master,
  depvar = FALSE,
  digits.stats = 5, 
  headers = list("Dependent Variables:" = c("In-District" = 3, "Out-of-District" = 3)),
  fitstat = ~ n,
  file = "/Users/hyoon/Desktop/dissertation/did_log_amount.tex",
  replace = TRUE
)


#################################################################
# 3. WINNER IDEOLOGY TABLE (Democrats vs. Republicans)
#################################################################

# Set 1: Democrats (party == 100)
m_win_dem <- list(
  did2s(data = winner %>% filter(party == 100), yname = "dwdime", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = winner %>% filter(party == 100), yname = "dwdime", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = winner %>% filter(party == 100), yname = "dwdime", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

# Set 2: Republicans (party == 200)
m_win_rep <- list(
  did2s(data = winner %>% filter(party == 200), yname = "dwdime", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = winner %>% filter(party == 200), yname = "dwdime", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = winner %>% filter(party == 200), yname = "dwdime", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

m_win_final <- c(m_win_dem, m_win_rep)

etable(
  m_win_final,
  tex = TRUE, style.tex = custom_style, postprocess.tex = custom_postprocess,
  se = "cluster", cluster = ~ district, drop = "Intercept",
  dict = dict_master,
  depvar = FALSE, 
  digits.stats = "r5",
  headers = list("Dependent Variables:" = c("Democrats" = 3, "Republicans" = 3)),
  fitstat = ~ n,
  file = "/Users/hyoon/Desktop/dissertation/did_winner.tex",
  replace = TRUE
)


#################################################################
# 4. SELECTION PROBABILITY TABLE (Democrats vs. Republicans)
#################################################################


# Set 1: Democrats (party == 100)
m_prob_dem <- list(
  did2s(data = prob_anl_dan %>% filter(party == 100), yname = "prob_dd", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = prob_anl_dan %>% filter(party == 100), yname = "prob_dd", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = prob_anl_dan %>% filter(party == 100), yname = "prob_dd", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

# Set 2: Republicans (party == 200)
m_prob_rep <- list(
  did2s(data = prob_anl_dan %>% filter(party == 200), yname = "prob_dd", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_affect10, ref=0), treatment = "nd_affect10", cluster_var = "district"),
  did2s(data = prob_anl_dan %>% filter(party == 200), yname = "prob_dd", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_death10, ref=0),  treatment = "nd_death10",  cluster_var = "district"),
  did2s(data = prob_anl_dan %>% filter(party == 200), yname = "prob_dd", first_stage = ~ 1 | district + cycle, second_stage = ~ i(nd_both10, ref=0),   treatment = "nd_both10",   cluster_var = "district")
)

m_prob_final <- c(m_prob_dem, m_prob_rep)

etable(
  m_prob_final,
  tex = TRUE, style.tex = custom_style, postprocess.tex = custom_postprocess,
  se = "cluster", cluster = ~ district, drop = "Intercept",
  dict = dict_master,
  depvar = FALSE, 
  headers = list("Dependent Variables:" = c("Democrats" = 3, "Republicans" = 3)),
  fitstat = ~ n,
  file = "/Users/hyoon/Desktop/dissertation/did_prob.tex",
  replace = TRUE
)



#################################################################
# SENSITIVITY: SHARE
#################################################################

library(did2s)
library(dplyr)
library(ggplot2)
library(purrr)
library(broom)

# 1. Define Model Sets [cite: 2026-02-03]
thresholds <- c(10, 20, 30)
types      <- c("affect", "death", "both")
outcomes   <- c("pct.amt.out", "pct.out")

# 2. Run models for all combinations
model_grid <- expand.grid(type = types, threshold = thresholds, yname = outcomes, stringsAsFactors = FALSE)

did_sens_results <- model_grid %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    # Run did2s
    model <- did2s(
      data = agg,
      yname = .x$yname,
      first_stage = ~ 1 | district^party + cycle^party,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    # Clean up results
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        threshold = as.character(.x$threshold),
        outcome = .x$yname
      )
  })

# 3. Refine for Professional Visualization [cite: 2026-02-03, 2026-02-10]
plot_did_sens <- did_sens_results %>%
  mutate(
    # Map outcome to clean labels
    outcome_clean = factor(outcome, 
                           levels = c("pct.amt.out", "pct.out"), 
                           labels = c("Amount", "Count")),
    
    # Standardize Disaster Labels
    type_label = case_when(
      type == "affect" ~ "Affected",
      type == "death"  ~ "Deaths",
      type == "both"   ~ "Both"
    ),
    type_label = factor(type_label, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Set threshold order (10 on top) [cite: 2026-02-03]
    threshold_fac = factor(threshold, levels = c("30", "20", "10"))
  )

# 4. Generate Visualization
ggplot(plot_did_sens, aes(x = estimate, y = type_label, color = threshold_fac)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with vertical dodging
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.6),
                  size = 0.8, fatten = 4) +
  
  # Two panels: Amount vs Count
  facet_wrap(~ outcome_clean, scales = "free_x") +
  
  # Professional palette (Green=10, Orange=20, Purple=30) [cite: 2026-02-03]
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Threshold (%)"
  ) +
  
  labs(
    x = "Coefficient Estimate",
    y = NULL,
    # title = "DiD Sensitivity Analysis: Out-of-District Share",
    # subtitle = "Comparing Estimated Treatment Effects across Disaster Severity Thresholds"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )

# 5. Save Output
ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/didshare_thr.png", 
  width = 10, height = 7, dpi = 300
)


#################################################################
# SENSITIVITY: COUNT
#################################################################

library(did2s)
library(dplyr)
library(ggplot2)
library(purrr)
library(broom)
library(stringr)

# 1. Define sets and run models for DiD [cite: 2026-02-03]
thresholds <- c("10", "20", "30")
types      <- c("affect", "death", "both")
outcomes   <- c("ln_n_in", "ln_n_out", "ln_amt_in", "ln_amt_out")

# Create grid and run did2s specifications
did_log_results <- expand.grid(
  outcome = outcomes,
  type = types,
  threshold = thresholds,
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    model <- did2s(
      data = agg,
      yname = .x$outcome,
      first_stage = ~ 1 | district^party + cycle^party,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        threshold = .x$threshold,
        outcome = .x$outcome
      )
  })

# 2. Refine formatting for Threshold-Color and Location-Shape [cite: 2026-02-10]
plot_did_log <- did_log_results %>%
  mutate(
    # Clean panel labels
    panel_group = ifelse(str_detect(outcome, "amt"), "Log Amount", "Log Count"),
    panel_group = factor(panel_group, levels = c("Log Amount", "Log Count")),
    
    # Location shape mapping
    location = ifelse(str_detect(outcome, "_in"), "In-District", "Out-of-District"),
    location = factor(location, levels = c("In-District", "Out-of-District")),
    
    # Disaster Category for Y-axis
    clean_type = case_when(
      type == "affect" ~ "Affected",
      type == "death"  ~ "Deaths",
      type == "both"   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Threshold color mapping (10 on top)
    threshold_fac = factor(threshold, levels = c("30", "20", "10"))
  )

# 3. Generate Visualization [cite: 2026-02-10]
ggplot(plot_did_log, aes(x = estimate, y = clean_type, color = threshold_fac, shape = location)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with dodging for location AND threshold
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.8),
                  size = 0.7, fatten = 3.5) +
  
  # Panel layout
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Professional palette (Green=10, Orange=20, Purple=30)
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Threshold (%)"
  ) +
  
  # Shape mapping
  scale_shape_manual(values = c("In-District" = 16, "Out-of-District" = 17), 
                     name = "Donor Location") +
  
  labs(
    x = "Coefficient Estimate",
    y = NULL,
    # title = "DiD Sensitivity: Impact on Log Fundraising Totals"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

# 4. Save Output
ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/didlog_thr.png", 
  width = 12, height = 7, dpi = 300
)



#################################################################
# SENSITIVITY: IDEO
#################################################################

library(did2s)
library(dplyr)
library(ggplot2)
library(purrr)
library(broom)
library(stringr)

# 1. Define Model Sets for DiD [cite: 2026-02-03]
thresholds <- c("10", "20", "30")
types      <- c("affect", "death", "both")
outcomes   <- c("dwdime")
parties    <- c(100, 200)

# Create grid and run did2s for both parties across all thresholds
did_ideo_results <- expand.grid(
  outcome = outcomes,
  type = types,
  threshold = thresholds,
  party_code = parties,
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    model <- did2s(
      data = winner %>% filter(party == .x$party_code),
      yname = .x$outcome,
      first_stage = ~ 1 | district + cycle,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        threshold = .x$threshold,
        party_code = .x$party_code
      )
  })

# 2. Refine for Professional Aesthetic [cite: 2026-02-10]
plot_did_ideo <- did_ideo_results %>%
  mutate(
    # Disaster Category for Y-axis
    clean_type = case_when(
      type == "affect" ~ "Affected",
      type == "death"  ~ "Deaths",
      type == "both"   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Threshold order (10 on top)
    threshold_fac = factor(threshold, levels = c("30", "20", "10")),
    
    # Party labels
    party_label = ifelse(party_code == 100, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  )

# 3. Generate Final Sensitivity Visualization
ggplot(plot_did_ideo, aes(x = estimate, y = clean_type, color = threshold_fac)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with dodging for thresholds
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.6),
                  size = 0.7, fatten = 4) +
  
  # Facet by Party (Left: Democrat, Right: Republican)
  facet_wrap(~ party_label, scales = "free_x") +
  
  # Professional Threshold Colors [cite: 2026-02-10]
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Threshold (%)"
  ) +
  
  labs(
    x = "Coefficient Estimate", 
    y = NULL,
    # title = "DiD Sensitivity: Impact on Winner's Ideology (DW-DIME)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

# 4. Save Output
ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/didwinner_th.png", 
  width = 10, height = 7, dpi = 300
)


#################################################################
# SENSITIVITY: PROB
#################################################################

library(did2s)
library(dplyr)
library(ggplot2)
library(purrr)
library(broom)
library(stringr)

# 1. Define Model Sets for DiD [cite: 2026-02-03]
thresholds <- c("10", "20", "30")
types      <- c("affect", "death", "both")
outcomes   <- c("prob_dd")
parties    <- c(100, 200)

# Create grid and run did2s specifications
did_prob_results <- expand.grid(
  outcome = outcomes,
  type = types,
  threshold = thresholds,
  party_code = parties,
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    model <- did2s(
      data = prob_anl_dan %>% filter(party == .x$party_code),
      yname = .x$outcome,
      first_stage = ~ 1 | district + cycle,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        threshold = .x$threshold,
        party_code = .x$party_code
      )
  })

# 2. Refine for Professional Partisan Aesthetic [cite: 2026-02-10]
plot_did_prob <- did_prob_results %>%
  mutate(
    # Disaster Category for Y-axis
    clean_type = case_when(
      type == "affect" ~ "Affected",
      type == "death"  ~ "Deaths",
      type == "both"   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Threshold order (10 on top)
    threshold_fac = factor(threshold, levels = c("30", "20", "10")),
    
    # Party labels - note: party_code 200 = Democrat per your logic
    party_label = ifelse(party_code == 200, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  )

# 3. Generate Final Sensitivity Visualization
ggplot(plot_did_prob, aes(x = estimate, y = clean_type, color = threshold_fac)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with dodging for thresholds [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.7),
                  size = 0.6, fatten = 3.5) +
  
  # Facet by Party
  facet_wrap(~ party_label, scales = "free_x") +
  
  # Professional Threshold Colors [cite: 2026-02-10]
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Threshold (%)"
  ) +
  
  labs(
    x = "Coefficient Estimate",
    y = NULL,
    # title = "DiD Sensitivity: Impact on Selection Probability (DW-DIME)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

# 4. Save Output
ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/didprob_thr.png", 
  width = 10, height = 7, dpi = 300
)



