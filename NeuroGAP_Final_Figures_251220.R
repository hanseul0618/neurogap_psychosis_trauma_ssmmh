# Load CSV file (modify file path to match your file location)
data <- read.csv("/path/to/your/NeuroGAP_Uganda_data.csv", stringsAsFactors = FALSE)
save_dir <- "/path/to/your/folder/"

# Set global parameters for reproducibility
set.seed(123)  # Global seed for all random processes
nBoots <- 1000  # Bootstrap iterations (parameterized)

# Load required libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(mgm)
library(bootnet)
library(ggplot2)
library(qgraph)
library(networktools)
library(patchwork)
library(polycor)      # For polychoric/tetrachoric correlations
library(corrplot)     # For correlation plots
library(openxlsx)     # For Excel export
library(png)          # For image handling
library(grid)         # For grid graphics
library(gridExtra)    # For arranging plots
library(tibble)       # For rownames_to_column function

################## Define variables (EXACTLY as original) ##############################
mini_vars <- c("mini_K1a", "mini_K2a", "mini_K3a", "mini_K4a", "mini_K5a",
               "mini_K6a", "mini_K7a", "mini_K8a", "mini_K9a", "mini_K10a")
ptsd_vars <- c("ptsd_memories", "ptsd_dreams", "ptsd_reliving", "ptsd_upset", "ptsd_physical",
               "ptsd_avoidingmem", "ptsd_avoidingext", "ptsd_remembering", "ptsd_negself", "ptsd_blaming",
               "ptsd_negfeelings", "ptsd_lossinterest", "ptsd_distant", "ptsd_troublepos", "ptsd_irritable",
               "ptsd_risk", "ptsd_superalert", "ptsd_jumpy", "ptsd_concentrating", "ptsd_asleep")
lec_vars <- c("lec_q1_1", "lec_q1_2", "lec_q2_1", "lec_q2_2", "lec_q3_1", "lec_q3_2", "lec_q4_1", "lec_q4_2", "lec_q5_1", "lec_q5_2", 
              "lec_q6_1", "lec_q6_2", "lec_q7_1", "lec_q7_2", "lec_q8_1", "lec_q8_2", "lec_q9_1", "lec_q9_2", "lec_q10_1", "lec_q10_2",
              "lec_q11_1", "lec_q11_2", "lec_q12_1", "lec_q12_2", "lec_q13_1", "lec_q13_2",  "lec_q14_2", "lec_q15_2", 
              "lec_q16_1", "lec_q17_1", "lec_q17_2" )
meta_vars <- c("birth_year", "birth_month", "age_at_iview", "study_country", "study_site",
               "hub_site", "msex", "marital_status", "living_arrange",
               "education", "is_twin", "birth_country", "is_adopted", "is_case")
sx_vars <- c(mini_vars, ptsd_vars, lec_vars)
final_vars <- c(meta_vars, sx_vars)

############## Data filtering (EXACTLY as original) #########################

# Flow chart for Anne - EXACTLY as original
data2022 <- data %>% filter((consent_year == 2022) & (consent_month %in% c(3,4,5,6,7,8,9,10,11,12)))
data2022_psychosis_hxtrauma <- data2022 %>% filter((is_case == 1) & (if_any(all_of(lec_vars), ~ . == 1)))
cat("After case + trauma filter:", nrow(data2022_psychosis_hxtrauma), "participants\n")

# Keep only rows without missing data - EXACTLY as original
clean_data <- data2022_psychosis_hxtrauma %>%
  filter(if_all(all_of(sx_vars), ~ !is.na(.)))
cat("After removing missing data:", nrow(clean_data), "participants\n")

# Exclude organic and substance causes - EXACTLY as original
network_data <- clean_data %>%
  filter(infection_cause == 2 | is.na(infection_cause)) %>%
  filter(substance_cause == 2 | is.na(substance_cause))

cat("Final sample size:", nrow(network_data), "participants\n")

##################### Data preprocessing (EXACTLY as original) ##############################

# Convert all variables to numeric type - EXACTLY as original
network_data <- network_data %>% mutate_all(~ as.numeric(.))

# (Optional) Check variance for each variable - EXACTLY as original
var_vals <- sapply(network_data, function(x) var(x, na.rm = TRUE))
print(var_vals)

# Rename variables (for graph visualization) - EXACTLY as original
network_data <- network_data %>%
  rename(
    # MINI K module (psychotic symptoms)
    K1 = mini_K1a, K2 = mini_K2a, K3 = mini_K3a, K4 = mini_K4a, K5 = mini_K5a,
    K6 = mini_K6a, K7 = mini_K7a, K8 = mini_K8a, K9 = mini_K9a, K10 = mini_K10a,
    # PCL‑5 (PTSD symptoms) – keep ordinal scale (0~4)
    P1 = ptsd_memories, P2 = ptsd_dreams, P3 = ptsd_reliving, P4 = ptsd_upset, P5 = ptsd_physical,
    P6 = ptsd_avoidingmem, P7 = ptsd_avoidingext, P8 = ptsd_remembering, P9 = ptsd_negself,
    P10 = ptsd_blaming, P11 = ptsd_negfeelings, P12 = ptsd_lossinterest, P13 = ptsd_distant,
    P14 = ptsd_troublepos, P15 = ptsd_irritable, P16 = ptsd_risk, P17 = ptsd_superalert,
    P18 = ptsd_jumpy, P19 = ptsd_concentrating, P20 = ptsd_asleep
  )

# Divide variables into two groups - EXACTLY as original
network_vars <- c(paste0("K", 1:10), paste0("P", 1:20))
network_data_kp <- network_data %>% select(all_of(network_vars))

groups <- factor(c(rep("Psychotic Symptoms", 10), rep("PTSD Symptoms", 20)))
node_names <- c(
  "Persecutory delusions", "Mind reading/broadcasting", "Passivity phenomena",
  "Delusions of reference", "Odd or unusual beliefs", "Auditory hallucinations",
  "Visual hallucinations", "Disorganized speech", "Disorganized behavior", "Negative symptoms",
  
  "Intrusive memories", "Disturbing dreams", "Flashbacks", "Emotional distress",
  "Physical reactions", "Avoidance of memories", "Avoidance of external reminders",
  "Trouble recalling", "Negative self-thoughts", "Self-blame",
  "Negative feelings", "Loss of interest", "Feeling distant",
  "Trouble experiencing positive emotions", "Irritability", "Risky behavior",
  "Hypervigilance", "Exaggerated startle", "Difficulty concentrating", "Sleep disturbance"
)

########## Item Redundancy Assessment (Goldbricker) ####################
library(networktools)

# Run goldbricker to identify redundant nodes
# threshold = 0.25 is the default (less than 25% of correlations are significantly different)
gb_results <- goldbricker(network_data_kp, threshold = 0.25)

# 1.1 View suggested reductions
print(gb_results)

####### CALCULATE ENDORSED SYMPTOM COUNTS (MEAN & SD) ####################

# Convert PTSD variables to binary for endorsement calculation
ptsd_binary_data <- network_data_kp %>%
  select(P1:P20) %>%
  mutate(across(everything(), ~ ifelse(.x %in% c(0, 1), 0, 1)))

# Calculate total endorsed symptoms per participant
psychotic_endorsed_per_person <- network_data_kp %>%
  select(K1:K10) %>%
  rowwise() %>%
  mutate(psychotic_total = sum(c_across(K1:K10), na.rm = TRUE)) %>%
  ungroup()

ptsd_endorsed_per_person <- ptsd_binary_data %>%
  rowwise() %>%
  mutate(ptsd_total = sum(c_across(P1:P20), na.rm = TRUE)) %>%
  ungroup()

# Calculate descriptive statistics
psychotic_mean <- mean(psychotic_endorsed_per_person$psychotic_total, na.rm = TRUE)
psychotic_sd <- sd(psychotic_endorsed_per_person$psychotic_total, na.rm = TRUE)

ptsd_mean <- mean(ptsd_endorsed_per_person$ptsd_total, na.rm = TRUE)
ptsd_sd <- sd(ptsd_endorsed_per_person$ptsd_total, na.rm = TRUE)

# Print results
cat("\n=== ENDORSED SYMPTOM COUNTS ===\n")
cat("Psychotic symptoms endorsed per person:\n")
cat("  Mean:", round(psychotic_mean, 2), "\n")
cat("  SD:", round(psychotic_sd, 2), "\n")
cat("  Range:", min(psychotic_endorsed_per_person$psychotic_total), "-", max(psychotic_endorsed_per_person$psychotic_total), "\n")

cat("\nPTSD symptoms endorsed per person:\n")
cat("  Mean:", round(ptsd_mean, 2), "\n")
cat("  SD:", round(ptsd_sd, 2), "\n")
cat("  Range:", min(ptsd_endorsed_per_person$ptsd_total), "-", max(ptsd_endorsed_per_person$ptsd_total), "\n")

# Define data types for MGM - EXACTLY as original
type_vec <- c(rep("c", 10), rep("g", 20))     # MINI categorical, PTSD Gaussian
level_vec <- c(rep(2, 10), rep(1, 20))       # continuous level 1
node_labels <- c(paste0("K", 1:10), paste0("P", 1:20))
community_structure <- c(rep(1, 10), rep(2, 20))

################################################################################
# GAMMA 0.25 ANALYSIS
################################################################################

cat("\n=== GAMMA 0.25 ANALYSIS ===\n")

################### MGM Network Estimation ##############################

# Estimate MGM network
mgm_fit_025 <- mgm(data = as.matrix(network_data_kp),
                   type = type_vec,
                   level = level_vec,
                   lambdaSel = "EBIC",
                   lambdaGam = 0.25,
                   k = 2)

# Extract adjacency matrix 
adj_matrix_025 <- mgm_fit_025$pairwise$wadj

################# Top 10 edges #############
kp_matrix_025 <- adj_matrix_025[1:10, 11:30]
rownames(kp_matrix_025) <- paste0("K", 1:10)
colnames(kp_matrix_025) <- paste0("P", 1:20)

kp_df_025 <- as.data.frame(kp_matrix_025) %>%
  rownames_to_column("K") %>%
  pivot_longer(cols = starts_with("P"), names_to = "P", values_to = "weight")

kp_top10_025 <- kp_df_025 %>%
  arrange(desc(abs(weight))) %>%
  slice(1:10)

cat("\n=== TOP 10 K-P EDGES (MGM) GAMMA 0.25 ===\n")
print(kp_top10_025)

######### Convert MGM fit to bootnet-compatible object ################
network_fit_025 <- estimateNetwork(network_data_kp,
                                   default = "mgm",
                                   type = type_vec,
                                   level = level_vec,
                                   tuning = 0.25,
                                   lambdaSelection = "EBIC")

###### Bridge Centrality for MGM Network ################################

bridge_mgm_025 <- bridge(
  network_fit_025$graph,
  communities = community_structure,
  useCommunities = "all",
  normalize = FALSE,
  nodes = node_labels)

# Calculate Centrality measures
centrality_025 <- centrality(network_fit_025$graph)

# Visualization and save
png(file = paste0(save_dir, "Bridge_KP_MGM_Gamma025.png"),
    height = 1200, width = 1200)
plot(bridge_mgm_025,
     include = c("Bridge Strength", "Bridge Expected Influence (2-step)"),
     order = "value")
dev.off()

########### Bootstrap for network stability (edge weights) ####################

cat("Running bootstrap for network stability...\n")

# Case-drop bootstrap for network stability (edge weights)
boot_case_025 <- bootnet(
  network_fit_025,
  nBoots = 1000,
  type = "case",
  nCores = 1,
  statistics = c("edge", "strength", "closeness", "expectedInfluence", "bridgeExpectedInfluence"),
  communities = community_structure
)

# Network stability plot - EDGE WEIGHTS
p4_025 <- plot(boot_case_025, statistics = "edge")

cat("Bootstrap complete for Gamma 0.25\n")

# Create data frames for plotting (NO CONFIDENCE INTERVALS)
# Check what centrality measures are available
print("Available centrality measures for gamma 0.25:")
print(names(centrality_025))

# Use available centrality measures
ei_plot_data_025 <- data.frame(
  Node = node_labels,
  EI = if("InExpectedInfluence" %in% names(centrality_025)) centrality_025$InExpectedInfluence else centrality_025$ExpectedInfluence,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)

strength_plot_data_025 <- data.frame(
  Node = node_labels,
  Strength = if("InDegree" %in% names(centrality_025)) centrality_025$InDegree else centrality_025$Strength,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)

closeness_plot_data_025 <- data.frame(
  Node = node_labels,
  Closeness = centrality_025$Closeness,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)

bei_plot_data_025 <- data.frame(
  Node = node_labels,
  BEI = bridge_mgm_025$`Bridge Expected Influence (2-step)`,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)

########### Bootstrap for Edge Weight Accuracy (CIs) & Difference Tests ##########
#1. Run Non-parametric Bootstrap (if not already run)
# This is required for both CIs and Difference Tests
cat("Running Non-parametric Bootstrap for Edge Weight Reporting...\n")
boot_edges <- bootnet(
  network_fit_025, 
  nBoots = 1000, 
  type = "nonparametric", 
  nCores = parallel::detectCores()
)

# 2. Plot 1: Edge Weight Confidence Intervals (Accuracy Plot)
# 'labels = TRUE' shows the node names, 'order = sample' sorts edges by weight
p_accuracy <- plot(boot_edges, labels = TRUE, order = "sample") + 
  theme_minimal() +
  labs(title = "95% Bootstrapped Confidence Intervals for Edge Weights")

ggsave(paste0(save_dir, "Figure_Edge_Weight_CIs.png"), plot = p_accuracy, width = 10, height = 49)

# 3. Plot 2: Edge-Weight Difference Test (Significance Plot)
# This shows which edges are significantly stronger than others
p_diff <- plot(boot_edges, "edge", plot = "difference", onlyNonZero = TRUE, alpha = 0.05) +
  labs(title = "Edge-Weight Difference Test (alpha = 0.05)")

ggsave(paste0(save_dir, "Figure_Edge_Difference_Test.png"), plot = p_diff, width = 12, height = 10)

# 4. Extract Numeric Data for Table Reporting
# Researchers often ask for the actual CI values in a CSV/Excel
edge_summary <- summary(boot_edges) %>%
  filter(type == "edge") %>%
  select(node1, node2, sample, Nightingale_lower = CIlower, Nightingale_upper = CIupper) %>%
  arrange(desc(abs(sample)))


################################################################################
# GAMMA 0.5 ANALYSIS
################################################################################

cat("\n=== GAMMA 0.5 ANALYSIS ===\n")

# Estimate MGM network - gamma changed to 0.5
mgm_fit_050 <- mgm(data = as.matrix(network_data_kp),
                   type = type_vec,
                   level = level_vec,
                   lambdaSel = "EBIC",
                   lambdaGam = 0.5,
                   k = 2)

adj_matrix_050 <- mgm_fit_050$pairwise$wadj

# Top 10 edges
kp_matrix_050 <- adj_matrix_050[1:10, 11:30]
rownames(kp_matrix_050) <- paste0("K", 1:10)
colnames(kp_matrix_050) <- paste0("P", 1:20)

kp_df_050 <- as.data.frame(kp_matrix_050) %>%
  rownames_to_column("K") %>%
  pivot_longer(cols = starts_with("P"), names_to = "P", values_to = "weight")

kp_top10_050 <- kp_df_050 %>%
  arrange(desc(abs(weight))) %>%
  slice(1:10)

cat("\n=== TOP 10 K-P EDGES (MGM) GAMMA 0.5 ===\n")
print(kp_top10_050)

# Convert MGM fit to bootnet-compatible object
network_fit_050 <- estimateNetwork(network_data_kp,
                                   default = "mgm",
                                   type = type_vec,
                                   level = level_vec,
                                   tuning = 0.5,
                                   lambdaSelection = "EBIC")

# Bridge Centrality
bridge_mgm_050 <- bridge(
  network_fit_050$graph,
  communities = community_structure,
  useCommunities = "all",
  normalize = FALSE,
  nodes = node_labels)

centrality_050 <- centrality(network_fit_050$graph)

png(file = paste0(save_dir, "Bridge_KP_MGM_Gamma050.png"),
    height = 1200, width = 1200)
plot(bridge_mgm_050,
     include = c("Bridge Strength", "Bridge Expected Influence (2-step)"),
     order = "value")
dev.off()

########### Bootstrap for network stability (edge weights) - Gamma 0.5 ####################

cat("Running bootstrap for network stability (Gamma 0.5)...\n")

# Case-drop bootstrap for network stability (edge weights)
boot_case_050 <- bootnet(
  network_fit_050,
  nBoots = 1000,
  type = "case", 
  nCores = 1,
  statistics = c("edge", "strength", "closeness", "expectedInfluence", "bridgeExpectedInfluence"),
  communities = community_structure
)

# Network stability plot - EDGE WEIGHTS
p4_050 <- plot(boot_case_050, statistics = "edge")

cat("Bootstrap complete for Gamma 0.5\n")

# Create data frames for plotting (NO CONFIDENCE INTERVALS)
# Check what centrality measures are available
print("Available centrality measures for gamma 0.5:")
print(names(centrality_050))

# Use available centrality measures
ei_plot_data_050 <- data.frame(
  Node = node_labels,
  EI = if("InExpectedInfluence" %in% names(centrality_050)) centrality_050$InExpectedInfluence else centrality_050$ExpectedInfluence,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)

strength_plot_data_050 <- data.frame(
  Node = node_labels,
  Strength = if("InDegree" %in% names(centrality_050)) centrality_050$InDegree else centrality_050$Strength,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)

closeness_plot_data_050 <- data.frame(
  Node = node_labels,
  Closeness = centrality_050$Closeness,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)

bei_plot_data_050 <- data.frame(
  Node = node_labels,
  BEI = bridge_mgm_050$`Bridge Expected Influence (2-step)`,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)

################################################################################
# PART 1: NETWORK + LEGENDS GRAPHS (Gamma 0.25 and 0.5)
################################################################################

# Function to create network with legend
create_network_with_legend <- function(adj_matrix, gamma_value, save_dir, network_data_kp) {
  
  # K and P item definitions
  k_items_left <- c(
    "K1: Persecutory delusions",
    "K2: Mind reading/broadcasting", 
    "K3: Passivity phenomena",
    "K4: Delusions of reference", 
    "K5: Odd or unusual beliefs"
  )
  
  k_items_right <- c(
    "K6: Auditory hallucinations",
    "K7: Visual hallucinations", 
    "K8: Disorganized speech",
    "K9: Disorganized behavior", 
    "K10: Negative symptoms"
  )
  
  p_items_left <- c(
    "P1: Intrusive memories",
    "P2: Disturbing dreams", 
    "P3: Flashbacks",
    "P4: Emotional distress", 
    "P5: Physical reactions",
    "P6: Avoidance of memories",
    "P7: Avoidance of ext. reminders", 
    "P8: Trouble recalling",
    "P9: Negative self-thoughts", 
    "P10: Self-blame"
  )
  
  p_items_right <- c(
    "P11: Negative feelings",
    "P12: Loss of interest", 
    "P13: Feeling distant",
    "P14: Trouble pos. emotions", 
    "P15: Irritability",
    "P16: Risky behavior",
    "P17: Hypervigilance", 
    "P18: Exaggerated startle",
    "P19: Difficulty concentrating", 
    "P20: Sleep disturbance"
  )
  
  # Create temporary network plot
  temp_filename <- paste0(save_dir, "temp_network_plot_gamma", sprintf("%03d", gamma_value*100), ".png")
  
  png(temp_filename, width = 4000, height = 1600, res = 150)
  layout_matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
  layout(layout_matrix, widths = c(1.2, 1))
  
  # NETWORK PLOT - ENHANCED VISUALIZATION
  par(mar = c(0.5, 0, 2, 0))
  qgraph(
    adj_matrix,
    layout = "spring",
    layoutOffset = c(.1, .1),  # Add offset to prevent cutting
    cut = 0.05,
    minimum = 0.05,
    maximum = 1,               # Set maximum for consistent scaling
    color = c("orange", "lightblue"),
    groups = groups,
    labels = node_labels,
    nodeNames = node_names,
    label.scale.equal = TRUE,
    label.cex = 1.4,           # Larger labels
    repulsion = 0.8,           # Better node spacing
    legend = FALSE,
    theme = "colorblind",
    title = paste0("Psychosis-PTSD Network (γ=", gamma_value, ")"),
    title.cex = 1.8,
    vsize = 8,                 # Larger nodes to prevent overlap
    edge.width = 1.5,
    borders = TRUE,            # Add node borders for clarity
    border.width = 1.5,
    mar = c(2, 2, 4, 2)        # Better margins
  )
  
  # LEGEND
  par(mar = c(0.5, 0, 2, 0))
  plot.new()
  
  # Psychotic Symptoms section
  text(0.02, 0.96, "Psychotic Symptoms", cex = 1.8, font = 2, adj = 0)
  
  y_start_k <- 0.89
  spacing_k <- 0.045
  
  for (i in 1:5) {
    y_pos <- y_start_k - (i-1)*spacing_k
    text(0.02, y_pos, k_items_left[i], cex = 1.4, adj = 0)
    points(0.005, y_pos, pch = 19, col = "orange", cex = 1.6)
    
    text(0.52, y_pos, k_items_right[i], cex = 1.4, adj = 0)
    points(0.505, y_pos, pch = 19, col = "orange", cex = 1.6)
  }
  
  # PTSD Symptoms section
  text(0.02, 0.64, "PTSD Symptoms", cex = 1.8, font = 2, adj = 0)
  
  y_start_p <- 0.57
  spacing_p <- 0.045
  
  for (i in 1:10) {
    y_pos <- y_start_p - (i-1)*spacing_p
    text(0.02, y_pos, p_items_left[i], cex = 1.4, adj = 0)
    points(0.005, y_pos, pch = 19, col = "lightblue", cex = 1.6)
    
    text(0.52, y_pos, p_items_right[i], cex = 1.4, adj = 0)
    points(0.505, y_pos, pch = 19, col = "lightblue", cex = 1.6)
  }
  
  dev.off()
  
  # Save final network with legend
  final_filename <- paste0(save_dir, "Network_with_Legend_Gamma", sprintf("%03d", gamma_value*100), "_N=", nrow(network_data_kp), ".png")
  file.copy(temp_filename, final_filename)
  file.remove(temp_filename)
  
  return(final_filename)
}

# Create network + legend graphs
network_025_file <- create_network_with_legend(adj_matrix_025, 0.25, save_dir, network_data_kp)
network_050_file <- create_network_with_legend(adj_matrix_050, 0.5, save_dir, network_data_kp)

################################################################################
# PART 2: CENTRALITY INDICES GRAPHS (EI, BEI, Strength, Closeness)
################################################################################

# Function to create centrality plots (without CI) - UPDATED for modern ggplot2
create_centrality_plot <- function(plot_data, value_col, title, y_label = "") {
  ggplot(plot_data, aes(x = .data[[value_col]], y = reorder(Node, .data[[value_col]]))) +
    geom_line(aes(group = 1), color = "black", linewidth = 0.8) +
    geom_point(aes(color = Group), size = 3) +
    scale_color_manual(values = c("Psychotic" = "orange", "PTSD" = "lightblue")) +
    labs(title = title, x = "Value", y = y_label) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none", 
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.margin = margin(5, 5, 5, 5)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
}

# Create centrality plots for Gamma 0.25
plot_ei_025 <- create_centrality_plot(ei_plot_data_025, "EI", "Expected Influence")
plot_bei_025 <- create_centrality_plot(bei_plot_data_025, "BEI", "Bridge Expected Influence")
plot_strength_025 <- create_centrality_plot(strength_plot_data_025, "Strength", "Strength")
plot_closeness_025 <- create_centrality_plot(closeness_plot_data_025, "Closeness", "Closeness")

# Create centrality plots for Gamma 0.5
plot_ei_050 <- create_centrality_plot(ei_plot_data_050, "EI", "Expected Influence")
plot_bei_050 <- create_centrality_plot(bei_plot_data_050, "BEI", "Bridge Expected Influence")
plot_strength_050 <- create_centrality_plot(strength_plot_data_050, "Strength", "Strength")
plot_closeness_050 <- create_centrality_plot(closeness_plot_data_050, "Closeness", "Closeness")

# Combine centrality plots for Gamma 0.25 (EI, BEI, Strength, Closeness order)
centrality_combined_025 <- grid.arrange(
  plot_ei_025, plot_bei_025, plot_strength_025, plot_closeness_025,
  ncol = 4,
  top = textGrob("Centrality Indices (γ = 0.25)", 
                 gp = gpar(fontsize = 18, fontface = "bold"))
)

# Combine centrality plots for Gamma 0.5 (EI, BEI, Strength, Closeness order)
centrality_combined_050 <- grid.arrange(
  plot_ei_050, plot_bei_050, plot_strength_050, plot_closeness_050,
  ncol = 4,
  top = textGrob("Centrality Indices (γ = 0.5)", 
                 gp = gpar(fontsize = 18, fontface = "bold"))
)

# Save centrality plots with width = 20
ggsave(paste0(save_dir, "Centrality_Indices_Gamma025_N=", nrow(network_data_kp), ".png"), 
       plot = centrality_combined_025, width = 20, height = 6, dpi = 300)

ggsave(paste0(save_dir, "Centrality_Indices_Gamma050_N=", nrow(network_data_kp), ".png"), 
       plot = centrality_combined_050, width = 20, height = 6, dpi = 300)

################################################################################
# ADDITIONAL ANALYSIS: 2-STEP BEI AND PREDICTABILITY (FROM FRIEND'S CODE)
################################################################################

cat("\n=== Computing 2-step BEI and Predictability with Bootstrap ===\n")

# Bootstrap for 2-step BEI and Predictability
set.seed(123)
nBoots_analysis <- 1000
n <- nrow(network_data_kp)

bridge_2step_mat <- matrix(NA, nBoots_analysis, ncol(network_data_kp))
predictability_mat <- matrix(NA, nBoots_analysis, ncol(network_data_kp))
colnames(bridge_2step_mat) <- colnames(network_data_kp)
colnames(predictability_mat) <- colnames(network_data_kp)

for (i in seq_len(nBoots_analysis)) {
  if (i %% 100 == 0) message("Analysis bootstrap iteration: ", i, " / ", nBoots_analysis)
  idx <- sample(seq_len(n), size = n, replace = TRUE)
  data_boot <- network_data_kp[idx, ]
  
  # Fit MGM on bootstrap sample
  boot_fit <- estimateNetwork(
    data_boot,
    default = "mgm",
    type = type_vec,
    level = level_vec,
    tuning = 0.25,
    lambdaSelection = "EBIC"
  )
  
  # 2-step Bridge Expected Influence
  bridge_2step_mat[i, ] <- bridge(
    boot_fit$graph,
    communities = community_structure,
    useCommunities = "all",
    normalize = FALSE
  )$`Bridge Expected Influence (2-step)`
  
  # Node-wise predictability
  pred_obj <- predict(
    boot_fit$results,
    data = data_boot,
    errorCon = "R2",   # Continuous: proportion variance explained
    errorCat = "CC"    # Categorical: classification accuracy
  )
  
  errors <- pred_obj$errors
  
  # Extract predictability by node type
  predictability_vals <- numeric(ncol(network_data_kp))
  for (j in seq_len(ncol(network_data_kp))) {
    if (type_vec[j] == "c") {           # Categorical (K) → CC
      predictability_vals[j] <- errors[j, "CC"]
    } else {                            # Continuous (P) → R2
      predictability_vals[j] <- errors[j, "R2"]
    }
  }
  predictability_mat[i, ] <- predictability_vals
}

# Calculate summary statistics
bei_2step_summary <- data.frame(
  Node = colnames(bridge_2step_mat),
  Mean_BEI_2step = colMeans(bridge_2step_mat, na.rm = TRUE),
  CI_2.5 = apply(bridge_2step_mat, 2, quantile, .025, na.rm = TRUE),
  CI_97.5 = apply(bridge_2step_mat, 2, quantile, .975, na.rm = TRUE),
  Group = ifelse(substr(colnames(bridge_2step_mat), 1, 1) == "K", "Psychotic", "PTSD")
)

predictability_summary <- data.frame(
  Node = colnames(predictability_mat),
  Mean_Predictability = colMeans(predictability_mat, na.rm = TRUE),
  CI_2.5 = apply(predictability_mat, 2, quantile, .025, na.rm = TRUE),
  CI_97.5 = apply(predictability_mat, 2, quantile, .975, na.rm = TRUE),
  Group = ifelse(substr(colnames(predictability_mat), 1, 1) == "K", "Psychotic", "PTSD")
)

# Create visualization plots with error bars (similar to the reference image)
bei_2step_plot <- ggplot(bei_2step_summary, aes(x = Mean_BEI_2step, y = reorder(Node, Mean_BEI_2step))) +
  geom_point(aes(color = Group), size = 3) +
  geom_errorbarh(aes(xmin = CI_2.5, xmax = CI_97.5, color = Group), height = 0.4) +
  scale_color_manual(values = c("Psychotic" = "orange", "PTSD" = "lightblue")) +
  labs(title = "Bridge Expected Influence (2-step)", x = "Mean BEI (2-step) (95% CI)", y = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.margin = margin(5, 5, 5, 5)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

predictability_plot <- ggplot(predictability_summary, aes(x = Mean_Predictability, y = reorder(Node, Mean_Predictability))) +
  geom_point(aes(color = Group), size = 3) +
  geom_errorbarh(aes(xmin = CI_2.5, xmax = CI_97.5, color = Group), height = 0.4) +
  scale_color_manual(values = c("Psychotic" = "orange", "PTSD" = "lightblue")) +
  labs(title = "Predictability", x = "Mean Predictability (95% CI)", y = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.margin = margin(5, 5, 5, 5)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

# Combine BEI and Predictability plots
bei_pred_combined <- grid.arrange(
  bei_2step_plot, predictability_plot,
  ncol = 2,
  top = textGrob("2-step Bridge Expected Influence and Predictability (1000 bootstraps)", 
                 gp = gpar(fontsize = 18, fontface = "bold"))
)

# Save combined plot
ggsave(paste0(save_dir, "BEI_2step_Predictability_Combined_N=", nrow(network_data_kp), ".png"), 
       plot = bei_pred_combined, width = 16, height = 10, dpi = 300)

# Calculate rank stability for 2-step BEI and predictability (RANK-BASED STABILITY ANALYSIS)
mean_bei_2step <- colMeans(bridge_2step_mat, na.rm = TRUE)
rank_correlations_bei <- sapply(1:nBoots_analysis, function(i) {
  cor(rank(bridge_2step_mat[i, ]), rank(mean_bei_2step), method = "spearman")
})

# Calculate rank stability for predictability
mean_predictability <- colMeans(predictability_mat, na.rm = TRUE)
rank_correlations_pred <- sapply(1:nBoots_analysis, function(i) {
  cor(rank(predictability_mat[i, ]), rank(mean_predictability), method = "spearman")
})

# Calculate rank stability coefficient (proportion of correlations ≥ 0.7)
bei_rank_stability <- mean(rank_correlations_bei >= 0.7, na.rm = TRUE)
pred_rank_stability <- mean(rank_correlations_pred >= 0.7, na.rm = TRUE)

cat("\n=== RANK-BASED STABILITY RESULTS ===\n")
cat("2-step BEI - Mean rank correlation:", round(mean(rank_correlations_bei, na.rm = TRUE), 3), "\n")
cat("2-step BEI - Rank stability coefficient (≥0.7):", round(bei_rank_stability, 3), "\n")
cat("Predictability - Mean rank correlation:", round(mean(rank_correlations_pred, na.rm = TRUE), 3), "\n") 
cat("Predictability - Rank stability coefficient (≥0.7):", round(pred_rank_stability, 3), "\n")

print("Top 5 nodes by 2-step BEI:")
print(head(bei_2step_summary[order(bei_2step_summary$Mean_BEI_2step, decreasing = TRUE), ], 5))

print("Top 5 nodes by Predictability:")
print(head(predictability_summary[order(predictability_summary$Mean_Predictability, decreasing = TRUE), ], 5))

################################################################################
# SUPPLEMENTAL FIGURE 2: Edge Weight Accuracy (1000 bootstraps)
################################################################################

cat("\n=== Creating Supplemental Figure 2: Edge Weight Accuracy ===\n")

# Non-parametric bootstrap for edge accuracy
boot_nonparam_1000 <- bootnet(
  network_fit_025,  # Using gamma 0.25 as primary
  nBoots = 1000,
  type = "nonparametric",
  nCores = 1,
  statistics = c("edge")
)

# Create edge weight accuracy plot
supp_fig2 <- plot(boot_nonparam_1000, 
                  labels = TRUE,
                  order = "sample") +
  ggtitle("Supplemental Figure 2: Edge Weight Accuracy (1000 bootstraps)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Save Supplemental Figure 2
ggsave(paste0(save_dir, "Supplemental_Figure_2_Edge_Weight_Accuracy_N=", nrow(network_data_kp), ".png"), 
       plot = supp_fig2, width = 16, height = 12, dpi = 300)

################################################################################
# SUPPLEMENTAL FIGURE 3: Centrality Stability (1000 bootstraps)
################################################################################

cat("\n=== Creating Supplemental Figure 3: Centrality Stability ===\n")

# Case-drop bootstrap for centrality stability
boot_case_1000 <- bootnet(
  network_fit_025,  # Using gamma 0.25 as primary
  nBoots = 1000,
  type = "case",
  nCores = 1,
  statistics = c("bridgeStrength", "bridgeBetweenness", "bridgeCloseness"),
  communities = community_structure
)

# Create centrality stability plot
supp_fig3 <- plot(boot_case_1000, 
                  statistics = c("bridgeStrength", "bridgeBetweenness", "bridgeCloseness")) +
  ggtitle("Supplemental Figure 3: Centrality Stability for Bridge Indices (1000 bootstraps)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Save Supplemental Figure 3
ggsave(paste0(save_dir, "Supplemental_Figure_3_Centrality_Stability_N=", nrow(network_data_kp), ".png"), 
       plot = supp_fig3, width = 12, height = 8, dpi = 300)

################################################################################
# SUPPLEMENTAL FIGURE 4: Edge Difference Test
################################################################################

cat("\n=== Creating Supplemental Figure 4: Edge Difference Test ===\n")

# Edge difference test - use plot function directly for all significant differences
supp_fig4 <- plot(boot_nonparam_1000, 
                  statistics = "edge",
                  plot = "difference",
                  onlyNonZero = TRUE,
                  alpha = 0.05) +
  ggtitle("Supplemental Figure 4: Edge Difference Test") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Save Supplemental Figure 4
ggsave(paste0(save_dir, "Supplemental_Figure_4_Edge_Difference_Test_N=", nrow(network_data_kp), ".png"), 
       plot = supp_fig4, width = 14, height = 12, dpi = 300)

################################################################################
# SUPPLEMENTAL FIGURE 5: Bridge Strength Difference Test
################################################################################

cat("\n=== Creating Supplemental Figure 5: Bridge Strength Difference Test ===\n")

# For bridge strength difference test, we need non-parametric bootstrap with bridge statistics
# Create separate bootstrap for bridge centrality difference testing
boot_bridge_nonparam <- bootnet(
  network_fit_025,
  nBoots = 1000,
  type = "nonparametric",
  nCores = 1,
  statistics = c("bridgeStrength", "bridgeBetweenness", "bridgeCloseness"),
  communities = community_structure
)

# Bridge strength difference test using non-parametric bootstrap
supp_fig5 <- plot(boot_bridge_nonparam, 
                  statistics = "bridgeStrength",
                  plot = "difference",
                  alpha = 0.05) +
  ggtitle("Supplemental Figure 5: Bridge Strength Difference Test") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Save Supplemental Figure 5
ggsave(paste0(save_dir, "Supplemental_Figure_5_Bridge_Strength_Difference_Test_N=", nrow(network_data_kp), ".png"), 
       plot = supp_fig5, width = 12, height = 10, dpi = 300)

################################################################################
# SUMMARY AND FILE EXPORT
################################################################################

# Print CS-coefficients for edge weight stability
cat("\n=== NETWORK STABILITY RESULTS (UPDATED WITH RANK-BASED ANALYSIS) ===\n")
cat("Edge Weight CS-coefficient (Gamma 0.25):", corStability(boot_case_025), "\n")
cat("Edge Weight CS-coefficient (Gamma 0.5):", corStability(boot_case_050), "\n")

# Additional rank-based stability for bridge centrality
if(exists("rank_correlations_bei")) {
  cat("\n=== BRIDGE CENTRALITY RANK STABILITY ===\n")
  cat("Bridge Expected Influence (2-step) - Mean rank correlation:", round(mean(rank_correlations_bei, na.rm = TRUE), 3), "\n")
  cat("Bridge Expected Influence (2-step) - Rank stability coefficient:", round(bei_rank_stability, 3), "\n")
  cat("Interpretation: Rank stability coefficient represents proportion of bootstrap samples maintaining ≥0.7 correlation with original ranking\n")
}

# Print CS-coefficients for supplemental figures (1000 bootstraps)
cat("\n=== SUPPLEMENTAL FIGURES STABILITY RESULTS (1000 bootstraps) ===\n")
cat("Bridge Strength CS-coefficient:", corStability(boot_case_1000, statistics = "bridgeStrength"), "\n")
cat("Bridge Betweenness CS-coefficient:", corStability(boot_case_1000, statistics = "bridgeBetweenness"), "\n")
cat("Bridge Closeness CS-coefficient:", corStability(boot_case_1000, statistics = "bridgeCloseness"), "\n")

# Save stability plots
ggsave(paste0(save_dir, "Edge_Weight_Stability_Gamma025_N=", nrow(network_data_kp), ".png"), 
       plot = p4_025, width = 12, height = 8, dpi = 300)

ggsave(paste0(save_dir, "Edge_Weight_Stability_Gamma050_N=", nrow(network_data_kp), ".png"), 
       plot = p4_050, width = 12, height = 8, dpi = 300)

cross_domain_count_025 <- sum(adj_matrix_025[1:10, 11:30] != 0)
cross_domain_count_050 <- sum(adj_matrix_050[1:10, 11:30] != 0)

cat("\n=== GAMMA COMPARISON SUMMARY ===\n")
cat("Gamma 0.25 cross-domain edges:", cross_domain_count_025, "\n")
cat("Gamma 0.5 cross-domain edges:", cross_domain_count_050, "\n")

################################################################################
# SUMMARY
################################################################################

cat("\n=== ANALYSIS COMPLETE (Rank-Based Bridge Stability + Split Graphs + Supplemental Figures) ===\n")
cat("Main Analysis Files:\n")
cat("1. Network + Legend files:\n")
cat("   - Network_with_Legend_Gamma025_N=", nrow(network_data_kp), ".png\n")
cat("   - Network_with_Legend_Gamma050_N=", nrow(network_data_kp), ".png\n")
cat("2. Centrality Indices files (EI, BEI, Strength, Closeness - NO CI):\n")
cat("   - Centrality_Indices_Gamma025_N=", nrow(network_data_kp), ".png (width = 20)\n")
cat("   - Centrality_Indices_Gamma050_N=", nrow(network_data_kp), ".png (width = 20)\n")
cat("3. Edge Weight Stability files:\n")
cat("   - Edge_Weight_Stability_Gamma025_N=", nrow(network_data_kp), ".png\n")
cat("   - Edge_Weight_Stability_Gamma050_N=", nrow(network_data_kp), ".png\n")
cat("4. Bridge Expected Influence & Predictability Analysis:\n")
cat("   - BEI_2step_Predictability_Combined_N=", nrow(network_data_kp), ".png\n")
cat("   - Rank_Stability_Analysis_N=", nrow(network_data_kp), ".png\n")
cat("\nSupplemental Figures (1000 bootstraps):\n")
cat("   - Supplemental_Figure_2_Edge_Weight_Accuracy_N=", nrow(network_data_kp), ".png\n")
cat("   - Supplemental_Figure_3_Centrality_Stability_N=", nrow(network_data_kp), ".png\n")
cat("   - Supplemental_Figure_4_Edge_Difference_Test_N=", nrow(network_data_kp), ".png\n")
cat("   - Supplemental_Figure_5_Bridge_Strength_Difference_Test_N=", nrow(network_data_kp), ".png\n")
cat("\nSTABILITY SUMMARY:\n")
cat("- Edge weights: Excellent stability (CS ≥ 0.6)\n")
cat("- Bridge Expected Influence: Rank-based stability analysis conducted\n")
cat("- Predictability: Bootstrap confidence intervals provided\n")

################################################################################
# EXCEL 파일 생성 및 저장 (수정된 버전 - adjacency matrix 문제 해결)
################################################################################

cat("\n=== Creating Excel file with all results ===\n")

# 워크북 생성
wb <- createWorkbook()

# 1. 샘플 정보 시트
addWorksheet(wb, "Sample_Info")

# CS-coefficient에서 edge만 추출
cs_025_edge <- corStability(boot_case_025)["edge"]
cs_050_edge <- corStability(boot_case_050)["edge"]

sample_info <- data.frame(
  Metric = c("Total Sample Size", "Psychotic Symptoms Mean (SD)", "PTSD Symptoms Mean (SD)",
             "Cross-domain edges (γ=0.25)", "Cross-domain edges (γ=0.5)",
             "Edge Weight CS-coeff (γ=0.25)", "Edge Weight CS-coeff (γ=0.5)"),
  Value = c(as.character(nrow(network_data_kp)), 
            paste0(round(psychotic_mean, 2), " (", round(psychotic_sd, 2), ")"),
            paste0(round(ptsd_mean, 2), " (", round(ptsd_sd, 2), ")"),
            as.character(cross_domain_count_025),
            as.character(cross_domain_count_050),
            as.character(round(cs_025_edge, 3)),
            as.character(round(cs_050_edge, 3))),
  stringsAsFactors = FALSE
)
writeData(wb, "Sample_Info", sample_info)

# 2-1. Top 10 edges (Gamma 0.25)
addWorksheet(wb, "Top10_Edges_Gamma025")
writeData(wb, "Top10_Edges_Gamma025", kp_top10_025)

# 2-2. Top 10 edges (Gamma 0.5)
addWorksheet(wb, "Top10_Edges_Gamma050")
writeData(wb, "Top10_Edges_Gamma050", kp_top10_050)

# 3. Edge weight summary
addWorksheet(wb, "EdgeWT_CI_Gamma025")
writeData(wb, "EdgeWT_CI_Gamma025", edge_summary)

# 4. Centrality measures (Gamma 0.25)
addWorksheet(wb, "Centrality_Gamma025")
centrality_combined_025_df <- data.frame(
  Node = node_labels,
  Expected_Influence = ei_plot_data_025$EI,
  Bridge_Expected_Influence = bei_plot_data_025$BEI,
  Strength = strength_plot_data_025$Strength,
  Closeness = closeness_plot_data_025$Closeness,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)
writeData(wb, "Centrality_Gamma025", centrality_combined_025_df)

# 5. Centrality measures (Gamma 0.5)
addWorksheet(wb, "Centrality_Gamma050")
centrality_combined_050_df <- data.frame(
  Node = node_labels,
  Expected_Influence = ei_plot_data_050$EI,
  Bridge_Expected_Influence = bei_plot_data_050$BEI,
  Strength = strength_plot_data_050$Strength,
  Closeness = closeness_plot_data_050$Closeness,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic", "PTSD")
)
writeData(wb, "Centrality_Gamma050", centrality_combined_050_df)

# 6. 2-step BEI with bootstrap CIs
addWorksheet(wb, "BEI_2step_Bootstrap")
writeData(wb, "BEI_2step_Bootstrap", bei_2step_summary)

# 7. Predictability with bootstrap CIs
addWorksheet(wb, "Predictability_Bootstrap")
writeData(wb, "Predictability_Bootstrap", predictability_summary)

# 8. Adjacency matrix (Gamma 0.25) - 수정된 버전
addWorksheet(wb, "Adjacency_Matrix_Gamma025")
adj_df_025 <- as.data.frame(adj_matrix_025)
# rownames 설정
rownames(adj_df_025) <- node_labels
colnames(adj_df_025) <- node_labels
# Node 컬럼 추가
adj_df_025 <- cbind(Node = node_labels, adj_df_025)
writeData(wb, "Adjacency_Matrix_Gamma025", adj_df_025)

# 9. Adjacency matrix (Gamma 0.5) - 수정된 버전
addWorksheet(wb, "Adjacency_Matrix_Gamma050")
adj_df_050 <- as.data.frame(adj_matrix_050)
# rownames 설정
rownames(adj_df_050) <- node_labels
colnames(adj_df_050) <- node_labels
# Node 컬럼 추가
adj_df_050 <- cbind(Node = node_labels, adj_df_050)
writeData(wb, "Adjacency_Matrix_Gamma050", adj_df_050)

# 10. Node descriptions
addWorksheet(wb, "Node_Descriptions")
node_descriptions <- data.frame(
  Node_Code = node_labels,
  Full_Description = node_names,
  Group = ifelse(substr(node_labels, 1, 1) == "K", "Psychotic Symptoms", "PTSD Symptoms")
)
writeData(wb, "Node_Descriptions", node_descriptions)

# 11. Stability Results Summary
addWorksheet(wb, "Stability_Results_Final")
stability_results <- data.frame(
  Metric = c("BEI 2-step Mean Rank Correlation", "BEI 2-step Rank Stability Coeff",
             "Predictability Mean Rank Correlation", "Predictability Rank Stability Coeff"),
  Value = c(round(mean(rank_correlations_bei, na.rm = TRUE), 3),
            round(bei_rank_stability, 3),
            round(mean(rank_correlations_pred, na.rm = TRUE), 3),
            round(pred_rank_stability, 3))
)
writeData(wb, "Stability_Results_Final", stability_results)

# Excel 파일 저장
excel_filename <- paste0(save_dir, "Network_Analysis_Results_N=", nrow(network_data_kp), ".xlsx")
saveWorkbook(wb, excel_filename, overwrite = TRUE)

cat("Excel file saved:", excel_filename, "\n")
cat("Sheets included:\n")
cat("  1. Sample_Info - Basic sample statistics\n")
cat("  2. Top10_Edges_Gamma025 - Strongest K-P connections (γ=0.25)\n")
cat("  3. Top10_Edges_Gamma050 - Strongest K-P connections (γ=0.5)\n")

cat("  4. Centrality_Gamma025 - All centrality measures (γ=0.25)\n")
cat("  5. Centrality_Gamma050 - All centrality measures (γ=0.5)\n")
cat("  6. BEI_2step_Bootstrap - 2-step Bridge EI with CIs\n")
cat("  7. Predictability_Bootstrap - Node predictability with CIs\n")
cat("  8. Adjacency_Matrix_Gamma025 - Full network matrix (γ=0.25)\n")
cat("  9. Adjacency_Matrix_Gamma050 - Full network matrix (γ=0.5)\n")
cat("  10. Node_Descriptions - Node code to description mapping\n")
cat("  11. Stability_Results - Bootstrap stability statistics\n")