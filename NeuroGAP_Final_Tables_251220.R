# Load required libraries (ALL AT THE TOP)
library(dplyr)
library(janitor)
library(knitr)
library(openxlsx)
library(tidyverse)
library(tidyr)
library(DiagrammeR)  # For flow chart
library(webshot)     # For exporting high-quality images
library(kableExtra)  # For publication-grade tables

# Load CSV file (modify file path to match your file location)
# Load CSV file (modify file path to match your file location)
data <- read.csv("/path/to/your/NeuroGAP_Uganda_data.csv", stringsAsFactors = FALSE)
save_dir <- "/path/to/your/folder/"

################## Define variables (EXACTLY as first script) ##############################
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
               "education", "is_twin", "birth_country", "is_adopted", "is_case",
               "consent_month", "consent_year", "control_diagnosis", "psychosis_primary", "consent_lang")
sx_vars <- c(mini_vars, ptsd_vars, lec_vars)
final_vars <- c(meta_vars, sx_vars)

############## Data filtering (EXACTLY as first script) #########################

# Calculate sample sizes for flow chart
total_sample <- nrow(data)
cat("Total NeuroGAP dataset:", total_sample, "participants\n")

# Flow chart for Anne - EXACTLY as original
data2022 <- data %>% filter((consent_year == 2022) & (consent_month %in% c(3,4,5,6,7,8,9,10,11,12)))
data2022_count <- nrow(data2022)
cat("After 2022 consent filter:", data2022_count, "participants\n")

data2022_psychosis_hxtrauma <- data2022 %>% filter((is_case == 1) & (if_any(all_of(lec_vars), ~ . == 1)))
case_trauma_count <- nrow(data2022_psychosis_hxtrauma)
cat("After case + trauma filter:", case_trauma_count, "participants\n")

# Keep only rows without missing data - EXACTLY as original
clean_data <- data2022_psychosis_hxtrauma %>%
  filter(if_all(all_of(sx_vars), ~ !is.na(.)))
clean_count <- nrow(clean_data)
cat("After removing missing data:", clean_count, "participants\n")

# Exclude organic and substance causes - EXACTLY as original
network_data <- clean_data %>%
  filter(infection_cause == 2 | is.na(infection_cause)) %>%
  filter(substance_cause == 2 | is.na(substance_cause))

final_count <- nrow(network_data)
cat("Final sample size:", final_count, "participants\n")

# Calculate exclusion numbers for flow chart
excluded_2022 <- total_sample - data2022_count
excluded_case_trauma <- data2022_count - case_trauma_count
excluded_missing <- case_trauma_count - clean_count
excluded_organic_substance <- clean_count - final_count

# Calculate detailed breakdown for excluded participants (ENHANCED VERSION)
# Controls vs No trauma breakdown
controls_count <- data2022 %>% filter(is_case == 0) %>% nrow()
no_trauma_count <- data2022 %>% filter(is_case == 1) %>% 
  filter(!if_any(all_of(lec_vars), ~ . == 1)) %>% nrow()

# Missing data breakdown by instrument (more detailed)
data_before_missing <- data2022_psychosis_hxtrauma

# MINI missing data
mini_missing <- data_before_missing %>% 
  filter(if_any(all_of(mini_vars), ~ is.na(.))) %>% nrow()

# PCL-5 missing data  
ptsd_missing <- data_before_missing %>% 
  filter(if_any(all_of(ptsd_vars), ~ is.na(.))) %>% nrow()

# LEC missing data
lec_missing <- data_before_missing %>% 
  filter(if_any(all_of(lec_vars), ~ is.na(.))) %>% nrow()

# Organic vs substance breakdown
organic_count <- clean_data %>% filter(infection_cause == 1) %>% nrow()
substance_count <- clean_data %>% filter(substance_cause == 1) %>% nrow()

# Additional exclusion details that could be added:
# Age exclusions (if any)
underage_count <- data2022 %>% filter(age_at_iview < 18) %>% nrow()
overage_count <- data2022 %>% filter(age_at_iview > 65) %>% nrow()  # if there's an upper limit

# Site-specific exclusions (if relevant)
site_exclusions <- data2022 %>% 
  group_by(study_site) %>% 
  summarise(
    total = n(),
    cases = sum(is_case == 1, na.rm = TRUE),
    with_trauma = sum(is_case == 1 & if_any(all_of(lec_vars), ~ . == 1), na.rm = TRUE),
    final_included = sum(study_site %in% network_data$study_site, na.rm = TRUE)
  )

# Language exclusions (if any)
language_exclusions <- data2022 %>% 
  filter(is.na(consent_lang) | !consent_lang %in% c(1, 31, 64, 65, 87)) %>% 
  nrow()

# Print enhanced exclusion details
cat("\n=== ENHANCED EXCLUSION BREAKDOWN ===\n")
cat("Total excluded for not consenting in 2022:", excluded_2022, "\n")
cat("Of those with 2022 consent:\n")
cat("  - Controls (not cases):", controls_count, "\n") 
cat("  - Cases without trauma history:", no_trauma_count, "\n")
cat("  - Total excluded (controls + no trauma):", excluded_case_trauma, "\n")
cat("\nOf those cases with trauma:\n")
cat("  - Missing MINI-K data:", mini_missing, "\n")
cat("  - Missing PCL-5 data:", ptsd_missing, "\n") 
cat("  - Missing LEC data:", lec_missing, "\n")
cat("  - Total with missing symptom data:", excluded_missing, "\n")
cat("\nOf those with complete data:\n")
cat("  - Organic psychosis excluded:", organic_count, "\n")
cat("  - Substance-induced excluded:", substance_count, "\n")
cat("  - Total excluded (organic/substance):", excluded_organic_substance, "\n")

if(underage_count > 0) cat("Additional: Underage participants (<18):", underage_count, "\n")
if(language_exclusions > 0) cat("Additional: Language/consent issues:", language_exclusions, "\n")

# Site-specific breakdown
cat("\nSite-specific inclusion rates:\n")
print(site_exclusions)

############## Missing data check #########################
# Check total missing values in dataset
sum(is.na(network_data))

# Check missing values by variable  
colSums(is.na(network_data))

################### Table 1: Demographics ########## 
table1 <- list()

# Sex at birth
table1$sex <- network_data %>%
  count(msex) %>%
  mutate(
    Percent = round(100 * n / sum(n), 1),
    msex = recode(msex, `1` = "Male", `0` = "Female")
  ) %>%
  rename(`Sex at birth` = msex, Count = n, `%` = Percent)

# Age categories
table1$age <- network_data %>%
  mutate(age_cat = case_when(
    age_at_iview >= 18 & age_at_iview <= 29 ~ "18–29",
    age_at_iview >= 30 & age_at_iview <= 44 ~ "30–44", 
    age_at_iview >= 45 & age_at_iview <= 59 ~ "45–59",
    age_at_iview >= 60 ~ "60+"
  )) %>%
  count(age_cat) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(`Age categories (years)` = age_cat, Count = n, `%` = Percent)

# Marital status
table1$marital <- network_data %>%
  filter(marital_status != 777) %>%  # unknown excluded
  mutate(marital_group = case_when(
    marital_status %in% c(1, 2) ~ "Married or cohabitating",
    marital_status == 3 ~ "Widowed",
    marital_status %in% c(4, 5) ~ "Divorced or separated",
    marital_status == 6 ~ "Single"
  )) %>%
  count(marital_group) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(`Marital status` = marital_group, Count = n, `%` = Percent)

# Education
table1$education <- network_data %>%
  mutate(education = case_when(
    education == 1 ~ "No formal education",
    education %in% c(2, 3) ~ "Primary",
    education %in% c(4, 5) ~ "Secondary", 
    education %in% c(6, 7) ~ "University"
  )) %>%
  count(education) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(`Level of education` = education, Count = n, `%` = Percent)

# Study site
table1$study_site <- network_data %>% 
  count(study_site) %>% 
  mutate(
    Percent = round(100 * n / sum(n), 1),
    study_site = case_when(
      study_site == 1 ~ "Butabika National Referral Hospital",
      study_site == 3 ~ "Mbarara Regional Referral Hospital", 
      study_site == 4 ~ "Arua Regional Referral Hospital",
      study_site == 5 ~ "Gulu Referral Hospital",
      TRUE ~ as.character(study_site)
    )
  ) %>%
  rename(`Study site` = study_site, Count = n, `%` = Percent)

# Primary diagnosis (3 categories)
table1$psychosis_primary <- network_data %>%
  mutate(psychosis_primary = case_when(
    psychosis_primary %in% c(2, 6) ~ "Bipolar Disorder & Mania NOS",
    psychosis_primary %in% c(7, 8) ~ "Schizophrenia & Psychosis NOS",
    psychosis_primary == 11 ~ "Schizoaffective Disorder"
  )) %>%
  count(psychosis_primary) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(`Primary diagnosis` = psychosis_primary, Count = n, `%` = Percent)

# Primary diagnosis (5 categories - detailed)
table1$psychosis_primary_5 <- network_data %>%
  mutate(psychosis_primary = case_when(
    psychosis_primary == 2 ~ "Bipolar Disorder",
    psychosis_primary == 6 ~ "Mania NOS", 
    psychosis_primary == 7 ~ "Psychotic Disorder NOS",
    psychosis_primary == 8 ~ "Schizophrenia",
    psychosis_primary == 11 ~ "Schizoaffective Disorder"
  )) %>%
  count(psychosis_primary) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(`Primary diagnosis (detailed)` = psychosis_primary, Count = n, `%` = Percent)

# Language of consent
table1$consent_lang <- network_data %>%
  mutate(consent_lang = case_when(
    consent_lang == 1 ~ "Acholi-Luo",
    consent_lang == 31 ~ "English",
    consent_lang == 64 ~ "Luganda", 
    consent_lang == 65 ~ "Lugbara",
    consent_lang == 87 ~ "Runyankole"
  )) %>%
  count(consent_lang) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(`Language of consent` = consent_lang, Count = n, `%` = Percent)

################################ Table 2: Symptom Endorsement ############################
n_total <- nrow(network_data)

# Create table2 with symptom endorsement counts
table2 <- network_data %>%
  select(all_of(sx_vars)) %>%
  summarise(across(everything(), ~sum(. >= 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Count") %>%
  mutate(
    Percent = round(100 * Count / n_total, 1),
    Variable = as.character(Variable)
  )

# PTSD item labels
ptsd_labels <- tibble::tibble(
  Variable = ptsd_vars,
  Label = c(
    "P1. In your lifetime, how much were you bothered by repeated, disturbing, and unwanted memories of the stressful experience?",
    "P2. In your lifetime, how much were you bothered by repeated, disturbing dreams of the stressful experience?",
    "P3. In your lifetime, how much were you bothered by suddenly feeling or acting as if the stressful experience were actually happening again (as if you were actually back there reliving it)?", 
    "P4. In your lifetime, how much were you bothered by feeling very upset when something reminded you of the stressful experience?",
    "P5. In your lifetime, how much were you bothered by having strong physical reactions when something reminded you of the stressful experience (for example, heart pounding, trouble breathing, sweating)?",
    "P6. In your lifetime, how much were you bothered by avoiding memories, thoughts, or feelings related to the stressful experience?",
    "P7. In your lifetime, how much were you bothered by avoiding external reminders of the stressful experience (for example, people, places, conversations, activities, objects, or situations)?",
    "P8. In your lifetime, how much were you bothered by trouble remembering important parts of the stressful experience?",
    "P9. In your lifetime, how much were you bothered by having strong negative beliefs about yourself, other people, or the world (for example, having thoughts such as: I am bad, there is something seriously wrong with me, no one can be trusted, the world is completely dangerous)?",
    "P10. In your lifetime, how much were you bothered by blaming yourself or someone else for the stressful experience or what happened after it?",
    "P11. In your lifetime, how much were you bothered by having strong negative feelings such as fear, horror, anger, guilt, or shame?",
    "P12. In your lifetime, how much were you bothered by loss of interest in activities that you used to enjoy?",
    "P13. In your lifetime, how much were you bothered by feeling distant or cut off from other people?",
    "P14. In your lifetime, how much were you bothered by trouble experiencing positive feelings (for example, being unable to feel happiness or have loving feelings for people close to you)?",
    "P15. In your lifetime, how much were you bothered by irritable behavior, angry outbursts, or acting aggressively?",
    "P16. In your lifetime, how much were you bothered by taking too many risks or doing things that could cause you harm?",
    "P17. In your lifetime, how much were you bothered by being \"superalert\" or watchful or on guard?",
    "P18. In your lifetime, how much were you bothered by feeling jumpy or easily startled?",
    "P19. In your lifetime, how much were you bothered by having difficulty concentrating?",
    "P20. In your lifetime, how much were you bothered by trouble falling or staying asleep?"
  )
)

# MINI K module labels
mini_labels <- tibble::tibble(
  Variable = mini_vars,
  Label = c(
    "K1. Have you ever believed that people were spying on you, or that someone was plotting against you, or trying to hurt you? [Persecutory delusions]",
    "K2. Have you ever believed that someone was reading your mind or could hear your thoughts, or that you could actually read someone's mind or hear what another person was thinking? [Delusion of mind reading and/or thought broadcasting]",
    "K3. Have you ever believed that someone or some force outside of yourself put thoughts in your mind that were not your own, or made you act in a way that was not your usual self? Have you ever felt that you were possessed? [Passivity phenomena (thought insertion, somatic passivity, and delusion of control)]",
    "K4. Have you ever believed that you were being sent special messages through the TV, radio, newspapers, books or magazines or that a person you did not personally know was particularly interested in you? [Ideas/delusions of reference]",
    "K5. Have your relatives or friends ever considered any of your beliefs odd or unusual? [Odd beliefs/delusions]",
    "K6. Have you ever heard things other people couldn't hear, such as voices? [Auditory hallucinations]",
    "K7. Have you ever had visions when you were awake or have you seen things other people couldn't see? [Visual hallucinations]",
    "K8. Did the patient ever exhibit disorganized, incoherent or derailed speech, or marked loosening of associations? [Disorganized speech]",
    "K9. Has the patient ever exhibited disorganized or catatonic behavior? [Disorganized or catatonic behavior]",
    "K10. Has the patient ever had negative symptoms, e.g. significant reduction of emotional expression or affective flattening, poverty of speech (alogia) or an inability to initiate or persist in goal-directed activities (avolition)? [Negative symptoms]"
  )
)

# Combine labels and create ordered table2
label_map <- bind_rows(mini_labels, ptsd_labels) %>%
  mutate(Variable = as.character(Variable))
ordered_vars <- c(mini_vars, ptsd_vars)

# Add labels and maintain ordering
table2_labeled <- table2 %>%
  left_join(label_map, by = "Variable") %>%
  mutate(Item = coalesce(Label, Variable)) %>%
  mutate(order = match(Variable, ordered_vars)) %>%
  arrange(order) %>%
  select(Item, Count, Percent)

################################ Table S1: Life Events Checklist ############################

# LEC item definitions
lec_combined_vars <- list(
  "Natural disaster (e.g., flood, hurricane, tornado, earthquake)"                  = c("lec_q1_1", "lec_q1_2"),
  "Fire or explosion"                                                               = c("lec_q2_1", "lec_q2_2"),
  "Transportation accident (e.g., car, boat, train, plane)"                         = c("lec_q3_1", "lec_q3_2"),
  "Serious accident at work/home/recreation"                                       = c("lec_q4_1", "lec_q4_2"),
  "Exposure to toxic substance (e.g., chemicals, radiation)"                        = c("lec_q5_1", "lec_q5_2"),
  "Physical assault"                                                                = c("lec_q6_1", "lec_q6_2"),
  "Assault with a weapon"                                                           = c("lec_q7_1", "lec_q7_2"),
  "Sexual assault (rape, forced act)"                                               = c("lec_q8_1", "lec_q8_2"),
  "Other unwanted sexual experience"                                                = c("lec_q9_1", "lec_q9_2"),
  "Combat or war-zone exposure"                                                     = c("lec_q10_1", "lec_q10_2"),
  "Captivity (e.g., hostage, POW)"                                                  = c("lec_q11_1", "lec_q11_2"),
  "Life-threatening illness or injury"                                              = c("lec_q12_1", "lec_q12_2"),
  "Severe human suffering"                                                          = c("lec_q13_1", "lec_q13_2"),
  "Sudden violent death (e.g., homicide, suicide)"                                  = "lec_q14_2",
  "Sudden accidental death"                                                         = "lec_q15_2",
  "Serious injury/harm you caused"                                                  = "lec_q16_1",
  "Any other very stressful event or experience"                                    = "lec_q17_1"
)

# Create Table S1 for LEC items
tableS1_lec <- tibble(Variable = names(lec_combined_vars)) %>%
  rowwise() %>%
  mutate(
    Count = {
      vars <- lec_combined_vars[[Variable]]
      row_sum <- Reduce(`|`, lapply(vars, function(v) network_data[[v]] == 1))
      sum(row_sum, na.rm = TRUE)
    },
    Percent = round(100 * Count / nrow(network_data), 1),
    Item = Variable
  ) %>%
  ungroup() %>%
  select(Item, Count, Percent)


## psychmed count
library(dplyr)

tableS1_psymedcount <- network_data %>%
  mutate(
    psych_meds_num_cat = factor(
      psych_meds_num,
      levels = 0:5,
      labels = as.character(0:5)
    )
  ) %>%
  count(psych_meds_num_cat, .drop = FALSE) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

tableS1_psymedcount

## table s1_medication p33 of neurogap psychosis protocol
med_code_map <- data.frame(
  med_code = c(
    15, 19, 20, 21, 22,
    23, 24, 25, 26, 27, 
    28, 29, 30, 31, 32, 
    33, 34, 35, 36, 37, 
    38, 39
  ),
  med_label = c(
    "carbamazepine",  "fluoxetine", "venlafaxine","sertraline",  "benzhexol / trihexyphenidyl",
    "procyclidine", "chlorpromazine", "zuclopenthixol", "flupenthixol", "fluphenazine decanoate",
    "clozapine","olanzapine", "quetiapine", "risperidone","haloperidol",
    "amisulpride","olanzapine","trifluoperazine","diazepam","lithium",
    "amitriptyline","aripiprazole"
  ),
  med_class = c(
    "anticonvulsant", "antidepressant", "antidepressant", "antidepressant","antiparkinsonian", 
    "antiparkinsonian",  rep("antipsychotic", 12), "anxiolytic",  "mood stabilizer",
    "antidepressant",    "antipsychotic"
  ),
  stringsAsFactors = FALSE
)



target_codes <- med_code_map$med_code
total_n <- nrow(network_data)

tableS1_psymedname <- lapply(target_codes, function(v) {
  n <- network_data %>%
    filter(
      med_name_1 == v |
        med_name_2 == v |
        med_name_3 == v |
        med_name_4 == v |
        med_name_5 == v
    ) %>%
    nrow()
  
  data.frame(
    med_code = v,
    N = n,
    percent = 100 * n / total_n
  )
}) %>%
  bind_rows() %>%
  left_join(med_code_map, by = "med_code") %>%
  relocate(med_code, med_label)

tableS1_psymedname

library(dplyr)
library(tidyr)


med_long <- network_data %>%
  select(subj_id, med_name_1, med_name_2, med_name_3, med_name_4, med_name_5) %>%
  pivot_longer(
    cols = starts_with("med_name"),
    names_to = "slot",
    values_to = "med_code"
  ) %>%
  filter(!is.na(med_code))

med_long <- med_long %>%
  left_join(med_code_map, by = "med_code")

med_class_person <- med_long %>%
   distinct(subj_id, med_class)

tableS1_psymedclass <- med_class_person %>%
  count(med_class) %>%
  mutate(
    percent = 100 * n / total_n
  ) %>%
  arrange(desc(n))

tableS1_psymedclass


## comorbidities

cidi_combined_vars <- list(
  "Arthritis or rheumatism"                                      = "cidi_q1",
  "Chronic back or neck problems"                                = "cidi_q2",
  "Frequent or severe headaches"                                 = "cidi_q3",
  "Any other chronic pain"                                       = "cidi_q4",
  "Seasonal allergies like hay fever"                            = "cidi_q5",
  "Stroke, heart attack, or heart disease"                      = c("cidi_q6", "cidi_q7", "cidi_q8"),
  "Stroke or heart attack"                                      = c("cidi_q6", "cidi_q7"),
  "Stroke"                                                     = "cidi_q6",
  "Heart attack"                                               = "cidi_q7",
  "Heart disease"                                                = "cidi_q8",
  "High blood pressure"                                          = "cidi_q9",
  "Chronic lung disease (e.g. asthma, COPD, or emphysema)"          = c("cidi_q10", "cidi_q12"), 
  "Asthma"                                                       = "cidi_q10",
  "Tuberculosis"                                                 = "cidi_q11",
  "Any other chronic lung disease (e.g., COPD or emphysema)"     = "cidi_q12",
  "Diabetes or high blood sugar"                                 = "cidi_q13",
  "An ulcer in your stomach or intestine"                        = "cidi_q14",
  "HIV infection or AIDS"                                        = "cidi_q15",
  "Epilepsy or seizures"                                         = "cidi_q16",
  "Cancer"                                                       = "cidi_q17"
)

tableS1_cidi <- tibble(Variable = names(cidi_combined_vars)) %>%
  rowwise() %>%
  mutate(
    Count = {
      vars <- cidi_combined_vars[[Variable]]
      row_sum <- Reduce(`|`, lapply(vars, function(v) network_data[[v]] == 1))
      sum(row_sum, na.rm = TRUE)
    },
    Percent = round(100 * Count / nrow(network_data), 1),
    Item = Variable
  ) %>%
  ungroup() %>%
  select(Item, Count, Percent)

## bmi
summary(network_data[, "bmi"])

network_data <- network_data %>%
  mutate(
    bmi_status = cut(
      bmi,
      breaks = c(-Inf, 25, 30, Inf),
      labels = c("normal or underweight", "overweight", "obesity"),
      right = FALSE
    )
  )

tableS1_bmi <- network_data %>%
  filter(!is.na(bmi_status)) %>%
  count(bmi_status) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

tableS1_bmi


################################ Publication-Grade Flow Chart ############################

# Create CONSORT-style flow chart with vertical layout (no diagonal lines)
flow_chart_code <- paste0("
digraph flowchart {
  
  # Graph attributes
  graph [layout = dot, rankdir = TB, fontname = 'Arial', fontsize = 12, 
         bgcolor = white, margin = 0.5, splines = ortho]
  
  # Node attributes
  node [shape = box, style = filled, fontname = 'Arial', fontsize = 11, 
        margin = 0.3, penwidth = 1.5]
  
  # Define nodes with sample sizes - Clean professional style
  start [label = '", total_sample, " total NeuroGAP-P records\\n- Consented participants\\n- Uganda sites\\n- 2022 data collection', 
         fillcolor = white, color = black, width = 4, height = 1]
  
  # Exclusion node 1 (positioned to the side)
  exclude1 [label = '", excluded_2022, " participants excluded\\n- Not consented in 2022 study period', 
            fillcolor = white, color = black, width = 3.2, height = 0.8]
  
  step1 [label = '", data2022_count, " participants consented\\nin 2022 (March-December)', 
         fillcolor = white, color = black, width = 3.5, height = 0.8]
  
  # Exclusion node 2
  exclude2 [label = '", excluded_case_trauma, " participants excluded\\n- Controls (not cases): n = ", controls_count, "\\n- No trauma history on LEC: n = ", no_trauma_count, "', 
            fillcolor = white, color = black, width = 3.2, height = 1]
  
  step2 [label = '", case_trauma_count, " cases with trauma history', 
         fillcolor = white, color = black, width = 3.5, height = 0.8]
  
  # Exclusion node 3
  exclude3 [label = '", excluded_missing, " participants excluded\\n- Missing data on symptom variables', 
            fillcolor = white, color = black, width = 3.2, height = 1]
  
  step3 [label = '", clean_count, " participants with\\ncomplete symptom data', 
         fillcolor = white, color = black, width = 3.5, height = 0.8]
  
  # Exclusion node 4
  exclude4 [label = '", excluded_organic_substance, " participants excluded\\n- Organic psychosis: n = ", organic_count, "\\n- Substance-induced psychosis: n = ", substance_count, "', 
            fillcolor = white, color = black, width = 3.2, height = 1]
  
  final [label = 'Final sample of ", final_count, "\\nparticipants included\\nin network analysis', 
         fillcolor = lightgray, color = black, width = 3.5, height = 1, 
         style = 'filled,bold', penwidth = 2]
  
  # Define main flow edges (vertical)
  start -> step1 [color = black, penwidth = 2]
  step1 -> step2 [color = black, penwidth = 2]  
  step2 -> step3 [color = black, penwidth = 2]
  step3 -> final [color = black, penwidth = 2]
  
  # Define exclusion edges (horizontal to the right)
  start -> exclude1 [color = red, penwidth = 1.5, style = dashed, constraint = false]
  step1 -> exclude2 [color = red, penwidth = 1.5, style = dashed, constraint = false]
  step2 -> exclude3 [color = red, penwidth = 1.5, style = dashed, constraint = false]
  step3 -> exclude4 [color = red, penwidth = 1.5, style = dashed, constraint = false]
  
  # Invisible edges to control layout (exclusions positioned to the right)
  exclude1 -> exclude2 [style = invis]
  exclude2 -> exclude3 [style = invis] 
  exclude3 -> exclude4 [style = invis]
  
  # Subgraph to position exclusions to the right
  subgraph cluster_exclusions {
    style = invis
    exclude1; exclude2; exclude3; exclude4;
  }
}
")

# Generate the flow chart
flow_chart <- grViz(flow_chart_code)

# Display flow chart
flow_chart

# Save flow chart as high-quality PNG (using different approach)
tryCatch({
  # Method 1: Direct export using DiagrammeRsvg
  if(require("DiagrammeRsvg", quietly = TRUE) && require("rsvg", quietly = TRUE)) {
    flow_chart %>%
      export_svg() %>%
      charToRaw() %>%
      rsvg::rsvg_png(paste0(save_dir, "Participant_Flow_Chart_N=", final_count, ".png"), 
                     width = 2400, height = 3200)
    
    flow_chart %>%
      export_svg() %>%
      charToRaw() %>%
      rsvg::rsvg_pdf(paste0(save_dir, "Participant_Flow_Chart_N=", final_count, ".pdf"), 
                     width = 8, height = 10)
    
    cat("\nFLOW CHART CREATED (Method 1)\n")
  } else {
    # Method 2: Alternative using webshot
    if(require("webshot", quietly = TRUE)) {
      # Save as HTML first, then convert
      temp_html <- paste0(save_dir, "temp_flowchart.html")
      flow_chart %>% 
        htmlwidgets::saveWidget(temp_html, selfcontained = TRUE)
      
      webshot(temp_html, 
              paste0(save_dir, "Participant_Flow_Chart_N=", final_count, ".png"),
              width = 800, height = 1000, delay = 2)
      
      # Clean up temp file
      if(file.exists(temp_html)) file.remove(temp_html)
      
      cat("\nFLOW CHART CREATED (Method 2)\n")
    } else {
      cat("\nFLOW CHART DISPLAYED (Install DiagrammeRsvg/rsvg or webshot for export)\n")
    }
  }
}, error = function(e) {
  cat("\nFLOW CHART DISPLAYED (Export failed, but chart is visible)\n")
  cat("Error:", e$message, "\n")
  cat("To save chart, install: install.packages(c('DiagrammeRsvg', 'rsvg', 'webshot'))\n")
})

################################ Display Tables with Better Formatting ############################

################################ Display Publication-Grade Tables ############################

# Enhanced table formatting function
format_publication_table <- function(data, title, col_names = NULL) {
  if(is.null(col_names)) col_names <- names(data)
  
  cat("\n")
  cat(paste(rep("═", 100), collapse = ""))
  cat("\n")
  cat(sprintf("%-98s", title))
  cat("\n")
  cat(paste(rep("═", 100), collapse = ""))
  cat("\n")
  
  # Print header
  header_format <- sprintf("%-60s %15s %15s", col_names[1], col_names[2], col_names[3])
  cat(header_format)
  cat("\n")
  cat(paste(rep("─", 100), collapse = ""))
  cat("\n")
  
  # Print data rows
  for(i in 1:nrow(data)) {
    row_format <- sprintf("%-60s %15s %15s", 
                          substr(as.character(data[i,1]), 1, 60),
                          as.character(data[i,2]), 
                          as.character(data[i,3]))
    cat(row_format)
    cat("\n")
  }
  
  cat(paste(rep("─", 100), collapse = ""))
  cat("\n")
}

# Function for Table 1 sections with publication formatting
format_table1_section <- function(section_data, section_title) {
  cat("\n")
  cat(sprintf("%-30s", toupper(section_title)))
  cat("\n")
  cat(paste(rep("─", 90), collapse = ""))
  cat("\n")
  
  for(i in 1:nrow(section_data)) {
    row_format <- sprintf("  %-55s %10s %10s", 
                          as.character(section_data[i,1]),
                          as.character(section_data[i,2]), 
                          as.character(section_data[i,3]))
    cat(row_format)
    cat("\n")
  }
}

# Print Table 1 with publication formatting
cat("\n\n")
cat(paste(rep("═", 100), collapse = ""))
cat("\n")
cat(sprintf("%s", paste0("Table 1. Demographic and clinical characteristics of the study population (N = ", final_count, ")")))
cat("\n")
cat(paste(rep("═", 100), collapse = ""))
cat("\n")
cat(sprintf("%-60s %15s %15s", "Characteristic", "Count", "%"))
cat("\n")
cat(paste(rep("─", 100), collapse = ""))

for (section in names(table1)) {
  format_table1_section(table1[[section]], gsub("_", " ", section))
}

cat("\n")
cat(paste(rep("═", 100), collapse = ""))
cat("\n")

# Print Table 2 with enhanced formatting
cat("\n\n")
cat(paste(rep("═", 120), collapse = ""))
cat("\n")
cat(sprintf("%s", paste0("Table 2. Item-level endorsements on the MINI and the PCL-5 (N = ", final_count, ")")))
cat("\n")
cat(paste(rep("═", 120), collapse = ""))
cat("\n")

# MINI-K Section with better formatting
cat("\n")
cat("PSYCHOTIC SYMPTOM ENDORSEMENT (MINI)")
cat("\n")
cat(paste(rep("─", 120), collapse = ""))
cat("\n")
cat(sprintf("%-85s %15s %15s", "Item", "Count", "%"))
cat("\n")
cat(paste(rep("─", 120), collapse = ""))
cat("\n")

mini_table <- table2_labeled %>% filter(grepl("^K[0-9]", Item))
for(i in 1:nrow(mini_table)) {
  # Truncate long descriptions for better formatting
  item_text <- as.character(mini_table[i,1])
  if(nchar(item_text) > 85) {
    item_text <- paste0(substr(item_text, 1, 82), "...")
  }
  
  row_format <- sprintf("%-85s %15s %15s", 
                        item_text,
                        as.character(mini_table[i,2]), 
                        as.character(mini_table[i,3]))
  cat(row_format)
  cat("\n")
}

# PCL-5 Section with better formatting
cat("\n")
cat("PTSD SYMPTOM ENDORSEMENT (PCL-5)")
cat("\n")
cat(paste(rep("─", 120), collapse = ""))
cat("\n")
cat(sprintf("%-85s %15s %15s", "Item", "Count", "%"))
cat("\n")
cat(paste(rep("─", 120), collapse = ""))
cat("\n")

ptsd_table <- table2_labeled %>% filter(grepl("^P[0-9]", Item))
for(i in 1:nrow(ptsd_table)) {
  # Truncate long descriptions for better formatting
  item_text <- as.character(ptsd_table[i,1])
  if(nchar(item_text) > 85) {
    item_text <- paste0(substr(item_text, 1, 82), "...")
  }
  
  row_format <- sprintf("%-85s %15s %15s", 
                        item_text,
                        as.character(ptsd_table[i,2]), 
                        as.character(ptsd_table[i,3]))
  cat(row_format)
  cat("\n")
}

cat(paste(rep("═", 120), collapse = ""))
cat("\n")

# Print Table S1 with enhanced formatting
cat("\n\n")
cat(paste(rep("═", 110), collapse = ""))
cat("\n")
cat(sprintf("%s", paste0("Table S1. Life Events Checklist (LEC-5) endorsement rates (N = ", final_count, ")")))
cat("\n")
cat(paste(rep("═", 110), collapse = ""))
cat("\n")
cat(sprintf("%-75s %15s %15s", "Life Event", "Count", "%"))
cat("\n")
cat(paste(rep("─", 110), collapse = ""))
cat("\n")

for(i in 1:nrow(tableS1_lec)) {
  # Truncate long descriptions for better formatting
  item_text <- as.character(tableS1_lec[i,1])
  if(nchar(item_text) > 75) {
    item_text <- paste0(substr(item_text, 1, 72), "...")
  }
  
  row_format <- sprintf("%-75s %15s %15s", 
                        item_text,
                        as.character(tableS1_lec[i,2]), 
                        as.character(tableS1_lec[i,3]))
  cat(row_format)
  cat("\n")
}

cat(paste(rep("═", 110), collapse = ""))
cat("\n")

# Create formatted text versions for publication
create_publication_text_tables <- function() {
  
  # Table 1 text version
  table1_text <- capture.output({
    cat("Table 1. Demographic and clinical characteristics of the study population (N = ", final_count, ")\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    
    for (section in names(table1)) {
      cat("\n", toupper(gsub("_", " ", section)), "\n")
      for(i in 1:nrow(table1[[section]])) {
        cat(sprintf("  %-40s %8s (%s%%)\n", 
                    table1[[section]][i,1], 
                    table1[[section]][i,2], 
                    table1[[section]][i,3]))
      }
    }
  })
  
  writeLines(table1_text, paste0(save_dir, "Table1_Demographics_N=", final_count, "_formatted.txt"))
  
  # Table 2 text version
  table2_text <- capture.output({
    cat("Table 2. Item-level endorsements on the MINI and the PCL-5 (N = ", final_count, ")\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    
    cat("\nPsychotic symptom endorsement (MINI)\n")
    mini_table <- table2_labeled %>% filter(grepl("^K[0-9]", Item))
    for(i in 1:nrow(mini_table)) {
      cat(sprintf("%-70s %6s (%s%%)\n", 
                  substr(mini_table[i,1], 1, 70),
                  mini_table[i,2], 
                  mini_table[i,3]))
    }
    
    cat("\nPTSD symptom endorsement (PCL-5)\n")
    ptsd_table <- table2_labeled %>% filter(grepl("^P[0-9]", Item))
    for(i in 1:nrow(ptsd_table)) {
      cat(sprintf("%-70s %6s (%s%%)\n", 
                  substr(ptsd_table[i,1], 1, 70),
                  ptsd_table[i,2], 
                  ptsd_table[i,3]))
    }
  })
  
  writeLines(table2_text, paste0(save_dir, "Table2_Symptoms_N=", final_count, "_formatted.txt"))
  
  # Table S1 text version
  tableS1_text <- capture.output({
    cat("Table S1. Life Events Checklist (LEC-5) endorsement rates (N = ", final_count, ")\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    
    for(i in 1:nrow(tableS1_lec)) {
      cat(sprintf("%-60s %6s (%s%%)\n", 
                  substr(tableS1_lec[i,1], 1, 60),
                  tableS1_lec[i,2], 
                  tableS1_lec[i,3]))
    }
  })
  
  writeLines(tableS1_text, paste0(save_dir, "TableS1_LEC_N=", final_count, "_formatted.txt"))
  
  cat("\nFormatted text tables saved for publication use\n")
}

# Generate formatted text tables
create_publication_text_tables()

# Create HTML tables for publication if kableExtra is available
if(require("kableExtra", quietly = TRUE)) {
  
  # Combine all table1 sections for HTML
  table1_combined <- data.frame()
  for(section_name in names(table1)) {
    section_data <- table1[[section_name]]
    names(section_data) <- c("Characteristic", "Count", "Percent")
    
    # Add section header
    header_row <- data.frame(
      Characteristic = toupper(gsub("_", " ", section_name)),
      Count = "",
      Percent = "",
      stringsAsFactors = FALSE
    )
    
    section_combined <- rbind(header_row, section_data)
    table1_combined <- rbind(table1_combined, section_combined)
  }
  
  # Table 1 HTML version
  table1_html <- table1_combined %>%
    kable(caption = paste0("Table 1. Demographic and clinical characteristics (N = ", final_count, ")"), 
          format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
  
  # Table 2 HTML version  
  table2_html <- table2_labeled %>%
    kable(caption = paste0("Table 2. Symptom endorsement rates (N = ", final_count, ")"),
          format = "html", col.names = c("Item", "Count", "%")) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
  
  # Table S1 HTML version
  tableS1_html <- tableS1_lec %>%
    kable(caption = paste0("Table S1. Life Events Checklist endorsement rates (N = ", final_count, ")"),
          format = "html", col.names = c("Item", "Count", "%")) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
  
  # Save HTML tables
  writeLines(as.character(table1_html), paste0(save_dir, "Table1_Demographics_N=", final_count, ".html"))
  writeLines(as.character(table2_html), paste0(save_dir, "Table2_Symptoms_N=", final_count, ".html"))  
  writeLines(as.character(tableS1_html), paste0(save_dir, "TableS1_LEC_N=", final_count, ".html"))
  
  cat("\nHTML tables saved for publication use\n")
}

################ Save all tables to Excel file #################

# Create workbook
wb <- createWorkbook()

# Table 1 sheet
addWorksheet(wb, "table1")
table1_combined_excel <- list()

for (name in names(table1)) {
  df <- table1[[name]]
  table1_combined_excel[[name]] <- df
}

# Write each table1 section with separators
start_row <- 1
for (name in names(table1_combined_excel)) {
  writeData(wb, "table1", paste("**", name, "**"), startRow = start_row, colNames = FALSE)
  writeData(wb, "table1", table1_combined_excel[[name]], startRow = start_row + 1, colNames = TRUE)
  start_row <- start_row + nrow(table1_combined_excel[[name]]) + 3  # Add spacing
}

# Table 2 sheet
addWorksheet(wb, "table2")
writeData(wb, "table2", table2_labeled)

# Table S1 sheet (LEC items)
addWorksheet(wb, "tableS1_lec")
writeData(wb, "tableS1_lec", tableS1_lec)

# Table S1 sheet (cidi comorbidities)
addWorksheet(wb, "tableS1_cidi")
writeData(wb, "tableS1_cidi", tableS1_cidi)

addWorksheet(wb, "tableS1_bmi")
writeData(wb, "tableS1_bmi", tableS1_bmi)

# Save workbook
excel_filename <- paste0(save_dir, "Descriptive_Tables_N=", final_count, ".xlsx")
saveWorkbook(wb, excel_filename, overwrite = TRUE)

cat("\nDESCRIPTIVE TABLES COMPLETE\n")
cat("Excel file saved:", excel_filename, "\n")
cat("Sheets included:\n")
cat("  1. table1 - Demographics and clinical characteristics\n")
cat("  2. table2 - Symptom endorsement rates (MINI-K and PCL-5)\n")
cat("  3. tableS1 - Life Events Checklist (LEC) endorsement rates\n")
cat("Final sample size for tables:", nrow(network_data), "participants\n")

################################ Flow Chart Summary ############################
cat("\nPARTICIPANT FLOW SUMMARY\n")
cat("Total NeuroGAP dataset:", total_sample, "\n")
cat("After 2022 consent filter:", data2022_count, "(excluded:", excluded_2022, ")\n")
cat("  - Controls excluded:", controls_count, "\n")
cat("  - No trauma history excluded:", no_trauma_count, "\n")
cat("After case + trauma filter:", case_trauma_count, "(excluded:", excluded_case_trauma, ")\n") 
cat("After removing missing data:", clean_count, "(excluded:", excluded_missing, ")\n")
cat("  - Organic psychosis excluded:", organic_count, "\n")
cat("  - Substance-induced excluded:", substance_count, "\n")
cat("Final analytic sample:", final_count, "(excluded:", excluded_organic_substance, ")\n")
cat("Total excluded:", total_sample - final_count, "\n")
cat("Inclusion rate:", round(100 * final_count / total_sample, 1), "%\n")

cat("\nFiles generated:\n")
cat("- Participant_Flow_Chart_N=", final_count, ".png (high-resolution)\n")
cat("- Participant_Flow_Chart_N=", final_count, ".pdf (publication quality)\n")
if(exists("table1_html")) {
  cat("- Table1_Demographics_N=", final_count, ".html (publication-grade)\n")
  cat("- Table2_Symptoms_N=", final_count, ".html (publication-grade)\n")
  cat("- TableS1_LEC_N=", final_count, ".html (publication-grade)\n")
}
cat("- Descriptive_Tables_N=", final_count, ".xlsx (raw data)\n")