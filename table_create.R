
# Set-up ------------------------------------------------------------------


checkpoint::checkpoint("2021-01-01", checkpointLocation = getwd())

library(gt)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(readr)

# import dataset
# replace NAs with "missing" in columns containing a character vector
data <- read_csv("data/data_extract.csv") %>%
  mutate(across(where(is.character),
                .fns = ~replace_na(.x, "missing")))

cheers <- readRDS("data/cheers.RDS")





# Table 1 -----------------------------------------------------------------
general <- data %>% 
  mutate(model_type_reordered = if_else(model_type_reordered == "regression model", "trial-based analysis", model_type_reordered)) %>%
  mutate(model_category = str_to_sentence(model_type_reordered),
         high_income = str_detect(income_group, "High"),
         middle_income = str_detect(income_group, "middle"),
         low_income = str_detect(income_group, "Low"),
         setting_hospital = case_when(setting_hos == TRUE ~ TRUE,
                                      setting_clinic == TRUE ~ TRUE,
                                      is.character(setting) ~ FALSE),
         setting_other = case_when(setting_pc == TRUE ~ FALSE,
                                   setting_hos == TRUE ~ FALSE,
                                   setting_ec == TRUE ~ FALSE,
                                   is.character(setting) ~ TRUE),
         disease_area_tb = if_else(disease_area_sub == "tuberculosis", TRUE, FALSE),
         disease_area_inf = if_else(disease_area_sub == "influenza", TRUE, FALSE),
         disease_area_pneum = if_else(disease_area_sub == "pneumonia", TRUE, FALSE),
         disease_area_general = str_detect(disease_area_sub, "respiratory"),
         diag_rdt = if_else(intervention_crp + intervention_infrt + intervention_pct > 0, TRUE, FALSE),
         diag_traditional = if_else(intervention_micros + intervention_cult > 0, TRUE, FALSE),
         pop_childad = case_when(
           population_children + population_adolescents >= 1 ~ 1,
           population_children < 1 & population_adolescents < 1 ~ 0),
         perspective_system = if_else((perspective_payer + perspective_system) > 0, TRUE, FALSE),
         perspective_provider = if_else((perspective_lab + perspective_centre + perspective_provider) > 0, TRUE, FALSE)) %>%
  select(author,
         year,
         title,
         model_category,
         `High income` = high_income,
         `Middle income` = middle_income,
         `Low income` = low_income,
         `Primary care` = setting_pc,
         Hospital = setting_hospital,
         `Emergency department` = setting_ec,
         `Rapid diagnostic test` = diag_rdt,
         `Traditional diagnostic` = diag_traditional,
         `Xpert` = intervention_xpert,
         `Clinical rule` = intervention_decisionrule,
         `Children and adolescents` = pop_childad,
         Elderly = population_elderly,
         `Cost-utility analysis` = study_type_cua,
         `Cost-effectiveness analysis` = study_type_cea,
         `Cost-minimization analysis` = study_type_cma,
         `Societal` = perspective_societal,
         System = perspective_system,
         Provider = perspective_provider,
         Tuberculosis = disease_area_tb,
         Influenza = disease_area_inf,
         Pneumonia = disease_area_pneum,
         Other = disease_area_general) %>%
  group_by(model_category) %>%
  summarise(Total = n(),
            across(.cols = !c("Total", "author", "year", "title"),
                   .fns = ~str_c(sum(.x, na.rm = TRUE), " (", percent(sum(.x, na.rm = TRUE) / n()), ")")))


transposed <- general %>%
  as.matrix() %>%
  t() %>%
  as_tibble(rownames = "key")

colnames(transposed) <- as.character(transposed[1,])
ex <- transposed %>% filter(!model_category == "model_category")

gen <- ex %>%
  select(model_category, `Trial-based analysis`, `Decision tree model`, `Markov model`, `Dynamic model`, Other)

gt_general <- gen %>%
  select(model_category, `Trial-based analysis`, `Decision tree model`, `Markov model`, `Dynamic model`, Other) %>%
  gt(rowname_col = "model_category") %>%
  tab_stubhead(label = " ") %>%
  tab_row_group(
    group = "Income",
    rows = 2:4
  ) %>%
  tab_row_group(
    group = "Setting",
    rows = 5:7
  ) %>%
  tab_row_group(
    group = "Diagnostic strategies",
    rows = 8:11
  ) %>%
  tab_row_group(
    group = "Population",
    rows = 12:13
  ) %>%
  tab_row_group(
    group = "Type of analysis",
    rows = 14:16
  ) %>%
  tab_row_group(
    group = "Perspective",
    rows = 17:19
  ) %>%
  tab_row_group(
    group = "Clinical indication",
    rows = 20:23
  ) %>%
  tab_source_note(
    source_note = "Note that not all items are reported by all articles; hence not all columns sum to the total included articles"
  ) %>%
  tab_footnote(
    footnote = "Includes rapid influenza tests, C-reactive protein tests and procalcitonin tests",
    locations = cells_stub(
      rows = 8
    )
  ) %>%
  tab_footnote(
    footnote = "Including microscopy and microbiological cultures",
    locations = cells_stub(
      rows = 9
    )
  ) %>%
  tab_footnote(
    footnote = "GeneXpert tuberculosis and rifampicin resistance test",
    locations = cells_stub(
      rows = 10
    )
  ) %>%
  tab_footnote(
    footnote = "Includes analyses from the perspective of a health centre, a laboratory or other provider of care",
    locations = cells_stub(
      rows = 19
    ) 
  ) %>%
  tab_footnote(
    footnote = "Includes the healthcare system's and healthcare payer's perspective",
    locations = cells_stub(
      rows = 18
    ) 
  ) %>%
  tab_footnote(
    footnote = "Including sinusitis, pharyngitis, sore throat, general respiratory infections",
    locations = cells_stub(
      rows = 23
    ) 
  ) %>%
  tab_footnote(
    footnote = "Including a microsimulation and two database studies",
    locations = cells_column_labels(
      columns = 5
    )
  ) %>%
  tab_footnote(
    footnote = "According to World Bank definitions",
    locations = cells_row_groups(
      groups = "Income"
    )
  ) %>%
  row_group_order(c("Income", "Population", "Setting","Clinical indication","Diagnostic strategies"))

gt_general



# Table 2 -----------------------------------------------------------------

methods <- data %>% 
  mutate(model_type_reordered = if_else(model_type_reordered == "regression model", "trial-based analysis", model_type_reordered)) %>%
  mutate(model_category = str_to_sentence(model_type_reordered),
         `Less than 1 year` = str_detect(horizon_categories, "<1 year"),
         `One year or more` = str_detect(horizon_categories, "1-5 years|>= 5 years"),
         `Lifetime` = str_detect(horizon_categories, "lifetime"),
         Unknown= str_detect(horizon_categories, "unknown"),
         `QALYs or DALYs` = if_else((outcome_qaly + outcome_daly) > 0, TRUE, FALSE),
         `Treatment-related` = str_detect(clinical_outcomes_categories, "Treatment"),
         `Based on diagnostic performance` = str_detect(clinical_outcomes_categories, "Diagnostic"),
         `Time-related` = str_detect(clinical_outcomes_categories, "Time|duration|Hospital"),
         `Single-study based` = str_detect(effectiveness, "Single"),
         `Synthesis` = str_detect(effectiveness, "Synt"),
         reporting_univariate = str_detect(result_uncertainty, "DSA|Sensitivity analysis graph (with one parameter varied)"),
         reporting_multivariate = str_detect(result_uncertainty, "Two-way|Three-way"),
         reporting_probabilistic = if_else(str_detect(result_uncertainty, "PSA|acceptability") + model_stoch >= 1, TRUE, FALSE)) %>%
  select(model_category,
         `Less than 1 year`,
         `One year or more`,
         `Lifetime`,
         Unknown,
         `QALYs or DALYs`,
         `Treatment-related`,
         `Based on diagnostic performance`,
         `Time-related`,
         `Single-study based`,
         `Synthesis`,
         `Resistance included in analysis`= model_amr,
         # `Table of deterministic sensitivity analysis` = reporting_dsatable,
         # `Tornado diagram` = reporting_tornado,
         # `CEAC` = reporting_ceac,
         Univariate = reporting_univariate,
         Multivariate = reporting_multivariate,
         Probabilistic = reporting_probabilistic) %>%
  group_by(model_category) %>%
  summarise(Total = n(),
            across(.cols = !Total,
                   .fns = ~str_c(sum(.x, na.rm = TRUE), " (", percent(sum(.x, na.rm = TRUE) / n()), ")")))

transposed <- methods %>%
  as.matrix() %>%
  t() %>%
  as_tibble(rownames = "key")

colnames(transposed) <- as.character(transposed[1,])
ex_met <- transposed %>% filter(!model_category == "model_category")


gt_methods <- ex_met %>%
  select(model_category, `Trial-based analysis`, `Decision tree model`, `Markov model`, `Dynamic model`, Other) %>%
  gt(rowname_col = "model_category") %>%
  tab_stubhead(label = " ") %>%
  tab_row_group(
    group = "Time horizon",
    rows = 2:5
  ) %>%
  tab_row_group(
    group = "Clinical outcomes reported",
    rows = c(6:9,12)
  ) %>%
  tab_row_group(
    group = "Measurement of effectiveness",
    rows = 10:11
  ) %>%
  tab_row_group(
    group = "Sensitivity analyses",
    rows = 13:15
  ) %>%
  tab_source_note(
    source_note = "Note that not all items are reported by all articles; hence not all columns sum to the total included articles"
  ) %>%
  tab_source_note(
    source_note = "QALY: Quality-Adjusted Life Year; DALY: Disability-Adjusted Life Year; CEAC: Cost-Effectiveness Acceptability Curve"
  ) %>%
  tab_footnote(
    footnote = "Excluding lifetime horizons",
    locations = cells_stub(
      rows = 3
    )
  ) %>%
  tab_footnote(
    footnote = "Includes number of correct diagnoses (for example true positives) and time to correct diagnosis",
    locations = cells_stub(
      rows = 7
    )
  ) %>%
  tab_footnote(
    footnote = "Includes time to correct diagnosis, hospital length-of-stay and disease duration",
    locations = cells_stub(
      rows = 9
    )
  ) %>%
  row_group_order(c("Time horizon","Measurement of effectiveness","Clinical outcomes reported", "Sensitivity analyses"))

gt_methods


# Table 3 -----------------------------------------------------------------


cheers_sum <- cheers %>%
  mutate(model_type_reordered = if_else(model_type_reordered == "regression analysis", "trial-based analysis", model_type_reordered)) %>%
  select(1:14, 26, 15:17, 27, 28, 18:24, 25) %>%
  group_by(model_type_reordered) %>%
  summarise(across(c(4:14),~sum(.x) / n()),
            valuationqalysdalys_12 = sum(valuationqalysdalys_12, na.rm = TRUE) / (n() - sum(is.na(valuationqalysdalys_12))),
            across(c(16:27),~sum(.x) / n())) %>%
  select(Title = title_1,
         Abstract = abstract_2,
         `Background and objectives` = context_3,
         `Target population and subgroups` = population_4,
         `Setting and location` = setting_5,
         `Study perspective` = perspective_6,
         `Comparators` = intervention_7,
         `Time horizon` = timehorizon_8,
         `Discount rate` = discount_9,
         `Choice of health outcomes` = outcomes_10,
         `Measurement of effectiveness` = effectiveness_11,
         `Measurement and valuation of preference based outcomes` = valuationqalysdalys_12,
         `Estimating resources and costs` = resources_13,
         `Currency, price date and conversion` = currency_14,
         `Choice of model` = design_15,
         Assumptions = assumptions_16,
         `Analytical methods` = analyticalmethods_17,
         `Reporting of parameters (including uncertainty)` = parameters_18,
         `Incremental costs and outcomes` = icer_19,
         `Characterising uncertainty` = uncertainty_20,
         `Characterising heterogeneity` = hetero_21,
         `Study findings, limitations, generalisability and current knowledge` = discussion_22,
         `Source of funding` = funding_23,
         `Conflicts of interest` = coi_24)



dtransposed <- cheers_sum %>%
  as.matrix() %>%
  t() %>%
  as_tibble()

colnames(dtransposed) <- as.character(levels(as.factor(cheers$model_type_reordered))) 

gt_cheers <- dtransposed %>%
  tibble::add_column(colnames(cheers_sum)) %>%
  select(`Cheers item` = `colnames(cheers_sum)`,
         `Trial-based analysis` = `trial-based analysis`,
         `Decision tree model` = `decision tree model`,
         `Markov model` = `markov model`,
         `Dynamic model` = `dynamic model`,
         `Other` = other) %>%
  gt(rowname_col = "Cheers item") %>%
  tab_stubhead(label = " ") %>%
  tab_row_group(
    group = "Title and abstract",
    rows = 1:2
  ) %>%
  tab_row_group(
    group = "Introduction",
    rows = 3
  ) %>%
  tab_row_group(
    group = "Methods",
    rows = 4:17
  ) %>%
  tab_row_group(
    group = "Results",
    rows = 18:21
  ) %>%
  tab_row_group(
    group = "Discussion",
    rows = 22
  ) %>%
  tab_row_group(
    group = "Other",
    rows = 23:24
  ) %>%
  row_group_order(c("Title and abstract", "Introduction", "Methods", "Results", "Discussion", "Other")) %>%
  tab_footnote(
    footnote = "Excluded are the articles without preference-based outcomes",
    locations = cells_stub(
      rows = 12
    )) %>%
  fmt_percent(everything(), decimals = 0)

gt_cheers


# Appendices --------------------------------------------------------------

appendix_data <- read_csv("data/appendix_data.csv")

gt_appendix_1 <- appendix_data %>%
  select(`Author (year)`, 
         `Country/countries`, 
         `Type of economic evaluation`,
         `Setting`,
         Population,
         Perspective,
         `Compared strategies`,
         `Time horizon`,
         `Inclusion of stochasticity`,
         `Inclusion of AMR`) %>%
  mutate(across(.fns = ~str_replace_na(.x, "Not reported")),
         across(.fns = ~str_replace(.x, "NA", "Not reported"))) %>%
  gt(rowname_col = "model_category") %>%
  tab_header(title = "Appendix III: Key characteristics of included articles") %>%
  tab_stubhead(label = " ") %>%
  tab_row_group(
    group = "Trial-based analyses",
    rows = appendix_data %>% filter(groupname == "trial-based analysis" | groupname == "regression model") %>% pull(row)
  ) %>%
  tab_row_group(
    group = "Decision trees",
    rows = appendix_data %>% filter(groupname == "decision tree model") %>% pull(row)
  ) %>%
  tab_row_group(
    group = "Markov models",
    rows = appendix_data %>% filter(groupname == "markov model") %>% pull(row)
  ) %>%
  tab_row_group(
    group = "Dynamic models",
    rows = appendix_data %>% filter(groupname == "dynamic model") %>% pull(row)
  ) %>%
  tab_row_group(
    group = "Other",
    rows = appendix_data %>% filter(groupname == "other") %>% pull(row)
  ) %>%
  row_group_order(c("Trial-based analyses", "Decision trees", "Markov models", "Dynamic models", "Other")) %>%
  tab_source_note(
    source_note = "CA: Cost Analysis; CEA: Cost-Effectiveness Analysis; CMA: Cost Minimization Analysis; CUA: Cost Utility Analysis"
  ) 

gt_appendix_1

gt_appendix_2 <- appendix_data %>%
  select(`Author (year)`, 
         ICER,
         `Currency (year)`,
         `Inclusion of stochasticity`,
         `Reporting of uncertainty`,
         `Main findings`,
         `Cost-effectiveness verdict`,
         `CHEERS score`) %>%
  mutate(`Reporting of uncertainty` = str_replace_all(`Reporting of uncertainty`,
                                                      "Deterministic sensitivity analysis\\s\\(DSA\\)",
                                                      "DSA"),
         `Reporting of uncertainty` = str_replace_all(`Reporting of uncertainty`,
                                                      "Probabilistic sensitivity analysis\\s\\(PSA\\)",
                                                      "PSA"),
         `Reporting of uncertainty` = str_replace_all(`Reporting of uncertainty`,
                                                      "Cost-effectiveness plane of PSA",
                                                      "CE plane"),
         `Reporting of uncertainty` = str_replace_all(`Reporting of uncertainty`,
                                                      "Cost-effectiveness acceptability curve\\(s\\)",
                                                      "CEAC"),
         `Reporting of uncertainty` = str_replace_all(`Reporting of uncertainty`,
                                                      "Tornado diagram of DSA",
                                                      "Tornado diagram"),
         `Reporting of uncertainty` = str_replace_all(`Reporting of uncertainty`,
                                                      "Sensitivity analysis graph \\(with one parameter varied\\)",
                                                      "One-way sensitivity analysis"),
         `Reporting of uncertainty` = str_replace_all(`Reporting of uncertainty`,
                                                      "Two-way sensitivity analysis graph",
                                                      "Two-way sensitivity analysis")) %>%
  mutate(across(.fns = ~str_replace_na(.x, "Not reported"))) %>%
  gt(rowname_col = "model_category") %>%
  tab_header(title = "Appendix IV: Key results of included articles") %>%
  tab_stubhead(label = "variables") %>%
  tab_row_group(
    group = "Trial-based analyses",
    rows = appendix_data %>% filter(groupname == "trial-based analysis" | groupname == "regression model") %>% pull(row)
  ) %>%
  tab_row_group(
    group = "Decision trees",
    rows = appendix_data %>% filter(groupname == "decision tree model") %>% pull(row)
  ) %>%
  tab_row_group(
    group = "Markov models",
    rows = appendix_data %>% filter(groupname == "markov model") %>% pull(row)
  ) %>%
  tab_row_group(
    group = "Dynamic models",
    rows = appendix_data %>% filter(groupname == "dynamic model") %>% pull(row)
  ) %>%
  tab_row_group(
    group = "Other",
    rows = appendix_data %>% filter(groupname == "other") %>% pull(row)
  ) %>%
  row_group_order(c("Trial-based analyses", "Decision trees", "Markov models", "Dynamic models", "Other")) %>%
  tab_source_note(
    source_note = "CE: Cost Effectiveness; CEAC: Cost-Effectiveness Acceptability Curve; DALY: Disability-Adjusted Life Year; DSA: Deterministic Sensitivity Analysis; PSA: Probabilistic Sensitivity Analysis; QALY: Quality-Adjusted Life Year"
  )
gt_appendix_2


# Save tables -------------------------------------------------------------

gtsave(gt_general, filename = "table1.html", path = str_c(getwd(),"/output"))
gtsave(gt_methods, filename = "table2.html", path = str_c(getwd(),"/output"))
gtsave(gt_appendix_1, filename = "appendix_table1.html", path = str_c(getwd(),"/output"))
gtsave(gt_appendix_2, filename = "appendix_table2.html", path = str_c(getwd(),"/output"))


