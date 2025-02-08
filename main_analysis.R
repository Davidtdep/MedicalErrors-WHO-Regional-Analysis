###############################################################################
# SECTION 1: LOAD LIBRARIES
# Description: Load all required libraries for data manipulation, plotting, 
# modeling, meta-analysis, and report generation.
###############################################################################
library(MASS)
library(readxl)
library(dplyr)
library(tidyr)
library(cluster)
library(pheatmap)
library(ggplot2)
library(broom)
library(car)
library(tools)
library(metafor)
library(forestplot)
library(knitr)

###############################################################################
# SECTION 2: MAIN DATA LOADING AND CLEANING
# Description: Load the main dataset, convert specified columns to numeric,
# filter out rows with missing region information, and handle missing values 
# in the 'Cuartil' column.
###############################################################################
data <- read_excel(
  "~/Desktop/MedicalErrors2/data/data.xlsx", 
  col_types = "text"
)

data <- data %>%
  mutate_at(c(1, 4, 5), as.numeric) %>%
  filter(!is.na(`Country by region`))

# Replace "-" with NA in the "Cuartil" column
data$Cuartil[data$Cuartil == "-"] <- NA

###############################################################################
# SECTION 3: READING ADDITIONAL INDICATORS
# Description: Load a list of CSV files containing health indicators from different
# sources. Each CSV file is read and stored in a list with its filename (without extension)
# as the name.
###############################################################################
csv_files <- c(
  "~/Desktop/MedicalErrors2/data/indicators/region/Adult mortality rate (probability of dying between 15 and 60 years per 1000 population).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Current health expenditure (CHE) as percentage of gross domestic product (GDP) (%).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Domestic general government health expenditure (GGHE-D) per capita in US$.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Gross domestic R&D expenditure on health (health GERD) as a % of gross domestic product (GDP).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Gross domestic R&D expenditure on health (health GERD) as a % of total GERD.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Health researchers (in full-time equivalent) per million inhabitants, by WHO Region.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Healthy life expectancy (HALE) at birth (years).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Life expectancy at birth (years).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Mortality rate among children ages 5 to 9 years (per 1000 children aged 5).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Neonatal mortality rate (0 to 27 days) per 1000 live births) (SDG 3.2.2).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/No. of grants by recipient's WHO region and income group.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Number of deaths among adolescents 10-19 years of age.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Out-of-pocket expenditure (OOP) per capita in US$.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Prevalence of diabetes.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Prevalence of hypertension among adults aged 30-79 years.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Prevalence of insufficient physical activity among adults aged 18+ years (crude estimate) (_).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Prevalence of obesity among adults, BMI _= 30 (age-standardized estimate) (_).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Total NCD Deaths (in thousands).csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Total NCD mortality rate (per 100 000 population) , age-standardized.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/UHC Service Coverage sub-index on noncommunicable diseases.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/UHC Service Coverage sub-index on service capacity an access.csv",
  "~/Desktop/MedicalErrors2/data/indicators/region/Under-five mortality rate (per 1000 live births) (SDG 3.2.1).csv"
)

indicators_list <- lapply(csv_files, function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  list(name = file_path_sans_ext(basename(path)), data = df)
})

###############################################################################
# SECTION 4: JOINING INDICATORS TO THE MAIN DATA
# Description: Define helper functions to reshape (pivot) the indicator datasets 
# and join them with the main data by matching region and publication year.
###############################################################################
# List of indicator names to join
indicators <- c(
  "Adult mortality rate (probability of dying between 15 and 60 years per 1000 population)",
  "Current health expenditure (CHE) as percentage of gross domestic product (GDP) (%)",
  "Domestic general government health expenditure (GGHE-D) per capita in US$",
  "Gross domestic R&D expenditure on health (health GERD) as a % of gross domestic product (GDP)",
  "Gross domestic R&D expenditure on health (health GERD) as a % of total GERD",
  "Health researchers (in full-time equivalent) per million inhabitants, by WHO Region",
  "Healthy life expectancy (HALE) at birth (years)",
  "Life expectancy at birth (years)",
  "Mortality rate among children ages 5 to 9 years (per 1000 children aged 5)",
  "Neonatal mortality rate (0 to 27 days) per 1000 live births) (SDG 3.2.2)",
  "No. of grants by recipient's WHO region and income group",
  "Number of deaths among adolescents 10-19 years of age",
  "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country",
  "Out-of-pocket expenditure (OOP) per capita in US$",
  "Prevalence of diabetes",
  "Prevalence of hypertension among adults aged 30-79 years",
  "Prevalence of insufficient physical activity among adults aged 18+ years (crude estimate) (_)",
  "Prevalence of obesity among adults, BMI _= 30 (age-standardized estimate) (_)",
  "Total NCD Deaths (in thousands)",
  "Total NCD mortality rate (per 100 000 population) , age-standardized",
  "UHC Service Coverage sub-index on noncommunicable diseases",
  "UHC Service Coverage sub-index on service capacity an access",
  "Under-five mortality rate (per 1000 live births) (SDG 3.2.1)"
)

# Function to pivot indicator data from wide to long format
pivot_indicator <- function(df, indicator_name) {
  df %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year_temp",
      values_to = indicator_name
    ) %>%
    mutate(año = as.numeric(sub("X", "", year_temp))) %>%
    select(-year_temp)
}

# Function to join a given indicator dataset to the main data
join_indicator <- function(data, indicator_name, indicator_df) {
  indicator_long <- pivot_indicator(indicator_df, indicator_name)
  left_join(
    data,
    indicator_long,
    by = c("Country by region" = "Location", "año de publicación" = "año")
  )
}

# Loop through each indicator file and join to main data if the indicator is in the list
for (indicator in indicators_list) {
  name_in_file <- indicator$name
  df           <- indicator$data
  
  if (name_in_file %in% indicators) {
    data <- join_indicator(data, name_in_file, df)
  }
}

###############################################################################
# SECTION 5: CREATING A SUMMARY DATASET
# Description: Generate a summarized dataset (data_summary) that aggregates 
# bibliometric metrics (citations, publications, H-index) by region and publication year.
# Also, join average indicator values.
###############################################################################
data_summary <- data %>%
  group_by(`Country by region`, `año de publicación`) %>%
  summarise(
    citations    = sum(`Número de citaciones`, na.rm = TRUE),
    publications = n(),
    Hindex       = mean(`Índice H`, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  rename(
    Region = `Country by region`,
    year   = `año de publicación`
  )

# Loop through each health indicator and join its average value by region and year
for (indicator in indicators) {
  summarized_data <- data %>%
    select(`año de publicación`, Region = `Country by region`, !!sym(indicator)) %>%
    group_by(Region, `año de publicación`) %>%
    summarise(value = mean(!!sym(indicator), na.rm = TRUE), .groups = "drop") %>%
    rename(year = `año de publicación`)
  
  data_summary <- data_summary %>%
    left_join(summarized_data, by = c("Region", "year")) %>%
    rename(!!indicator := value)
}

###############################################################################
# SECTION 6: SETTING UP EQUIVALENCES/ABBREVIATIONS AND SIGNIFICANCE FUNCTIONS
# Description: Define equivalence mappings for bibliometric indicators and helper 
# functions to assign significance asterisks based on p-values.
###############################################################################
# Create a combined list of all indicators (bibliometric + health)
all_indicators <- c("citations", "publications", "Hindex", indicators)

# Fixed abbreviations for some bibliometric indicators
specific_values <- c("CIT", "PUB", "HDX")
specific_names  <- c("citations", "publications", "Hindex")

# Create letters for the remaining indicators
letters_vec <- LETTERS[1:(length(all_indicators) - length(specific_values))]
names(specific_values) <- specific_names
names(letters_vec) <- all_indicators[!(all_indicators %in% specific_names)]
equivalences <- c(specific_values, letters_vec)

# Function to add an equivalent column based on indicator names
add_equivalent_column <- function(df, equivalences) {
  df %>% mutate(Equivalent = equivalences[Indicator])
}

# Function to assign asterisks based on p-value thresholds
assign_asterisks <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) "***"
  else if (p_value < 0.01) "**"
  else if (p_value < 0.05) "*"
  else ""
}

# Function to add a significance column to a dataframe
add_significance_column <- function(df) {
  df %>%
    mutate(Significance = sapply(P_Value, assign_asterisks))
}

###############################################################################
# SECTION 7: RUNNING MANUAL LINEAR MODELS
# Description: Define a function to run manual linear regressions in two parts:
# (1) Dependent variable ~ measure and (2) measure ~ Independent variable.
# The function returns a results dataframe with model coefficients and statistics.
###############################################################################
run_manual_lms <- function(data_summary, measure, all_indicators, region_var = "Region") {
  # Define dependent and independent indicators based on column indices
  dependant_indices <- c(4, 10:13, 15, 18:23, 26)
  dependant_indicators <- all_indicators[dependant_indices]
  
  # Independent indicators (excluding first 3 and the dependent ones)
  independant_indicators <- all_indicators[-c(1:3, dependant_indices)]
  
  results_df <- data.frame(
    Region = character(),
    DependentVariable = character(),
    IndependentVariable = character(),
    Coefficient = numeric(),
    StdError = numeric(),
    tValue = numeric(),
    PValue = numeric(),
    ResidualStdError = numeric(),
    DFResidual = numeric(),
    MultipleR2 = numeric(),
    AdjustedR2 = numeric(),
    FStatistic = numeric(),
    FStatisticPValue = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 1) Run models: (DependentIndicator ~ measure)
  for (region in unique(data_summary[[region_var]])) {
    regional_data <- data_summary[data_summary[[region_var]] == region, ]
    
    for (indicator in dependant_indicators) {
      formula_str <- paste0("`", indicator, "` ~ ", measure)
      model <- lm(as.formula(formula_str), data = regional_data)
      s <- summary(model)
      
      coef_summary <- s$coefficients
      if (measure %in% rownames(coef_summary)) {
        Coefficient <- coef_summary[measure, "Estimate"]
        StdError    <- coef_summary[measure, "Std. Error"]
        tValue      <- coef_summary[measure, "t value"]
        PValue      <- coef_summary[measure, "Pr(>|t|)"]
      } else {
        Coefficient <- StdError <- tValue <- PValue <- NA
      }
      
      if (!is.null(s$fstatistic)) {
        f_statistic    <- s$fstatistic["value"]
        f_stat_p_value <- pf(s$fstatistic["value"], s$fstatistic["numdf"], s$fstatistic["dendf"], lower.tail = FALSE)
      } else {
        f_statistic <- f_stat_p_value <- NA
      }
      
      results_df <- rbind(results_df, data.frame(
        Region              = region,
        DependentVariable   = indicator,
        IndependentVariable = measure,
        Coefficient         = Coefficient,
        StdError            = StdError,
        tValue              = tValue,
        PValue              = PValue,
        ResidualStdError    = s$sigma,
        DFResidual          = s$df[2],
        MultipleR2          = s$r.squared,
        AdjustedR2          = s$adj.r.squared,
        FStatistic          = f_statistic,
        FStatisticPValue    = f_stat_p_value,
        stringsAsFactors    = FALSE
      ))
    }
  }
  
  # 2) Run models: (measure ~ IndependentIndicator)
  for (region in unique(data_summary[[region_var]])) {
    regional_data <- data_summary[data_summary[[region_var]] == region, ]
    
    for (indicator in independant_indicators) {
      formula_str <- paste0(measure, " ~ `", indicator, "`")
      model <- lm(as.formula(formula_str), data = regional_data)
      s <- summary(model)
      
      coef_summary <- s$coefficients
      row_name <- paste0("`", indicator, "`")
      
      if (row_name %in% rownames(coef_summary)) {
        Coefficient <- coef_summary[row_name, "Estimate"]
        StdError    <- coef_summary[row_name, "Std. Error"]
        tValue      <- coef_summary[row_name, "t value"]
        PValue      <- coef_summary[row_name, "Pr(>|t|)"]
      } else {
        Coefficient <- StdError <- tValue <- PValue <- NA
      }
      
      if (!is.null(s$fstatistic)) {
        f_statistic    <- s$fstatistic["value"]
        f_stat_p_value <- pf(s$fstatistic["value"], s$fstatistic["numdf"], s$fstatistic["dendf"], lower.tail = FALSE)
      } else {
        f_statistic <- f_stat_p_value <- NA
      }
      
      results_df <- rbind(results_df, data.frame(
        Region              = region,
        DependentVariable   = measure,
        IndependentVariable = indicator,
        Coefficient         = Coefficient,
        StdError            = StdError,
        tValue              = tValue,
        PValue              = PValue,
        ResidualStdError    = s$sigma,
        DFResidual          = s$df[2],
        MultipleR2          = s$r.squared,
        AdjustedR2          = s$adj.r.squared,
        FStatistic          = f_statistic,
        FStatisticPValue    = f_stat_p_value,
        stringsAsFactors    = FALSE
      ))
    }
  }
  
  return(results_df)
}

# Run the manual linear models for each measure
results_publications <- run_manual_lms(data_summary, "publications", all_indicators)
results_citations    <- run_manual_lms(data_summary, "citations",    all_indicators)
results_hindex       <- run_manual_lms(data_summary, "Hindex",       all_indicators)

###############################################################################
# SECTION 8: CREATING HEATMAPS FOR RESULTS
# Description: Prepare data and create custom heatmaps displaying normalized 
# regression coefficients along with significance asterisks.
###############################################################################
create_heatmap_data <- function(results, metric_name) {
  # Process the results to add asterisks and normalize coefficients
  results <- results %>%
    mutate(IndependentAsterisk = ifelse(IndependentVariable != metric_name, "*", "")) %>%
    mutate(SignificanceAsterisk = case_when(
      PValue < 0.001 ~ "***",
      PValue < 0.01  ~ "**",
      PValue < 0.05  ~ "*",
      TRUE           ~ ""
    )) %>%
    group_by(DependentVariable, IndependentVariable) %>%
    mutate(CoefficientNormalized = (Coefficient - mean(Coefficient, na.rm = TRUE)) / sd(Coefficient, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      DependentLetter = letters_vec[DependentVariable],
      IndependentLetter = letters_vec[IndependentVariable],
      RowLabel = ifelse(
        DependentVariable == metric_name,
        paste0(IndependentLetter, IndependentAsterisk),
        paste0(DependentLetter, IndependentAsterisk)
      ),
      CellLabel = ifelse(
        DependentVariable == metric_name,
        paste0(IndependentLetter, SignificanceAsterisk),
        paste0(DependentLetter, SignificanceAsterisk)
      )
    )
  
  # Reshape data for heatmap (rows: indicators, columns: regions)
  heatmap_data <- results %>%
    select(Region, RowLabel, CoefficientNormalized) %>%
    spread(key = Region, value = CoefficientNormalized)
  
  heatmap_matrix <- as.matrix(heatmap_data[, -1])
  rownames(heatmap_matrix) <- heatmap_data$RowLabel
  heatmap_matrix[is.na(heatmap_matrix)] <- 0
  
  # Create significance matrix
  significance_data <- results %>%
    select(Region, RowLabel, SignificanceAsterisk) %>%
    spread(key = Region, value = SignificanceAsterisk)
  
  significance_matrix <- as.matrix(significance_data[, -1])
  rownames(significance_matrix) <- significance_data$RowLabel
  
  return(list(heatmap_matrix = heatmap_matrix, significance_matrix = significance_matrix))
}

# Function to create a custom heatmap plot using the prepared matrices
create_custom_heatmap <- function(heatmap_matrix, significance_matrix, title) {
  pheatmap(
    heatmap_matrix,
    clustering_distance_rows = "manhattan",
    clustering_distance_cols = "manhattan",
    clustering_method = "ward.D2",
    show_rownames = TRUE,
    show_colnames = TRUE,
    display_numbers = significance_matrix,
    fontsize_number = 10,
    number_color = "black",
    main = title,
    border_color = NA,
    color = colorRampPalette(c("blue", "white", "red"))(50),
    breaks = seq(-2, 2, length.out = 51)
  )
}

# Example usage of heatmap creation for each measure
data_publications <- create_heatmap_data(results_publications, "publications")
create_custom_heatmap(data_publications$heatmap_matrix, data_publications$significance_matrix, "Publications")

data_citations <- create_heatmap_data(results_citations, "citations")
create_custom_heatmap(data_citations$heatmap_matrix, data_citations$significance_matrix, "Citations")

data_hindex <- create_heatmap_data(results_hindex, "Hindex")
create_custom_heatmap(data_hindex$heatmap_matrix, data_hindex$significance_matrix, "H-index")

###############################################################################
# SECTION 9: EXTRACTION OF SIGNIFICANT RESULTS
# Description: Define a function to extract significant model results based 
# on a p-value cutoff. The results include key model statistics.
###############################################################################
extract_significant_results <- function(results_df, p_cutoff = 0.05) {
  results_df %>%
    filter(PValue < p_cutoff) %>%
    select(Region, DependentVariable, IndependentVariable, Coefficient, StdError, 
           tValue, PValue, ResidualStdError, DFResidual, MultipleR2, AdjustedR2, 
           FStatistic, FStatisticPValue)
}

significant_results_publications <- extract_significant_results(results_publications)
significant_results_citations    <- extract_significant_results(results_citations)
significant_results_hindex       <- extract_significant_results(results_hindex)

# Optionally, add row numbers to the significant results tables
significant_results_publications$RowNumber <- seq_len(nrow(significant_results_publications))
significant_results_citations$RowNumber    <- seq_len(nrow(significant_results_citations))
significant_results_hindex$RowNumber       <- seq_len(nrow(significant_results_hindex))

###############################################################################
# SECTION 10: FOREST PLOTS (META-ANALYSIS)
# Description: Define a generic function to perform meta-analysis on the 
# regression results and generate forest plots (both normalized and original).
###############################################################################
perform_meta_analysis_and_forest_plot <- function(
    results_df,
    measure_label,
    indicator_list,
    dep_or_indep = c("DependentVariable","IndependentVariable"),
    normalized_title = "Forest Plot (Normalized)",
    unnormalized_title = "Forest Plot (Unnormalized)",
    min_studies = 2
) {
  dep_or_indep <- match.arg(dep_or_indep)
  
  # Dataframe to store meta-analysis results
  meta_results <- data.frame(
    Indicator   = character(),
    Coefficient = numeric(),
    CI_Lower    = numeric(),
    CI_Upper    = numeric(),
    Weight      = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each indicator in the provided list
  for (indicator in indicator_list) {
    if (dep_or_indep == "DependentVariable") {
      subset_data <- results_df[results_df$DependentVariable == indicator, ]
    } else {
      subset_data <- results_df[results_df$IndependentVariable == indicator, ]
    }
    
    # Remove rows with NA coefficients or standard errors
    subset_data <- subset_data[!is.na(subset_data$Coefficient) & !is.na(subset_data$StdError), ]
    
    if (nrow(subset_data) >= min_studies) {
      res <- rma(yi = Coefficient, sei = StdError, data = subset_data, method = "REML")
      weight <- sum(1 / res$vi)
      
      meta_results <- rbind(meta_results, data.frame(
        Indicator   = indicator,
        Coefficient = res$b[1],
        CI_Lower    = res$ci.lb,
        CI_Upper    = res$ci.ub,
        Weight      = weight,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(meta_results) == 0) {
    message("Meta-analysis could not be performed for ", measure_label)
    return(NULL)
  }
  
  # Normalize coefficients (Z-score)
  meta_results_norm <- meta_results %>%
    mutate(
      Coefficient_Z = (Coefficient - mean(Coefficient)) / sd(Coefficient),
      CI_Lower_Z    = (CI_Lower   - mean(Coefficient)) / sd(Coefficient),
      CI_Upper_Z    = (CI_Upper   - mean(Coefficient)) / sd(Coefficient)
    )
  
  # Create a scientific formatted table for meta-analysis results
  meta_results_table <- meta_results %>%
    mutate(
      `Coefficient (95% CI)` = paste0(
        round(Coefficient, 2), 
        " (", round(CI_Lower, 2), ", ", round(CI_Upper, 2), ")"
      ),
      Weight = round(Weight, 2)
    ) %>%
    select(Indicator, `Coefficient (95% CI)`, Weight)
  
  # Generate forest plot with unnormalized coefficients
  plot_original <- ggplot(meta_results, aes(x = Coefficient, y = Indicator)) +
    geom_point(aes(size = Weight), shape = 22, fill = alpha("black", 0.7)) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 0.8) +
    scale_size_continuous(range = c(3, 8)) +
    labs(
      title = paste(unnormalized_title, "-", measure_label),
      x     = "Coefficient (95% CI)",
      y     = "Indicators",
      size  = "Precision"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_blank(),
      axis.ticks       = element_blank()
    )
  
  # Generate forest plot with normalized (Z-score) coefficients
  plot_normalized <- ggplot(meta_results_norm, aes(x = Coefficient_Z, y = Indicator)) +
    geom_point(aes(size = Weight), shape = 22, fill = alpha("black", 0.7)) +
    geom_errorbarh(aes(xmin = CI_Lower_Z, xmax = CI_Upper_Z), height = 0.2) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 0.8) +
    scale_size_continuous(range = c(3, 8)) +
    labs(
      title = paste(normalized_title, "-", measure_label),
      x     = "Coefficient Z (95% CI)",
      y     = "Indicators",
      size  = "Precision"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_blank(),
      axis.ticks       = element_blank()
    )
  
  # Return a list with meta-analysis results, tables, and plots
  return(list(
    meta_results       = meta_results,
    meta_results_table = meta_results_table,
    plot_original      = plot_original,
    plot_normalized    = plot_normalized
  ))
}

###############################################################################
# SECTION 11: EXAMPLES OF FOREST PLOTS USAGE
# Description: Run meta-analysis and forest plot functions for different sets 
# of indicators (dependent and independent) for publications, citations, and H-index.
###############################################################################
# a) Publications - Dependent Indicators
all_indicators_dependent <- c(
  "Adult mortality rate (probability of dying between 15 and 60 years per 1000 population)",
  "Mortality rate among children ages 5 to 9 years (per 1000 children aged 5)",
  "Neonatal mortality rate (0 to 27 days) per 1000 live births) (SDG 3.2.2)",
  "Number of deaths among adolescents 10-19 years of age",
  "Total NCD Deaths (in thousands)",
  "Total NCD mortality rate (per 100 000 population) , age-standardized",
  "Under-five mortality rate (per 1000 live births) (SDG 3.2.1)",
  "Prevalence of diabetes",
  "Prevalence of hypertension among adults aged 30-79 years",
  "Prevalence of insufficient physical activity among adults aged 18+ years (crude estimate) (_)",
  "Prevalence of obesity among adults, BMI _= 30 (age-standardized estimate) (_)",
  "Healthy life expectancy (HALE) at birth (years)",
  "Life expectancy at birth (years)"
)

res_pub_dep <- perform_meta_analysis_and_forest_plot(
  results_df     = results_publications,
  measure_label  = "Publications (Dep)",
  indicator_list = all_indicators_dependent,
  dep_or_indep   = "DependentVariable"
)

res_cit_dep <- perform_meta_analysis_and_forest_plot(
  results_df     = results_citations,
  measure_label  = "Citations (Dep)",
  indicator_list = all_indicators_dependent,
  dep_or_indep   = "DependentVariable"
)

res_hdx_dep <- perform_meta_analysis_and_forest_plot(
  results_df     = results_hindex,
  measure_label  = "Hindex (Dep)",
  indicator_list = all_indicators_dependent,
  dep_or_indep   = "DependentVariable"
)

# Display meta-analysis tables and forest plots for dependent indicators
res_hdx_dep$meta_results_table
res_pub_dep$plot_original
res_pub_dep$plot_normalized

# b) Publications - Independent Indicators
all_indicators_independent <- c(
  "Current health expenditure (CHE) as percentage of gross domestic product (GDP) (%)",
  "Domestic general government health expenditure (GGHE-D) per capita in US$",
  "Gross domestic R&D expenditure on health (health GERD) as a % of gross domestic product (GDP)",
  "Gross domestic R&D expenditure on health (health GERD) as a % of total GERD",
  "Health researchers (in full-time equivalent) per million inhabitants, by WHO Region",
  "No. of grants by recipient's WHO region and income group",
  "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country",
  "Out-of-pocket expenditure (OOP) per capita in US$",
  "UHC Service Coverage sub-index on noncommunicable diseases",
  "UHC Service Coverage sub-index on service capacity an access"
)

res_pub_ind <- perform_meta_analysis_and_forest_plot(
  results_df     = results_publications,
  measure_label  = "Publications (Ind)",
  indicator_list = all_indicators_independent,
  dep_or_indep   = "IndependentVariable"
)

res_cit_ind <- perform_meta_analysis_and_forest_plot(
  results_df     = results_citations,
  measure_label  = "Citations (Ind)",
  indicator_list = all_indicators_independent,
  dep_or_indep   = "IndependentVariable"
)

res_hdx_ind <- perform_meta_analysis_and_forest_plot(
  results_df     = results_hindex,
  measure_label  = "Hindex (Ind)",
  indicator_list = all_indicators_independent,
  dep_or_indep   = "IndependentVariable"
)

# Display meta-analysis tables and forest plots for independent indicators
res_hdx_ind$meta_results_table
res_pub_ind$plot_original
res_pub_ind$plot_normalized

###############################################################################
# SECTION 12: EXPORT TABLES (OPTIONAL)
# Description: Export the meta-analysis results tables to CSV files. Uncomment the 
# write.csv lines if you wish to export the results.
###############################################################################
# write.csv(res_pub_dep$meta_results_table, "res_pub_dep.csv", row.names = FALSE)
# write.csv(res_cit_dep$meta_results_table, "res_cit_dep.csv", row.names = FALSE)
# write.csv(res_hdx_dep$meta_results_table, "res_hdx_dep.csv", row.names = FALSE)
# write.csv(res_pub_ind$meta_results_table, "res_pub_ind.csv", row.names = FALSE)
# write.csv(res_cit_ind$meta_results_table, "res_cit_ind.csv", row.names = FALSE)
# write.csv(res_hdx_ind$meta_results_table, "res_hdx_ind.csv", row.names = FALSE)
