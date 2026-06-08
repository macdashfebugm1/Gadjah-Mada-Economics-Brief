# ============================================================
# HH Consumption and GFCF External Consistency Checks - No Electricity Robustness
# using indicators.xlsx and indicators2_no_electricity.xlsx
#
# Purpose:
# Robustness version: all electricity indicators are excluded.
# 1. Build a Household Consumption Activity Index (HHAI).
# 2. Build a GFCF / Investment Activity Index (IAI).
# 3. Compare both indices with official seasonally adjusted GDP components:
#       - household consumption: c
#       - gross fixed capital formation: i
# 4. Assess whether the post-2025Q2 increases in household consumption
#    and GFCF are mirrored by independent activity indicators.
#
# Input files:
#   gdp_all.xlsx
#   indicators.xlsx
#   indicators2_no_electricity.xlsx
#
# Main output folders:
#   outputs_hh_gfcf_reai_no_electricity/
#   tables_hh_gfcf_reai_no_electricity/
#   figures_hh_gfcf_reai_no_electricity/
#
# Main interpretation:
#   This is an external consistency check. It does not prove that GDP is
#   right or wrong. It asks whether the reported component-level increases
#   are broadly aligned with related independent indicators.
# ============================================================


# ============================================================
# 0. Packages
# ============================================================
#setwd("C:\\Users\\User\\OneDrive\\UGM\\2026 S01\\PKM Makro")

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "lubridate",
  "zoo",
  "ggplot2",
  "stringr",
  "lmtest",
  "sandwich"
)

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(stringr)
library(lmtest)
library(sandwich)

# Explicit dplyr aliases to reduce namespace conflicts.
filter <- dplyr::filter
select <- dplyr::select
mutate <- dplyr::mutate
arrange <- dplyr::arrange
group_by <- dplyr::group_by
summarise <- dplyr::summarise
left_join <- dplyr::left_join
full_join <- dplyr::full_join
bind_rows <- dplyr::bind_rows
distinct <- dplyr::distinct
pull <- dplyr::pull


# ============================================================
# 1. User settings
# ============================================================

gdp_file <- "gdp_all.xlsx"
gdp_sheet <- "My Series"

indicator_file_1 <- "indicators.xlsx"
indicator_file_2 <- "indicators2_no_electricity.xlsx"

break_quarter <- "2025 Q2"

# Main sample for post-COVID consistency checks.
start_year_for_analysis <- 2022
start_quarter_for_analysis <- 1

analysis_sample_label <- paste0(
  start_year_for_analysis,
  "Q",
  start_quarter_for_analysis,
  " onward"
)

analysis_sample_stub <- paste0(
  start_year_for_analysis,
  "Q",
  start_quarter_for_analysis,
  "_onward"
)

event_window_quarters <- c(
  "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4",
  "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4",
  "2026 Q1", "2026 Q2", "2026 Q3", "2026 Q4"
)

# Minimum number of non-missing observations required to include a variable.
min_nonmissing_for_index <- 8

# If TRUE, show detailed variable availability in the command window.
print_variable_diagnostics <- TRUE

# Robustness version:
# If TRUE, all electricity indicators are excluded from the canonical indicator set,
# transformations, PCA candidate groups, anchors, best-fit search, and indicator-shift tables.
exclude_electricity_data <- TRUE

# Best-fit robustness exercise.
# The script also constructs alternative HHAI/IAI indices from the subset of
# indicators AND the individual PC that maximize the pre-2025Q2 correlation
# with official C/I y-o-y growth.
# This is a favorable benchmark for the official series, so it should be used
# as a robustness check, not as the preferred baseline specification.
run_bestfit_subset_indices <- TRUE
bestfit_min_vars <- 3
bestfit_max_vars <- 6
bestfit_min_pre_obs <- 8
bestfit_candidate_pcs <- c(1, 2, 3)

# Number-of-factors robustness exercise.
# Baseline index plots still use PC1. This block checks whether the bridge-gap
# result changes when the pre-2025Q2 bridge model uses PC1, PC1-PC2, or PC1-PC3.
run_factor_number_robustness <- TRUE
factor_robustness_n_factors <- c(1, 2, 3)


# ============================================================
# 2. Output folders
# ============================================================

dir.create("outputs_hh_gfcf_reai_no_electricity", showWarnings = FALSE)
dir.create("tables_hh_gfcf_reai_no_electricity", showWarnings = FALSE)
dir.create("figures_hh_gfcf_reai_no_electricity", showWarnings = FALSE)


# ============================================================
# 3. Helper functions
# ============================================================

parse_one_date <- function(x) {
  
  if (is.na(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  
  x_chr <- trimws(as.character(x))
  
  if (grepl("^[0-9]{1,2}/[0-9]{4}$", x_chr)) {
    return(as.Date(paste0("01/", x_chr), format = "%d/%m/%Y"))
  }
  
  parsed <- suppressWarnings(as.Date(x_chr, format = "%Y-%m-%d"))
  if (!is.na(parsed)) return(parsed)
  
  parsed <- suppressWarnings(as.Date(x_chr, format = "%d/%m/%Y"))
  if (!is.na(parsed)) return(parsed)
  
  parsed <- suppressWarnings(as.Date(x_chr, format = "%m/%d/%Y"))
  if (!is.na(parsed)) return(parsed)
  
  parsed <- suppressWarnings(as.Date(x_chr, format = "%Y/%m/%d"))
  if (!is.na(parsed)) return(parsed)
  
  return(as.Date(NA))
}

clean_colnames <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("\\.+", "_", x)
  x <- gsub("-", "_", x)
  x <- gsub("/", "_", x)
  x <- gsub(":", "", x)
  x <- gsub("\\(", "", x)
  x <- gsub("\\)", "", x)
  x <- gsub("[^a-z0-9_]", "", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x[x == ""] <- "blank"
  x
}

to_numeric_clean <- function(x) {
  as.numeric(gsub(",", "", as.character(x)))
}

safe_log_growth <- function(x, lag_n = 4) {
  x <- as.numeric(x)
  out <- 100 * (log(x) - log(dplyr::lag(x, lag_n)))
  out[is.infinite(out)] <- NA_real_
  out[!is.finite(out)] <- NA_real_
  return(out)
}

safe_qoq_log_growth <- function(x) {
  safe_log_growth(x, lag_n = 1)
}

sum_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

last_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[length(x)]
}

mean_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 4) return(NA_real_)
  cor(x[ok], y[ok])
}

safe_rolling_cor <- function(x, y, width = 8, min_obs = 5) {
  
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  out <- rep(NA_real_, length(x))
  
  for (i in seq_along(x)) {
    
    start_i <- max(1, i - width + 1)
    end_i <- i
    
    x_i <- x[start_i:end_i]
    y_i <- y[start_i:end_i]
    
    ok <- is.finite(x_i) & is.finite(y_i)
    
    if (sum(ok) >= min_obs) {
      out[i] <- cor(x_i[ok], y_i[ok])
    }
  }
  
  return(out)
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

signif_stars <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(
      p < 0.01, "***",
      ifelse(
        p < 0.05, "**",
        ifelse(p < 0.10, "*", "")
      )
    )
  )
}

safe_name <- function(x) {
  x <- gsub("[^a-zA-Z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

print_console_table <- function(x) {
  x_df <- as.data.frame(x, stringsAsFactors = FALSE)
  print(x_df, row.names = FALSE)
}

standardize_vector <- function(x) {
  x <- as.numeric(x)
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - m) / s
}

# Rescale an index to the same mean and standard deviation as an official series.
# This does NOT mean the index is a growth-rate estimate. It is only for visual
# comparison on the official growth-rate scale.
rescale_index_to_official <- function(index, official) {
  index <- as.numeric(index)
  official <- as.numeric(official)
  
  index_z <- standardize_vector(index)
  official_mean <- mean(official, na.rm = TRUE)
  official_sd <- sd(official, na.rm = TRUE)
  
  if (is.na(official_sd) || official_sd == 0) {
    return(rep(NA_real_, length(index)))
  }
  
  official_mean + index_z * official_sd
}

# Finds the first column whose cleaned name contains all patterns.
find_col_contains_all <- function(names_vec, patterns) {
  candidates <- names_vec
  
  for (p in patterns) {
    candidates <- candidates[grepl(p, candidates)]
  }
  
  if (length(candidates) == 0) return(NA_character_)
  candidates[1]
}

rename_if_found <- function(df, canonical_name, patterns_list) {
  
  if (canonical_name %in% names(df)) return(df)
  
  for (patterns in patterns_list) {
    found <- find_col_contains_all(names(df), patterns)
    if (!is.na(found)) {
      names(df)[names(df) == found] <- canonical_name
      return(df)
    }
  }
  
  return(df)
}


# ============================================================
# 4. Read GDP component data
# ============================================================

if (!file.exists(gdp_file)) {
  stop("gdp_all.xlsx not found in the working directory.")
}

gdp_raw <- readxl::read_excel(
  gdp_file,
  sheet = gdp_sheet,
  .name_repair = "unique"
)

names(gdp_raw) <- make.unique(clean_colnames(names(gdp_raw)), sep = "_")

required_gdp_vars <- c("gdp", "c", "i")

missing_gdp_vars <- setdiff(required_gdp_vars, names(gdp_raw))

if (length(missing_gdp_vars) > 0) {
  stop(
    paste(
      "Missing required GDP/component columns:",
      paste(missing_gdp_vars, collapse = ", ")
    )
  )
}

gdp_components <- gdp_raw %>%
  mutate(
    date_parsed = as.Date(
      sapply(date, parse_one_date),
      origin = "1970-01-01"
    )
  ) %>%
  filter(!is.na(date_parsed)) %>%
  arrange(date_parsed) %>%
  mutate(
    across(
      all_of(required_gdp_vars),
      to_numeric_clean
    ),
    year_num = lubridate::year(date_parsed),
    month_num = lubridate::month(date_parsed),
    quarter_num = case_when(
      month_num == 3  ~ 1,
      month_num == 6  ~ 2,
      month_num == 9  ~ 3,
      month_num == 12 ~ 4,
      TRUE ~ lubridate::quarter(date_parsed)
    ),
    quarter_year = paste0(year_num, " Q", quarter_num),
    quarter_date = as.yearqtr(quarter_year, format = "%Y Q%q")
  ) %>%
  filter(!is.na(quarter_num)) %>%
  arrange(year_num, quarter_num) %>%
  mutate(
    c_qtq_value = c - dplyr::lag(c),
    i_qtq_value = i - dplyr::lag(i),
    c_yoy_growth = safe_log_growth(c, lag_n = 4),
    i_yoy_growth = safe_log_growth(i, lag_n = 4),
    c_qoq_growth = safe_qoq_log_growth(c),
    i_qoq_growth = safe_qoq_log_growth(i),
    sample_analysis = (
      year_num > start_year_for_analysis |
        (year_num == start_year_for_analysis & quarter_num >= start_quarter_for_analysis)
    ),
    post_2025Q2 = case_when(
      year_num < 2025 ~ 0,
      year_num == 2025 & quarter_num < 2 ~ 0,
      year_num == 2025 & quarter_num >= 2 ~ 1,
      year_num > 2025 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    quarter_year,
    quarter_date,
    year_num,
    quarter_num,
    c,
    i,
    c_qtq_value,
    i_qtq_value,
    c_yoy_growth,
    i_yoy_growth,
    c_qoq_growth,
    i_qoq_growth,
    sample_analysis,
    post_2025Q2
  )

write.csv(
  gdp_components,
  "outputs_hh_gfcf_reai_no_electricity/gdp_consumption_gfcf_transformations.csv",
  row.names = FALSE
)


# ============================================================
# 5. Read and clean indicator files
# ============================================================

read_indicator_file <- function(file_path, source_name) {
  
  if (!file.exists(file_path)) {
    warning(paste(file_path, "not found. Skipping."))
    return(NULL)
  }
  
  sheet_name <- readxl::excel_sheets(file_path)[1]
  
  df <- readxl::read_excel(
    file_path,
    sheet = sheet_name,
    .name_repair = "unique"
  )
  
  names(df) <- make.unique(clean_colnames(names(df)), sep = "_")
  
  if (!("date" %in% names(df))) {
    stop(paste("Column 'date' not found in", file_path))
  }
  
  df <- df %>%
    mutate(
      date_parsed = as.Date(
        sapply(date, parse_one_date),
        origin = "1970-01-01"
      )
    ) %>%
    filter(!is.na(date_parsed)) %>%
    arrange(date_parsed) %>%
    mutate(
      year_num = lubridate::year(date_parsed),
      month_num = lubridate::month(date_parsed),
      quarter_num = lubridate::quarter(date_parsed),
      quarter_year = paste0(year_num, " Q", quarter_num),
      quarter_date = as.yearqtr(quarter_year, format = "%Y Q%q"),
      source_file = source_name
    )
  
  numeric_cols <- setdiff(names(df), c(
    "date",
    "date_parsed",
    "year_num",
    "month_num",
    "quarter_num",
    "quarter_year",
    "quarter_date",
    "source_file"
  ))
  
  df <- df %>%
    mutate(
      across(
        all_of(numeric_cols),
        to_numeric_clean
      )
    )
  
  return(df)
}

ind1_raw <- read_indicator_file(indicator_file_1, "indicators.xlsx")
ind2_raw <- read_indicator_file(indicator_file_2, "indicators2_no_electricity.xlsx")

indicator_raw <- bind_rows(ind1_raw, ind2_raw)

# If both files contain the same date-column values and similar columns, duplicate
# rows may exist. We keep the rows but quarterly aggregation will average/sum/last
# by quarter and variable. Distinct columns are preserved.

raw_indicator_names <- names(indicator_raw)

write.csv(
  data.frame(raw_cleaned_column_names = raw_indicator_names),
  "outputs_hh_gfcf_reai_no_electricity/raw_indicator_cleaned_column_names.csv",
  row.names = FALSE
)


# ============================================================
# 6. Canonicalize indicator names
# ============================================================

indicator_raw <- indicator_raw %>%
  rename_if_found(
    "cci_present",
    list(c("cci", "present"), c("consumer", "confidence", "present"))
  ) %>%
  rename_if_found(
    "cci_expectations",
    list(c("cci", "expect"), c("consumer", "confidence", "expect"))
  ) %>%
  rename_if_found(
    "retail_sales_real",
    list(c("retail", "sales", "real"), c("retail", "real"))
  ) %>%
  rename_if_found(
    "ceic_leading",
    list(c("ceic", "leading"), c("leading", "index"))
  ) %>%
  rename_if_found(
    "pmi",
    list(c("pmi"), c("prompt", "mfg", "index"))
  ) %>%
  rename_if_found(
    "capacity_utilization",
    list(c("capacity", "utilization"), c("capacity", "utilisation"))
  ) %>%
  rename_if_found(
    "business_activity",
    list(c("business", "activity"))
  ) %>%
  rename_if_found(
    "investment_realization",
    list(c("investment", "realization"), c("investment", "realisation"))
  ) %>%
  rename_if_found(
    "bank_investment_loans",
    list(c("bank", "investment", "loan"), c("investment", "loans"))
  ) %>%
  rename_if_found(
    "bank_working_capital_loans",
    list(c("bank", "working", "capital"), c("working", "capital", "loans"))
  ) %>%
  rename_if_found(
    "hh_motor_vehicle_loans",
    list(c("hh", "motor", "vehicle"), c("household", "motor", "vehicle"))
  ) %>%
  rename_if_found(
    "bank_wholesale_retail_loans",
    list(c("bank", "wholesale", "retail"), c("wholesale", "retail", "loans"))
  ) %>%
  rename_if_found(
    "housing_loans_idr",
    list(
      c("housing", "loans", "idr"),
      c("housing", "loans"),
      c("property", "loans", "idr"),
      c("property", "loans")
    )
  ) %>%
  rename_if_found(
    "bls_new_loans",
    list(c("bls", "new", "loans"))
  ) %>%
  rename_if_found(
    "bls_working_capital",
    list(c("bls", "working", "capital"))
  ) %>%
  rename_if_found(
    "bls_investment_loans",
    list(c("bls", "investment", "loans"))
  ) %>%
  rename_if_found(
    "bls_consumption_loans",
    list(c("bls", "consumption", "loans"))
  ) %>%
  rename_if_found(
    "bls_housing_property",
    list(c("bls", "housing"), c("bls", "property"))
  ) %>%
  rename_if_found(
    "bls_motor_vehicle",
    list(c("bls", "motor", "vehicle"))
  ) %>%
  rename_if_found(
    "bls_credit_card",
    list(c("bls", "credit", "card"))
  ) %>%
  rename_if_found(
    "bls_multipurpose",
    list(c("bls", "multipurpose"))
  ) %>%
  rename_if_found(
    "cement_consumption",
    list(c("cement", "consumption"), c("cement", "indonesia"))
  ) %>%
  rename_if_found(
    "capital_goods_imports",
    list(c("capital", "goods", "import"))
  ) %>%
  rename_if_found(
    "motor_vehicle_sales",
    list(c("motor", "vehicle", "sales"))
  ) %>%
  rename_if_found(
    "ecommerce",
    list(
      c("e_commerce", "transactions", "value"),
      c("e_commerce", "value"),
      c("ecommerce", "value"),
      c("e_commerce"),
      c("ecommerce")
    )
  )

canonical_indicator_names <- c(
  "cci_present",
  "cci_expectations",
  "retail_sales_real",
  "ceic_leading",
  "pmi",
  "capacity_utilization",
  "business_activity",
  "investment_realization",
  "bank_investment_loans",
  "bank_working_capital_loans",
  "hh_motor_vehicle_loans",
  "bank_wholesale_retail_loans",
  "housing_loans_idr",
  "bls_new_loans",
  "bls_working_capital",
  "bls_investment_loans",
  "bls_consumption_loans",
  "bls_housing_property",
  "bls_motor_vehicle",
  "bls_credit_card",
  "bls_multipurpose",
  "cement_consumption",
  "capital_goods_imports",
  "motor_vehicle_sales",
  "ecommerce"
)

available_canonical <- intersect(canonical_indicator_names, names(indicator_raw))

# Safeguard for this no-electricity robustness version.
# If any electricity columns are still present because of unusual column names,
# drop them before quarterly aggregation and PCA construction.
if (isTRUE(exclude_electricity_data)) {
  electricity_vars_to_drop <- names(indicator_raw)[
    grepl("electricity|listrik|pln", names(indicator_raw), ignore.case = TRUE)
  ]
  if (length(electricity_vars_to_drop) > 0) {
    cat("\nDropping electricity-related variables for no-electricity robustness run:\n")
    print(electricity_vars_to_drop)
    indicator_raw <- indicator_raw %>% select(-all_of(electricity_vars_to_drop))
  }
  canonical_indicator_names <- setdiff(canonical_indicator_names, electricity_vars_to_drop)
}



if ("housing_loans_idr" %in% names(indicator_raw)) {
  cat("\nDetected housing loan variable as housing_loans_idr.\n")
  cat("Source interpretation: Housing Loans: IDR bn: Indonesia.\n")
}

if (print_variable_diagnostics) {
  cat("\n============================================================\n")
  cat("Available canonical indicators\n")
  cat("============================================================\n")
  print(available_canonical)
  
  missing_canonical <- setdiff(canonical_indicator_names, names(indicator_raw))
  cat("\nMissing canonical indicators\n")
  cat("============================================================\n")
  print(missing_canonical)
}


# ============================================================
# 7. Quarterly aggregation of indicators
# ============================================================

# Classification of raw indicators by aggregation rule.
# - mean: survey indices, diffusion indices, balances, utilization rates.
# - last: credit/outstanding stock variables.

mean_vars <- intersect(c(
  "cci_present",
  "cci_expectations",
  "ceic_leading",
  "pmi",
  "capacity_utilization",
  "business_activity",
  "bls_new_loans",
  "bls_working_capital",
  "bls_investment_loans",
  "bls_consumption_loans",
  "bls_housing_property",
  "bls_motor_vehicle",
  "bls_credit_card",
  "bls_multipurpose"
), names(indicator_raw))

sum_vars <- intersect(c(
  "retail_sales_real",
  "investment_realization",
  "cement_consumption",
  "capital_goods_imports",
  "motor_vehicle_sales",
  "ecommerce"
), names(indicator_raw))

last_vars <- intersect(c(
  "bank_investment_loans",
  "bank_working_capital_loans",
  "hh_motor_vehicle_loans",
  "bank_wholesale_retail_loans",
  "housing_loans_idr"
), names(indicator_raw))

indicator_quarterly_mean <- NULL
indicator_quarterly_sum <- NULL
indicator_quarterly_last <- NULL

if (length(mean_vars) > 0) {
  indicator_quarterly_mean <- indicator_raw %>%
    group_by(quarter_year, quarter_date, year_num, quarter_num) %>%
    summarise(
      across(all_of(mean_vars), mean_or_na),
      .groups = "drop"
    )
}

if (length(sum_vars) > 0) {
  indicator_quarterly_sum <- indicator_raw %>%
    group_by(quarter_year, quarter_date, year_num, quarter_num) %>%
    summarise(
      across(all_of(sum_vars), sum_or_na),
      .groups = "drop"
    )
}

if (length(last_vars) > 0) {
  indicator_quarterly_last <- indicator_raw %>%
    arrange(date_parsed) %>%
    group_by(quarter_year, quarter_date, year_num, quarter_num) %>%
    summarise(
      across(all_of(last_vars), last_or_na),
      .groups = "drop"
    )
}

base_quarters <- indicator_raw %>%
  distinct(quarter_year, quarter_date, year_num, quarter_num) %>%
  arrange(year_num, quarter_num)

indicator_quarterly <- base_quarters

if (!is.null(indicator_quarterly_mean)) {
  indicator_quarterly <- indicator_quarterly %>%
    left_join(indicator_quarterly_mean, by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))
}

if (!is.null(indicator_quarterly_sum)) {
  indicator_quarterly <- indicator_quarterly %>%
    left_join(indicator_quarterly_sum, by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))
}

if (!is.null(indicator_quarterly_last)) {
  indicator_quarterly <- indicator_quarterly %>%
    left_join(indicator_quarterly_last, by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))
}

indicator_quarterly <- indicator_quarterly %>%
  arrange(year_num, quarter_num)

write.csv(
  indicator_quarterly,
  "outputs_hh_gfcf_reai_no_electricity/indicator_quarterly_raw_aggregated.csv",
  row.names = FALSE
)


# ============================================================
# 8. Indicator transformations
# ============================================================

indicator_transformed <- indicator_quarterly %>%
  arrange(year_num, quarter_num)

# Variables transformed into y-o-y log growth.
# Housing loans are in IDR bn in indicators.xlsx and are treated as an IDR credit-stock variable.
yoy_transform_vars <- intersect(c(
  "retail_sales_real",
  "investment_realization",
  "cement_consumption",
  "capital_goods_imports",
  "motor_vehicle_sales",
  "bank_investment_loans",
  "bank_working_capital_loans",
  "hh_motor_vehicle_loans",
  "bank_wholesale_retail_loans",
  "housing_loans_idr",
  "ecommerce"
), names(indicator_transformed))

for (v in yoy_transform_vars) {
  indicator_transformed[[paste0("g_", v, "_yoy")]] <- safe_log_growth(
    indicator_transformed[[v]],
    lag_n = 4
  )
  
  indicator_transformed[[paste0("g_", v, "_qoq")]] <- safe_log_growth(
    indicator_transformed[[v]],
    lag_n = 1
  )
}

write.csv(
  indicator_transformed,
  "outputs_hh_gfcf_reai_no_electricity/indicator_quarterly_transformed.csv",
  row.names = FALSE
)


# ============================================================
# 9. Define HH consumption and GFCF indicator groups
# ============================================================

# Household consumption indicators:
# - confidence / expectations
# - retail activity
# - household consumer-credit demand
# - household credit-related BLS variables
# - motor vehicle-related indicators if available

hh_candidate_vars <- c(
  "cci_present",
  "cci_expectations",
  "g_retail_sales_real_yoy",
  "g_ecommerce_yoy",
  "g_hh_motor_vehicle_loans_yoy",
  "g_housing_loans_idr_yoy",
  "g_motor_vehicle_sales_yoy",
  "bls_consumption_loans",
  "bls_housing_property",
  "bls_motor_vehicle",
  "bls_credit_card",
  "bls_multipurpose",
  "ecommerce"
)

# GFCF / investment indicators:
# - investment realization
# - capacity utilization / business conditions
# - PMI / industrial conditions
# - cement consumption
# - investment and working-capital loans
# - capital-goods imports if available

gfcf_candidate_vars <- c(
  "g_investment_realization_yoy",
  "capacity_utilization",
  "business_activity",
  "pmi",
  "g_cement_consumption_yoy",
  "g_bank_investment_loans_yoy",
  "g_bank_working_capital_loans_yoy",
  "g_capital_goods_imports_yoy",
  "bls_investment_loans",
  "bls_working_capital"
)

hh_available_vars <- intersect(hh_candidate_vars, names(indicator_transformed))
gfcf_available_vars <- intersect(gfcf_candidate_vars, names(indicator_transformed))

# Keep only variables with enough observations in the analysis sample.
indicator_analysis_availability <- indicator_transformed %>%
  mutate(
    sample_analysis = (
      year_num > start_year_for_analysis |
        (year_num == start_year_for_analysis & quarter_num >= start_quarter_for_analysis)
    )
  ) %>%
  filter(sample_analysis)

keep_vars_by_nonmissing <- function(df, vars, min_nonmissing = 8) {
  vars <- intersect(vars, names(df))
  vars[sapply(vars, function(v) sum(!is.na(df[[v]])) >= min_nonmissing)]
}

hh_index_vars <- keep_vars_by_nonmissing(
  indicator_analysis_availability,
  hh_available_vars,
  min_nonmissing = min_nonmissing_for_index
)

gfcf_index_vars <- keep_vars_by_nonmissing(
  indicator_analysis_availability,
  gfcf_available_vars,
  min_nonmissing = min_nonmissing_for_index
)

if (print_variable_diagnostics) {
  cat("\n============================================================\n")
  cat("HH candidate variables available for HHAI\n")
  cat("============================================================\n")
  print(hh_available_vars)
  
  cat("\nHH variables used after non-missing filter\n")
  cat("============================================================\n")
  print(hh_index_vars)
  
  cat("\nGFCF candidate variables available for IAI\n")
  cat("============================================================\n")
  print(gfcf_available_vars)
  
  cat("\nGFCF variables used after non-missing filter\n")
  cat("============================================================\n")
  print(gfcf_index_vars)
}

availability_table <- data.frame(
  variable = c(hh_available_vars, gfcf_available_vars),
  index_group = c(
    rep("Household consumption", length(hh_available_vars)),
    rep("GFCF / investment", length(gfcf_available_vars))
  ),
  nonmissing_analysis_sample = c(
    sapply(hh_available_vars, function(v) sum(!is.na(indicator_analysis_availability[[v]]))),
    sapply(gfcf_available_vars, function(v) sum(!is.na(indicator_analysis_availability[[v]])))
  ),
  used_in_index = c(
    hh_available_vars %in% hh_index_vars,
    gfcf_available_vars %in% gfcf_index_vars
  )
)

write.csv(
  availability_table,
  "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_indicator_availability.csv",
  row.names = FALSE
)


# ============================================================
# 10. PCA index construction function
# ============================================================

construct_pca_index <- function(df, vars, index_name, anchor_vars = NULL) {
  
  vars <- unique(as.character(vars))
  vars <- intersect(vars, names(df))
  
  if (length(vars) < 2) {
    stop(paste("Too few variables for", index_name, ":", paste(vars, collapse = ", ")))
  }
  
  index_data <- df %>%
    select(quarter_year, quarter_date, year_num, quarter_num, all_of(vars)) %>%
    arrange(year_num, quarter_num)
  
  index_data_imp <- index_data %>%
    mutate(across(all_of(vars), ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
    mutate(across(all_of(vars), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
    mutate(across(all_of(vars), ~ zoo::na.locf(.x, fromLast = TRUE, na.rm = FALSE)))
  
  usable_vars <- vars[
    sapply(vars, function(v) {
      x <- index_data_imp[[v]]
      sum(!is.na(x)) >= min_nonmissing_for_index &&
        is.finite(sd(x, na.rm = TRUE)) &&
        sd(x, na.rm = TRUE) > 0
    })
  ]
  
  if (length(usable_vars) < 2) {
    stop(paste("Too few usable variables for", index_name))
  }
  
  X <- index_data_imp[, usable_vars, drop = FALSE]
  X_scaled <- scale(X)
  pca <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
  
  max_score_cols <- min(
    ncol(pca$x),
    max(factor_robustness_n_factors, na.rm = TRUE)
  )
  
  scores <- as.data.frame(pca$x[, seq_len(max_score_cols), drop = FALSE])
  names(scores) <- paste0(index_name, "_PC", seq_len(max_score_cols))
  
  for (j in seq_len(max_score_cols)) {
    scores[[j]] <- standardize_vector(scores[[j]])
  }
  
  pc1_name <- paste0(index_name, "_PC1")
  pc1 <- scores[[pc1_name]]
  
  # Orient PC1 so that it is positively correlated with anchors.
  if (!is.null(anchor_vars)) {
    anchor_vars <- intersect(anchor_vars, usable_vars)
    if (length(anchor_vars) > 0) {
      anchor_z <- rowMeans(
        as.data.frame(scale(index_data_imp[, anchor_vars, drop = FALSE])),
        na.rm = TRUE
      )
      if (safe_cor(pc1, anchor_z) < 0) {
        pc1 <- -pc1
        scores[[pc1_name]] <- pc1
        pca$rotation[, 1] <- -pca$rotation[, 1]
      }
    }
  }
  
  index_out <- index_data %>%
    select(quarter_year, quarter_date, year_num, quarter_num) %>%
    mutate(index = pc1)
  
  names(index_out)[names(index_out) == "index"] <- index_name
  
  factor_scores_out <- index_data %>%
    select(quarter_year, quarter_date, year_num, quarter_num) %>%
    bind_cols(scores)
  
  loadings_out <- data.frame(
    variable = rownames(pca$rotation),
    loading_pc1 = as.numeric(pca$rotation[, 1]),
    index_name = index_name,
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(abs(loading_pc1)))
  
  loadings_wide <- as.data.frame(pca$rotation[, seq_len(max_score_cols), drop = FALSE])
  names(loadings_wide) <- paste0("loading_pc", seq_len(max_score_cols))
  loadings_wide$variable <- rownames(pca$rotation)
  loadings_wide$index_name <- index_name
  loadings_wide <- loadings_wide %>%
    select(index_name, variable, everything())
  
  explained_out <- data.frame(
    index_name = index_name,
    component = paste0("PC", seq_along(pca$sdev)),
    variance_share = pca$sdev^2 / sum(pca$sdev^2),
    cumulative_variance_share = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  )
  
  return(list(
    index = index_out,
    factor_scores = factor_scores_out,
    loadings = loadings_out,
    loadings_wide = loadings_wide,
    explained = explained_out,
    usable_vars = usable_vars,
    pca = pca
  ))
}



# ============================================================
# 10A. Best-fit subset-and-PC selection function
# ============================================================

select_bestfit_pca_index <- function(
    indicator_df,
    gdp_df,
    candidate_vars,
    target_var,
    index_name,
    required_real_activity_vars = NULL,
    min_vars = 3,
    max_vars = 6,
    min_pre_obs = 8,
    candidate_pcs = c(1, 2, 3)
) {
  
  candidate_vars <- unique(as.character(candidate_vars))
  candidate_vars <- intersect(candidate_vars, names(indicator_df))
  candidate_pcs <- sort(unique(as.integer(candidate_pcs)))
  candidate_pcs <- candidate_pcs[is.finite(candidate_pcs) & candidate_pcs >= 1]
  
  if (length(candidate_vars) < min_vars) {
    stop(paste("Too few candidate variables for best-fit", index_name))
  }
  
  if (length(candidate_pcs) == 0) {
    stop("candidate_pcs must contain at least one positive integer.")
  }
  
  max_vars <- min(max_vars, length(candidate_vars))
  
  all_combos <- list()
  combo_counter <- 1
  
  for (k in seq(min_vars, max_vars)) {
    
    combo_k <- combn(candidate_vars, k, simplify = FALSE)
    
    if (!is.null(required_real_activity_vars) && length(required_real_activity_vars) > 0) {
      combo_k <- combo_k[
        sapply(combo_k, function(z) any(z %in% required_real_activity_vars))
      ]
    }
    
    for (z in combo_k) {
      all_combos[[combo_counter]] <- z
      combo_counter <- combo_counter + 1
    }
  }
  
  if (length(all_combos) == 0) {
    stop(paste("No valid best-fit combinations for", index_name))
  }
  
  evaluation_rows <- list()
  eval_counter <- 1
  
  cat("\n============================================================\n")
  cat("Selecting best-fit indicator subset and PC for", index_name, "\n")
  cat("Target variable:", target_var, "\n")
  cat("Number of candidate variables:", length(candidate_vars), "\n")
  cat("Candidate PCs:", paste(candidate_pcs, collapse = ", "), "\n")
  cat("Number of subsets evaluated:", length(all_combos), "\n")
  cat("============================================================\n")
  
  for (combo in all_combos) {
    
    tmp_name <- paste0(index_name, "_tmp")
    
    tmp_result <- tryCatch(
      construct_pca_index(
        df = indicator_df,
        vars = combo,
        index_name = tmp_name,
        anchor_vars = NULL
      ),
      error = function(e) NULL
    )
    
    if (is.null(tmp_result)) next
    
    tmp_data <- gdp_df %>%
      select(
        quarter_year,
        quarter_date,
        year_num,
        quarter_num,
        all_of(target_var),
        sample_analysis,
        post_2025Q2
      ) %>%
      left_join(
        tmp_result$factor_scores,
        by = c("quarter_year", "quarter_date", "year_num", "quarter_num")
      ) %>%
      filter(
        sample_analysis,
        post_2025Q2 == 0
      )
    
    available_pc_cols <- names(tmp_result$factor_scores)
    available_pc_nums <- as.integer(gsub(paste0("^", tmp_name, "_PC"), "", available_pc_cols))
    available_pc_nums <- available_pc_nums[is.finite(available_pc_nums)]
    pcs_to_try <- intersect(candidate_pcs, available_pc_nums)
    
    if (length(pcs_to_try) == 0) next
    
    for (pc_j in pcs_to_try) {
      
      pc_col <- paste0(tmp_name, "_PC", pc_j)
      
      obs_pre <- sum(is.finite(tmp_data[[target_var]]) & is.finite(tmp_data[[pc_col]]))
      
      if (obs_pre < min_pre_obs) next
      
      corr_pre <- safe_cor(tmp_data[[target_var]], tmp_data[[pc_col]])
      
      if (is.na(corr_pre)) next
      
      evaluation_rows[[eval_counter]] <- data.frame(
        index_name = index_name,
        target_var = target_var,
        n_vars = length(combo),
        selected_pc = paste0("PC", pc_j),
        selected_pc_number = pc_j,
        vars = paste(combo, collapse = " | "),
        corr_pre = corr_pre,
        abs_corr_pre = abs(corr_pre),
        observations_pre = obs_pre,
        stringsAsFactors = FALSE
      )
      
      eval_counter <- eval_counter + 1
    }
  }
  
  evaluation_table <- bind_rows(evaluation_rows) %>%
    arrange(desc(abs_corr_pre), n_vars, selected_pc_number)
  
  if (nrow(evaluation_table) == 0) {
    stop(paste("No evaluable best-fit subset/PC for", index_name))
  }
  
  best_row <- evaluation_table[1, ]
  best_vars <- strsplit(best_row$vars, " \\| ")[[1]]
  best_pc_number <- as.integer(best_row$selected_pc_number)
  
  best_result <- construct_pca_index(
    df = indicator_df,
    vars = best_vars,
    index_name = index_name,
    anchor_vars = NULL
  )
  
  pc_col <- paste0(index_name, "_PC", best_pc_number)
  
  if (!(pc_col %in% names(best_result$factor_scores))) {
    stop(paste("Selected PC column not found:", pc_col))
  }
  
  check_data <- gdp_df %>%
    select(
      quarter_year,
      quarter_date,
      year_num,
      quarter_num,
      all_of(target_var),
      sample_analysis,
      post_2025Q2
    ) %>%
    left_join(
      best_result$factor_scores,
      by = c("quarter_year", "quarter_date", "year_num", "quarter_num")
    ) %>%
    filter(
      sample_analysis,
      post_2025Q2 == 0
    )
  
  corr_check <- safe_cor(check_data[[target_var]], check_data[[pc_col]])
  
  # Orient the selected PC so that its pre-2025Q2 correlation with the target
  # variable is positive. Other PCs are left unchanged.
  if (!is.na(corr_check) && corr_check < 0) {
    
    best_result$factor_scores[[pc_col]] <- -best_result$factor_scores[[pc_col]]
    
    if (paste0("loading_pc", best_pc_number) %in% names(best_result$loadings_wide)) {
      best_result$loadings_wide[[paste0("loading_pc", best_pc_number)]] <-
        -best_result$loadings_wide[[paste0("loading_pc", best_pc_number)]]
    }
    
    if (best_pc_number == 1) {
      best_result$loadings$loading_pc1 <- -best_result$loadings$loading_pc1
      best_result$pca$rotation[, 1] <- -best_result$pca$rotation[, 1]
    }
  }
  
  # The best-fit index itself is the selected PC, not necessarily PC1.
  best_index <- best_result$factor_scores %>%
    select(quarter_year, quarter_date, year_num, quarter_num, all_of(pc_col)) %>%
    rename(index = all_of(pc_col))
  
  names(best_index)[names(best_index) == "index"] <- index_name
  
  best_result$index <- best_index
  
  cat("\nBest-fit subset and PC for", index_name, "\n")
  cat("Target:", target_var, "\n")
  cat("Selected PC:", paste0("PC", best_pc_number), "\n")
  cat("Variables:", paste(best_vars, collapse = ", "), "\n")
  cat("Absolute pre-2025Q2 correlation:", round(best_row$abs_corr_pre, 3), "\n")
  cat("Signed pre-2025Q2 correlation:", round(best_row$corr_pre, 3), "\n")
  cat("============================================================\n")
  
  list(
    index = best_result$index,
    factor_scores = best_result$factor_scores,
    loadings = best_result$loadings,
    loadings_wide = best_result$loadings_wide,
    explained = best_result$explained,
    usable_vars = best_result$usable_vars,
    pca = best_result$pca,
    selected_vars = best_vars,
    selected_pc = paste0("PC", best_pc_number),
    selected_pc_number = best_pc_number,
    evaluation_table = evaluation_table,
    best_row = best_row
  )
}


# ============================================================
# 11. Construct baseline and best-fit HHAI/IAI
# ============================================================

hh_anchor_vars <- intersect(c(
  "cci_present",
  "cci_expectations",
  "g_retail_sales_real_yoy",
  "bls_consumption_loans",
  "ecommerce"
), hh_index_vars)

gfcf_anchor_vars <- intersect(c(
  "g_investment_realization_yoy",
  "capacity_utilization",
  "business_activity",
  "g_cement_consumption_yoy",
  "g_bank_investment_loans_yoy"
), gfcf_index_vars)

# Baseline economically motivated indices.
hh_pca <- construct_pca_index(indicator_transformed, hh_index_vars, "HHAI", hh_anchor_vars)
gfcf_pca <- construct_pca_index(indicator_transformed, gfcf_index_vars, "IAI", gfcf_anchor_vars)

# Best-fit robustness indices.
hh_real_activity_vars <- intersect(c(
  "g_retail_sales_real_yoy",
  "g_motor_vehicle_sales_yoy"
), hh_index_vars)

gfcf_real_activity_vars <- intersect(c(
  "g_investment_realization_yoy",
  "g_cement_consumption_yoy",
  "g_capital_goods_imports_yoy"
), gfcf_index_vars)

if (run_bestfit_subset_indices) {
  hh_bestfit <- select_bestfit_pca_index(
    indicator_df = indicator_transformed,
    gdp_df = gdp_components,
    candidate_vars = hh_index_vars,
    target_var = "c_yoy_growth",
    index_name = "HHAI_bestfit",
    required_real_activity_vars = hh_real_activity_vars,
    min_vars = bestfit_min_vars,
    max_vars = bestfit_max_vars,
    min_pre_obs = bestfit_min_pre_obs,
    candidate_pcs = bestfit_candidate_pcs
  )
  gfcf_bestfit <- select_bestfit_pca_index(
    indicator_df = indicator_transformed,
    gdp_df = gdp_components,
    candidate_vars = gfcf_index_vars,
    target_var = "i_yoy_growth",
    index_name = "IAI_bestfit",
    required_real_activity_vars = gfcf_real_activity_vars,
    min_vars = bestfit_min_vars,
    max_vars = bestfit_max_vars,
    min_pre_obs = bestfit_min_pre_obs,
    candidate_pcs = bestfit_candidate_pcs
  )
} else {
  hh_bestfit <- NULL
  gfcf_bestfit <- NULL
}

index_combined <- hh_pca$index %>%
  full_join(gfcf_pca$index,
            by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))

if (!is.null(hh_bestfit)) {
  index_combined <- index_combined %>%
    full_join(hh_bestfit$index,
              by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))
}
if (!is.null(gfcf_bestfit)) {
  index_combined <- index_combined %>%
    full_join(gfcf_bestfit$index,
              by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))
}
index_combined <- index_combined %>% arrange(year_num, quarter_num)

factor_scores_combined <- hh_pca$factor_scores %>%
  full_join(gfcf_pca$factor_scores,
            by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))

if (!is.null(hh_bestfit)) {
  factor_scores_combined <- factor_scores_combined %>%
    full_join(hh_bestfit$factor_scores,
              by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))
}

if (!is.null(gfcf_bestfit)) {
  factor_scores_combined <- factor_scores_combined %>%
    full_join(gfcf_bestfit$factor_scores,
              by = c("quarter_year", "quarter_date", "year_num", "quarter_num"))
}

factor_scores_combined <- factor_scores_combined %>% arrange(year_num, quarter_num)

index_loadings <- bind_rows(
  hh_pca$loadings,
  gfcf_pca$loadings,
  if (!is.null(hh_bestfit)) hh_bestfit$loadings else NULL,
  if (!is.null(gfcf_bestfit)) gfcf_bestfit$loadings else NULL
)

index_explained <- bind_rows(
  hh_pca$explained,
  gfcf_pca$explained,
  if (!is.null(hh_bestfit)) hh_bestfit$explained else NULL,
  if (!is.null(gfcf_bestfit)) gfcf_bestfit$explained else NULL
)

write.csv(index_combined, "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_activity_indices.csv", row.names = FALSE)
write.csv(factor_scores_combined, "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_activity_factor_scores.csv", row.names = FALSE)
write.csv(index_loadings, "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_activity_index_loadings.csv", row.names = FALSE)

index_loadings_wide <- bind_rows(
  hh_pca$loadings_wide,
  gfcf_pca$loadings_wide,
  if (!is.null(hh_bestfit)) hh_bestfit$loadings_wide else NULL,
  if (!is.null(gfcf_bestfit)) gfcf_bestfit$loadings_wide else NULL
)

write.csv(index_loadings_wide, "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_activity_index_loadings_multifactor.csv", row.names = FALSE)
write.csv(index_explained, "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_activity_index_variance_explained.csv", row.names = FALSE)

if (!is.null(hh_bestfit)) {
  write.csv(hh_bestfit$evaluation_table,
            paste0("outputs_hh_gfcf_reai_no_electricity/hhai_bestfit_subset_evaluation_", analysis_sample_stub, ".csv"),
            row.names = FALSE)
  write.csv(data.frame(
    index_name = "HHAI_bestfit",
    selected_pc = hh_bestfit$selected_pc,
    selected_variable = hh_bestfit$selected_vars
  ),
  paste0("outputs_hh_gfcf_reai_no_electricity/hhai_bestfit_selected_variables_", analysis_sample_stub, ".csv"),
  row.names = FALSE)
}
if (!is.null(gfcf_bestfit)) {
  write.csv(gfcf_bestfit$evaluation_table,
            paste0("outputs_hh_gfcf_reai_no_electricity/iai_bestfit_subset_evaluation_", analysis_sample_stub, ".csv"),
            row.names = FALSE)
  write.csv(data.frame(
    index_name = "IAI_bestfit",
    selected_pc = gfcf_bestfit$selected_pc,
    selected_variable = gfcf_bestfit$selected_vars
  ),
  paste0("outputs_hh_gfcf_reai_no_electricity/iai_bestfit_selected_variables_", analysis_sample_stub, ".csv"),
  row.names = FALSE)
}

cat("\n============================================================\n")
cat("Best-fit selected variables\n")
cat("============================================================\n")
if (!is.null(hh_bestfit)) {
  cat("HHAI_bestfit selected PC:\n")
  print(hh_bestfit$selected_pc)
  cat("HHAI_bestfit selected variables:\n")
  print(hh_bestfit$selected_vars)
}
if (!is.null(gfcf_bestfit)) {
  cat("IAI_bestfit selected PC:\n")
  print(gfcf_bestfit$selected_pc)
  cat("IAI_bestfit selected variables:\n")
  print(gfcf_bestfit$selected_vars)
}



# ============================================================
# 12. Merge indices with GDP components
# ============================================================

analysis_data <- gdp_components %>%
  left_join(
    index_combined,
    by = c("quarter_year", "quarter_date", "year_num", "quarter_num")
  ) %>%
  left_join(
    factor_scores_combined,
    by = c("quarter_year", "quarter_date", "year_num", "quarter_num")
  ) %>%
  arrange(year_num, quarter_num) %>%
  filter(sample_analysis)

write.csv(
  analysis_data,
  paste0(
    "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_reai_analysis_data_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)


# ============================================================
# 13. Correlation and pre/post consistency tables
# ============================================================

make_consistency_row <- function(df, component_name, gdp_growth_var, gdp_qtq_var, index_var) {
  
  d_pre <- df %>% filter(post_2025Q2 == 0)
  d_post <- df %>% filter(post_2025Q2 == 1)
  
  data.frame(
    component = component_name,
    official_yoy_growth_var = gdp_growth_var,
    official_qtq_value_var = gdp_qtq_var,
    activity_index = index_var,
    observations_all = sum(!is.na(df[[gdp_growth_var]]) & !is.na(df[[index_var]])),
    observations_pre = sum(!is.na(d_pre[[gdp_growth_var]]) & !is.na(d_pre[[index_var]])),
    observations_post = sum(!is.na(d_post[[gdp_growth_var]]) & !is.na(d_post[[index_var]])),
    corr_yoy_all = safe_cor(df[[gdp_growth_var]], df[[index_var]]),
    corr_yoy_pre = safe_cor(d_pre[[gdp_growth_var]], d_pre[[index_var]]),
    corr_yoy_post = safe_cor(d_post[[gdp_growth_var]], d_post[[index_var]]),
    corr_qtq_all = safe_cor(df[[gdp_qtq_var]], df[[index_var]]),
    corr_qtq_pre = safe_cor(d_pre[[gdp_qtq_var]], d_pre[[index_var]]),
    corr_qtq_post = safe_cor(d_post[[gdp_qtq_var]], d_post[[index_var]]),
    official_yoy_mean_pre = mean_or_na(d_pre[[gdp_growth_var]]),
    official_yoy_mean_post = mean_or_na(d_post[[gdp_growth_var]]),
    official_yoy_shift = mean_or_na(d_post[[gdp_growth_var]]) - mean_or_na(d_pre[[gdp_growth_var]]),
    official_qtq_mean_pre = mean_or_na(d_pre[[gdp_qtq_var]]),
    official_qtq_mean_post = mean_or_na(d_post[[gdp_qtq_var]]),
    official_qtq_shift = mean_or_na(d_post[[gdp_qtq_var]]) - mean_or_na(d_pre[[gdp_qtq_var]]),
    index_mean_pre = mean_or_na(d_pre[[index_var]]),
    index_mean_post = mean_or_na(d_post[[index_var]]),
    index_shift = mean_or_na(d_post[[index_var]]) - mean_or_na(d_pre[[index_var]]),
    stringsAsFactors = FALSE
  )
}

consistency_summary <- bind_rows(
  make_consistency_row(analysis_data, "Household consumption: baseline HHAI", "c_yoy_growth", "c_qtq_value", "HHAI"),
  if ("HHAI_bestfit" %in% names(analysis_data)) make_consistency_row(analysis_data, "Household consumption: best-fit HHAI", "c_yoy_growth", "c_qtq_value", "HHAI_bestfit") else NULL,
  make_consistency_row(analysis_data, "GFCF: baseline IAI", "i_yoy_growth", "i_qtq_value", "IAI"),
  if ("IAI_bestfit" %in% names(analysis_data)) make_consistency_row(analysis_data, "GFCF: best-fit IAI", "i_yoy_growth", "i_qtq_value", "IAI_bestfit") else NULL
)

write.csv(
  consistency_summary,
  paste0(
    "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_reai_consistency_summary_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

cat("\n============================================================\n")
cat("HH consumption and GFCF consistency summary\n")
cat("Sample:", analysis_sample_label, "\n")
cat("============================================================\n")
print_console_table(
  consistency_summary %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    )
)


# ============================================================
# 13A. Pre/post and rolling correlations
# ============================================================

rolling_correlation_width <- 8
rolling_correlation_min_obs <- 5

# Rolling correlations use an 8-quarter window by default.
# They are diagnostic because the post-2025Q2 period has few observations.

analysis_data <- analysis_data %>%
  arrange(year_num, quarter_num) %>%
  mutate(
    rolling_corr_c_yoy_HHAI = safe_rolling_cor(
      c_yoy_growth,
      HHAI,
      width = rolling_correlation_width,
      min_obs = rolling_correlation_min_obs
    ),
    rolling_corr_c_qtq_HHAI = safe_rolling_cor(
      c_qtq_value,
      HHAI,
      width = rolling_correlation_width,
      min_obs = rolling_correlation_min_obs
    ),
    rolling_corr_i_yoy_IAI = safe_rolling_cor(
      i_yoy_growth,
      IAI,
      width = rolling_correlation_width,
      min_obs = rolling_correlation_min_obs
    ),
    rolling_corr_i_qtq_IAI = safe_rolling_cor(
      i_qtq_value,
      IAI,
      width = rolling_correlation_width,
      min_obs = rolling_correlation_min_obs
    )
  )

if ("HHAI_bestfit" %in% names(analysis_data)) {
  analysis_data <- analysis_data %>%
    mutate(
      rolling_corr_c_yoy_HHAI_bestfit = safe_rolling_cor(c_yoy_growth, HHAI_bestfit, width = rolling_correlation_width, min_obs = rolling_correlation_min_obs)
    )
} else {
  analysis_data$rolling_corr_c_yoy_HHAI_bestfit <- NA_real_
}

if ("IAI_bestfit" %in% names(analysis_data)) {
  analysis_data <- analysis_data %>%
    mutate(
      rolling_corr_i_yoy_IAI_bestfit = safe_rolling_cor(i_yoy_growth, IAI_bestfit, width = rolling_correlation_width, min_obs = rolling_correlation_min_obs)
    )
} else {
  analysis_data$rolling_corr_i_yoy_IAI_bestfit <- NA_real_
}

make_correlation_rows <- function(df, component_name, official_yoy_var, official_qtq_var, index_var) {
  bind_rows(
    data.frame(component = component_name, official_series = official_yoy_var, activity_index = index_var, period = "Full sample",
               observations = sum(is.finite(df[[official_yoy_var]]) & is.finite(df[[index_var]])),
               correlation = safe_cor(df[[official_yoy_var]], df[[index_var]]), stringsAsFactors = FALSE),
    data.frame(component = component_name, official_series = official_yoy_var, activity_index = index_var, period = "Pre-2025Q2",
               observations = sum(is.finite(df[[official_yoy_var]][df$post_2025Q2 == 0]) & is.finite(df[[index_var]][df$post_2025Q2 == 0])),
               correlation = safe_cor(df[[official_yoy_var]][df$post_2025Q2 == 0], df[[index_var]][df$post_2025Q2 == 0]), stringsAsFactors = FALSE),
    data.frame(component = component_name, official_series = official_yoy_var, activity_index = index_var, period = "Post-2025Q2",
               observations = sum(is.finite(df[[official_yoy_var]][df$post_2025Q2 == 1]) & is.finite(df[[index_var]][df$post_2025Q2 == 1])),
               correlation = safe_cor(df[[official_yoy_var]][df$post_2025Q2 == 1], df[[index_var]][df$post_2025Q2 == 1]), stringsAsFactors = FALSE),
    data.frame(component = component_name, official_series = official_qtq_var, activity_index = index_var, period = "Full sample",
               observations = sum(is.finite(df[[official_qtq_var]]) & is.finite(df[[index_var]])),
               correlation = safe_cor(df[[official_qtq_var]], df[[index_var]]), stringsAsFactors = FALSE),
    data.frame(component = component_name, official_series = official_qtq_var, activity_index = index_var, period = "Pre-2025Q2",
               observations = sum(is.finite(df[[official_qtq_var]][df$post_2025Q2 == 0]) & is.finite(df[[index_var]][df$post_2025Q2 == 0])),
               correlation = safe_cor(df[[official_qtq_var]][df$post_2025Q2 == 0], df[[index_var]][df$post_2025Q2 == 0]), stringsAsFactors = FALSE),
    data.frame(component = component_name, official_series = official_qtq_var, activity_index = index_var, period = "Post-2025Q2",
               observations = sum(is.finite(df[[official_qtq_var]][df$post_2025Q2 == 1]) & is.finite(df[[index_var]][df$post_2025Q2 == 1])),
               correlation = safe_cor(df[[official_qtq_var]][df$post_2025Q2 == 1], df[[index_var]][df$post_2025Q2 == 1]), stringsAsFactors = FALSE)
  )
}

correlation_summary <- bind_rows(
  make_correlation_rows(analysis_data, "Household consumption: baseline HHAI", "c_yoy_growth", "c_qtq_value", "HHAI"),
  if ("HHAI_bestfit" %in% names(analysis_data)) make_correlation_rows(analysis_data, "Household consumption: best-fit HHAI", "c_yoy_growth", "c_qtq_value", "HHAI_bestfit") else NULL,
  make_correlation_rows(analysis_data, "GFCF: baseline IAI", "i_yoy_growth", "i_qtq_value", "IAI"),
  if ("IAI_bestfit" %in% names(analysis_data)) make_correlation_rows(analysis_data, "GFCF: best-fit IAI", "i_yoy_growth", "i_qtq_value", "IAI_bestfit") else NULL
)

rolling_correlation_data <- analysis_data %>%
  select(
    quarter_year,
    quarter_date,
    year_num,
    quarter_num,
    post_2025Q2,
    rolling_corr_c_yoy_HHAI,
    rolling_corr_c_qtq_HHAI,
    rolling_corr_i_yoy_IAI,
    rolling_corr_i_qtq_IAI,
    rolling_corr_c_yoy_HHAI_bestfit,
    rolling_corr_i_yoy_IAI_bestfit
  )

write.csv(
  correlation_summary,
  paste0(
    "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_pre_post_correlation_summary_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

write.csv(
  rolling_correlation_data,
  paste0(
    "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_rolling_correlation_data_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

cat("\n============================================================\n")
cat("Pre/post correlation summary\n")
cat("============================================================\n")
print_console_table(
  correlation_summary %>%
    mutate(
      correlation = round(correlation, 3)
    )
)



# ============================================================
# 14. Bridge regressions: official component growth vs activity index
# ============================================================

run_bridge <- function(df, component_name, official_var, index_var) {
  
  d <- df %>%
    filter(
      !is.na(.data[[official_var]]),
      !is.na(.data[[index_var]])
    ) %>%
    arrange(year_num, quarter_num) %>%
    mutate(t = row_number())
  
  d_pre <- d %>%
    filter(post_2025Q2 == 0)
  
  if (nrow(d_pre) < 8) {
    return(NULL)
  }
  
  bridge_model <- lm(
    as.formula(paste0(official_var, " ~ t + ", index_var)),
    data = d_pre
  )
  
  nw_lag <- min(3, max(0, floor(nobs(bridge_model) / 4)))
  nw_vcov <- NeweyWest(bridge_model, lag = nw_lag, prewhite = FALSE)
  nw_ct <- coeftest(bridge_model, vcov = nw_vcov)
  
  d$implied_component_growth <- as.numeric(
    predict(bridge_model, newdata = d)
  )
  
  d$consistency_gap <- d[[official_var]] - d$implied_component_growth
  
  gap_pre_mean <- mean_or_na(d$consistency_gap[d$post_2025Q2 == 0])
  gap_pre_sd <- sd(d$consistency_gap[d$post_2025Q2 == 0], na.rm = TRUE)
  
  d$gap_zscore_pre_distribution <- (
    d$consistency_gap - gap_pre_mean
  ) / gap_pre_sd
  
  gap_summary <- d %>%
    group_by(post_2025Q2) %>%
    summarise(
      observations = sum(!is.na(consistency_gap)),
      mean_official_growth = mean_or_na(.data[[official_var]]),
      mean_index = mean_or_na(.data[[index_var]]),
      mean_implied_growth = mean_or_na(implied_component_growth),
      mean_consistency_gap = mean_or_na(consistency_gap),
      mean_gap_zscore = mean_or_na(gap_zscore_pre_distribution),
      .groups = "drop"
    ) %>%
    mutate(
      period = ifelse(post_2025Q2 == 1, "Post-2025Q2", "Pre-2025Q2"),
      component = component_name,
      official_var = official_var,
      index_var = index_var
    ) %>%
    select(
      component,
      official_var,
      index_var,
      period,
      observations,
      mean_official_growth,
      mean_index,
      mean_implied_growth,
      mean_consistency_gap,
      mean_gap_zscore
    )
  
  return(list(
    model = bridge_model,
    nw_ct = nw_ct,
    fitted_data = d,
    gap_summary = gap_summary
  ))
}

hh_bridge <- run_bridge(
  df = analysis_data,
  component_name = "Household consumption",
  official_var = "c_yoy_growth",
  index_var = "HHAI"
)

gfcf_bridge <- run_bridge(
  df = analysis_data,
  component_name = "GFCF: baseline IAI",
  official_var = "i_yoy_growth",
  index_var = "IAI"
)

hh_bestfit_bridge <- if ("HHAI_bestfit" %in% names(analysis_data)) {
  run_bridge(analysis_data, "Household consumption: best-fit HHAI", "c_yoy_growth", "HHAI_bestfit")
} else NULL

gfcf_bestfit_bridge <- if ("IAI_bestfit" %in% names(analysis_data)) {
  run_bridge(analysis_data, "GFCF: best-fit IAI", "i_yoy_growth", "IAI_bestfit")
} else NULL

bridge_gap_summary <- bind_rows(
  if (!is.null(hh_bridge)) hh_bridge$gap_summary else NULL,
  if (!is.null(hh_bestfit_bridge)) hh_bestfit_bridge$gap_summary else NULL,
  if (!is.null(gfcf_bridge)) gfcf_bridge$gap_summary else NULL,
  if (!is.null(gfcf_bestfit_bridge)) gfcf_bestfit_bridge$gap_summary else NULL
)

write.csv(
  bridge_gap_summary,
  paste0(
    "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_bridge_gap_summary_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

if (!is.null(hh_bridge)) {
  write.csv(
    hh_bridge$fitted_data,
    paste0(
      "outputs_hh_gfcf_reai_no_electricity/hh_consumption_bridge_fitted_",
      analysis_sample_stub,
      ".csv"
    ),
    row.names = FALSE
  )
}

if (!is.null(gfcf_bridge)) {
  write.csv(
    gfcf_bridge$fitted_data,
    paste0(
      "outputs_hh_gfcf_reai_no_electricity/gfcf_bridge_fitted_",
      analysis_sample_stub,
      ".csv"
    ),
    row.names = FALSE
  )
}

cat("\n============================================================\n")
cat("Bridge regression output: Household consumption\n")
cat("Official component: c_yoy_growth; Activity index: HHAI\n")
cat("Estimated only on pre-2025Q2 sample\n")
cat("============================================================\n")

if (!is.null(hh_bridge)) {
  print(summary(hh_bridge$model))
  cat("\nNewey-West coefficient test:\n")
  print(hh_bridge$nw_ct)
} else {
  cat("Household consumption bridge model not estimated: too few observations.\n")
}

cat("\n============================================================\n")
cat("Bridge regression output: GFCF\n")
cat("Official component: i_yoy_growth; Activity index: IAI\n")
cat("Estimated only on pre-2025Q2 sample\n")
cat("============================================================\n")

if (!is.null(gfcf_bridge)) {
  print(summary(gfcf_bridge$model))
  cat("\nNewey-West coefficient test:\n")
  print(gfcf_bridge$nw_ct)
} else {
  cat("GFCF bridge model not estimated: too few observations.\n")
}

cat("\n============================================================\n")
cat("Bridge gap summary\n")
cat("============================================================\n")
print_console_table(
  bridge_gap_summary %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    )
)


# ============================================================
# 14A. Number-of-factors robustness: PC1 vs PC1-PC2 vs PC1-PC3
# ============================================================

run_factor_bridge <- function(
    df,
    component_name,
    official_var,
    pc_prefix,
    index_type,
    n_factors
) {
  
  pc_vars <- paste0(pc_prefix, "_PC", seq_len(n_factors))
  pc_vars <- intersect(pc_vars, names(df))
  
  if (length(pc_vars) < n_factors) return(NULL)
  
  d <- df %>%
    filter(!is.na(.data[[official_var]])) %>%
    arrange(year_num, quarter_num) %>%
    mutate(t = row_number())
  
  complete_needed <- c(official_var, pc_vars)
  d <- d[stats::complete.cases(d[, complete_needed, drop = FALSE]), ]
  
  d_pre <- d %>% filter(post_2025Q2 == 0)
  
  min_obs_needed <- length(pc_vars) + 5
  
  if (nrow(d_pre) < min_obs_needed) return(NULL)
  
  rhs <- paste(c("t", pc_vars), collapse = " + ")
  bridge_model <- lm(as.formula(paste0(official_var, " ~ ", rhs)), data = d_pre)
  
  nw_lag <- min(3, max(0, floor(nobs(bridge_model) / 4)))
  nw_vcov <- NeweyWest(bridge_model, lag = nw_lag, prewhite = FALSE)
  nw_ct <- coeftest(bridge_model, vcov = nw_vcov)
  
  d$implied_component_growth <- as.numeric(predict(bridge_model, newdata = d))
  d$consistency_gap <- d[[official_var]] - d$implied_component_growth
  
  gap_pre_mean <- mean_or_na(d$consistency_gap[d$post_2025Q2 == 0])
  gap_pre_sd <- sd(d$consistency_gap[d$post_2025Q2 == 0], na.rm = TRUE)
  
  d$gap_zscore_pre_distribution <- ifelse(
    is.na(gap_pre_sd) | gap_pre_sd == 0,
    NA_real_,
    (d$consistency_gap - gap_pre_mean) / gap_pre_sd
  )
  
  gap_summary <- d %>%
    group_by(post_2025Q2) %>%
    summarise(
      observations = sum(!is.na(consistency_gap)),
      mean_official_growth = mean_or_na(.data[[official_var]]),
      mean_implied_growth = mean_or_na(implied_component_growth),
      mean_consistency_gap = mean_or_na(consistency_gap),
      mean_abs_consistency_gap = mean(abs(consistency_gap), na.rm = TRUE),
      mean_gap_zscore = mean_or_na(gap_zscore_pre_distribution),
      .groups = "drop"
    ) %>%
    mutate(
      period = ifelse(post_2025Q2 == 1, "Post-2025Q2", "Pre-2025Q2"),
      component = component_name,
      index_type = index_type,
      official_var = official_var,
      pc_prefix = pc_prefix,
      n_factors = n_factors,
      pc_vars_used = paste(pc_vars, collapse = " + "),
      pre_r_squared = summary(bridge_model)$r.squared,
      pre_adj_r_squared = summary(bridge_model)$adj.r.squared,
      pre_rmse = sqrt(mean(residuals(bridge_model)^2, na.rm = TRUE))
    ) %>%
    select(
      component,
      index_type,
      official_var,
      pc_prefix,
      n_factors,
      pc_vars_used,
      period,
      observations,
      mean_official_growth,
      mean_implied_growth,
      mean_consistency_gap,
      mean_abs_consistency_gap,
      mean_gap_zscore,
      pre_r_squared,
      pre_adj_r_squared,
      pre_rmse
    )
  
  list(
    model = bridge_model,
    nw_ct = nw_ct,
    fitted_data = d,
    gap_summary = gap_summary
  )
}

factor_bridge_specs <- bind_rows(
  data.frame(
    component = "Household consumption",
    index_type = "Baseline HHAI",
    official_var = "c_yoy_growth",
    pc_prefix = "HHAI",
    stringsAsFactors = FALSE
  ),
  if ("HHAI_bestfit_PC1" %in% names(analysis_data)) {
    data.frame(
      component = "Household consumption",
      index_type = "Best-fit HHAI",
      official_var = "c_yoy_growth",
      pc_prefix = "HHAI_bestfit",
      stringsAsFactors = FALSE
    )
  } else NULL,
  data.frame(
    component = "GFCF",
    index_type = "Baseline IAI",
    official_var = "i_yoy_growth",
    pc_prefix = "IAI",
    stringsAsFactors = FALSE
  ),
  if ("IAI_bestfit_PC1" %in% names(analysis_data)) {
    data.frame(
      component = "GFCF",
      index_type = "Best-fit IAI",
      official_var = "i_yoy_growth",
      pc_prefix = "IAI_bestfit",
      stringsAsFactors = FALSE
    )
  } else NULL
)

factor_bridge_results <- list()

if (run_factor_number_robustness) {
  for (s in seq_len(nrow(factor_bridge_specs))) {
    for (nf in factor_robustness_n_factors) {
      tmp <- run_factor_bridge(
        df = analysis_data,
        component_name = factor_bridge_specs$component[s],
        official_var = factor_bridge_specs$official_var[s],
        pc_prefix = factor_bridge_specs$pc_prefix[s],
        index_type = factor_bridge_specs$index_type[s],
        n_factors = nf
      )
      
      if (!is.null(tmp)) {
        result_name <- paste0(
          safe_name(factor_bridge_specs$component[s]),
          "_",
          safe_name(factor_bridge_specs$index_type[s]),
          "_",
          nf,
          "f"
        )
        factor_bridge_results[[result_name]] <- tmp
      }
    }
  }
}

factor_robustness_gap_summary <- bind_rows(
  lapply(factor_bridge_results, function(x) x$gap_summary)
)

if (nrow(factor_robustness_gap_summary) > 0) {
  
  write.csv(
    factor_robustness_gap_summary,
    paste0(
      "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_factor_number_robustness_gap_summary_",
      analysis_sample_stub,
      ".csv"
    ),
    row.names = FALSE
  )
  
  factor_robustness_post_summary <- factor_robustness_gap_summary %>%
    filter(period == "Post-2025Q2") %>%
    arrange(component, index_type, n_factors)
  
  write.csv(
    factor_robustness_post_summary,
    paste0(
      "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_factor_number_robustness_post_summary_",
      analysis_sample_stub,
      ".csv"
    ),
    row.names = FALSE
  )
  
  cat("\n============================================================\n")
  cat("Number-of-factors robustness: bridge gap summary\n")
  cat("============================================================\n")
  print_console_table(
    factor_robustness_gap_summary %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  )
  
  cat("\n============================================================\n")
  cat("Number-of-factors robustness: post-2025Q2 summary only\n")
  cat("============================================================\n")
  print_console_table(
    factor_robustness_post_summary %>%
      select(
        component,
        index_type,
        n_factors,
        observations,
        mean_official_growth,
        mean_implied_growth,
        mean_consistency_gap,
        mean_gap_zscore,
        pre_r_squared,
        pre_adj_r_squared
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  )
  
  for (nm in names(factor_bridge_results)) {
    write.csv(
      factor_bridge_results[[nm]]$fitted_data,
      paste0(
        "outputs_hh_gfcf_reai_no_electricity/factor_robustness_fitted_",
        nm,
        "_",
        analysis_sample_stub,
        ".csv"
      ),
      row.names = FALSE
    )
  }
  
} else {
  factor_robustness_post_summary <- data.frame()
  cat("\n============================================================\n")
  cat("Number-of-factors robustness not estimated.\n")
  cat("Reason: insufficient usable factor scores or observations.\n")
  cat("============================================================\n")
}



# ============================================================
# 15. Indicator contribution diagnostics
# ============================================================

make_indicator_shift_table <- function(df, vars, group_name) {
  
  vars <- intersect(vars, names(df))
  
  out <- lapply(vars, function(v) {
    
    d_pre <- df %>% filter(post_2025Q2 == 0)
    d_post <- df %>% filter(post_2025Q2 == 1)
    
    data.frame(
      group = group_name,
      variable = v,
      pre_mean = mean_or_na(d_pre[[v]]),
      post_mean = mean_or_na(d_post[[v]]),
      post_minus_pre = mean_or_na(d_post[[v]]) - mean_or_na(d_pre[[v]]),
      nonmissing_pre = sum(!is.na(d_pre[[v]])),
      nonmissing_post = sum(!is.na(d_post[[v]])),
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(out) %>%
    arrange(desc(abs(post_minus_pre)))
}

indicator_shift_summary <- bind_rows(
  make_indicator_shift_table(
    df = analysis_data %>%
      left_join(
        indicator_transformed,
        by = c("quarter_year", "quarter_date", "year_num", "quarter_num")
      ),
    vars = hh_index_vars,
    group_name = "Household consumption indicators"
  ),
  make_indicator_shift_table(
    df = analysis_data %>%
      left_join(
        indicator_transformed,
        by = c("quarter_year", "quarter_date", "year_num", "quarter_num")
      ),
    vars = gfcf_index_vars,
    group_name = "GFCF / investment indicators"
  )
)

write.csv(
  indicator_shift_summary,
  paste0(
    "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_indicator_pre_post_shift_summary_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

cat("\n============================================================\n")
cat("Indicator pre/post shift summary\n")
cat("============================================================\n")
print_console_table(
  indicator_shift_summary %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    )
)


# ============================================================
# 16. Figures: actual official component growth vs activity indices
# ============================================================

# Important:
# The official series is plotted in its actual unit:
#   - y-o-y growth is shown in percent
#   - q-t-q value change is shown in original GDP units
#
# The activity index is a PCA index and does NOT have a natural percent-growth unit.
# To put it on the same axis, it is rescaled to have the same mean and standard
# deviation as the official series over the plotted sample.
#
# Therefore, read the dashed activity-index line as:
#   "index-implied direction on the official scale"
# not as an actual y-o-y growth rate.

plot_index_vs_component_actual <- function(
    df,
    official_var,
    official_label,
    index_var,
    index_label,
    file_stub,
    y_axis_label
) {
  
  d <- df %>%
    filter(!is.na(.data[[official_var]]), !is.na(.data[[index_var]])) %>%
    arrange(year_num, quarter_num) %>%
    mutate(
      quarter_year_factor = factor(quarter_year, levels = unique(quarter_year)),
      index_rescaled_to_official = rescale_index_to_official(
        .data[[index_var]],
        .data[[official_var]]
      )
    )
  
  break_x <- which(levels(d$quarter_year_factor) == break_quarter)
  
  p <- ggplot(d, aes(x = quarter_year_factor, group = 1)) +
    geom_line(aes(y = .data[[official_var]], linetype = official_label), linewidth = 0.9) +
    geom_point(aes(y = .data[[official_var]], shape = official_label), size = 1.4) +
    geom_line(aes(y = index_rescaled_to_official, linetype = index_label), linewidth = 0.9) +
    geom_point(aes(y = index_rescaled_to_official, shape = index_label), size = 1.4) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Quarter",
      y = y_axis_label,
      linetype = NULL,
      shape = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 16, face = "bold"),  # X-axis label size
      axis.title.y = element_text(size = 16, face = "bold"),  # Y-axis label size
      strip.text = element_text(face = "bold", size = 15),
      legend.text=element_text(size=11),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  if (length(break_x) == 1) {
    p <- p +
      geom_vline(
        xintercept = break_x,
        linetype = "dotted",
        linewidth = 0.8
      )
  }
  
  print(p)
  
  ggsave(
    filename = paste0(
      "figures_hh_gfcf_reai_no_electricity/",
      file_stub,
      "_",
      analysis_sample_stub,
      ".png"
    ),
    plot = p,
    width = 9,
    height = 4.8,
    dpi = 300
  )
  
  return(p)
}

p_hh_yoy_actual <- plot_index_vs_component_actual(
  df = analysis_data,
  official_var = "c_yoy_growth",
  official_label = "Official HH consumption y-o-y growth",
  index_var = "HHAI",
  index_label = "HHAI, rescaled to official-growth scale",
  file_stub = "hh_consumption_actual_yoy_growth_vs_hhai_rescaled",
  y_axis_label = "Official y-o-y growth, percent"
)

p_gfcf_yoy_actual <- plot_index_vs_component_actual(
  df = analysis_data,
  official_var = "i_yoy_growth",
  official_label = "Official GFCF y-o-y growth",
  index_var = "IAI",
  index_label = "IAI, rescaled to official-growth scale",
  file_stub = "gfcf_actual_yoy_growth_vs_iai_rescaled",
  y_axis_label = "Official y-o-y growth, percent"
)

p_hh_qtq_actual <- plot_index_vs_component_actual(
  df = analysis_data,
  official_var = "c_qtq_value",
  official_label = "Official HH consumption q-t-q value change",
  index_var = "HHAI",
  index_label = "HHAI, rescaled to official-value-change scale",
  file_stub = "hh_consumption_actual_qtq_value_vs_hhai_rescaled",
  y_axis_label = "Official q-t-q value change"
)

p_gfcf_qtq_actual <- plot_index_vs_component_actual(
  df = analysis_data,
  official_var = "i_qtq_value",
  official_label = "Official GFCF q-t-q value change",
  index_var = "IAI",
  index_label = "IAI, rescaled to official-value-change scale",
  file_stub = "gfcf_actual_qtq_value_vs_iai_rescaled",
  y_axis_label = "Official q-t-q value change"
)

if ("HHAI_bestfit" %in% names(analysis_data)) {
  p_hh_yoy_bestfit_actual <- plot_index_vs_component_actual(
    analysis_data, "c_yoy_growth", "Official HH consumption y-o-y growth",
    "HHAI_bestfit", "Best-fit HHAI, rescaled", "hh_consumption_actual_yoy_growth_vs_hhai_bestfit_rescaled",
    "Official y-o-y growth, percent"
  )
}
if ("IAI_bestfit" %in% names(analysis_data)) {
  p_gfcf_yoy_bestfit_actual <- plot_index_vs_component_actual(
    analysis_data, "i_yoy_growth", "Official GFCF y-o-y growth",
    "IAI_bestfit", "Best-fit IAI, rescaled", "gfcf_actual_yoy_growth_vs_iai_bestfit_rescaled",
    "Official y-o-y growth, percent"
  )
}

# Export the actual official growth and rescaled-index values used in the figures.
plot_values_actual_scale <- analysis_data %>%
  mutate(
    HHAI_rescaled_to_c_yoy = rescale_index_to_official(HHAI, c_yoy_growth),
    IAI_rescaled_to_i_yoy = rescale_index_to_official(IAI, i_yoy_growth),
    HHAI_rescaled_to_c_qtq = rescale_index_to_official(HHAI, c_qtq_value),
    IAI_rescaled_to_i_qtq = rescale_index_to_official(IAI, i_qtq_value),
    HHAI_bestfit_rescaled_to_c_yoy = if ("HHAI_bestfit" %in% names(.)) rescale_index_to_official(HHAI_bestfit, c_yoy_growth) else NA_real_,
    IAI_bestfit_rescaled_to_i_yoy = if ("IAI_bestfit" %in% names(.)) rescale_index_to_official(IAI_bestfit, i_yoy_growth) else NA_real_
  ) %>%
  select(
    quarter_year,
    year_num,
    quarter_num,
    c_yoy_growth,
    HHAI,
    HHAI_rescaled_to_c_yoy,
    c_qtq_value,
    HHAI_rescaled_to_c_qtq,
    i_yoy_growth,
    IAI,
    IAI_rescaled_to_i_yoy,
    HHAI_bestfit_rescaled_to_c_yoy,
    IAI_bestfit_rescaled_to_i_yoy,
    i_qtq_value,
    IAI_rescaled_to_i_qtq,
    post_2025Q2
  )

write.csv(
  plot_values_actual_scale,
  paste0(
    "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_actual_scale_plot_values_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

cat("\n============================================================\n")
cat("Actual-scale plot values: official growth and rescaled indices\n")
cat("============================================================\n")
print_console_table(
  plot_values_actual_scale %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    )
)



# ============================================================
# 17. Figures: bridge consistency gaps
# ============================================================

plot_bridge_gap <- function(bridge_obj, component_title, file_stub) {
  
  if (is.null(bridge_obj)) return(NULL)
  
  d <- bridge_obj$fitted_data %>%
    arrange(year_num, quarter_num) %>%
    mutate(
      quarter_year_factor = factor(quarter_year, levels = unique(quarter_year))
    )
  
  break_x <- which(levels(d$quarter_year_factor) == break_quarter)
  
  p <- ggplot(d, aes(x = quarter_year_factor, y = consistency_gap, group = 1)) +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.4) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Quarter",
      y = "Official growth minus index-implied growth"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
      panel.grid.minor = element_blank()
    )
  
  if (length(break_x) == 1) {
    p <- p +
      geom_vline(
        xintercept = break_x,
        linetype = "dotted",
        linewidth = 0.8
      )
  }
  
  print(p)
  
  ggsave(
    filename = paste0(
      "figures_hh_gfcf_reai_no_electricity/",
      file_stub,
      "_",
      analysis_sample_stub,
      ".png"
    ),
    plot = p,
    width = 9,
    height = 4.8,
    dpi = 300
  )
  
  return(p)
}

p_hh_gap <- plot_bridge_gap(
  bridge_obj = hh_bridge,
  component_title = "Household consumption",
  file_stub = "hh_consumption_bridge_consistency_gap"
)

p_gfcf_gap <- plot_bridge_gap(
  bridge_obj = gfcf_bridge,
  component_title = "GFCF",
  file_stub = "gfcf_bridge_consistency_gap"
)

p_hh_bestfit_gap <- plot_bridge_gap(hh_bestfit_bridge, "Household consumption: best-fit HHAI", "hh_consumption_bestfit_bridge_consistency_gap")
p_gfcf_bestfit_gap <- plot_bridge_gap(gfcf_bestfit_bridge, "GFCF: best-fit IAI", "gfcf_bestfit_bridge_consistency_gap")




# ============================================================
# 17A. Figures: rolling correlations
# ============================================================

plot_rolling_correlation <- function(df, rolling_var, file_stub) {
  
  d <- df %>%
    filter(!is.na(.data[[rolling_var]])) %>%
    arrange(year_num, quarter_num) %>%
    mutate(
      quarter_year_factor = factor(quarter_year, levels = unique(quarter_year))
    )
  
  if (nrow(d) == 0) return(NULL)
  
  break_x <- which(levels(d$quarter_year_factor) == break_quarter)
  
  p <- ggplot(
    d,
    aes(
      x = quarter_year_factor,
      y = .data[[rolling_var]],
      group = 1
    )
  ) +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.2) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Quarter",
      y = paste0(rolling_correlation_width, "-quarter rolling correlation")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
      panel.grid.minor = element_blank()
    )
  
  if (length(break_x) == 1) {
    p <- p +
      geom_vline(
        xintercept = break_x,
        linetype = "dotted",
        linewidth = 0.8
      )
  }
  
  print(p)
  
  ggsave(
    filename = paste0(
      "figures_hh_gfcf_reai_no_electricity/",
      file_stub,
      "_",
      analysis_sample_stub,
      ".png"
    ),
    plot = p,
    width = 9,
    height = 4.8,
    dpi = 300
  )
  
  return(p)
}

p_roll_hh_yoy <- plot_rolling_correlation(
  analysis_data,
  "rolling_corr_c_yoy_HHAI",
  "rolling_corr_hh_consumption_yoy_HHAI"
)

p_roll_hh_qtq <- plot_rolling_correlation(
  analysis_data,
  "rolling_corr_c_qtq_HHAI",
  "rolling_corr_hh_consumption_qtq_HHAI"
)

p_roll_gfcf_yoy <- plot_rolling_correlation(
  analysis_data,
  "rolling_corr_i_yoy_IAI",
  "rolling_corr_gfcf_yoy_IAI"
)

p_roll_gfcf_qtq <- plot_rolling_correlation(
  analysis_data,
  "rolling_corr_i_qtq_IAI",
  "rolling_corr_gfcf_qtq_IAI"
)

p_roll_hh_yoy_bestfit <- plot_rolling_correlation(analysis_data, "rolling_corr_c_yoy_HHAI_bestfit", "rolling_corr_hh_consumption_yoy_HHAI_bestfit")
p_roll_gfcf_yoy_bestfit <- plot_rolling_correlation(analysis_data, "rolling_corr_i_yoy_IAI_bestfit", "rolling_corr_gfcf_yoy_IAI_bestfit")


# ============================================================
# 17B. Figures: number-of-factors robustness gaps
# ============================================================

plot_factor_robustness_post_gap <- function(df, component_filter, index_filter, file_stub) {
  
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  d <- df %>%
    filter(
      period == "Post-2025Q2",
      component == component_filter,
      index_type == index_filter
    ) %>%
    arrange(n_factors)
  
  if (nrow(d) == 0) return(NULL)
  
  p <- ggplot(d, aes(x = factor(n_factors), y = mean_consistency_gap, group = 1)) +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Number of PCA factors in bridge model",
      y = "Post-2025Q2 mean gap"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  print(p)
  
  ggsave(
    filename = paste0(
      "figures_hh_gfcf_reai_no_electricity/",
      file_stub,
      "_",
      analysis_sample_stub,
      ".png"
    ),
    plot = p,
    width = 6.5,
    height = 4.2,
    dpi = 300
  )
  
  return(p)
}

if (exists("factor_robustness_gap_summary") && nrow(factor_robustness_gap_summary) > 0) {
  
  p_factor_hh_baseline <- plot_factor_robustness_post_gap(
    factor_robustness_gap_summary,
    "Household consumption",
    "Baseline HHAI",
    "factor_robustness_post_gap_hh_baseline"
  )
  
  p_factor_hh_bestfit <- plot_factor_robustness_post_gap(
    factor_robustness_gap_summary,
    "Household consumption",
    "Best-fit HHAI",
    "factor_robustness_post_gap_hh_bestfit"
  )
  
  p_factor_gfcf_baseline <- plot_factor_robustness_post_gap(
    factor_robustness_gap_summary,
    "GFCF",
    "Baseline IAI",
    "factor_robustness_post_gap_gfcf_baseline"
  )
  
  p_factor_gfcf_bestfit <- plot_factor_robustness_post_gap(
    factor_robustness_gap_summary,
    "GFCF",
    "Best-fit IAI",
    "factor_robustness_post_gap_gfcf_bestfit"
  )
}

# ============================================================
# 18. Save R objects
# ============================================================

saveRDS(
  list(
    gdp_components = gdp_components,
    indicator_raw = indicator_raw,
    indicator_quarterly = indicator_quarterly,
    indicator_transformed = indicator_transformed,
    hh_index_vars = hh_index_vars,
    gfcf_index_vars = gfcf_index_vars,
    hh_pca = hh_pca,
    gfcf_pca = gfcf_pca,
    hh_bestfit = hh_bestfit,
    gfcf_bestfit = gfcf_bestfit,
    index_combined = index_combined,
    analysis_data = analysis_data,
    consistency_summary = consistency_summary,
    correlation_summary = correlation_summary,
    rolling_correlation_data = rolling_correlation_data,
    bridge_gap_summary = bridge_gap_summary,
    hh_bridge = hh_bridge,
    gfcf_bridge = gfcf_bridge,
    hh_bestfit_bridge = hh_bestfit_bridge,
    gfcf_bestfit_bridge = gfcf_bestfit_bridge,
    indicator_shift_summary = indicator_shift_summary
  ),
  paste0(
    "outputs_hh_gfcf_reai_no_electricity/hh_gfcf_reai_objects_",
    analysis_sample_stub,
    ".rds"
  )
)

cat("\nSaved R objects to outputs_hh_gfcf_reai_no_electricity/.\n")