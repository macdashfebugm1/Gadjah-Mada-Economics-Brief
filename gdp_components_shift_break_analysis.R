# ============================================================
# GDP Components: Value Shifts and Structural Break Tests
#
# Purpose:
# 1. Read seasonally adjusted GDP expenditure-side components from gdp_all.xlsx.
# 2. Examine whether component-level quarterly value changes shift after 2025Q2.
# 3. Test for known and unknown structural breaks in:
#       a. levels
#       b. q-t-q value changes
#       c. y-o-y log growth
# 4. Use a post-COVID main analysis sample, so COVID does not dominate the figures.
# 5. Print regression output in the command window.
# 6. Export CSV tables, LaTeX tables, figures, and R objects.
#
# Input file:
#   gdp_all.xlsx
#
# Required columns:
#   date, gdp, c, g, i, ch_stock, stat_dis, ex, im
#
# Optional columns:
#   diff_gdp, diff_c, diff_g, diff_i, diff_ch_stock,
#   diff_stat_dis, diff_ex, diff_im
#
# Notes:
# - The GDP and component series are assumed to be seasonally adjusted.
# - q-t-q value changes are computed directly from the level series.
# - y-o-y growth is computed as 100 * [log(x_t) - log(x_{t-4})].
# - For variables that can be zero or negative, such as inventory changes
#   and statistical discrepancy, y-o-y log growth is not computed.
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

packages <- c(
  "readxl",
  "dplyr",
  "lubridate",
  "ggplot2",
  "zoo",
  "tidyr",
  "strucchange",
  "lmtest",
  "sandwich",
  "stringr",
  "scales"
)

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(tidyr)
library(strucchange)
library(lmtest)
library(sandwich)
library(stringr)
library(scales)

# Explicit dplyr aliases to reduce namespace conflicts.
filter <- dplyr::filter
select <- dplyr::select
mutate <- dplyr::mutate
arrange <- dplyr::arrange
group_by <- dplyr::group_by
summarise <- dplyr::summarise
left_join <- dplyr::left_join
bind_rows <- dplyr::bind_rows
distinct <- dplyr::distinct
pull <- dplyr::pull


# ============================================================
# 1. User settings
# ============================================================

gdp_file <- "gdp_all.xlsx"
gdp_sheet <- "My Series"

break_quarter <- "2025 Q2"

# Main analysis sample.
# Use 2022Q1 onward to focus on the post-COVID period.
# This avoids the 2020Q2 collapse and 2020-2021 rebound dominating the visual scale.
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

# Event-window figure/table around the candidate break.
event_window_quarters <- c(
  "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4",
  "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4",
  "2026 Q1", "2026 Q2", "2026 Q3", "2026 Q4"
)

# Keep full-sample unknown-break tests as a robustness check.
run_full_sample_unknown_breaks <- TRUE

# Print regression output in command window.
print_regression_output <- TRUE


# ============================================================
# 2. Output folders
# ============================================================

dir.create("outputs_gdp_components", showWarnings = FALSE)
dir.create("tables_gdp_components", showWarnings = FALSE)
dir.create("figures_gdp_components", showWarnings = FALSE)


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

safe_name <- function(x) {
  x <- gsub("[^a-zA-Z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
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

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("_", "\\\\_", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("#", "\\\\#", x)
  x
}

write_latex_table <- function(df, file, caption, label, align = NULL) {
  
  if (is.null(align)) {
    align <- paste0("l", paste(rep("c", ncol(df) - 1), collapse = ""))
  }
  
  con <- file(file, open = "w", encoding = "UTF-8")
  writeLines("\\begin{table}[!htbp]", con)
  writeLines("\\centering", con)
  writeLines("\\small", con)
  writeLines(paste0("\\caption{", caption, "}"), con)
  writeLines(paste0("\\label{", label, "}"), con)
  writeLines(paste0("\\begin{tabular}{", align, "}"), con)
  writeLines("\\hline", con)
  
  header <- paste(latex_escape(names(df)), collapse = " & ")
  writeLines(paste0(header, " \\\\"), con)
  writeLines("\\hline", con)
  
  for (i in seq_len(nrow(df))) {
    row <- paste(latex_escape(df[i, ]), collapse = " & ")
    writeLines(paste0(row, " \\\\"), con)
  }
  
  writeLines("\\hline", con)
  writeLines("\\end{tabular}", con)
  writeLines("\\end{table}", con)
  close(con)
}

extract_coeftest <- function(ct_obj) {
  
  ct_mat <- as.matrix(ct_obj)
  if (ncol(ct_mat) < 4) return(NULL)
  
  data.frame(
    term = rownames(ct_mat),
    estimate = ct_mat[, 1],
    std_error = ct_mat[, 2],
    t_value = ct_mat[, 3],
    p_value = ct_mat[, 4],
    row.names = NULL,
    check.names = FALSE
  )
}

get_term_value <- function(ct_df, term_name, value_name) {
  
  if (is.null(ct_df)) return(NA_real_)
  if (!(term_name %in% ct_df$term)) return(NA_real_)
  if (!(value_name %in% names(ct_df))) return(NA_real_)
  
  ct_df %>%
    filter(term == term_name) %>%
    pull(all_of(value_name)) %>%
    .[1]
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) <= 1) return(NA_real_)
  sd(x, na.rm = TRUE)
}

safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
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

required_level_vars <- c(
  "gdp",
  "c",
  "g",
  "i",
  "ch_stock",
  "stat_dis",
  "ex",
  "im"
)

if (!("date" %in% names(gdp_raw))) {
  stop("Column 'date' not found in gdp_all.xlsx.")
}

missing_level_vars <- setdiff(required_level_vars, names(gdp_raw))

if (length(missing_level_vars) > 0) {
  stop(
    paste(
      "Missing required GDP/component columns:",
      paste(missing_level_vars, collapse = ", ")
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
      all_of(required_level_vars),
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
    quarter_year = paste0(year_num, " Q", quarter_num)
  ) %>%
  filter(!is.na(quarter_num)) %>%
  arrange(year_num, quarter_num) %>%
  mutate(
    quarter_year = factor(quarter_year, levels = unique(quarter_year)),
    t_full = row_number()
  )

component_labels <- data.frame(
  component = c("gdp", "c", "g", "i", "ch_stock", "stat_dis", "ex", "im"),
  label = c(
    "GDP",
    "Household consumption",
    "Government consumption",
    "Gross fixed capital formation",
    "Change in inventories",
    "Statistical discrepancy",
    "Exports",
    "Imports"
  ),
  stringsAsFactors = FALSE
)

positive_for_log_growth <- c("gdp", "c", "g", "i", "ex", "im")


# ============================================================
# 5. Construct transformations: level, q-t-q value change, y-o-y growth
# ============================================================

for (v in required_level_vars) {
  diff_name <- paste0("qtq_value_", v)
  yoy_name  <- paste0("yoy_growth_", v)
  
  # Main q-t-q value change used in this script.
  gdp_components[[diff_name]] <- gdp_components[[v]] - dplyr::lag(gdp_components[[v]])
  
  # y-o-y log growth only for positive level variables.
  if (v %in% positive_for_log_growth) {
    gdp_components[[yoy_name]] <- safe_log_growth(gdp_components[[v]], lag_n = 4)
  } else {
    gdp_components[[yoy_name]] <- NA_real_
  }
}

# Keep user-provided diff_* variables if available, but the script uses
# the recomputed qtq_value_* variables above.
existing_diff_vars <- intersect(
  paste0("diff_", required_level_vars),
  names(gdp_components)
)

if (length(existing_diff_vars) > 0) {
  gdp_components <- gdp_components %>%
    mutate(
      across(
        all_of(existing_diff_vars),
        to_numeric_clean
      )
    )
}

write.csv(
  gdp_components,
  "outputs_gdp_components/gdp_components_clean_with_transformations.csv",
  row.names = FALSE
)


# ============================================================
# 6. Create long-format dataset for testing
# ============================================================

level_long <- gdp_components %>%
  select(
    quarter_year,
    year_num,
    quarter_num,
    date_parsed,
    t_full,
    all_of(required_level_vars)
  ) %>%
  pivot_longer(
    cols = all_of(required_level_vars),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    transformation = "Level"
  )

qtq_long <- gdp_components %>%
  select(
    quarter_year,
    year_num,
    quarter_num,
    date_parsed,
    t_full,
    starts_with("qtq_value_")
  ) %>%
  pivot_longer(
    cols = starts_with("qtq_value_"),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    component = gsub("^qtq_value_", "", component),
    transformation = "q-t-q value change"
  )

yoy_long <- gdp_components %>%
  select(
    quarter_year,
    year_num,
    quarter_num,
    date_parsed,
    t_full,
    starts_with("yoy_growth_")
  ) %>%
  pivot_longer(
    cols = starts_with("yoy_growth_"),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    component = gsub("^yoy_growth_", "", component),
    transformation = "y-o-y log growth"
  )

gdp_long_all <- bind_rows(
  level_long,
  qtq_long,
  yoy_long
) %>%
  dplyr::left_join(component_labels, by = "component") %>%
  mutate(
    label = ifelse(is.na(label), component, label),
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
    ),
    period_2025Q2 = ifelse(post_2025Q2 == 1, "Post-2025Q2", "Pre-2025Q2")
  )

write.csv(
  gdp_long_all,
  "outputs_gdp_components/gdp_components_long_all_transformations.csv",
  row.names = FALSE
)


# ============================================================
# 7. Descriptive pre/post shift tables
# ============================================================

gdp_long_analysis <- gdp_long_all %>%
  filter(sample_analysis) %>%
  arrange(component, transformation, year_num, quarter_num)

pre_post_summary <- gdp_long_analysis %>%
  filter(!is.na(post_2025Q2)) %>%
  group_by(component, label, transformation, period_2025Q2) %>%
  summarise(
    observations = sum(!is.na(value)),
    mean = safe_mean(value),
    median = median(value, na.rm = TRUE),
    sd = safe_sd(value),
    min = safe_min(value),
    max = safe_max(value),
    .groups = "drop"
  )

pre_post_wide <- pre_post_summary %>%
  select(
    component,
    label,
    transformation,
    period_2025Q2,
    observations,
    mean,
    median,
    sd
  ) %>%
  pivot_wider(
    names_from = period_2025Q2,
    values_from = c(observations, mean, median, sd),
    names_sep = "_"
  ) %>%
  mutate(
    mean_shift_post_minus_pre = `mean_Post-2025Q2` - `mean_Pre-2025Q2`,
    median_shift_post_minus_pre = `median_Post-2025Q2` - `median_Pre-2025Q2`,
    mean_ratio_post_to_pre = `mean_Post-2025Q2` / `mean_Pre-2025Q2`
  ) %>%
  arrange(transformation, component)

write.csv(
  pre_post_summary,
  paste0(
    "outputs_gdp_components/gdp_components_pre_post_summary_long_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

write.csv(
  pre_post_wide,
  paste0(
    "outputs_gdp_components/gdp_components_pre_post_summary_wide_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

qtq_shift_table <- pre_post_wide %>%
  filter(transformation == "q-t-q value change") %>%
  mutate(
    `Pre mean` = fmt_num(`mean_Pre-2025Q2`, 1),
    `Post mean` = fmt_num(`mean_Post-2025Q2`, 1),
    `Post - pre` = fmt_num(mean_shift_post_minus_pre, 1),
    `Pre median` = fmt_num(`median_Pre-2025Q2`, 1),
    `Post median` = fmt_num(`median_Post-2025Q2`, 1),
    `Post / pre` = fmt_num(mean_ratio_post_to_pre, 2)
  ) %>%
  select(
    Component = label,
    `Pre obs.` = `observations_Pre-2025Q2`,
    `Post obs.` = `observations_Post-2025Q2`,
    `Pre mean`,
    `Post mean`,
    `Post - pre`,
    `Pre median`,
    `Post median`,
    `Post / pre`
  )

write_latex_table(
  qtq_shift_table,
  file = paste0(
    "tables_gdp_components/gdp_components_qtq_value_shift_pre_post_2025Q2_",
    analysis_sample_stub,
    ".tex"
  ),
  caption = paste0(
    "Pre- and post-2025Q2 comparison of q-t-q value changes in seasonally adjusted GDP and expenditure-side components. ",
    "The main analysis sample is ",
    analysis_sample_label,
    "."
  ),
  label = "tab:gdp_component_qtq_value_shift_pre_post",
  align = "lcccccccc"
)

cat("\n============================================================\n")
cat("Pre/post q-t-q value shift table\n")
cat("Sample:", analysis_sample_label, "\n")
cat("============================================================\n")
print(qtq_shift_table)


# ============================================================
# 8. Event-window table around 2025Q2
# ============================================================

event_window_table <- gdp_long_all %>%
  filter(quarter_year %in% event_window_quarters) %>%
  filter(transformation %in% c("Level", "q-t-q value change", "y-o-y log growth")) %>%
  arrange(component, transformation, year_num, quarter_num) %>%
  select(
    Component = label,
    Transformation = transformation,
    Quarter = quarter_year,
    Value = value
  )

write.csv(
  event_window_table,
  "outputs_gdp_components/gdp_components_event_window_2025Q2.csv",
  row.names = FALSE
)

event_window_qtq_table <- event_window_table %>%
  filter(Transformation == "q-t-q value change") %>%
  mutate(
    Value = fmt_num(Value, 1)
  ) %>%
  pivot_wider(
    names_from = Quarter,
    values_from = Value
  )

write_latex_table(
  event_window_qtq_table,
  file = "tables_gdp_components/gdp_components_qtq_value_change_event_window_2025Q2.tex",
  caption = "Event-window q-t-q value changes in seasonally adjusted GDP and expenditure-side components.",
  label = "tab:gdp_component_qtq_event_window",
  align = paste0("l", paste(rep("c", ncol(event_window_qtq_table) - 1), collapse = ""))
)

cat("\n============================================================\n")
cat("Event-window q-t-q value changes around 2025Q2\n")
cat("============================================================\n")
print(event_window_qtq_table)


# ============================================================
# 9. Known and unknown break-test functions
# ============================================================

run_break_tests_one_series <- function(data_one, break_quarter = "2025 Q2") {
  
  d <- data_one %>%
    filter(!is.na(value)) %>%
    arrange(year_num, quarter_num) %>%
    mutate(
      t = row_number(),
      quarter_year_char = as.character(quarter_year)
    )
  
  if (nrow(d) < 16) {
    return(NULL)
  }
  
  # ------------------------------------------------------------
  # Known break at 2025Q2
  # ------------------------------------------------------------
  
  break_obs <- which(d$quarter_year_char == break_quarter)
  
  if (length(break_obs) == 1) {
    
    d_known <- d %>%
      mutate(
        post = ifelse(t >= break_obs, 1, 0),
        time_after = ifelse(t >= break_obs, t - break_obs + 1, 0)
      )
    
    known_model <- lm(value ~ t + post + time_after, data = d_known)
    nw_lag <- min(3, max(0, floor(nobs(known_model) / 4)))
    
    nw_vcov <- NeweyWest(
      known_model,
      lag = nw_lag,
      prewhite = FALSE
    )
    
    nw_ct <- coeftest(
      known_model,
      vcov = nw_vcov
    )
    
    ct <- extract_coeftest(nw_ct)
    
    joint_test <- tryCatch(
      {
        waldtest(
          known_model,
          . ~ t,
          vcov = nw_vcov
        )
      },
      error = function(e) NULL
    )
    
    joint_df <- if (!is.null(joint_test)) as.data.frame(joint_test) else NULL
    
    joint_F <- if (!is.null(joint_df) && "F" %in% names(joint_df)) {
      joint_df$F[2]
    } else {
      NA_real_
    }
    
    joint_p <- if (!is.null(joint_df) && "Pr(>F)" %in% names(joint_df)) {
      joint_df$`Pr(>F)`[2]
    } else {
      NA_real_
    }
    
    known_results <- list(
      d_known = d_known,
      known_model = known_model,
      nw_ct = nw_ct,
      post_estimate = get_term_value(ct, "post", "estimate"),
      post_se = get_term_value(ct, "post", "std_error"),
      post_p_value = get_term_value(ct, "post", "p_value"),
      time_after_estimate = get_term_value(ct, "time_after", "estimate"),
      time_after_se = get_term_value(ct, "time_after", "std_error"),
      time_after_p_value = get_term_value(ct, "time_after", "p_value"),
      joint_F = joint_F,
      joint_p_value = joint_p,
      known_nobs = nobs(known_model)
    )
    
  } else {
    
    known_results <- list(
      d_known = d,
      known_model = NULL,
      nw_ct = NULL,
      post_estimate = NA_real_,
      post_se = NA_real_,
      post_p_value = NA_real_,
      time_after_estimate = NA_real_,
      time_after_se = NA_real_,
      time_after_p_value = NA_real_,
      joint_F = NA_real_,
      joint_p_value = NA_real_,
      known_nobs = nrow(d)
    )
  }
  
  # ------------------------------------------------------------
  # Unknown break
  # ------------------------------------------------------------
  
  unknown_results <- tryCatch(
    {
      fs <- Fstats(value ~ t, data = d, from = 0.15)
      supF_test <- sctest(fs)
      
      bp <- breakpoints(
        value ~ t,
        data = d,
        h = 0.15,
        breaks = 1
      )
      
      ci <- tryCatch(
        confint(bp),
        warning = function(w) NULL,
        error = function(e) NULL
      )
      
      selected_break <- breakpoints(bp)$breakpoints
      selected_break_quarter <- NA_character_
      
      if (!all(is.na(selected_break))) {
        selected_break_quarter <- d$quarter_year_char[selected_break[1]]
      }
      
      ci_low_q <- NA_character_
      ci_high_q <- NA_character_
      
      if (!is.null(ci) && !is.null(ci$confint)) {
        ci_low_idx <- ci$confint[1, 1]
        ci_high_idx <- ci$confint[1, 3]
        
        if (!is.na(ci_low_idx) && ci_low_idx >= 1 && ci_low_idx <= nrow(d)) {
          ci_low_q <- d$quarter_year_char[ci_low_idx]
        }
        
        if (!is.na(ci_high_idx) && ci_high_idx >= 1 && ci_high_idx <= nrow(d)) {
          ci_high_q <- d$quarter_year_char[ci_high_idx]
        }
      }
      
      fs_values <- as.numeric(fs$Fstats)
      
      n <- nrow(d)
      first_candidate <- floor(n * 0.15) + 1
      last_candidate  <- n - floor(n * 0.15)
      
      candidate_t <- round(seq(
        from = first_candidate,
        to = last_candidate,
        length.out = length(fs_values)
      ))
      
      fs_df <- data.frame(
        t = candidate_t,
        quarter_year = d$quarter_year_char[candidate_t],
        Fstat = fs_values
      )
      
      list(
        fs = fs,
        fs_df = fs_df,
        supF_test = supF_test,
        bp = bp,
        ci = ci,
        unknown_supF_statistic = as.numeric(supF_test$statistic),
        unknown_supF_p_value = as.numeric(supF_test$p.value),
        unknown_selected_break = selected_break_quarter,
        unknown_ci_lower = ci_low_q,
        unknown_ci_upper = ci_high_q
      )
    },
    error = function(e) {
      list(
        fs = NULL,
        fs_df = NULL,
        supF_test = NULL,
        bp = NULL,
        ci = NULL,
        unknown_supF_statistic = NA_real_,
        unknown_supF_p_value = NA_real_,
        unknown_selected_break = NA_character_,
        unknown_ci_lower = NA_character_,
        unknown_ci_upper = NA_character_
      )
    }
  )
  
  c(known_results, unknown_results)
}


# ============================================================
# 10. Run break tests on post-COVID analysis sample
# ============================================================

series_keys <- gdp_long_analysis %>%
  distinct(component, label, transformation) %>%
  arrange(transformation, component)

break_results <- list()
break_summary_rows <- list()
counter <- 1

cat("\n============================================================\n")
cat("Running GDP component break tests\n")
cat("Main analysis sample:", analysis_sample_label, "\n")
cat("Known break:", break_quarter, "\n")
cat("============================================================\n")

for (i in seq_len(nrow(series_keys))) {
  
  comp_i <- series_keys$component[i]
  label_i <- series_keys$label[i]
  trans_i <- series_keys$transformation[i]
  
  data_i <- gdp_long_analysis %>%
    filter(
      component == comp_i,
      transformation == trans_i
    ) %>%
    arrange(year_num, quarter_num)
  
  cat("\n------------------------------------------------------------\n")
  cat("Series:", label_i, "\n")
  cat("Transformation:", trans_i, "\n")
  cat("Sample:", analysis_sample_label, "\n")
  cat("------------------------------------------------------------\n")
  
  res_i <- run_break_tests_one_series(
    data_i,
    break_quarter = break_quarter
  )
  
  key_i <- paste(comp_i, safe_name(trans_i), sep = "__")
  break_results[[key_i]] <- res_i
  
  if (!is.null(res_i)) {
    
    if (print_regression_output) {
      
      cat("\nKnown-break model at", break_quarter, "\n")
      cat("Model: value = alpha + beta*t + delta*post + theta*time_after + error\n")
      
      if (!is.null(res_i$known_model)) {
        cat("\nOLS summary:\n")
        print(summary(res_i$known_model))
        
        cat("\nNewey-West coefficient test:\n")
        print(res_i$nw_ct)
        
        cat("\nKey known-break estimates:\n")
        cat("Post level shift:", res_i$post_estimate, "\n")
        cat("Post level shift p-value:", res_i$post_p_value, "\n")
        cat("Post trend change:", res_i$time_after_estimate, "\n")
        cat("Post trend change p-value:", res_i$time_after_p_value, "\n")
        cat("Joint p-value:", res_i$joint_p_value, "\n")
      } else {
        cat("Known-break model not estimated for this series.\n")
      }
      
      cat("\nUnknown-break test:\n")
      cat("Sup-F statistic:", res_i$unknown_supF_statistic, "\n")
      cat("Sup-F p-value:", res_i$unknown_supF_p_value, "\n")
      cat("Selected unknown break:", res_i$unknown_selected_break, "\n")
      cat("Confidence interval:", res_i$unknown_ci_lower, "to", res_i$unknown_ci_upper, "\n")
    }
    
    break_summary_rows[[counter]] <- data.frame(
      component = comp_i,
      label = label_i,
      transformation = trans_i,
      sample = analysis_sample_label,
      known_post_estimate = res_i$post_estimate,
      known_post_se = res_i$post_se,
      known_post_p_value = res_i$post_p_value,
      known_time_after_estimate = res_i$time_after_estimate,
      known_time_after_se = res_i$time_after_se,
      known_time_after_p_value = res_i$time_after_p_value,
      known_joint_F = res_i$joint_F,
      known_joint_p_value = res_i$joint_p_value,
      unknown_supF_statistic = res_i$unknown_supF_statistic,
      unknown_supF_p_value = res_i$unknown_supF_p_value,
      unknown_selected_break = res_i$unknown_selected_break,
      unknown_ci_lower = res_i$unknown_ci_lower,
      unknown_ci_upper = res_i$unknown_ci_upper,
      observations = res_i$known_nobs,
      stringsAsFactors = FALSE
    )
    
    counter <- counter + 1
  }
}

break_summary <- do.call(rbind, break_summary_rows)

write.csv(
  break_summary,
  paste0(
    "outputs_gdp_components/gdp_components_break_summary_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)


# ============================================================
# 10A. Known-break regression summary table
# ============================================================

known_break_regression_summary <- break_summary %>%
  mutate(
    post_shift_with_stars = paste0(
      fmt_num(known_post_estimate, 3),
      signif_stars(known_post_p_value)
    ),
    post_shift_se = paste0("(", fmt_num(known_post_se, 3), ")"),
    trend_change_with_stars = paste0(
      fmt_num(known_time_after_estimate, 3),
      signif_stars(known_time_after_p_value)
    ),
    trend_change_se = paste0("(", fmt_num(known_time_after_se, 3), ")"),
    joint_p_value_formatted = fmt_num(known_joint_p_value, 4)
  ) %>%
  select(
    component,
    label,
    transformation,
    sample,
    observations,
    known_post_estimate,
    known_post_se,
    known_post_p_value,
    known_time_after_estimate,
    known_time_after_se,
    known_time_after_p_value,
    known_joint_F,
    known_joint_p_value,
    post_shift_with_stars,
    post_shift_se,
    trend_change_with_stars,
    trend_change_se,
    joint_p_value_formatted
  )

write.csv(
  known_break_regression_summary,
  paste0(
    "outputs_gdp_components/gdp_components_known_break_regression_summary_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

known_break_regression_table <- known_break_regression_summary %>%
  select(
    Component = label,
    Transformation = transformation,
    Observations = observations,
    `Post-2025Q2 shift` = post_shift_with_stars,
    `NW SE, shift` = post_shift_se,
    `Post-2025Q2 trend change` = trend_change_with_stars,
    `NW SE, trend` = trend_change_se,
    `Joint p-value` = joint_p_value_formatted
  )

write_latex_table(
  known_break_regression_table,
  file = paste0(
    "tables_gdp_components/gdp_components_known_break_regression_summary_",
    analysis_sample_stub,
    ".tex"
  ),
  caption = paste0(
    "Known-break regression summary for seasonally adjusted GDP and expenditure-side components. ",
    "The candidate break is 2025Q2 and the main analysis sample is ",
    analysis_sample_label,
    ". The model is ",
    "$y_t = \\alpha + \\beta t + \\delta Post_t + \\theta TimeAfter_t + \\varepsilon_t$. ",
    "Newey-West standard errors are reported in parentheses. ",
    "*, **, and *** denote significance at the 10\\%, 5\\%, and 1\\% levels."
  ),
  label = "tab:gdp_component_known_break_regression_summary",
  align = "llcccccc"
)

cat("\n============================================================\n")
cat("Known-break regression summary table\n")
cat("Model: value = alpha + beta*t + delta*post + theta*time_after + error\n")
cat("Sample:", analysis_sample_label, "\n")
cat("Candidate break:", break_quarter, "\n")
cat("============================================================\n")
print(known_break_regression_table)


# ============================================================
# 11. Optional full-sample unknown break tests
# ============================================================

if (run_full_sample_unknown_breaks) {
  
  full_series_keys <- gdp_long_all %>%
    distinct(component, label, transformation) %>%
    arrange(transformation, component)
  
  full_unknown_rows <- list()
  full_counter <- 1
  
  cat("\n============================================================\n")
  cat("Running full-sample unknown-break tests\n")
  cat("These are robustness checks and may be dominated by COVID.\n")
  cat("============================================================\n")
  
  for (i in seq_len(nrow(full_series_keys))) {
    
    comp_i <- full_series_keys$component[i]
    label_i <- full_series_keys$label[i]
    trans_i <- full_series_keys$transformation[i]
    
    data_i <- gdp_long_all %>%
      filter(
        component == comp_i,
        transformation == trans_i
      ) %>%
      filter(!is.na(value)) %>%
      arrange(year_num, quarter_num) %>%
      mutate(t = row_number())
    
    if (nrow(data_i) < 16) next
    
    unknown_i <- tryCatch(
      {
        fs <- Fstats(value ~ t, data = data_i, from = 0.15)
        supF_test <- sctest(fs)
        bp <- breakpoints(value ~ t, data = data_i, h = 0.15, breaks = 1)
        selected_break <- breakpoints(bp)$breakpoints
        
        selected_break_quarter <- NA_character_
        if (!all(is.na(selected_break))) {
          selected_break_quarter <- as.character(data_i$quarter_year[selected_break[1]])
        }
        
        data.frame(
          component = comp_i,
          label = label_i,
          transformation = trans_i,
          sample = "Full sample",
          unknown_supF_statistic = as.numeric(supF_test$statistic),
          unknown_supF_p_value = as.numeric(supF_test$p.value),
          unknown_selected_break = selected_break_quarter,
          observations = nrow(data_i),
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        data.frame(
          component = comp_i,
          label = label_i,
          transformation = trans_i,
          sample = "Full sample",
          unknown_supF_statistic = NA_real_,
          unknown_supF_p_value = NA_real_,
          unknown_selected_break = NA_character_,
          observations = nrow(data_i),
          stringsAsFactors = FALSE
        )
      }
    )
    
    full_unknown_rows[[full_counter]] <- unknown_i
    
    if (print_regression_output) {
      cat("\nFull-sample unknown break |", label_i, "|", trans_i, "\n")
      cat("Sup-F statistic:", unknown_i$unknown_supF_statistic, "\n")
      cat("Sup-F p-value:", unknown_i$unknown_supF_p_value, "\n")
      cat("Selected unknown break:", unknown_i$unknown_selected_break, "\n")
    }
    
    full_counter <- full_counter + 1
  }
  
  full_unknown_summary <- do.call(rbind, full_unknown_rows)
  
  write.csv(
    full_unknown_summary,
    "outputs_gdp_components/gdp_components_unknown_break_summary_full_sample.csv",
    row.names = FALSE
  )
}


# ============================================================
# 12. LaTeX break-summary tables
# ============================================================

break_summary_table <- break_summary %>%
  mutate(
    `Post level shift` = paste0(
      fmt_num(known_post_estimate, 3),
      signif_stars(known_post_p_value)
    ),
    `NW SE, post` = paste0("(", fmt_num(known_post_se, 3), ")"),
    `Post trend change` = paste0(
      fmt_num(known_time_after_estimate, 3),
      signif_stars(known_time_after_p_value)
    ),
    `NW SE, trend` = paste0("(", fmt_num(known_time_after_se, 3), ")"),
    `Known joint p-value` = fmt_num(known_joint_p_value, 4),
    `Unknown Sup-F` = fmt_num(unknown_supF_statistic, 3),
    `Unknown p-value` = fmt_num(unknown_supF_p_value, 4)
  ) %>%
  select(
    Component = label,
    Transformation = transformation,
    `Post level shift`,
    `NW SE, post`,
    `Post trend change`,
    `NW SE, trend`,
    `Known joint p-value`,
    `Unknown Sup-F`,
    `Unknown p-value`,
    `Unknown break` = unknown_selected_break,
    Observations = observations
  )

write_latex_table(
  break_summary_table,
  file = paste0(
    "tables_gdp_components/gdp_components_break_summary_",
    analysis_sample_stub,
    ".tex"
  ),
  caption = paste0(
    "Known and unknown structural-break tests for seasonally adjusted GDP and expenditure-side components. ",
    "The main analysis sample is ",
    analysis_sample_label,
    ". Known-break estimates use 2025Q2 as the candidate break point. ",
    "Newey-West standard errors are reported in parentheses. ",
    "*, **, and *** denote significance at the 10\\%, 5\\%, and 1\\% levels."
  ),
  label = "tab:gdp_component_break_summary",
  align = "llccccccccc"
)

for (trans_i in unique(break_summary$transformation)) {
  
  tab_i <- break_summary %>%
    filter(transformation == trans_i) %>%
    mutate(
      `Post shift` = paste0(
        fmt_num(known_post_estimate, 3),
        signif_stars(known_post_p_value)
      ),
      `SE` = paste0("(", fmt_num(known_post_se, 3), ")"),
      `Slope change` = paste0(
        fmt_num(known_time_after_estimate, 3),
        signif_stars(known_time_after_p_value)
      ),
      `Slope SE` = paste0("(", fmt_num(known_time_after_se, 3), ")"),
      `Joint p` = fmt_num(known_joint_p_value, 4),
      `Sup-F` = fmt_num(unknown_supF_statistic, 3),
      `Sup-F p` = fmt_num(unknown_supF_p_value, 4)
    ) %>%
    select(
      Component = label,
      `Post shift`,
      SE,
      `Slope change`,
      `Slope SE`,
      `Joint p`,
      `Sup-F`,
      `Sup-F p`,
      `Unknown break` = unknown_selected_break
    )
  
  file_stub <- safe_name(tolower(trans_i))
  
  write_latex_table(
    tab_i,
    file = paste0(
      "tables_gdp_components/gdp_components_break_summary_",
      file_stub,
      "_",
      analysis_sample_stub,
      ".tex"
    ),
    caption = paste0(
      "Structural-break tests for ",
      trans_i,
      " of seasonally adjusted GDP and expenditure-side components. ",
      "The main analysis sample is ",
      analysis_sample_label,
      "."
    ),
    label = paste0("tab:gdp_component_break_", file_stub),
    align = "lcccccccc"
  )
}


# ============================================================
# 13. Figures: component series and break markers
# ============================================================

plot_component_series <- function(data_plot, comp_i, trans_i) {
  
  d <- data_plot %>%
    filter(
      component == comp_i,
      transformation == trans_i,
      !is.na(value)
    ) %>%
    arrange(year_num, quarter_num) %>%
    mutate(
      quarter_year_char = as.character(quarter_year),
      quarter_year_factor = factor(quarter_year_char, levels = unique(quarter_year_char))
    )
  
  if (nrow(d) == 0) return(NULL)
  
  break_x <- which(levels(d$quarter_year_factor) == break_quarter)
  
  p <- ggplot(
    d,
    aes(
      x = quarter_year_factor,
      y = value,
      group = 1
    )
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.1) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Quarter",
      y = trans_i
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
        linewidth = 0.9
      )
  }
  
  return(p)
}

for (comp_i in required_level_vars) {
  for (trans_i in c("Level", "q-t-q value change", "y-o-y log growth")) {
    
    p_i <- plot_component_series(
      data_plot = gdp_long_analysis,
      comp_i = comp_i,
      trans_i = trans_i
    )
    
    if (!is.null(p_i)) {
      
      file_stub <- paste0(
        safe_name(comp_i),
        "_",
        safe_name(tolower(trans_i))
      )
      
      print(p_i)
      
      ggsave(
        filename = paste0(
          "figures_gdp_components/gdp_component_",
          file_stub,
          "_",
          analysis_sample_stub,
          ".png"
        ),
        plot = p_i,
        width = 9,
        height = 4.8,
        dpi = 300
      )
    }
  }
}

for (trans_i in c("Level", "q-t-q value change", "y-o-y log growth")) {
  
  d_trans <- gdp_long_analysis %>%
    filter(
      transformation == trans_i,
      !is.na(value)
    ) %>%
    arrange(year_num, quarter_num) %>%
    mutate(
      quarter_year_char = as.character(quarter_year)
    )
  
  quarter_levels <- d_trans %>%
    distinct(quarter_year_char, year_num, quarter_num) %>%
    arrange(year_num, quarter_num) %>%
    pull(quarter_year_char)
  
  d_trans <- d_trans %>%
    mutate(
      quarter_year_factor = factor(quarter_year_char, levels = quarter_levels)
    )
  
  break_x <- which(quarter_levels == break_quarter)
  
  p_trans <- ggplot(
    d_trans,
    aes(
      x = quarter_year_factor,
      y = value,
      group = 1
    )
  ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 0.8) +
    facet_wrap(~ label, scales = "free_y", ncol = 2) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Quarter",
      y = trans_i
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 16, face = "bold"),  
      axis.title.y = element_text(size = 16, face = "bold"),  
      strip.text = element_text(face = "bold", size = 15),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(labels = scales::comma)   
  
  if (length(break_x) == 1) {
    p_trans <- p_trans +
      geom_vline(
        xintercept = break_x,
        linetype = "dotted",
        linewidth = 0.8
      )
  }
  
  print(p_trans)
  
  ggsave(
    filename = paste0(
      "figures_gdp_components/gdp_components_faceted_",
      safe_name(tolower(trans_i)),
      "_",
      analysis_sample_stub,
      ".png"
    ),
    plot = p_trans,
    width = 10,
    height = 8,
    dpi = 300
  )
}


# ============================================================
# 14. Figures: event-window q-t-q value changes
# ============================================================

event_qtq_plot_data <- gdp_long_all %>%
  filter(
    transformation == "q-t-q value change",
    quarter_year %in% event_window_quarters,
    !is.na(value)
  ) %>%
  arrange(year_num, quarter_num) %>%
  mutate(
    quarter_year_char = as.character(quarter_year)
  )

event_quarter_levels <- event_qtq_plot_data %>%
  distinct(quarter_year_char, year_num, quarter_num) %>%
  arrange(year_num, quarter_num) %>%
  pull(quarter_year_char)

event_qtq_plot_data <- event_qtq_plot_data %>%
  mutate(
    quarter_year_factor = factor(quarter_year_char, levels = event_quarter_levels)
  )

event_break_x <- which(event_quarter_levels == break_quarter)

p_event_qtq <- ggplot(
  event_qtq_plot_data,
  aes(
    x = quarter_year_factor,
    y = value,
    group = 1
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.3) +
  facet_wrap(~ label, scales = "free_y", ncol = 2) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Quarter",
    y = "q-t-q value change"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

if (length(event_break_x) == 1) {
  p_event_qtq <- p_event_qtq +
    geom_vline(
      xintercept = event_break_x,
      linetype = "dotted",
      linewidth = 0.8
    )
}

print(p_event_qtq)

ggsave(
  filename = "figures_gdp_components/gdp_components_event_window_qtq_value_change.png",
  plot = p_event_qtq,
  width = 10,
  height = 8,
  dpi = 300
)


# ============================================================
# 15. Figures: F-statistics for unknown breaks
# ============================================================

for (nm in names(break_results)) {
  
  res_i <- break_results[[nm]]
  if (is.null(res_i)) next
  if (is.null(res_i$fs_df)) next
  
  d_fs <- res_i$fs_df %>%
    mutate(
      quarter_year_factor = factor(quarter_year, levels = unique(quarter_year))
    )
  
  p_fs <- ggplot(
    d_fs,
    aes(
      x = quarter_year_factor,
      y = Fstat,
      group = 1
    )
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 0.8) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Candidate break date",
      y = "F-statistic"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(p_fs)
  
  ggsave(
    filename = paste0(
      "figures_gdp_components/gdp_component_fstats_",
      safe_name(nm),
      "_",
      analysis_sample_stub,
      ".png"
    ),
    plot = p_fs,
    width = 9,
    height = 4.8,
    dpi = 300
  )
}


# ============================================================
# 16. Save R objects
# ============================================================

saveRDS(
  list(
    gdp_components = gdp_components,
    gdp_long_all = gdp_long_all,
    gdp_long_analysis = gdp_long_analysis,
    analysis_sample_label = analysis_sample_label,
    analysis_sample_stub = analysis_sample_stub,
    pre_post_summary = pre_post_summary,
    pre_post_wide = pre_post_wide,
    break_summary = break_summary,
    known_break_regression_summary = known_break_regression_summary,
    known_break_regression_table = known_break_regression_table,
    break_results = break_results,
    event_window_table = event_window_table,
    event_window_qtq_table = event_window_qtq_table
  ),
  paste0(
    "outputs_gdp_components/gdp_components_break_analysis_objects_",
    analysis_sample_stub,
    ".rds"
  )
)

cat("\n============================================================\n")
cat("GDP component shift and break analysis completed.\n")
cat("Main analysis sample:", analysis_sample_label, "\n")
cat("Known break:", break_quarter, "\n")
cat("Outputs saved in:\n")
cat("  outputs_gdp_components/\n")
cat("  tables_gdp_components/\n")
cat("  figures_gdp_components/\n")
cat("============================================================\n")


# ============================================================
# 17. Final summary and conclusion
# ============================================================

# This section avoids print(n = Inf, width = Inf), because base data.frame
# printing can interpret n = Inf as an invalid na.print argument.

print_console_table <- function(x) {
  x_df <- as.data.frame(x, stringsAsFactors = FALSE)
  print(x_df, row.names = FALSE)
}

cat("\n\n")
cat("============================================================\n")
cat("FINAL SUMMARY: GDP COMPONENT SHIFT AND BREAK EVIDENCE\n")
cat("============================================================\n")
cat("Main analysis sample:", analysis_sample_label, "\n")
cat("Candidate break:", break_quarter, "\n")
cat("Transformation of main interest: q-t-q value change\n")
cat("============================================================\n\n")


# ------------------------------------------------------------
# 17.1 Summary of q-t-q value shifts
# ------------------------------------------------------------

qtq_final_summary <- pre_post_wide %>%
  dplyr::filter(transformation == "q-t-q value change") %>%
  dplyr::mutate(
    abs_shift = abs(mean_shift_post_minus_pre),
    pre_mean_num = `mean_Pre-2025Q2`,
    post_mean_num = `mean_Post-2025Q2`,
    shift_num = mean_shift_post_minus_pre,
    ratio_num = mean_ratio_post_to_pre
  ) %>%
  dplyr::arrange(dplyr::desc(abs_shift)) %>%
  dplyr::select(
    Component = label,
    `Pre-2025Q2 mean q-t-q change` = pre_mean_num,
    `Post-2025Q2 mean q-t-q change` = post_mean_num,
    `Post - pre shift` = shift_num,
    `Post / pre ratio` = ratio_num,
    `Pre observations` = `observations_Pre-2025Q2`,
    `Post observations` = `observations_Post-2025Q2`
  )

cat("1. Average q-t-q value changes before and after 2025Q2\n")
cat("------------------------------------------------------------\n")

qtq_final_summary_print <- qtq_final_summary %>%
  dplyr::mutate(
    `Pre-2025Q2 mean q-t-q change` = round(`Pre-2025Q2 mean q-t-q change`, 1),
    `Post-2025Q2 mean q-t-q change` = round(`Post-2025Q2 mean q-t-q change`, 1),
    `Post - pre shift` = round(`Post - pre shift`, 1),
    `Post / pre ratio` = round(`Post / pre ratio`, 2)
  )

print_console_table(qtq_final_summary_print)


# ------------------------------------------------------------
# 17.2 Known-break regression summary for q-t-q value changes
# ------------------------------------------------------------

known_qtq_final_summary <- break_summary %>%
  dplyr::filter(transformation == "q-t-q value change") %>%
  dplyr::mutate(
    post_shift_sig = dplyr::case_when(
      known_post_p_value < 0.01 ~ "***",
      known_post_p_value < 0.05 ~ "**",
      known_post_p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    trend_change_sig = dplyr::case_when(
      known_time_after_p_value < 0.01 ~ "***",
      known_time_after_p_value < 0.05 ~ "**",
      known_time_after_p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    joint_sig = dplyr::case_when(
      known_joint_p_value < 0.01 ~ "***",
      known_joint_p_value < 0.05 ~ "**",
      known_joint_p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  dplyr::arrange(known_joint_p_value) %>%
  dplyr::select(
    Component = label,
    `Post shift` = known_post_estimate,
    `Post shift p-value` = known_post_p_value,
    `Post shift sig.` = post_shift_sig,
    `Trend change` = known_time_after_estimate,
    `Trend change p-value` = known_time_after_p_value,
    `Trend sig.` = trend_change_sig,
    `Joint p-value` = known_joint_p_value,
    `Joint sig.` = joint_sig,
    `Unknown break` = unknown_selected_break
  )

cat("\n\n")
cat("2. Known-break regression results for q-t-q value changes\n")
cat("------------------------------------------------------------\n")
cat("Model: value = alpha + beta*t + delta*post + theta*time_after + error\n")
cat("Stars: *** p<0.01, ** p<0.05, * p<0.10\n\n")

known_qtq_final_summary_print <- known_qtq_final_summary %>%
  dplyr::mutate(
    `Post shift` = round(`Post shift`, 3),
    `Post shift p-value` = round(`Post shift p-value`, 4),
    `Trend change` = round(`Trend change`, 3),
    `Trend change p-value` = round(`Trend change p-value`, 4),
    `Joint p-value` = round(`Joint p-value`, 4)
  )

print_console_table(known_qtq_final_summary_print)


# ------------------------------------------------------------
# 17.3 Identify components with strongest evidence of shift
# ------------------------------------------------------------

shift_threshold <- median(abs(qtq_final_summary$`Post - pre shift`), na.rm = TRUE)

qtq_evidence_summary <- qtq_final_summary %>%
  dplyr::left_join(
    known_qtq_final_summary %>%
      dplyr::select(
        Component,
        `Post shift p-value`,
        `Trend change p-value`,
        `Joint p-value`,
        `Unknown break`
      ),
    by = "Component"
  ) %>%
  dplyr::mutate(
    evidence_flag = dplyr::case_when(
      `Joint p-value` < 0.05 &
        abs(`Post - pre shift`) > shift_threshold ~ "Strong",
      `Joint p-value` < 0.10 |
        abs(`Post - pre shift`) > shift_threshold ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  dplyr::arrange(
    factor(evidence_flag, levels = c("Strong", "Moderate", "Weak")),
    dplyr::desc(abs(`Post - pre shift`))
  )

cat("\n\n")
cat("3. Components ranked by descriptive and regression evidence\n")
cat("------------------------------------------------------------\n")

qtq_evidence_summary_print <- qtq_evidence_summary %>%
  dplyr::mutate(
    `Pre-2025Q2 mean q-t-q change` = round(`Pre-2025Q2 mean q-t-q change`, 1),
    `Post-2025Q2 mean q-t-q change` = round(`Post-2025Q2 mean q-t-q change`, 1),
    `Post - pre shift` = round(`Post - pre shift`, 1),
    `Post / pre ratio` = round(`Post / pre ratio`, 2),
    `Post shift p-value` = round(`Post shift p-value`, 4),
    `Trend change p-value` = round(`Trend change p-value`, 4),
    `Joint p-value` = round(`Joint p-value`, 4)
  )

print_console_table(qtq_evidence_summary_print)


# ------------------------------------------------------------
# 17.4 Save final summary tables
# ------------------------------------------------------------

write.csv(
  qtq_final_summary,
  paste0(
    "outputs_gdp_components/gdp_components_final_qtq_shift_summary_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

write.csv(
  known_qtq_final_summary,
  paste0(
    "outputs_gdp_components/gdp_components_final_known_break_qtq_summary_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

write.csv(
  qtq_evidence_summary,
  paste0(
    "outputs_gdp_components/gdp_components_final_evidence_ranking_",
    analysis_sample_stub,
    ".csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 17.5 Automatic conclusion text
# ------------------------------------------------------------

strong_components <- qtq_evidence_summary %>%
  dplyr::filter(evidence_flag == "Strong") %>%
  dplyr::pull(Component)

moderate_components <- qtq_evidence_summary %>%
  dplyr::filter(evidence_flag == "Moderate") %>%
  dplyr::pull(Component)

gdp_row <- qtq_evidence_summary %>%
  dplyr::filter(Component == "GDP")

cat("============================================================\n")
cat("END OF FINAL SUMMARY\n")
cat("============================================================\n")