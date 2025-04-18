#' Process Implausible DBH Changes Across Measurement Years
#'
#' @description
#' Detects and manages inconsistencies in tree diameter (DBH) measurements across years within PSPs.
#' The function flags implausible negative growth, cleans the dataset accordingly, and excludes plots with a high proportion of anomalies.
#' This helps improve the reliability of forest dynamics analysis.
#'
#' @param Trees A `data.table` of tree measurements over time within PSPs.
#'
#' @return A cleaned `data.table` with problematic entries removed or flagged.
#'
#' @export
#'
#' @import data.table
#' @importFrom dplyr group_by summarise filter mutate case_when
#' @importFrom magrittr %>%
#`
process_dbh_issues <- function(Trees, plots) {    # two arguments: Trees: a data frame containing tree measurements over time, and plots: a list of plot identifiers to focus the processing on.
  Trees <- Trees %>% filter(Plot %in% plots)      # Filters the input data to include only the plots listed in plots, narrowing down the analysis to relevant data.

  # Sorts the data chronologically by plot, tree ID (treenum), and measurement year (MeasYr).
  # Within each tree’s record across years, calculates the difference in DBH (Diameter at Breast Height) between successive measurements to evaluate growth
  Trees <- Trees %>%
    arrange(Plot, treenum, MeasYr) %>%
    group_by(Plot, treenum) %>%
    mutate(diff_dbh = DBH - lag(DBH))  # # A negative value indicates a reduction in DBH, which is biologically implausible in most cases and may indicate a measurement error.

  # Creates two diagnostic subsets:negative_growth and na_values
  negative_growth <- Trees %>% filter(diff_dbh < -0.5)  # trees showing suspicious negative growth greater than 0.5 cm (used as a threshold for likely error).
  na_values <- Trees %>% filter(is.na(diff_dbh))        # records where diff_dbh is NA (typically the first measurement for a tree).

  Trees <- Trees %>%
    mutate(DBH = ifelse(is.na(DBH) | DBH == 0, -1, DBH))   # Replaces missing (NA) or zero DBH values with -1 to flag them clearly as invalid values.

  dbh_issues <- Trees %>%
    filter(diff_dbh < 0) %>%                                                       # Filters out all records with negative growth for inspection.
    select(PSP, Plot, RemeasID, treenum, DBH, MeasYr, diff_dbh, MeasNum) %>%       # Selects only relevant columns and arranges them for easy review.
    arrange(Plot, treenum, MeasYr)

  dbh_check <- dbh_issues %>%
    left_join(Trees %>%   #   Joins dbh_issues with additional metadata (e.g., species name, cause of mortality, age class) to assist in diagnosing potential causes of measurement error.
                select(PSP, Plot, RemeasID, treenum, MeasYr, Latin_full, cause, agecl, MeasNum),
              by = c("PSP", "Plot", "RemeasID", "treenum", "MeasYr"))

  # 📊 Calculate total negative growth percentage per plot
  negative_growth_summary <- Trees %>%        # Computes a summary statistic per plot:
    filter(!is.na(diff_dbh)) %>%
    group_by(Plot) %>%
    summarise(
      total_neg_growth = sum(diff_dbh[diff_dbh < 0], na.rm = TRUE),                  # the sum of all negative DBH changes.
      total_growth = sum(abs(diff_dbh), na.rm = TRUE),                               # the sum of absolute DBH changes (positive and negative).
      neg_growth_pct = ifelse(total_growth > 0, 100 * abs(total_neg_growth) / total_growth, NA_real_) #  the percentage of negative growth relative to total measured change
    ) %>%
    arrange(Plot)

  # Exports both the detailed issue log and the summary report to CSV files for further inspection or documentation
  write.csv(dbh_check, "dbh_issues.csv", row.names = FALSE)
  write.csv(negative_growth_summary, "negative_growth_summary.csv", row.names = FALSE)

  # ❌ Remove plots with more than 5% negative growth
  plots_to_keep <- negative_growth_summary %>%
    filter(neg_growth_pct <= 5 | is.na(neg_growth_pct)) %>% # Keeps only plots where the negative DBH growth percentage is ≤ 5%, or missing (i.e., plots with no DBH change data).
    pull(Plot)

  Trees <- Trees %>% filter(Plot %in% plots_to_keep) # Removes plots with >5% negative DBH change from the final processed_data.

  # Returns a list containing:
  return(list(
    processed_data = Trees,      # the cleaned and filtered tree dataset.
    dbh_check = dbh_check,       # the table of detected DBH inconsistencies.
    negative_growth_summary = negative_growth_summary     # plot-level statistics on negative growth.
  ))
}




