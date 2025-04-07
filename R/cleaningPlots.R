#'
#' Finding where multiple PSPs are associated with the same treenum in the same Plot
#'
#' @param Trees a data.table of individual tree measurements within species (PSPs)
#'
#' @return a list containing corrected PSP assignments
#' @export
#' @importFrom data.table data.table
#' @importFrom dplyr filter mutate group_by summarise arrange select left_join ungroup case_when n_distinct
#' @importFrom magrittr %>%
#'

treenum_to_multiplePSP <- function(Trees) {
  incorrect_trees <- Trees %>%
    group_by(Plot, treenum) %>%
    filter(n_distinct(PSP) > 1) %>%
    ungroup()

  correct_psp <- incorrect_trees %>%
    group_by(Plot, treenum, PSP, Latin_full) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count)) %>%
    group_by(Plot, treenum) %>%
    slice_max(count, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(Plot, treenum, PSP, Latin_full)

  trees_corrected <- Trees %>%
    left_join(correct_psp, by = c("Plot", "treenum")) %>%
    mutate(PSP = coalesce(PSP.y, PSP.x),
           Latin_full = coalesce(Latin_full.y, Latin_full.x)) %>%
    select(-PSP.x, -PSP.y, -Latin_full.x, -Latin_full.y)

  return(list(incorrect_trees = incorrect_trees,
              correct_psp = correct_psp,
              PSP_TREE_YIMO_corrected = trees_corrected))
}

#'
#' processing problems with dbh in PSP data
#'
#' @param Trees a data.table of individual tree measurements within species (PSPs)
#' @param plots a vector of plot IDs
#'
#' @return a list of standardized plot and tree data.tables
#' @export
#' @importFrom data.table data.table
#' @importFrom dplyr filter mutate group_by summarise arrange select left_join ungroup case_when
#' @importFrom magrittr %>%
#'

process_dbh_issues <- function(Trees, plots) {
  Trees <- Trees %>% filter(Plot %in% plots)

  Trees <- Trees %>%
    arrange(Plot, treenum, MeasYr) %>%
    group_by(Plot, treenum) %>%
    mutate(diff_dbh = DBH - lag(DBH))

  negative_growth <- Trees %>% filter(diff_dbh < 0)
  na_values <- Trees %>% filter(is.na(diff_dbh))

  Trees <- Trees %>%
    mutate(DBH = ifelse(is.na(DBH) | DBH == 0, -1, DBH))

  dbh_issues <- Trees %>%
    filter(diff_dbh < 0) %>%
    select(PSP, Plot, RemeasID, treenum, DBH, MeasYr, diff_dbh, MeasNum) %>%
    arrange(Plot, treenum, MeasYr)

  dbh_check <- dbh_issues %>%
    left_join(Trees %>%
                select(PSP, Plot, RemeasID, treenum, MeasYr, Latin_full, cause, agecl, MeasNum),
              by = c("PSP", "Plot", "RemeasID", "treenum", "MeasYr"))

  write.csv(dbh_check, "dbh_issues.csv", row.names = FALSE)

  return(list(processed_data = Trees, dbh_check = dbh_check))
}

#' classify trees according to their status (living, dead or regenerated)
#'
#' @param Trees a data.table of individual tree measurements within species (PSPs)
#'
#' @return a list of trees categorized as Regeneration, Mortality, or Alive
#' @export
#' @importFrom data.table data.table
#' @importFrom dplyr filter mutate group_by summarise left_join ungroup case_when
#' @importFrom magrittr %>%
#'

classify_tree_status <- function(Trees) {
  measyr_interval_per_plot <- Trees %>%
    group_by(Plot) %>%
    summarise(first_plot_year = min(MeasYr, na.rm = TRUE),
              last_plot_year = max(MeasYr, na.rm = TRUE))

  measyr_interval_per_treenum <- Trees %>%
    group_by(Plot, treenum) %>%
    summarise(first_tree_year = min(MeasYr, na.rm = TRUE),
              last_tree_year = max(MeasYr, na.rm = TRUE)) %>%
    ungroup()

  measyr_interval_per_treenum <- measyr_interval_per_treenum %>%
    left_join(measyr_interval_per_plot, by = "Plot")

  Trees <- Trees %>%
    left_join(measyr_interval_per_treenum, by = c("Plot", "treenum"))

  Trees <- Trees %>%
    mutate(status = case_when(
      MeasYr == first_tree_year & first_tree_year > first_plot_year ~ "Regeneration",
      MeasYr == last_tree_year & last_tree_year < last_plot_year ~ "Mortality",
      MeasYr > first_tree_year & MeasYr < last_tree_year ~ "Consistent",
      TRUE ~ "Survival"
    ))

  Regen <- Trees %>% filter(status == "Regeneration")
  Mortality <- Trees %>% filter(status == "Mortality")
  Alive <- Trees %>% filter(status %in% c("Consistent", "Survival"))

  write.csv(Trees, "Status of trees due to be Alive, Mortality and Regeneration", row.names = FALSE)

  return(list(Trees = Trees, Regen = Regen, Mortality = Mortality, Alive = Alive))
}

