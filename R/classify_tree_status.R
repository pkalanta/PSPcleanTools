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

