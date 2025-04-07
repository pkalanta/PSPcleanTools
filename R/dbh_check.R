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
