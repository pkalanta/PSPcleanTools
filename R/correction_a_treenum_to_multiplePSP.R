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
