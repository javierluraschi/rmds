#' Serch Terms
#'
#' Relevant search terms to use when searching for Rmds.
#'
#' @export
rmds_terms <- function() {
  c("keras", "dplyr", "tidyverse", "readr", "caret",
    "DBI", "odbc", "foreign", "tidyr", "lubridte",
    "ggplot2", "car", "mgcv", "glmnet", "survival",
    "shiny", "zoo", "data.table", "parallel", "jsonlite",
    "httr")
}

#' Fetch Rmds
#'
#' Fetches Rmds from various public locations and stores results using the
#' \code{pins} package under an \code{rmds} board.
#'
#' @export
rmds_fetch <- function(terms = rmds_terms(), board = "rmds") {
  if (!board %in% pins::board_list())
    pins::board_register("github", board, repo = "javierluraschi/rmds", branch = "datasets")

  for (term in terms) {
    github_fetch_urls(term, board)
  }
}
