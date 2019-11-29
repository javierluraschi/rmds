#' Serch Terms
#'
#' Relevant search terms to use when searching for Rmds.
#'
#' @export
rmds_terms <- function() {
  c("tensorflow", "sparklyr", "keras", "dplyr", "tidyverse",
    "readr", "caret", "DBI", "odbc", "foreign", "tidyr", "lubridte",
    "ggplot2", "car", "mgcv", "glmnet", "survival",
    "shiny", "zoo", "data.table", "parallel", "jsonlite",
    "httr")
}

#' Initialize Rmds
#'
#' Initializes the boards required to fetch and process Rmds.
#'
#' @export
rmds_init <- function(board = "rmds") {
  if (!board %in% pins::board_list())
    pins::board_register("github", board, repo = "javierluraschi/rmds", branch = "datasets")
}

rmds_validate <- function(board) {
  if (!board %in% pins::board_list()) stop("Boards not initialized run rmds_init()")
}

#' Fetch Rmds
#'
#' Fetches Rmds from various public locations and stores results using the
#' \code{pins} package under an \code{rmds} board.
#'
#' @export
rmds_fetch <- function(terms = rmds_terms(), board = "rmds") {
  rmds_validate(board)
  searched <- tryCatch(unique(pin_get("urls", board = board)$search), error = function(e) "")

  for (term in terms) {
    if (term %in% searched) next
    github_fetch_urls(term, board)
  }
}

#' Post Process Rmds
#'
#' Process' urls stored int he "urls" pin and generatese a "rmds" pin with actual code.
#'
#' @export
rmds_process <- function(board = "rmds") {
  rmds_validate(board)
  urls <- pin_get("urls", board = board)
  rmds <- tryCatch(unique(pin_get("rmds", board = board)), error = function(e) {
    data.frame(url = character(), code = character(), source = character(), stringsAsFactors = FALSE) })

  for (idx in 1:nrow(urls)) {
    if (idx %% 10 == 0) message("Processing ", idx, "/", nrow(urls))
    if (idx %% 1000 == 0) pins::pin(rmds, "rmds", board = board)

    current <- urls[idx,]
    if (current$url %in% rmds$url) next
    url <- current$url

    url <- structure(url, class = current$source)
    result <- process_rmd(url)

    rmds <- rbind(rmds, result)
  }

  pins::pin(rmds, "rmds", board = board)

  rmds
}

process_rmd <- function(url) {
  UseMethod("process_rmd")
}
