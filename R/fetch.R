#' Feetch Rmds
#'
#' Fetches Rmds from various public locations and stores results using the
#' \code{pins} package under an \code{rmds} board.
#'
#' @export
rmds_fetch <- function(terms = "sparklyr", board = "rmds") {
  if (!board %in% pins::board_list())
    pins::board_register("github", board, repo = "javierluraschi/rmds", branch = "datasets")

  for (term in terms) {
    github_fetch_urls(term, board)
  }
}
