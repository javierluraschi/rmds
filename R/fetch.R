#' Feetch Rmds
#'
#' Fetches Rmds from various public locations and stores results using the
#' \code{pins} package under an \code{rmds} board.
#'
#' @export
rmds_fetch <- function(term = "sparklyr") {
  if (!"rmds" %in% pins::board_list())
    pins::board_register("github", "rmds", repo = "javierluraschi/rmds", branch = "datasets")

  github_fetch_urls(term)
}
