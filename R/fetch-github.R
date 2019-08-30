github_headers <- function(board) {
  httr::add_headers(Authorization = paste("token", Sys.getenv("GITHUB_PAT")))
}

github_fetch_urls <- function(term, board) {
  message("Processing ", term)

  response <- httr::GET(paste0("https://api.github.com/search/code?q=extension%3ARmd+", term, "&per_page=100"), github_headers())
  headers <- httr::headers(response)

  page_start <- 1
  page_end <- 1

  if (!is.null(headers$link)) {
    matches <- regexec("page=([0-9]+).*page=([0-9]+)", headers$link)
    matches <- regmatches(headers$link, matches)
    if (length(matches[[1]]) == 3) {
      page_end <- as.integer(matches[[1]][3])
    }
  }

  urls <- c()

  for (page_current in page_start:page_end) {
    max_poll <- 60 * 60 / as.integer(response$headers$`x-ratelimit-limit`)
    message("Processing page ", page_current, "/", page_end, " wait (", max_poll, "s)", " rates (",
            response$headers$`x-ratelimit-limit`, ",",
            response$headers$`x-ratelimit-remaining`, ",",
            response$headers$`x-ratelimit-reset`, ")")

    search_url <- paste0("https://api.github.com/search/code?q=extension%3ARmd+", term, "&page=", page_current, "&per_page=100")
    response <- httr::GET(search_url, github_headers())
    if (httr::http_error(response)) stop(httr::content(response))

    content <- httr::content(response)
    urls <- c(urls, sapply(content$items, function(e) e$url))

    Sys.sleep(max_poll)

    if (response$headers$`x-ratelimit-remaining` <= 0) {
      wait_secs <- response$headers$`x-ratelimit-reset` - as.integere(Sys.time())
      message("Ratelimit exceeded (", wait_secs, "s), waiting: ", appendLF = FALSE)
      while (as.integere(Sys.time()) <= response$headers$`x-ratelimit-reset`) {
        Sys.sleep(1)
        message(".", appendLF = FALSE)
      }
      message("Done!")
    }
  }

  github_update_urls(term, urls, board)
}

github_update_urls <- function(term, urls, board) {
  index_new <- data.frame(source = "github",
                          search = term,
                          url = urls,
                          stringsAsFactors = FALSE)

  index_old <- NULL
  if ("urls" %in% pins::pin_find(board = board)$name) {
    index_old <- pins::pin_get("urls", board = board)
  }

  index_new <- unique(rbind(index_old, index_new))

  pins::pin(index_new, "urls", board = board)
}
