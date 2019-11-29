# Provides support to find and index CRAN resources, supports running
# locally or in Spark clusters.
#
# Local:
#   cran_index <- cran_find_local()
#
# Cluster:
#   library(sparklyr)
#   sc <- spark_connect(master = "yarn", config = cran_find_config(10))
#
#   data <- cran_find_resources(sc, 10^2, 10^1)
#   data <- cran_find_resources(sc, 10^3, 10^2)
#   data <- cran_find_resources(sc, 10^5, 10^4)
#
#   cran_index <- data %>% collect()
#
# Saving:
#   cran_save_dataset(cran_index)
#   cran_clean_dataset()

cran_process_rd <- function(rd_data, collect = FALSE) {
  if ("Rd_tag" %in% names(attributes(rd_data)) && identical(attributes(rd_data)[["Rd_tag"]], "\\examples")) {
    collect <- TRUE
  }

  if (collect && "RCODE" %in% attributes(rd_data)) {
    as.character(rd_data)
  }
  else if (is.list(rd_data)) {
    paste(lapply(rd_data, function(e) cran_process_rd(e, collect = collect)), collapse = "")
  }
  else {
    ""
  }
}

cran_process_package <- function(package) {
  results <- data.frame(name = c(), description = c(), rows = c(), cols = c(), class = c())

  if (!dir.exists(file.path("packages", package))) {
    utils::download.packages(package, "packages", repos = "https://cran.rstudio.com/")

    tar <- dir("packages", pattern = "*.tar.gz", full.names = TRUE)[1]
    utils::untar(tar, exdir = "packages")

    unlink("packages/*.gz")
  }

  package_path <- file.path("packages", package)
  docs_paths <- dir(file.path(package_path, "man"), full.names = TRUE)
  for (docs_path in docs_paths) {
    new_result <- tryCatch({
      rd_data <- tools::parse_Rd(docs_path)
      code <- cran_process_rd(rd_data)
      split <- strsplit(code, "\n")[[1]]
      only_code <- gsub(" +", "", Filter(function(e) !grepl("^ ?#.*", e), split))
      only_code[!grepl("^$", only_code)]
    }, error = function(e) {
      ""
    })

    results <- paste(
      results,
      new_result,
      sep = ""
    )
  }

  paste(
    "library(",
    package,
    ")\n",
    results,
    sep = ""
  )
}

cran_process_packages <- function(packages) {
  if (!dir.exists("packages")) dir.create("packages")

  results <- data.frame(source = character(), url = as.character(), code = character(), stringsAsFactors = FALSE)

  for (package in packages) {
    new_code <- tryCatch({
      cran_process_package(package)
    }, error = function(e) {
      warning(paste("error", package, sep = ":"))
    })

    new_result <- data.frame(source = "packages", url = paste0("package://", package), code = new_code, stringsAsFactors = FALSE)

    results <- rbind(
      results,
      new_result
    )
  }

  results
}

cran_find_resources <- function(sc,
                                samples = 2,
                                repartition = sc$config[["sparklyr.shell.num-executors"]]) {
  pkgnames <- available.packages()[,1]

  packages <- copy_to(
    sc,
    data.frame(package = pkgnames[1:samples]),
    repartition = ifelse(is.null(repartition), 0, repartition),
    overwrite = T)

  # package dependencies
  context <- list(
    cran_process_packages = cran_process_packages,
    cran_process_package = cran_process_package,
    cran_process_rd = cran_process_rd
  )

  packages %>% spark_apply(
    function(df, context) {
      for (name in names(context)) assign(name, context[[name]], envir = .GlobalEnv)
      cran_process_packages(df$package)
    },
    context = context,
    columns = list(source = "character", url = "character", code = "character"),
    name = "cran_resources")
}

cran_find_local <- function(samples = 1) {
  pkgnames <- available.packages()[,1]

  cran_process_packages(pkgnames[1:samples])
}

cran_find_config <- function(workers = 3, worker_cpus = 8) {
  config <- spark_config()

  config["sparklyr.shell.driver-memory"] <- "8g"
  config["sparklyr.shell.executor-memory"] <- "1g"
  config["sparklyr.shell.executor-cores"] <- 1
  config["sparklyr.shell.num-executors"] <- workers * worker_cpus
  config["spark.speculation"] <- TRUE
  config["spark.speculation.multiplier"] <- 4
  config["spark.memory.fraction"] <- 0.8

  config
}

cran_save_dataset <- function(cran_index) {
  if (!dir.exists("data")) dir.create("data")

  cranfiles <- dplyr::transmute(
    cran_index,
    package = gsub(":.*", "", name),
    dataset = gsub(".*:", "", name),
    description = description,
    rows = rows,
    cols = cols,
    class = class
  )

  save(cranfiles, file = "data/cranfiles.rda")
}

cran_clean_dataset <- function(cran_index) {
  cranfiles <- get(load("data/cranfiles.rda"))
  cranfiles <- cranfiles[cranfiles$package != "error" & cranfiles$rows > 0 & cranfiles$cols > 0,]
  cranfiles$metadata <- sapply(1:nrow(cranfiles), function(e) paste0('{"rows":', cranfiles[e,]$rows, ',"cols":', cranfiles[e,]$cols, '}'))
  save(cranfiles, file = "data/cranfiles.rda")
}
