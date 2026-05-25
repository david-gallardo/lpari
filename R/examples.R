#' List bundled example data sets
#'
#' @return A data frame describing the examples bundled with the package.
#' @export
available_lpari_examples <- function() {
  data.frame(
    name = c("old_faithful", "reaven_miller_diabetes", "iris", "holzinger_swineford"),
    title = c(
      "Old Faithful geyser eruptions",
      "Reaven-Miller diabetes biomarkers",
      "Iris flower benchmark",
      "Holzinger-Swineford mental ability"
    ),
    n = c(272L, 145L, 150L, 301L),
    p = c(2L, 5L, 4L, 9L),
    label = c(NA_character_, "group", "species", "school"),
    role = c(
      "primary example",
      "applied clinical example",
      "supplementary cautionary example",
      "supplementary cautionary example"
    ),
    stringsAsFactors = FALSE
  )
}

#' Load a bundled LPA-RI example
#'
#' @param name One of `"old_faithful"`, `"reaven_miller_diabetes"`,
#'   `"iris"`, or `"holzinger_swineford"`.
#' @return A list with `data`, `indicators`, `label`, `id`, and `title`.
#' @export
load_lpari_example <- function(name = c("old_faithful", "reaven_miller_diabetes", "iris", "holzinger_swineford")) {
  name <- match.arg(name)
  if (name == "old_faithful") {
    data_env <- new.env(parent = emptyenv())
    utils::data("lpari_faithful", package = "lpari", envir = data_env)
    return(list(
      data = get("lpari_faithful", envir = data_env),
      indicators = c("eruptions", "waiting"),
      label = NULL,
      id = "observation_id",
      title = "Old Faithful geyser eruptions"
    ))
  }
  if (name == "iris") {
    data_env <- new.env(parent = emptyenv())
    utils::data("lpari_iris", package = "lpari", envir = data_env)
    return(list(
      data = get("lpari_iris", envir = data_env),
      indicators = c("sepal_length", "sepal_width", "petal_length", "petal_width"),
      label = "species",
      id = "subject_id",
      title = "Iris flower benchmark"
    ))
  }
  if (name == "reaven_miller_diabetes") {
    data_env <- new.env(parent = emptyenv())
    utils::data("lpari_reaven_miller_diabetes", package = "lpari", envir = data_env)
    return(list(
      data = get("lpari_reaven_miller_diabetes", envir = data_env),
      indicators = c("rw", "fpg", "glucose", "insulin", "sspg"),
      label = "group",
      id = "subject_id",
      title = "Reaven-Miller diabetes biomarkers"
    ))
  }
  data_env <- new.env(parent = emptyenv())
  utils::data("lpari_holzinger_swineford", package = "lpari", envir = data_env)
  list(
    data = get("lpari_holzinger_swineford", envir = data_env),
    indicators = paste0("x", 1:9),
    label = "school",
    id = "subject_id",
    title = "Holzinger-Swineford mental ability"
  )
}

#' Run a bundled LPA-RI example
#'
#' @param name One of `"old_faithful"`, `"reaven_miller_diabetes"`,
#'   `"iris"`, or `"holzinger_swineford"`.
#' @param n_starts Number of random starts for each candidate K.
#' @param k Candidate profile counts.
#' @param seed Optional random seed.
#' @param ... Additional arguments passed to [fit_lpari()].
#' @return An `lpari_result` object.
#' @export
lpari_example <- function(name = c("old_faithful", "reaven_miller_diabetes", "iris", "holzinger_swineford"),
                          n_starts = 25,
                          k = 1:5,
                          seed = 20260525,
                          ...) {
  ex <- load_lpari_example(name)
  labels <- if (is.null(ex$label)) NULL else ex$data[[ex$label]]
  id <- if (is.null(ex$id)) NULL else ex$data[[ex$id]]
  fit_lpari(
    ex$data[, ex$indicators, drop = FALSE],
    k = k,
    n_starts = n_starts,
    labels = labels,
    id = id,
    seed = seed,
    ...
  )
}
