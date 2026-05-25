scale_score <- function(x, low, high) {
  if (!is.finite(x) || !is.finite(low) || !is.finite(high) || high <= low) {
    return(0)
  }
  pmax(0, pmin(1, (x - low) / (high - low)))
}

safe_min <- function(x, default = NA_real_) {
  x <- x[is.finite(x)]
  if (length(x) == 0) default else min(x)
}

choose2 <- function(x) {
  x * (x - 1) / 2
}

log_sum_exp_rows <- function(log_values) {
  row_max <- apply(log_values, 1, max)
  row_max + log(rowSums(exp(log_values - row_max)))
}

count_lpa_parameters <- function(k, p, variance_model = "varying") {
  variance_model <- match.arg(variance_model, c("varying", "equal"))
  mean_parameters <- k * p
  mixing_parameters <- k - 1
  variance_parameters <- if (variance_model == "equal") p else k * p
  mean_parameters + mixing_parameters + variance_parameters
}

prepare_lpa_matrix <- function(data, scale = TRUE) {
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or matrix.", call. = FALSE)
  }
  numeric <- vapply(data, is.numeric, logical(1))
  if (!any(numeric)) {
    stop("`data` must contain numeric indicators.", call. = FALSE)
  }
  data <- data[, numeric, drop = FALSE]
  complete <- stats::complete.cases(data)
  x <- as.matrix(data[complete, , drop = FALSE])
  storage.mode(x) <- "double"
  if (nrow(x) < 2) {
    stop("At least two complete observations are required.", call. = FALSE)
  }
  keep <- apply(x, 2, function(col) stats::sd(col) > 0)
  if (!any(keep)) {
    stop("At least one indicator must have non-zero variance.", call. = FALSE)
  }
  x <- x[, keep, drop = FALSE]
  center <- rep(0, ncol(x))
  spread <- rep(1, ncol(x))
  names(center) <- colnames(x)
  names(spread) <- colnames(x)
  if (isTRUE(scale)) {
    center <- colMeans(x)
    spread <- apply(x, 2, stats::sd)
    spread[spread == 0 | is.na(spread)] <- 1
    x <- sweep(sweep(x, 2, center, "-"), 2, spread, "/")
  }
  attr(x, "center") <- center
  attr(x, "scale") <- spread
  list(
    x = x,
    complete_cases = complete,
    variables = colnames(x),
    center = center,
    scale = spread
  )
}

rbind_fill <- function(rows) {
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame())
  }
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    missing <- setdiff(columns, names(row))
    for (col in missing) {
      row[[col]] <- NA
    }
    row[, columns, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Adjusted Rand index
#'
#' Computes the adjusted Rand index between two partitions.
#'
#' @param x A vector of labels.
#' @param y A vector of labels with the same length as `x`.
#' @return A numeric adjusted Rand index.
#' @export
adjusted_rand_index <- function(x, y) {
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.", call. = FALSE)
  }
  x <- as.factor(x)
  y <- as.factor(y)
  tab <- table(x, y)
  n <- sum(tab)
  if (n < 2) {
    return(NA_real_)
  }
  sum_cells <- sum(choose2(tab))
  sum_rows <- sum(choose2(rowSums(tab)))
  sum_cols <- sum(choose2(colSums(tab)))
  total <- choose2(n)
  expected <- sum_rows * sum_cols / total
  maximum <- (sum_rows + sum_cols) / 2
  if (abs(maximum - expected) < .Machine$double.eps) {
    return(NA_real_)
  }
  (sum_cells - expected) / (maximum - expected)
}
