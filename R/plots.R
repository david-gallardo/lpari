#' Plot LPA-RI plausibility across candidate K values
#'
#' @param x An `lpari_result` object or a posterior data frame.
#' @param main Plot title.
#' @param col Bar color.
#' @param ... Additional arguments passed to [graphics::barplot()].
#' @return Invisibly returns the plotted posterior data.
#' @export
plot_lpari_posterior <- function(x,
                                 main = "LPA-RI plausibility over K",
                                 col = "#111111",
                                 ...) {
  posterior <- if (inherits(x, "lpari_result")) x$posterior else x
  if (!all(c("candidate_k", "posterior_k") %in% names(posterior))) {
    stop("`x` must contain `candidate_k` and `posterior_k`.", call. = FALSE)
  }
  posterior <- posterior[order(posterior$candidate_k), , drop = FALSE]
  graphics::barplot(
    height = posterior$posterior_k,
    names.arg = posterior$candidate_k,
    xlab = "Candidate K",
    ylab = "Calibrated plausibility",
    ylim = c(0, max(0.05, min(1, max(posterior$posterior_k, na.rm = TRUE) + 0.08))),
    main = main,
    col = col,
    border = NA,
    ...
  )
  invisible(posterior)
}

#' Plot selected profile means
#'
#' @param x An `lpari_result` object or a profile-means data frame.
#' @param main Plot title.
#' @param ... Additional arguments passed to [graphics::matplot()].
#' @return Invisibly returns the plotted profile means.
#' @export
plot_lpari_profiles <- function(x,
                                main = "LPA-RI selected profile means",
                                ...) {
  means <- if (inherits(x, "lpari_result")) x$profile_means else x
  if (!"class" %in% names(means) || nrow(means) == 0) {
    stop("`x` must contain a `class` column and at least one profile.", call. = FALSE)
  }
  values <- as.matrix(means[, setdiff(names(means), "class"), drop = FALSE])
  colors <- grDevices::hcl.colors(nrow(values), palette = "Dark 3")
  graphics::matplot(
    x = seq_len(ncol(values)),
    y = t(values),
    type = "b",
    pch = 19,
    lty = 1,
    col = colors,
    xaxt = "n",
    xlab = "Indicator",
    ylab = "Profile mean",
    main = main,
    ...
  )
  graphics::axis(1, at = seq_len(ncol(values)), labels = colnames(values), las = 2)
  graphics::abline(h = 0, col = "gray70", lty = 2)
  graphics::legend(
    "topright",
    legend = paste("Class", means$class),
    col = colors,
    lty = 1,
    pch = 19,
    bty = "n"
  )
  invisible(means)
}
