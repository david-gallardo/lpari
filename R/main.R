make_assignments <- function(fit_result, selected_k, labels = NULL, id = NULL) {
  fit <- fit_result$fits[[as.character(selected_k)]]
  if (is.null(fit)) {
    return(data.frame())
  }
  n <- nrow(fit_result$data)
  if (is.null(id)) {
    id <- seq_len(n)
  } else if (length(id) == length(fit_result$complete_cases)) {
    id <- id[fit_result$complete_cases]
  }
  out <- data.frame(
    id = id,
    class = fit$classes,
    max_probability = apply(fit$posterior, 1, max),
    stringsAsFactors = FALSE
  )
  if (!is.null(labels)) {
    if (length(labels) == length(fit_result$complete_cases)) {
      labels <- labels[fit_result$complete_cases]
    }
    if (length(labels) == n) {
      out$label <- labels
    }
  }
  out
}

make_profile_means <- function(fit_result, selected_k) {
  fit <- fit_result$fits[[as.character(selected_k)]]
  if (is.null(fit)) {
    return(data.frame())
  }
  means <- as.data.frame(fit$means)
  names(means) <- fit_result$variables
  means$class <- seq_len(nrow(means))
  means <- means[, c("class", fit_result$variables), drop = FALSE]
  rownames(means) <- NULL
  means
}

#' Fit LPA models and select K with LPA-RI
#'
#' This is the main user-facing workflow. It fits candidate latent profile
#' models, computes conventional information criteria, scores each candidate K
#' with LPA-RI, and returns a calibrated plausibility summary.
#'
#' @param data A data frame or matrix containing numeric LPA indicators.
#' @param k Integer vector of candidate profile counts.
#' @param n_starts Number of random starts for each candidate K.
#' @param max_iter Maximum EM iterations per start.
#' @param tol EM convergence tolerance.
#' @param variance_model Either `"varying"` or `"equal"` diagonal variances.
#' @param scale Logical; if `TRUE`, indicators are z-standardized.
#' @param labels Optional external labels used only for descriptive validation.
#' @param id Optional observation identifiers.
#' @param seed Optional random seed.
#' @param temperature Optional posterior calibration temperature.
#' @return An object of class `lpari_result` containing the raw LPA-RI
#'   selected K (`selected_k`), the conservative null-gated recommendation
#'   (`recommended_k`), the null-gate diagnostics (`null_gate`), selection
#'   summaries, calibrated plausibility values, fitted models, assignments,
#'   and profile means.
#' @export
fit_lpari <- function(data,
                      k = 1:5,
                      n_starts = 50,
                      max_iter = 500,
                      tol = 1e-6,
                      variance_model = c("varying", "equal"),
                      scale = TRUE,
                      labels = NULL,
                      id = NULL,
                      seed = NULL,
                      temperature = NULL) {
  variance_model <- match.arg(variance_model)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  fit <- fit_lpa_models(
    data,
    profiles = k,
    n_starts = n_starts,
    max_iter = max_iter,
    tol = tol,
    variance_model = variance_model,
    scale = scale
  )
  candidates <- make_lpari_candidate_rows(
    fit,
    dataset_id = "user_data",
    dataset_title = "User data",
    indicators = fit$variables
  )
  scored <- lpari_score_candidates(candidates)
  scored <- lpari_posterior(scored, temperature = temperature)
  selection <- lpari_selection_summary(scored)
  null_gate <- lpari_null_gate(scored)
  lpari_k <- selection$selected_k[selection$method == "LPA_RI"][[1]]
  recommended_k <- if (isTRUE(null_gate$gate_pass[[1]])) lpari_k else 1L
  posterior <- scored[order(-scored$posterior_k, scored$candidate_k), , drop = FALSE]
  posterior <- posterior[, c(
    "candidate_k",
    "lpari_score",
    "posterior_k",
    "cumulative_posterior",
    "AIC",
    "BIC",
    "CAIC",
    "SABIC",
    "ICL",
    "Entropy",
    "prob_mean",
    "n_min"
  ), drop = FALSE]
  assignments <- make_assignments(fit, lpari_k, labels = labels, id = id)
  profile_means <- make_profile_means(fit, lpari_k)
  external_validation <- NULL
  if (!is.null(labels) && "label" %in% names(assignments)) {
    external_validation <- data.frame(
      selected_k = lpari_k,
      ari_external = adjusted_rand_index(assignments$label, assignments$class),
      mean_max_probability = mean(assignments$max_probability, na.rm = TRUE),
      min_class_size = min(table(assignments$class)),
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      call = match.call(),
      selected_k = lpari_k,
      recommended_k = recommended_k,
      null_gate = null_gate,
      selection = selection,
      posterior = posterior,
      fit_table = fit$fit_table,
      candidates = candidates,
      scored_candidates = scored,
      fits = fit$fits,
      data = fit$data,
      variables = fit$variables,
      assignments = assignments,
      profile_means = profile_means,
      external_validation = external_validation,
      variance_model = variance_model,
      scaled = scale
    ),
    class = "lpari_result"
  )
}

#' @export
print.lpari_result <- function(x, ...) {
  best <- x$selection[x$selection$method == "LPA_RI", , drop = FALSE]
  bic <- x$selection[x$selection$method == "BIC", , drop = FALSE]
  cat("LPA-RI result\n")
  cat("  Selected K:", x$selected_k, "\n")
  if (!is.null(x$recommended_k)) {
    cat("  Recommended K after null gate:", x$recommended_k, "\n")
  }
  if (!is.null(x$null_gate)) {
    cat("  Null gate:", x$null_gate$recommended_action[[1]], "\n")
  }
  cat("  LPA-RI posterior:", sprintf("%.3f", best$posterior_best), "\n")
  if (nrow(bic) == 1) {
    cat("  BIC selected K:", bic$selected_k, "\n")
  }
  cat("  Indicators:", paste(x$variables, collapse = ", "), "\n")
  invisible(x)
}

#' @export
summary.lpari_result <- function(object, ...) {
  out <- list(
    selected_k = object$selected_k,
    recommended_k = object$recommended_k,
    null_gate = object$null_gate,
    selection = object$selection,
    posterior = object$posterior,
    fit_table = object$fit_table,
    external_validation = object$external_validation
  )
  class(out) <- "summary.lpari_result"
  out
}
