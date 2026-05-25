load_lpari_coefficients <- function() {
  data_env <- new.env(parent = emptyenv())
  utils::data("lpari_coefficients", package = "lpari", envir = data_env)
  get("lpari_coefficients", envir = data_env)
}

load_lpari_temperature <- function() {
  data_env <- new.env(parent = emptyenv())
  utils::data("lpari_temperature", package = "lpari", envir = data_env)
  get("lpari_temperature", envir = data_env)
}

lpari_default_temperature <- function() {
  temp <- load_lpari_temperature()
  idx <- which(temp$scope == "operational_all_data")
  if (length(idx) == 0) {
    idx <- seq_len(nrow(temp))
  }
  temp$temperature[idx[[1]]]
}

#' Score candidate profile counts with LPA-RI
#'
#' @param candidates Candidate rows produced internally by [fit_lpari()] or a
#'   data frame containing all [lpari_feature_names()] columns.
#' @param coefficients Optional coefficient table with `term` and `estimate`
#'   columns. By default, the package uses the Study 1 coefficients bundled in
#'   `lpari_coefficients`.
#' @return The input data frame with `lpari_eta`, `lpari_probability`, and
#'   `lpari_score` columns.
#' @export
lpari_score_candidates <- function(candidates, coefficients = NULL) {
  if (is.null(coefficients)) {
    coefficients <- load_lpari_coefficients()
  }
  if (!all(c("term", "estimate") %in% names(coefficients))) {
    stop("`coefficients` must contain `term` and `estimate` columns.", call. = FALSE)
  }
  scored <- candidates
  eta <- rep(0, nrow(scored))
  intercept <- coefficients$estimate[coefficients$term == "(Intercept)"]
  if (length(intercept) == 1) {
    eta <- eta + intercept
  }
  for (i in seq_len(nrow(coefficients))) {
    term <- coefficients$term[i]
    if (term == "(Intercept)") {
      next
    }
    if (!term %in% names(scored)) {
      scored[[term]] <- 0
    }
    scored[[term]][!is.finite(scored[[term]])] <- 0
    eta <- eta + coefficients$estimate[i] * scored[[term]]
  }

  if (!"error" %in% names(scored)) {
    scored$error <- NA_character_
  }
  valid <- is.na(scored$error) | scored$error == ""
  valid <- valid & is.finite(scored$candidate_k)
  scored$lpari_eta <- NA_real_
  scored$lpari_probability <- NA_real_
  scored$lpari_score <- NA_real_
  scored$lpari_eta[valid] <- eta[valid]
  scored$lpari_probability[valid] <- stats::plogis(eta[valid])
  scored$lpari_score[valid] <- 100 * scored$lpari_probability[valid]
  scored
}

softmax_by_task <- function(scored, temperature) {
  out <- scored
  if (!"task_id" %in% names(out)) {
    out$task_id <- 1L
  }
  out$posterior_k <- NA_real_
  tasks <- unique(out$task_id)
  for (task in tasks) {
    idx <- which(out$task_id == task & is.finite(out$lpari_eta))
    if (length(idx) == 0) {
      next
    }
    eta <- out$lpari_eta[idx] / temperature
    exp_eta <- exp(eta - max(eta))
    out$posterior_k[idx] <- exp_eta / sum(exp_eta)
  }
  out
}

#' Compute calibrated plausibility over candidate K values
#'
#' Converts LPA-RI linear scores into a temperature-calibrated distribution over
#' candidate profile counts within each task or data set. This is a calibrated
#' selection distribution, not a generative Bayesian posterior.
#'
#' @param candidates A candidate table. If it has not yet been scored,
#'   [lpari_score_candidates()] is called first.
#' @param temperature Optional positive temperature. By default, the calibrated
#'   operational Study 1 temperature bundled in the package is used.
#' @return A scored candidate table with `posterior_k` and
#'   `cumulative_posterior`.
#' @export
lpari_posterior <- function(candidates, temperature = NULL) {
  if (!"lpari_eta" %in% names(candidates)) {
    candidates <- lpari_score_candidates(candidates)
  }
  if (is.null(temperature)) {
    temperature <- lpari_default_temperature()
  }
  if (!is.finite(temperature) || temperature <= 0) {
    stop("`temperature` must be a positive finite number.", call. = FALSE)
  }
  out <- softmax_by_task(candidates, temperature = temperature)
  out$cumulative_posterior <- NA_real_
  for (task in unique(out$task_id)) {
    idx <- which(out$task_id == task & is.finite(out$posterior_k))
    if (length(idx) == 0) {
      next
    }
    ord <- idx[order(-out$posterior_k[idx], out$candidate_k[idx])]
    out$cumulative_posterior[ord] <- cumsum(out$posterior_k[ord])
  }
  out
}

#' Select a profile count
#'
#' @param candidates A candidate table produced by [fit_lpari()] or
#'   [fit_lpa_models()] followed by LPA-RI candidate construction.
#' @param method One of `"LPA_RI"`, `"AIC"`, `"BIC"`, `"CAIC"`, `"SABIC"`,
#'   or `"ICL"`.
#' @return A one-row data frame with the selected K and score.
#' @export
lpari_select <- function(candidates, method = "LPA_RI") {
  method <- match.arg(method, c("LPA_RI", lpari_criteria))
  if (method == "LPA_RI") {
    scored <- lpari_posterior(candidates)
    selected <- scored[
      order(-scored$posterior_k, -scored$lpari_probability, scored$delta_BIC_log, scored$candidate_k),
      ,
      drop = FALSE
    ][1, , drop = FALSE]
    return(data.frame(
      method = "LPA_RI",
      selected_k = selected$candidate_k,
      score = selected$lpari_score,
      posterior_best = selected$posterior_k,
      stringsAsFactors = FALSE
    ))
  }
  selected <- candidates[which.min(candidates[[method]]), , drop = FALSE]
  data.frame(
    method = method,
    selected_k = selected$candidate_k,
    score = selected[[method]],
    posterior_best = NA_real_,
    stringsAsFactors = FALSE
  )
}

lpari_selection_summary <- function(scored) {
  methods <- c(lpari_criteria, "LPA_RI")
  rows <- lapply(methods, function(method) lpari_select(scored, method = method))
  do.call(rbind, rows)
}
