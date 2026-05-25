lpari_criteria <- c("AIC", "BIC", "CAIC", "SABIC", "ICL")

pairwise_profile_distances <- function(fit) {
  k <- fit$k
  if (k < 2) {
    return(numeric(0))
  }
  pooled_variance <- colSums(fit$variances * fit$proportions)
  pooled_variance <- pmax(pooled_variance, 1e-4)
  distances <- numeric(0)
  for (i in seq_len(k - 1)) {
    for (j in (i + 1):k) {
      distances <- c(
        distances,
        sqrt(sum((fit$means[i, ] - fit$means[j, ])^2 / pooled_variance))
      )
    }
  }
  distances
}

safe_log1p_delta <- function(x) {
  log1p(pmax(0, x))
}

#' Names of the predictors used by LPA-RI
#'
#' @return A character vector with the feature names expected by
#'   [lpari_score_candidates()].
#' @export
lpari_feature_names <- function() {
  c(
    "is_best_AIC",
    "is_best_BIC",
    "is_best_CAIC",
    "is_best_SABIC",
    "is_best_ICL",
    "delta_AIC_log",
    "delta_BIC_log",
    "delta_CAIC_log",
    "delta_SABIC_log",
    "delta_ICL_log",
    "delta_BIC_per_n",
    "delta_ICL_per_n",
    "Entropy",
    "prob_mean",
    "n_min_prop",
    "class_size_score",
    "profile_separation_score",
    "classification_certainty_score",
    "sample_parameter_score",
    "sample_parameter_ratio",
    "log_n",
    "p",
    "candidate_k",
    "candidate_k_over_p",
    "is_single_profile"
  )
}

make_lpari_candidate_rows <- function(fit_result,
                                      dataset_id = NA_character_,
                                      dataset_title = NA_character_,
                                      indicators = NULL,
                                      task_id = 1L) {
  criteria <- lpari_criteria
  fit_table <- fit_result$fit_table
  selected <- vapply(criteria, function(criterion) {
    select_profile_by(fit_table, criterion)
  }, numeric(1))

  rows <- fit_table
  names(rows)[names(rows) == "k"] <- "candidate_k"
  rows$task_id <- task_id
  rows$dataset_id <- dataset_id
  rows$dataset_title <- dataset_title
  rows$rep <- 1L
  rows$true_k <- NA_integer_
  rows$separation <- NA_real_
  rows$rho <- NA_real_
  rows$balance <- NA_character_
  rows$correct <- NA_real_

  for (criterion in criteria) {
    rows[[paste0("selected_", criterion)]] <- selected[[criterion]]
    rows[[paste0("is_best_", criterion)]] <- as.numeric(rows$candidate_k == selected[[criterion]])
    rows[[paste0("delta_", criterion)]] <- rows[[criterion]] - min(rows[[criterion]], na.rm = TRUE)
    rows[[paste0("delta_", criterion, "_log")]] <- safe_log1p_delta(rows[[paste0("delta_", criterion)]])
  }

  best_cols <- paste0("is_best_", criteria)
  rows$criteria_votes <- rowMeans(rows[, best_cols, drop = FALSE], na.rm = TRUE)
  rows$min_distance <- vapply(rows$candidate_k, function(k) {
    fit <- fit_result$fits[[as.character(k)]]
    if (is.null(fit)) {
      return(NA_real_)
    }
    safe_min(pairwise_profile_distances(fit), default = 0)
  }, numeric(1))

  rows$n_min_prop <- rows$n_min / rows$n
  rows$sample_parameter_ratio <- rows$n / rows$parameters
  rows$log_n <- log(rows$n)
  rows$candidate_k_over_p <- rows$candidate_k / rows$p
  rows$is_single_profile <- as.numeric(rows$candidate_k == 1)
  rows$delta_BIC_per_n <- rows$delta_BIC / rows$n
  rows$delta_ICL_per_n <- rows$delta_ICL / rows$n
  rows$class_size_score <- mapply(
    function(value, p) scale_score(value, low = 1, high = max(5, 2 * p)),
    rows$n_min,
    rows$p
  )
  rows$profile_separation_score <- ifelse(
    rows$candidate_k < 2,
    0,
    vapply(rows$min_distance, scale_score, numeric(1), low = 1.5, high = 4.0)
  )
  rows$classification_certainty_score <- rowMeans(
    cbind(
      vapply(rows$prob_mean, scale_score, numeric(1), low = 0.70, high = 0.90),
      rows$Entropy
    ),
    na.rm = TRUE
  )
  rows$sample_parameter_score <- vapply(
    rows$sample_parameter_ratio,
    scale_score,
    numeric(1),
    low = 1,
    high = 5
  )
  rows$error <- NA_character_
  if (is.null(indicators)) {
    indicators <- fit_result$variables
  }
  rows$indicator_set <- paste(indicators, collapse = ";")
  rows
}
