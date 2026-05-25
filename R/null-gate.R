lpari_get_candidates <- function(x) {
  if (inherits(x, "lpari_result")) {
    return(x$scored_candidates)
  }
  x
}

criterion_delta_vs_null <- function(rows, criterion) {
  k1 <- rows[rows$candidate_k == 1, , drop = FALSE]
  multi <- rows[rows$candidate_k >= 2, , drop = FALSE]
  if (nrow(k1) == 0 || nrow(multi) == 0 || !criterion %in% names(rows)) {
    return(list(delta = NA_real_, selected_k = NA_integer_, n_min = NA_real_))
  }
  best <- multi[which.min(multi[[criterion]]), , drop = FALSE]
  list(
    delta = k1[[criterion]][1] - best[[criterion]][1],
    selected_k = best$candidate_k[1],
    n_min = if ("n_min" %in% names(best)) best$n_min[1] else NA_real_
  )
}

#' Evaluate the conservative null gate before LPA-RI interpretation
#'
#' LPA-RI is calibrated for difficult multiclass enumeration and should not be
#' used as a stand-alone test that latent profiles exist. This helper implements
#' a conservative gate: retain the one-profile solution unless conventional
#' information criteria provide enough evidence that a multiclass solution
#' improves over K = 1.
#'
#' @param x A candidate table or an `lpari_result` object.
#' @param criteria Character vector of information criteria used for the gate.
#'   Defaults to `c("BIC", "CAIC")`.
#' @param min_delta Minimum improvement required for a multiclass model over
#'   the one-profile model on each criterion. For example, `10` requires the
#'   best multiclass criterion value to be at least 10 points lower than K = 1.
#'   The default `0` asks only whether the criterion selects K > 1.
#' @param require_all Logical; if `TRUE`, all criteria must pass. If `FALSE`,
#'   at least one criterion must pass.
#' @param min_class_size Optional minimum class size for the best multiclass
#'   model under each criterion.
#' @param min_class_prop Optional minimum class proportion for the best
#'   multiclass model under each criterion.
#' @return A data frame with one row per task and a `gate_pass` decision.
#' @export
lpari_null_gate <- function(x,
                            criteria = c("BIC", "CAIC"),
                            min_delta = 0,
                            require_all = TRUE,
                            min_class_size = NULL,
                            min_class_prop = NULL) {
  candidates <- lpari_get_candidates(x)
  if (!all(c("candidate_k", criteria) %in% names(candidates))) {
    stop("`x` must contain `candidate_k` and the requested criteria.", call. = FALSE)
  }
  if (!"task_id" %in% names(candidates)) {
    candidates$task_id <- 1L
  }
  if (!"n" %in% names(candidates)) {
    candidates$n <- NA_real_
  }

  rows <- lapply(split(candidates, candidates$task_id), function(task_rows) {
    deltas <- lapply(criteria, function(criterion) {
      criterion_delta_vs_null(task_rows, criterion)
    })
    names(deltas) <- criteria
    delta_values <- vapply(deltas, function(z) z$delta, numeric(1))
    selected_values <- vapply(deltas, function(z) z$selected_k, numeric(1))
    n_min_values <- vapply(deltas, function(z) z$n_min, numeric(1))
    finite_delta <- is.finite(delta_values)
    pass <- finite_delta & delta_values >= min_delta

    if (!is.null(min_class_size)) {
      pass <- pass & is.finite(n_min_values) & n_min_values >= min_class_size
    }
    if (!is.null(min_class_prop)) {
      n_task <- task_rows$n[is.finite(task_rows$n)][1]
      if (length(n_task) == 0 || is.na(n_task)) {
        pass <- pass & FALSE
      } else {
        pass <- pass & is.finite(n_min_values) & n_min_values >= ceiling(min_class_prop * n_task)
      }
    }

    gate_pass <- if (require_all) all(pass) else any(pass)
    best_index <- if (any(finite_delta)) {
      which.max(ifelse(finite_delta, delta_values, -Inf))
    } else {
      NA_integer_
    }
    data.frame(
      task_id = task_rows$task_id[1],
      gate_pass = gate_pass,
      recommended_action = if (gate_pass) "run_lpari" else "retain_K1",
      criteria = paste(criteria, collapse = "+"),
      min_delta = min_delta,
      require_all = require_all,
      min_delta_observed = if (any(finite_delta)) min(delta_values[finite_delta]) else NA_real_,
      max_delta_observed = if (any(finite_delta)) max(delta_values[finite_delta]) else NA_real_,
      best_multiclass_k = if (!is.na(best_index)) selected_values[best_index] else NA_real_,
      best_multiclass_n_min = if (!is.na(best_index)) n_min_values[best_index] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
