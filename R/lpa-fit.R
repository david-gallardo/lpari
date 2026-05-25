initial_classes <- function(x, k) {
  n <- nrow(x)
  if (k == 1) {
    return(rep(1, n))
  }
  km <- try(stats::kmeans(x, centers = k, nstart = 1, iter.max = 50), silent = TRUE)
  if (!inherits(km, "try-error") && length(unique(km$cluster)) == k) {
    return(km$cluster)
  }
  classes <- sample(seq_len(k), n, replace = TRUE)
  missing <- setdiff(seq_len(k), unique(classes))
  if (length(missing) > 0 && n >= k) {
    classes[seq_along(missing)] <- missing
  }
  classes
}

estimate_from_classes <- function(x, classes, k, variance_model, min_variance) {
  n <- nrow(x)
  p <- ncol(x)
  z <- matrix(0, nrow = n, ncol = k)
  z[cbind(seq_len(n), classes)] <- 1
  nk <- colSums(z)
  proportions <- pmax(nk / n, .Machine$double.eps)
  means <- matrix(0, nrow = k, ncol = p)
  variances <- matrix(min_variance, nrow = k, ncol = p)
  global_var <- pmax(apply(x, 2, stats::var), min_variance)

  for (j in seq_len(k)) {
    if (nk[j] > 1) {
      means[j, ] <- colMeans(x[classes == j, , drop = FALSE])
      variances[j, ] <- pmax(apply(x[classes == j, , drop = FALSE], 2, stats::var), min_variance)
    } else {
      means[j, ] <- x[sample(seq_len(n), 1), ]
      variances[j, ] <- global_var
    }
  }

  if (variance_model == "equal") {
    variances[,] <- matrix(rep(global_var, each = k), nrow = k)
  }

  list(proportions = proportions, means = means, variances = variances)
}

log_density_diag <- function(x, proportions, means, variances) {
  k <- nrow(means)
  log_values <- matrix(NA_real_, nrow = nrow(x), ncol = k)
  for (j in seq_len(k)) {
    centered <- sweep(x, 2, means[j, ], "-")
    log_det <- sum(log(variances[j, ]))
    quad <- rowSums(sweep(centered^2, 2, variances[j, ], "/"))
    log_values[, j] <- log(proportions[j]) -
      0.5 * (ncol(x) * log(2 * pi) + log_det + quad)
  }
  log_values
}

fit_lpa_em <- function(data,
                       k,
                       n_starts = 20,
                       max_iter = 500,
                       tol = 1e-6,
                       variance_model = "varying",
                       min_variance = 1e-4) {
  variance_model <- match.arg(variance_model, c("varying", "equal"))
  x <- as.matrix(data)
  n <- nrow(x)
  p <- ncol(x)
  if (k > n) {
    stop("`k` cannot be larger than the number of observations.", call. = FALSE)
  }

  best <- NULL
  starts <- if (k == 1) 1 else n_starts

  for (start in seq_len(starts)) {
    classes <- initial_classes(x, k)
    estimates <- estimate_from_classes(x, classes, k, variance_model, min_variance)
    previous_loglik <- -Inf
    converged <- FALSE
    z <- NULL
    loglik <- -Inf
    iter <- 0

    for (iter in seq_len(max_iter)) {
      log_values <- log_density_diag(x, estimates$proportions, estimates$means, estimates$variances)
      row_loglik <- log_sum_exp_rows(log_values)
      loglik <- sum(row_loglik)
      z <- exp(log_values - row_loglik)
      nk <- colSums(z)

      if (any(nk < .Machine$double.eps)) {
        converged <- FALSE
        break
      }

      estimates$proportions <- nk / n
      estimates$means <- t(z) %*% x / nk

      if (variance_model == "varying") {
        for (j in seq_len(k)) {
          centered <- sweep(x, 2, estimates$means[j, ], "-")
          estimates$variances[j, ] <- pmax(colSums(centered^2 * z[, j]) / nk[j], min_variance)
        }
      } else {
        pooled <- rep(0, p)
        for (j in seq_len(k)) {
          centered <- sweep(x, 2, estimates$means[j, ], "-")
          pooled <- pooled + colSums(centered^2 * z[, j])
        }
        pooled <- pmax(pooled / n, min_variance)
        estimates$variances[,] <- matrix(rep(pooled, each = k), nrow = k)
      }

      if (is.finite(previous_loglik) && abs(loglik - previous_loglik) < tol) {
        converged <- TRUE
        break
      }
      previous_loglik <- loglik
    }

    if (is.null(z)) {
      next
    }
    if (is.null(best) || loglik > best$loglik) {
      best <- list(
        k = k,
        n = n,
        p = p,
        loglik = loglik,
        proportions = estimates$proportions,
        means = estimates$means,
        variances = estimates$variances,
        posterior = z,
        classes = max.col(z),
        converged = converged,
        iterations = iter,
        variance_model = variance_model
      )
    }
  }

  if (is.null(best)) {
    stop("No LPA solution converged for k = ", k, call. = FALSE)
  }

  class_entropy <- -sum(best$posterior * log(pmax(best$posterior, .Machine$double.eps)))
  entropy_norm <- if (k == 1) 1 else 1 - class_entropy / (n * log(k))
  max_prob <- apply(best$posterior, 1, max)
  hard_counts <- tabulate(best$classes, nbins = k)
  parameters <- count_lpa_parameters(k, p, variance_model)

  best$parameters <- parameters
  best$AIC <- -2 * best$loglik + 2 * parameters
  best$AWE <- -2 * best$loglik + 2 * parameters * (1.5 + log(n))
  best$BIC <- -2 * best$loglik + log(n) * parameters
  best$CAIC <- -2 * best$loglik + (log(n) + 1) * parameters
  best$CLC <- -2 * best$loglik + 2 * class_entropy
  best$KIC <- -2 * best$loglik + 3 * parameters
  best$SABIC <- -2 * best$loglik + log((n + 2) / 24) * parameters
  best$ICL <- best$BIC + 2 * class_entropy
  best$Entropy <- entropy_norm
  best$prob_min <- min(max_prob)
  best$prob_mean <- mean(max_prob)
  best$prob_max <- max(max_prob)
  best$n_min <- min(hard_counts)
  best$n_max <- max(hard_counts)
  best
}

#' Fit candidate latent profile models
#'
#' Fits diagonal Gaussian latent profile models for a range of candidate profile
#' counts. This is a lightweight fitter intended for LPA-RI enumeration and
#' reproducible examples; it is not a replacement for a full mixture modelling
#' platform when complex covariance structures are needed.
#'
#' @param data A data frame or matrix containing numeric indicators.
#' @param profiles Integer vector of candidate profile counts.
#' @param n_starts Number of random starts for each candidate K.
#' @param max_iter Maximum EM iterations per start.
#' @param tol EM convergence tolerance on the log-likelihood.
#' @param variance_model Either `"varying"` for class-specific diagonal
#'   variances or `"equal"` for a common diagonal variance.
#' @param scale Logical; if `TRUE`, indicators are z-standardized before
#'   fitting.
#' @return A list with fitted models, a fit-index table, and the analysis data.
#' @export
fit_lpa_models <- function(data,
                           profiles = 1:5,
                           n_starts = 20,
                           max_iter = 500,
                           tol = 1e-6,
                           variance_model = c("varying", "equal"),
                           scale = TRUE) {
  variance_model <- match.arg(variance_model)
  prepared <- prepare_lpa_matrix(data, scale = scale)
  x <- prepared$x
  profiles <- sort(unique(as.integer(profiles)))
  profiles <- profiles[is.finite(profiles) & profiles >= 1 & profiles <= nrow(x)]
  if (length(profiles) == 0) {
    stop("No valid candidate profile counts were supplied.", call. = FALSE)
  }

  fits <- list()
  for (k in profiles) {
    fit <- try(
      fit_lpa_em(
        x,
        k = k,
        n_starts = n_starts,
        max_iter = max_iter,
        tol = tol,
        variance_model = variance_model
      ),
      silent = TRUE
    )
    if (!inherits(fit, "try-error")) {
      fits[[as.character(k)]] <- fit
    }
  }

  if (length(fits) == 0) {
    stop("No LPA models could be fitted.", call. = FALSE)
  }

  fit_table <- do.call(
    rbind,
    lapply(fits, function(fit) {
      data.frame(
        k = fit$k,
        n = fit$n,
        p = fit$p,
        parameters = fit$parameters,
        logLik = fit$loglik,
        AIC = fit$AIC,
        AWE = fit$AWE,
        BIC = fit$BIC,
        CAIC = fit$CAIC,
        CLC = fit$CLC,
        KIC = fit$KIC,
        SABIC = fit$SABIC,
        ICL = fit$ICL,
        Entropy = fit$Entropy,
        prob_min = fit$prob_min,
        prob_mean = fit$prob_mean,
        prob_max = fit$prob_max,
        n_min = fit$n_min,
        n_max = fit$n_max,
        converged = fit$converged
      )
    })
  )
  rownames(fit_table) <- NULL

  fit_table$LRT_chisq <- NA_real_
  fit_table$LRT_df <- NA_real_
  fit_table$LRT_p <- NA_real_
  for (i in seq_len(nrow(fit_table))) {
    current_k <- fit_table$k[i]
    previous_idx <- match(current_k - 1, fit_table$k)
    if (!is.na(previous_idx)) {
      lrt <- 2 * (fit_table$logLik[i] - fit_table$logLik[previous_idx])
      df <- fit_table$parameters[i] - fit_table$parameters[previous_idx]
      fit_table$LRT_chisq[i] <- lrt
      fit_table$LRT_df[i] <- df
      fit_table$LRT_p[i] <- stats::pchisq(lrt, df = df, lower.tail = FALSE)
    }
  }

  list(
    fits = fits,
    fit_table = fit_table,
    data = x,
    complete_cases = prepared$complete_cases,
    variables = prepared$variables,
    center = prepared$center,
    scale = prepared$scale,
    variance_model = variance_model
  )
}

#' Select a profile count from a fit-index table
#'
#' @param fit_table A table returned by [fit_lpa_models()].
#' @param criterion Fit index to use.
#' @return The selected number of profiles.
#' @export
select_profile_by <- function(fit_table, criterion = "BIC") {
  if (!criterion %in% names(fit_table)) {
    stop("Unknown criterion: ", criterion, call. = FALSE)
  }
  if (criterion %in% c("Entropy", "prob_min", "prob_mean", "prob_max")) {
    fit_table$k[which.max(fit_table[[criterion]])]
  } else {
    fit_table$k[which.min(fit_table[[criterion]])]
  }
}
