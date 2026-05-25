test_that("bundled examples load", {
  examples <- available_lpari_examples()
  expect_true(all(c("old_faithful", "iris", "holzinger_swineford") %in% examples$name))
  faithful <- load_lpari_example("old_faithful")
  expect_true(all(faithful$indicators %in% names(faithful$data)))
  expect_null(faithful$label)
})

test_that("LPA-RI scores candidate rows", {
  ex <- load_lpari_example("old_faithful")
  fit <- fit_lpa_models(ex$data[, ex$indicators], profiles = 1:3, n_starts = 2)
  candidates <- lpari:::make_lpari_candidate_rows(fit)
  scored <- lpari_score_candidates(candidates)
  posterior <- lpari_posterior(scored)
  expect_true(all(c("lpari_score", "posterior_k") %in% names(posterior)))
  expect_equal(sum(posterior$posterior_k), 1, tolerance = 1e-8)
})

test_that("main workflow returns an lpari_result", {
  ex <- load_lpari_example("old_faithful")
  fit <- fit_lpari(
    ex$data[, ex$indicators],
    k = 1:3,
    n_starts = 2,
    id = ex$data[[ex$id]],
    seed = 1
  )
  expect_s3_class(fit, "lpari_result")
  expect_true(fit$selected_k %in% 1:3)
  expect_equal(sum(fit$posterior$posterior_k), 1, tolerance = 1e-8)
})
