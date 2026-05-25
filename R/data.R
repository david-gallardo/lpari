#' Study 1 LPA-RI coefficients
#'
#' A coefficient table used by [lpari_score_candidates()] to compute the LPA-RI
#' linear predictor. These coefficients were trained in Study 1 simulations to
#' predict exact recovery of the true number of profiles.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{term}{Model term.}
#'   \item{estimate}{Logistic-regression coefficient.}
#' }
"lpari_coefficients"

#' LPA-RI posterior calibration temperature
#'
#' Temperature values used to convert LPA-RI linear scores into calibrated
#' plausibility over candidate K values.
#'
#' @format A data frame with columns `scope`, `temperature`, and `log_loss`.
"lpari_temperature"

#' Old Faithful geyser eruption data
#'
#' A package copy of the classic Old Faithful data set with an observation
#' identifier added. It is the primary worked example because its two-variable
#' bimodal structure is a clear demonstration of latent profile enumeration.
#'
#' @format A data frame with 272 rows and 3 columns:
#' \describe{
#'   \item{observation_id}{Observation identifier.}
#'   \item{eruptions}{Eruption time in minutes.}
#'   \item{waiting}{Waiting time to the next eruption in minutes.}
#' }
"lpari_faithful"

#' Iris flower benchmark data
#'
#' A cleaned copy of Fisher's iris data with package-consistent variable names.
#'
#' @format A data frame with 150 rows and 6 columns:
#' \describe{
#'   \item{subject_id}{Observation identifier.}
#'   \item{species}{Iris species.}
#'   \item{sepal_length, sepal_width, petal_length, petal_width}{Numeric flower measurements.}
#' }
"lpari_iris"

#' Reaven-Miller diabetes biomarker data
#'
#' A clinical biomarker dataset from Reaven and Miller (1979), with five
#' continuous metabolic indicators and an external diagnostic group label:
#' `normal`, `chemical`, or `overt`.
#'
#' @format A data frame with 145 rows and 7 columns:
#' \describe{
#'   \item{subject_id}{Observation identifier.}
#'   \item{rw}{Relative weight.}
#'   \item{fpg}{Fasting plasma glucose.}
#'   \item{glucose}{Glucose response to oral glucose.}
#'   \item{insulin}{Insulin response to oral glucose.}
#'   \item{sspg}{Steady-state plasma glucose.}
#'   \item{group}{External diagnostic group label.}
#' }
#' @source Reaven, G. M., & Miller, R. G. (1979). An attempt to define the
#' nature of chemical diabetes using a multidimensional analysis.
#' \emph{Diabetologia, 16}, 17-24. https://doi.org/10.1007/BF00423145.
#' Public R data version obtained from package \pkg{rrcov}.
"lpari_reaven_miller_diabetes"

#' Holzinger-Swineford mental ability data
#'
#' A small educational psychology benchmark with nine cognitive indicators and
#' school as an external descriptive label.
#'
#' @format A data frame with columns `subject_id`, `school`, and indicators
#'   `x1` to `x9`.
"lpari_holzinger_swineford"
