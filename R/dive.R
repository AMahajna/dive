# ============================================================
# DIVE
# Diversity-Informed Valuation of Ecosystem Functioning
#
# Main user-facing function
# ============================================================


#' Run DIVE analysis
#'
#' @description
#' DIVE calculates alpha-diversity metrics and evaluates their
#' temporal association with ecosystem functionality.
#'
#' Evenly sampled time series are analyzed using CCF.
#'
#' Unevenly sampled time series are analyzed using an exact-lag
#' DCF. No lag binning or bootstrap resampling is performed.
#'
#' For both methods:
#'
#' negative lag = alpha diversity precedes functionality
#'
#' lag zero = contemporaneous association
#'
#' @param tse A TreeSummarizedExperiment containing an assay
#'   named `"counts"` and a `Date` column in `colData(tse)`.
#'
#' @param functionality Numeric vector containing ecosystem
#'   functionality values corresponding exactly to the samples
#'   in `tse`.
#'
#' @param sampling_even Numeric indicator. Use `1` for evenly
#'   sampled data and `0` for unevenly sampled data.
#'
#' @param output_dir Directory in which DIVE output will be
#'   written.
#'
#' @param rarefaction_iterations Number of repeated rarefaction
#'   rounds used for alpha-diversity estimation.
#'
#' @param rarefaction_depth Rarefaction depth. If `NULL`, the
#'   smallest sample library size is used.
#'
#' @param max_lag Maximum lag. For DCF this is expressed in
#'   days. For CCF this is expressed as number of sampling
#'   intervals.
#'
#' @param seed Random-number seed used by repeated rarefaction.
#'
#' @return An object of class `dive_result`.
#'
#' @export
dive <- function(
    tse,
    functionality,
    sampling_even,
    output_dir = "DIVE_results",
    rarefaction_iterations = 50L,
    rarefaction_depth = NULL,
    max_lag = 63,
    seed = 123
) {

  # ----------------------------------------------------------
  # Validate analysis type
  # ----------------------------------------------------------

  if (
    length(sampling_even) != 1L ||
    !is.numeric(sampling_even) ||
    !is.finite(sampling_even) ||
    !sampling_even %in% c(0, 1)
  ) {

    stop(
      "`sampling_even` must be either 0 or 1."
    )
  }


  rarefaction_iterations <- .validate_positive_integer(
    rarefaction_iterations,
    "rarefaction_iterations"
  )


  if (
    length(max_lag) != 1L ||
    !is.numeric(max_lag) ||
    !is.finite(max_lag) ||
    max_lag < 0
  ) {

    stop(
      "`max_lag` must be one non-negative number."
    )
  }


  # ----------------------------------------------------------
  # Read sampling dates
  # ----------------------------------------------------------

  dates <- .get_dive_dates(
    tse = tse
  )


  if (length(dates) < 2L) {

    stop(
      "At least two sampling dates are required."
    )
  }


  # ----------------------------------------------------------
  # Validate functionality
  # ----------------------------------------------------------

  functionality <- .prepare_functionality(
    functionality = functionality,
    n_samples = length(dates)
  )


  # ----------------------------------------------------------
  # Sort TSE, dates and functionality chronologically
  # ----------------------------------------------------------

  chronological_order <- order(
    dates
  )


  dates <- dates[
    chronological_order
  ]


  tse <- tse[
    ,
    chronological_order
  ]


  functionality <- functionality[
    chronological_order
  ]


  # ----------------------------------------------------------
  # Inspect temporal sampling
  # ----------------------------------------------------------

  sampling <- .inspect_sampling(
    dates = dates,
    sampling_even = sampling_even
  )


  # ----------------------------------------------------------
  # Calculate alpha diversity
  # ----------------------------------------------------------

  set.seed(
    seed
  )


  alpha <- .calculate_alpha(
    tse = tse,
    rarefaction_depth = rarefaction_depth,
    rarefaction_iterations = rarefaction_iterations
  )


  if (
    !is.data.frame(alpha) &&
    !is.matrix(alpha)
  ) {

    stop(
      "Alpha-diversity calculation did not return a ",
      "two-dimensional table."
    )
  }


  if (nrow(alpha) != length(dates)) {

    stop(
      "Alpha-diversity output does not contain one row ",
      "for every sampling date."
    )
  }


  # ==========================================================
  # DCF
  # Unevenly sampled data
  # ==========================================================

  if (sampling_even == 0) {

    method <- "DCF"


    correlation <- .calculate_dcf_table(
      alpha = alpha,
      functionality = functionality,
      dates = dates,
      max_lag_days = max_lag
    )


    # ==========================================================
    # CCF
    # Evenly sampled data
    # ==========================================================

  } else {

    method <- "CCF"


    sampling_intervals <- as.numeric(
      diff(
        dates
      )
    )


    sampling_interval_days <- stats::median(
      sampling_intervals,
      na.rm = TRUE
    )


    if (
      !is.finite(sampling_interval_days) ||
      sampling_interval_days <= 0
    ) {

      stop(
        "Could not determine a valid sampling interval."
      )
    }


    correlation <- .calculate_ccf_table(
      alpha = alpha,
      functionality = functionality,
      max_lag = max_lag,
      sampling_interval_days = sampling_interval_days
    )
  }


  # ----------------------------------------------------------
  # Restore alpha-metric ordering
  # ----------------------------------------------------------

  metric_order <- colnames(
    alpha
  )


  correlation$metric <- factor(
    correlation$metric,
    levels = metric_order
  )


  correlation <- correlation[
    order(
      correlation$metric,
      correlation$lag_days
    ),
    ,
    drop = FALSE
  ]


  correlation$metric <- as.character(
    correlation$metric
  )


  rownames(
    correlation
  ) <- NULL


  # ----------------------------------------------------------
  # Record settings
  # ----------------------------------------------------------

  settings <- list(
    sampling_even = sampling_even,
    analysis_method = method,
    rarefaction_iterations = rarefaction_iterations,
    rarefaction_depth = rarefaction_depth,
    max_lag = max_lag,
    lag_definition = "negative lag means alpha diversity precedes functionality",
    dcf_lag_method = if (
      method == "DCF"
    ) {
      "exact observed day differences; no binning"
    } else {
      NA_character_
    },
    seed = seed
  )


  # ----------------------------------------------------------
  # Construct result
  # ----------------------------------------------------------

  result <- list(
    alpha = alpha,
    correlation = correlation,
    settings = settings,
    sampling = sampling,
    method = method
  )


  class(
    result
  ) <- "dive_result"


  # ----------------------------------------------------------
  # Write tables and figures
  # ----------------------------------------------------------

  .write_dive_output(
    result = result,
    sample_dates = dates,
    output_dir = output_dir
  )


  result
}
