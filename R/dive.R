# ============================================================
# DIVE
# Main user-facing analysis function
# ============================================================


#' Diversity-Informed Valuation of Ecosystem Functioning
#'
#' Calculates microbial alpha-diversity descriptors and relates
#' them to ecosystem functionality through temporal association
#' analysis.
#'
#' Regularly sampled data are analyzed using lagged Pearson
#' cross-correlation. Irregularly sampled data are analyzed using
#' an exact-lag discrete correlation function.
#'
#' Negative lag values indicate that alpha diversity precedes
#' ecosystem functionality.
#'
#' @param tse A TreeSummarizedExperiment or compatible
#' SummarizedExperiment object containing a \code{counts} assay.
#' Sampling dates must be stored in \code{colData(tse)$Date}.
#'
#' @param functionality Numeric vector containing one ecosystem
#' functionality value for each sample, in the same order as the
#' columns of \code{tse}.
#'
#' @param sampling_even Integer. Use 1 for evenly sampled data
#' and 0 for unevenly sampled data.
#'
#' @param functionality_name Character label describing the
#' ecosystem functionality.
#'
#' @param output_dir Directory in which DIVE outputs will be
#' written.
#'
#' @param rarefaction_iterations Number of repeated rarefaction
#' iterations used by \code{mia::addAlpha()}.
#'
#' @param rarefaction_depth Rarefaction depth. If NULL, the
#' smallest observed library size is used.
#'
#' @param max_lag Maximum temporal lag. For irregular sampling
#' this is expressed in days. For regular sampling it represents
#' the maximum number of sampling intervals.
#'
#' @param seed Random seed used for alpha-diversity rarefaction.
#'
#' @return An object of class \code{dive_result}.
#'
#' @export
# ------------------------------------------------------------

dive <- function(
    tse,
    functionality,
    sampling_even,
    functionality_name = "Functionality",
    output_dir = "DIVE_results",
    rarefaction_iterations = 50L,
    rarefaction_depth = NULL,
    max_lag = 63,
    seed = 123
) {

  # ----------------------------------------------------------
  # Validate general arguments
  # ----------------------------------------------------------

  if (
    !is.character(
      functionality_name
    ) ||
    length(
      functionality_name
    ) != 1L
  ) {

    stop(
      "functionality_name must be a single character string.",
      call. = FALSE
    )
  }


  if (
    !is.character(
      output_dir
    ) ||
    length(
      output_dir
    ) != 1L
  ) {

    stop(
      "output_dir must be a single character string.",
      call. = FALSE
    )
  }


  sampling_even <- as.integer(
    sampling_even
  )


  if (
    !sampling_even %in% c(
      0L,
      1L
    )
  ) {

    stop(
      "sampling_even must be either 0 or 1.",
      call. = FALSE
    )
  }


  rarefaction_iterations <- .validate_positive_integer(
    rarefaction_iterations,
    "rarefaction_iterations"
  )


  if (
    !.is_single_number(
      seed
    )
  ) {

    stop(
      "seed must be a single finite number.",
      call. = FALSE
    )
  }


  # ----------------------------------------------------------
  # Dates and functionality
  # ----------------------------------------------------------

  dates <- .get_dive_dates(
    tse
  )


  functionality <- .prepare_functionality(

    functionality = functionality,

    n_samples = length(
      dates
    )
  )


  # ----------------------------------------------------------
  # Sort all observations chronologically
  # ----------------------------------------------------------

  chronological_order <- order(
    dates
  )


  dates <- dates[
    chronological_order
  ]


  functionality <- functionality[
    chronological_order
  ]


  tse <- tse[
    ,
    chronological_order
  ]


  # ----------------------------------------------------------
  # Inspect sampling pattern
  # ----------------------------------------------------------

  sampling_information <- .inspect_sampling(

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

    rarefaction_iterations =
      rarefaction_iterations
  )


  resolved_rarefaction_depth <- attr(
    alpha,
    "rarefaction_depth"
  )


  # ----------------------------------------------------------
  # Temporal association
  # ----------------------------------------------------------

  if (
    sampling_even == 0L
  ) {

    correlation <- .calculate_dcf_table(

      alpha = alpha,

      functionality = functionality,

      dates = dates,

      max_lag = max_lag
    )


    method <- "DCF"

  } else {

    correlation <- .calculate_ccf_table(

      alpha = alpha,

      functionality = functionality,

      dates = dates,

      max_lag = max_lag
    )


    method <- "CCF"
  }


  # ----------------------------------------------------------
  # Construct result object
  # ----------------------------------------------------------

  result <- list(

    alpha = alpha,

    functionality = functionality,

    dates = dates,

    correlation = correlation,

    sampling = sampling_information,

    method = method,

    settings = list(

      functionality_name =
        functionality_name,

      sampling_even =
        sampling_even,

      output_dir =
        output_dir,

      rarefaction_iterations =
        rarefaction_iterations,

      rarefaction_depth =
        resolved_rarefaction_depth,

      max_lag =
        max_lag,

      seed =
        seed
    )
  )


  class(
    result
  ) <- "dive_result"


  # ----------------------------------------------------------
  # Write results
  # ----------------------------------------------------------

  .write_dive_output(

    result = result,

    output_dir = output_dir
  )


  result
}
