# ============================================================
# DIVE
# Sampling-date handling and functionality validation
# ============================================================


# ------------------------------------------------------------
# Extract sampling dates from a TreeSummarizedExperiment
# ------------------------------------------------------------

.get_dive_dates <- function(tse) {

  sample_metadata <- SummarizedExperiment::colData(
    tse
  )

  if (
    !"Date" %in% colnames(
      sample_metadata
    )
  ) {

    stop(
      "The TSE must contain a column named 'Date' in colData(tse).",
      call. = FALSE
    )
  }


  dates <- sample_metadata$Date


  if (
    !inherits(
      dates,
      "Date"
    )
  ) {

    dates <- tryCatch(

      as.Date(
        dates
      ),

      error = function(e) {

        stop(
          "colData(tse)$Date could not be converted to class 'Date'.",
          call. = FALSE
        )
      }
    )
  }


  if (
    anyNA(
      dates
    )
  ) {

    stop(
      "colData(tse)$Date contains missing or invalid dates.",
      call. = FALSE
    )
  }


  if (
    anyDuplicated(
      dates
    )
  ) {

    warning(
      "Duplicate sampling dates were detected in colData(tse)$Date."
    )
  }


  dates
}


# ------------------------------------------------------------
# Validate functionality vector
# ------------------------------------------------------------

.prepare_functionality <- function(
    functionality,
    n_samples
) {

  if (
    !is.numeric(
      functionality
    )
  ) {

    stop(
      "functionality must be a numeric vector.",
      call. = FALSE
    )
  }


  functionality <- as.numeric(
    functionality
  )


  if (
    length(
      functionality
    ) != n_samples
  ) {

    stop(
      "The length of functionality must equal the number of samples in the TSE.",
      call. = FALSE
    )
  }


  if (
    anyNA(
      functionality
    )
  ) {

    stop(
      "functionality contains missing values.",
      call. = FALSE
    )
  }


  if (
    any(
      !is.finite(
        functionality
      )
    )
  ) {

    stop(
      "functionality contains non-finite values.",
      call. = FALSE
    )
  }


  functionality
}


# ------------------------------------------------------------
# Inspect temporal sampling
# ------------------------------------------------------------

.inspect_sampling <- function(
    dates,
    sampling_even
) {

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


  dates_sorted <- sort(
    dates
  )


  intervals <- as.numeric(
    diff(
      dates_sorted
    )
  )


  if (
    length(
      intervals
    ) == 0L
  ) {

    stop(
      "At least two sampling dates are required.",
      call. = FALSE
    )
  }


  observed_even <- length(
    unique(
      intervals
    )
  ) == 1L


  if (
    sampling_even == 1L &&
    !observed_even
  ) {

    warning(
      paste0(
        "sampling_even = 1 was requested, but the observed ",
        "sampling intervals are not identical."
      )
    )
  }


  selected_method <- if (
    sampling_even == 1L
  ) {

    "CCF"

  } else {

    "Exact-lag DCF"
  }


  summary_table <- data.frame(

    n_samples = length(
      dates_sorted
    ),

    first_date = min(
      dates_sorted
    ),

    last_date = max(
      dates_sorted
    ),

    study_duration_days = as.numeric(
      max(
        dates_sorted
      ) -
        min(
          dates_sorted
        )
    ),

    minimum_interval_days = min(
      intervals
    ),

    median_interval_days = stats::median(
      intervals
    ),

    mean_interval_days = mean(
      intervals
    ),

    maximum_interval_days = max(
      intervals
    ),

    sd_interval_days = stats::sd(
      intervals
    ),

    observed_even_sampling = observed_even,

    requested_sampling_even = sampling_even,

    selected_method = selected_method,

    stringsAsFactors = FALSE
  )


  list(

    summary = summary_table,

    intervals = intervals,

    observed_even = observed_even,

    selected_method = selected_method
  )
}
