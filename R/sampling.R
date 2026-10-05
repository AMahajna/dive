# ============================================================
# DIVE
# Sampling and functionality utilities
# ============================================================


.get_dive_dates <- function(
    tse
) {

  sample_metadata <- SummarizedExperiment::colData(
    tse
  )


  if (!"Date" %in% colnames(sample_metadata)) {

    stop(
      "`colData(tse)` must contain a column named `Date`."
    )
  }


  dates <- sample_metadata[["Date"]]


  if (!inherits(
    dates,
    "Date"
  )) {

    stop(
      "`colData(tse)$Date` must have class `Date`."
    )
  }


  if (anyNA(
    dates
  )) {

    stop(
      "`colData(tse)$Date` contains missing values."
    )
  }


  if (anyDuplicated(
    dates
  )) {

    warning(
      "Duplicate sampling dates were detected."
    )
  }


  dates
}


.prepare_functionality <- function(
    functionality,
    n_samples
) {

  if (!is.numeric(
    functionality
  )) {

    stop(
      "`functionality` must be a numeric vector."
    )
  }


  if (length(
    functionality
  ) != n_samples) {

    stop(
      "The length of `functionality` must equal the ",
      "number of samples in `tse`."
    )
  }


  if (anyNA(
    functionality
  )) {

    stop(
      "`functionality` contains missing values."
    )
  }


  if (any(
    !is.finite(
      functionality
    )
  )) {

    stop(
      "`functionality` contains non-finite values."
    )
  }


  as.numeric(
    functionality
  )
}


.inspect_sampling <- function(
    dates,
    sampling_even
) {

  if (!inherits(
    dates,
    "Date"
  )) {

    stop(
      "`dates` must have class `Date`."
    )
  }


  if (length(
    dates
  ) < 2L) {

    stop(
      "At least two dates are required."
    )
  }


  intervals_days <- as.numeric(
    diff(
      sort(
        dates
      )
    )
  )


  observed_even <- length(
    unique(
      intervals_days
    )
  ) == 1L


  if (
    sampling_even == 1 &&
    !observed_even
  ) {

    warning(
      "`sampling_even = 1` was selected, but the observed ",
      "sampling intervals are not identical."
    )
  }


  data.frame(
    n_samples = length(dates),

    first_sample_date = as.character(
      min(dates)
    ),

    last_sample_date = as.character(
      max(dates)
    ),

    study_duration_days = as.numeric(
      max(dates) -
        min(dates)
    ),

    minimum_interval_days = min(
      intervals_days
    ),

    median_interval_days = stats::median(
      intervals_days
    ),

    mean_interval_days = mean(
      intervals_days
    ),

    maximum_interval_days = max(
      intervals_days
    ),

    interval_sd_days = stats::sd(
      intervals_days
    ),

    observed_sampling_even = observed_even,

    user_selected_sampling_even =
      sampling_even,

    selected_method = if (
      sampling_even == 1
    ) {
      "CCF"
    } else {
      "DCF"
    },

    stringsAsFactors = FALSE
  )
}
