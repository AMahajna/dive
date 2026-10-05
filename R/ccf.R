# ============================================================
# DIVE
# Cross-correlation analysis for regularly sampled data
# ============================================================


# ------------------------------------------------------------
# Calculate lagged Pearson correlations for one metric
#
# Only lags from -max_lag to 0 are calculated.
#
# Negative lag means alpha diversity precedes functionality.
# ------------------------------------------------------------

.calculate_single_ccf <- function(
    alpha_values,
    functionality,
    max_lag
) {

  alpha_values <- as.numeric(
    alpha_values
  )


  functionality <- as.numeric(
    functionality
  )


  n_samples <- length(
    alpha_values
  )


  max_lag <- .validate_positive_integer(
    max_lag,
    "max_lag"
  )


  max_lag <- min(
    max_lag,
    n_samples - 1L
  )


  lag_steps <- seq(
    from = -max_lag,
    to = 0L,
    by = 1L
  )


  result_rows <- vector(
    mode = "list",
    length = length(
      lag_steps
    )
  )


  for (
    lag_index in seq_along(
      lag_steps
    )
  ) {

    current_lag <- lag_steps[
      lag_index
    ]


    shift <- abs(
      current_lag
    )


    if (
      shift == 0L
    ) {

      x <- alpha_values

      y <- functionality

    } else {

      x <- alpha_values[
        seq_len(
          n_samples - shift
        )
      ]


      y <- functionality[
        seq.int(
          from = shift + 1L,
          to = n_samples
        )
      ]
    }


    complete_cases <- stats::complete.cases(
      x,
      y
    )


    x <- x[
      complete_cases
    ]


    y <- y[
      complete_cases
    ]


    n_pairs <- length(
      x
    )


    correlation <- NA_real_


    if (
      n_pairs >= 3L &&
      stats::sd(
        x
      ) > 0 &&
      stats::sd(
        y
      ) > 0
    ) {

      correlation <- stats::cor(
        x,
        y,
        method = "pearson"
      )
    }


    result_rows[[lag_index]] <- data.frame(

      lag = current_lag,

      n_pairs = n_pairs,

      correlation = correlation
    )
  }


  result <- do.call(
    rbind,
    result_rows
  )


  rownames(
    result
  ) <- NULL


  result
}


# ------------------------------------------------------------
# Calculate CCF table for all metrics
# ------------------------------------------------------------

.calculate_ccf_table <- function(
    alpha,
    functionality,
    dates,
    max_lag
) {

  sampling_interval_days <- stats::median(
    as.numeric(
      diff(
        dates
      )
    )
  )


  metric_names <- colnames(
    alpha
  )


  result_rows <- vector(
    mode = "list",
    length = length(
      metric_names
    )
  )


  for (
    metric_index in seq_along(
      metric_names
    )
  ) {

    metric_name <- metric_names[
      metric_index
    ]


    metric_result <- .calculate_single_ccf(

      alpha_values =
        alpha[[metric_name]],

      functionality =
        functionality,

      max_lag =
        max_lag
    )


    metric_result$metric <- metric_name


    metric_result$lag_days <-
      metric_result$lag *
      sampling_interval_days


    metric_result <- metric_result[
      ,
      c(
        "metric",
        "lag",
        "lag_days",
        "n_pairs",
        "correlation"
      ),
      drop = FALSE
    ]


    result_rows[[metric_index]] <- metric_result
  }


  result <- do.call(
    rbind,
    result_rows
  )


  rownames(
    result
  ) <- NULL


  result
}
