# ============================================================
# DIVE
# Exact-lag discrete correlation function
#
# Used for unevenly sampled data.
#
# No temporal binning is performed.
#
# lag_days = alpha_date - functionality_date
#
# Negative lag:
# alpha diversity precedes functionality.
#
# Lag zero:
# alpha diversity and functionality come from the exact same
# sampling date.
# ============================================================


.calculate_single_dcf <- function(
    x,
    y,
    dates,
    max_lag_days
) {

  if (
    length(x) != length(y) ||
    length(x) != length(dates)
  ) {

    stop(
      "`x`, `y`, and `dates` must have identical lengths."
    )
  }


  finite_x <- is.finite(
    x
  )


  finite_y <- is.finite(
    y
  )


  if (sum(
    finite_x
  ) < 2L) {

    stop(
      "At least two finite alpha-diversity values are required."
    )
  }


  if (sum(
    finite_y
  ) < 2L) {

    stop(
      "At least two finite functionality values are required."
    )
  }


  x_mean <- mean(
    x[
      finite_x
    ]
  )


  y_mean <- mean(
    y[
      finite_y
    ]
  )


  x_sd <- sqrt(
    mean(
      (
        x[
          finite_x
        ] -
          x_mean
      )^2
    )
  )


  y_sd <- sqrt(
    mean(
      (
        y[
          finite_y
        ] -
          y_mean
      )^2
    )
  )


  if (
    !is.finite(x_sd) ||
    x_sd == 0
  ) {

    stop(
      "Alpha-diversity values have zero variance."
    )
  }


  if (
    !is.finite(y_sd) ||
    y_sd == 0
  ) {

    stop(
      "Functionality values have zero variance."
    )
  }


  x_standardized <- (
    x -
      x_mean
  ) / x_sd


  y_standardized <- (
    y -
      y_mean
  ) / y_sd


  pairs <- expand.grid(
    alpha_index = seq_along(
      x
    ),
    functionality_index = seq_along(
      y
    ),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )


  pairs$alpha_date <- dates[
    pairs$alpha_index
  ]


  pairs$functionality_date <- dates[
    pairs$functionality_index
  ]


  pairs$lag_days <- as.numeric(
    pairs$alpha_date -
      pairs$functionality_date
  )


  pairs$udcf <-
    x_standardized[
      pairs$alpha_index
    ] *
    y_standardized[
      pairs$functionality_index
    ]


  pairs <- pairs[
    is.finite(
      pairs$udcf
    ) &
      pairs$lag_days <= 0 &
      pairs$lag_days >= -max_lag_days,
    ,
    drop = FALSE
  ]


  if (nrow(
    pairs
  ) == 0L) {

    return(
      data.frame(
        lag_days = numeric(0),
        n_pairs = integer(0),
        dcf = numeric(0),
        dcf_sd = numeric(0),
        dcf_se = numeric(0)
      )
    )
  }


  exact_lags <- sort(
    unique(
      pairs$lag_days
    )
  )


  result_rows <- vector(
    mode = "list",
    length = length(
      exact_lags
    )
  )


  for (lag_index in seq_along(
    exact_lags
  )) {

    exact_lag <- exact_lags[
      lag_index
    ]


    lag_values <- pairs$udcf[
      pairs$lag_days ==
        exact_lag
    ]


    n_pairs <- length(
      lag_values
    )


    dcf_value <- mean(
      lag_values
    )


    if (n_pairs > 1L) {

      dcf_sd <- stats::sd(
        lag_values
      )


      dcf_se <- dcf_sd /
        sqrt(
          n_pairs
        )

    } else {

      dcf_sd <- NA_real_

      dcf_se <- NA_real_
    }


    result_rows[[lag_index]] <- data.frame(
      lag_days = exact_lag,
      n_pairs = n_pairs,
      dcf = dcf_value,
      dcf_sd = dcf_sd,
      dcf_se = dcf_se
    )
  }


  output <- do.call(
    rbind,
    result_rows
  )


  rownames(
    output
  ) <- NULL


  output
}


.calculate_dcf_table <- function(
    alpha,
    functionality,
    dates,
    max_lag_days
) {

  alpha <- as.data.frame(
    alpha
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


  for (metric_index in seq_along(
    metric_names
  )) {

    metric_name <- metric_names[
      metric_index
    ]


    metric_result <- .calculate_single_dcf(
      x = alpha[[metric_name]],
      y = functionality,
      dates = dates,
      max_lag_days = max_lag_days
    )


    metric_result$metric <-
      metric_name


    metric_result <- metric_result[
      ,
      c(
        "metric",
        "lag_days",
        "n_pairs",
        "dcf",
        "dcf_sd",
        "dcf_se"
      ),
      drop = FALSE
    ]


    result_rows[[metric_index]] <-
      metric_result
  }


  output <- do.call(
    rbind,
    result_rows
  )


  rownames(
    output
  ) <- NULL


  output
}


.run_dcf_analysis <- function(
    alpha,
    functionality,
    dates,
    max_lag_days
) {

  .calculate_dcf_table(
    alpha = alpha,
    functionality = functionality,
    dates = dates,
    max_lag_days = max_lag_days
  )
}
