# ============================================================
# DIVE
# Cross-correlation function
#
# Used for evenly sampled data.
#
# Convention:
#
# negative lag = alpha diversity precedes functionality
#
# lag = -1:
# alpha at observation t is paired with functionality at t + 1
# ============================================================


.calculate_single_ccf <- function(
    x,
    y,
    max_lag,
    sampling_interval_days
) {

  if (length(
    x
  ) != length(
    y
  )) {

    stop(
      "`x` and `y` must have identical lengths."
    )
  }


  n <- length(
    x
  )


  max_lag <- min(
    as.integer(
      max_lag
    ),
    n - 2L
  )


  lag_steps <- seq.int(
    from = -max_lag,
    to = 0L
  )


  result_rows <- vector(
    mode = "list",
    length = length(
      lag_steps
    )
  )


  for (lag_index in seq_along(
    lag_steps
  )) {

    lag_value <- lag_steps[
      lag_index
    ]


    k <- abs(
      lag_value
    )


    if (k == 0L) {

      x_lag <- x

      y_lag <- y

    } else {

      x_lag <- x[
        seq_len(
          n - k
        )
      ]


      y_lag <- y[
        seq.int(
          from = k + 1L,
          to = n
        )
      ]
    }


    complete <- is.finite(
      x_lag
    ) &
      is.finite(
        y_lag
      )


    x_complete <- x_lag[
      complete
    ]


    y_complete <- y_lag[
      complete
    ]


    n_pairs <- length(
      x_complete
    )


    if (
      n_pairs >= 3L &&
      stats::sd(
        x_complete
      ) > 0 &&
      stats::sd(
        y_complete
      ) > 0
    ) {

      correlation <- stats::cor(
        x_complete,
        y_complete,
        method = "pearson"
      )

    } else {

      correlation <- NA_real_
    }


    result_rows[[lag_index]] <- data.frame(
      lag = lag_value,

      lag_days =
        lag_value *
        sampling_interval_days,

      n_pairs = n_pairs,

      correlation = correlation
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


.calculate_ccf_table <- function(
    alpha,
    functionality,
    max_lag,
    sampling_interval_days
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


    metric_result <- .calculate_single_ccf(
      x = alpha[[metric_name]],
      y = functionality,
      max_lag = max_lag,
      sampling_interval_days = sampling_interval_days
    )


    metric_result$metric <-
      metric_name


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


.run_ccf_analysis <- function(
    alpha,
    functionality,
    max_lag,
    sampling_interval_days
) {

  .calculate_ccf_table(
    alpha = alpha,
    functionality = functionality,
    max_lag = max_lag,
    sampling_interval_days = sampling_interval_days
  )
}
