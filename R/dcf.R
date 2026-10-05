# ============================================================
# DIVE
# Exact-lag discrete correlation function
# for irregularly sampled observations
# ============================================================


# ------------------------------------------------------------
# Calculate exact-lag DCF for one alpha-diversity metric
#
# Lag convention:
#
# lag_days = alpha_date - functionality_date
#
# Negative lag:
# alpha diversity precedes functionality
#
# lag 0:
# alpha diversity and functionality were measured on
# the same sampling date
# ------------------------------------------------------------

.calculate_single_dcf <- function(
    alpha_values,
    functionality,
    dates,
    max_lag
) {

  alpha_values <- as.numeric(
    alpha_values
  )


  functionality <- as.numeric(
    functionality
  )


  max_lag <- .validate_nonnegative_number(
    max_lag,
    "max_lag"
  )


  alpha_z <- .standardize_vector(
    alpha_values
  )


  functionality_z <- .standardize_vector(
    functionality
  )


  if (
    all(
      is.na(
        alpha_z
      )
    )
  ) {

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


  if (
    all(
      is.na(
        functionality_z
      )
    )
  ) {

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


  pair_rows <- vector(
    mode = "list",
    length = length(
      alpha_values
    ) *
      length(
        functionality
      )
  )


  row_index <- 1L


  for (
    alpha_index in seq_along(
      alpha_values
    )
  ) {

    for (
      function_index in seq_along(
        functionality
      )
    ) {

      lag_days <- as.numeric(
        dates[alpha_index] -
          dates[function_index]
      )


      if (
        lag_days < -max_lag ||
        lag_days > 0
      ) {

        next
      }


      udcf <- alpha_z[alpha_index] *
        functionality_z[function_index]


      if (
        !is.finite(
          udcf
        )
      ) {

        next
      }


      pair_rows[[row_index]] <- data.frame(

        lag_days = lag_days,

        udcf = udcf
      )


      row_index <- row_index + 1L
    }
  }


  pair_rows <- pair_rows[
    !vapply(
      pair_rows,
      is.null,
      logical(1)
    )
  ]


  if (
    length(
      pair_rows
    ) == 0L
  ) {

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


  pair_table <- do.call(
    rbind,
    pair_rows
  )


  observed_lags <- sort(
    unique(
      pair_table$lag_days
    )
  )


  result_rows <- vector(
    mode = "list",
    length = length(
      observed_lags
    )
  )


  for (
    lag_index in seq_along(
      observed_lags
    )
  ) {

    current_lag <- observed_lags[
      lag_index
    ]


    lag_values <- pair_table$udcf[
      pair_table$lag_days ==
        current_lag
    ]


    n_pairs <- length(
      lag_values
    )


    dcf_value <- mean(
      lag_values
    )


    dcf_sd <- if (
      n_pairs >= 2L
    ) {

      stats::sd(
        lag_values
      )

    } else {

      NA_real_
    }


    dcf_se <- if (
      n_pairs >= 2L
    ) {

      dcf_sd /
        sqrt(
          n_pairs
        )

    } else {

      NA_real_
    }


    result_rows[[lag_index]] <- data.frame(

      lag_days = current_lag,

      n_pairs = n_pairs,

      dcf = dcf_value,

      dcf_sd = dcf_sd,

      dcf_se = dcf_se
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
# Calculate exact-lag DCF for all alpha-diversity metrics
# ------------------------------------------------------------

.calculate_dcf_table <- function(
    alpha,
    functionality,
    dates,
    max_lag
) {

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


    metric_result <- .calculate_single_dcf(

      alpha_values =
        alpha[[metric_name]],

      functionality =
        functionality,

      dates =
        dates,

      max_lag =
        max_lag
    )


    if (
      nrow(
        metric_result
      ) == 0L
    ) {

      next
    }


    metric_result$metric <- metric_name


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


    result_rows[[metric_index]] <- metric_result
  }


  result_rows <- result_rows[
    !vapply(
      result_rows,
      is.null,
      logical(1)
    )
  ]


  if (
    length(
      result_rows
    ) == 0L
  ) {

    return(
      data.frame(
        metric = character(0),
        lag_days = numeric(0),
        n_pairs = integer(0),
        dcf = numeric(0),
        dcf_sd = numeric(0),
        dcf_se = numeric(0)
      )
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
