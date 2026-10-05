# ============================================================
# DIVE
# Plotting
# ============================================================


.plot_dcf_metric <- function(
    result,
    metric,
    show_title = TRUE
) {

  metric_data <- result$correlation[
    result$correlation$metric ==
      metric,
    ,
    drop = FALSE
  ]


  p <- ggplot2::ggplot(
    metric_data,
    ggplot2::aes(
      x = lag_days,
      y = dcf
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = n_pairs
      ),
      na.rm = TRUE
    ) +
    ggplot2::scale_size_continuous(
      name = "Exact\npairs"
    ) +
    ggplot2::labs(
      x = "Exact lag (days)",
      y = "DCF"
    ) +
    ggplot2::theme_bw(
      base_size = 12
    )


  if (show_title) {

    p <- p +
      ggplot2::labs(
        title = paste0(
          "Exact-lag DCF: ",
          metric
        )
      )
  }


  p
}


.plot_ccf_metric <- function(
    result,
    metric,
    show_title = TRUE
) {

  metric_data <- result$correlation[
    result$correlation$metric ==
      metric,
    ,
    drop = FALSE
  ]


  p <- ggplot2::ggplot(
    metric_data,
    ggplot2::aes(
      x = lag_days,
      y = correlation
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      size = 2.5,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      x = "Lag (days)",
      y = "Correlation"
    ) +
    ggplot2::theme_bw(
      base_size = 12
    )


  if (show_title) {

    p <- p +
      ggplot2::labs(
        title = paste0(
          "CCF: ",
          metric
        )
      )
  }


  p
}


#' Plot an individual DIVE result
#'
#' @param x A `dive_result`.
#' @param metric Alpha-diversity metric name.
#' @param show_title Logical.
#' @param ... Additional arguments.
#'
#' @return A ggplot object.
#'
#' @export
plot.dive_result <- function(
    x,
    metric,
    show_title = TRUE,
    ...
) {

  .validate_dive_result(
    x
  )


  if (!metric %in% colnames(
    x$alpha
  )) {

    stop(
      "Unknown alpha-diversity metric."
    )
  }


  if (x$method == "DCF") {

    return(
      .plot_dcf_metric(
        result = x,
        metric = metric,
        show_title = show_title
      )
    )
  }


  .plot_ccf_metric(
    result = x,
    metric = metric,
    show_title = show_title
  )
}


#' Plot all DIVE temporal associations
#'
#' @param result A `dive_result`.
#'
#' @return A ggplot object.
#'
#' @export
plot_dive_heatmap <- function(
    result
) {

  .validate_dive_result(
    result
  )


  plot_data <- result$correlation


  if (result$method == "DCF") {

    plot_data$association <-
      plot_data$dcf

    legend_title <- "DCF"

    x_title <- "Exact lag (days)"

  } else {

    plot_data$association <-
      plot_data$correlation

    legend_title <- "Correlation"

    x_title <- "Lag (days)"
  }


  plot_data$metric <- factor(
    plot_data$metric,
    levels = rev(
      colnames(
        result$alpha
      )
    )
  )


  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = lag_days,
      y = metric,
      fill = association
    )
  ) +
    ggplot2::geom_tile(
      width = 1,
      height = 0.9
    ) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0,
      name = legend_title
    ) +
    ggplot2::labs(
      x = x_title,
      y = "Alpha-diversity metric"
    ) +
    ggplot2::theme_bw(
      base_size = 11
    ) +
    ggplot2::theme(
      panel.grid =
        ggplot2::element_blank()
    )
}
