# ============================================================
# DIVE
# Plotting functions
# ============================================================


# ------------------------------------------------------------
# Internal association plot for one alpha metric
# ------------------------------------------------------------

.plot_metric_association <- function(
    result,
    metric,
    show_title = TRUE
) {

  .validate_dive_result(
    result
  )


  correlation_table <- result$correlation


  metric_data <- correlation_table[
    correlation_table$metric ==
      metric,
    ,
    drop = FALSE
  ]


  if (
    nrow(
      metric_data
    ) == 0L
  ) {

    stop(
      paste0(
        "No temporal-association results were found for metric: ",
        metric
      ),
      call. = FALSE
    )
  }


  if (
    identical(
      result$method,
      "DCF"
    )
  ) {

    plot_object <- ggplot2::ggplot(

      metric_data,

      ggplot2::aes(
        x = lag_days,
        y = dcf
      )
    ) +

      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dotted",
        linewidth = 0.4,
        color = "grey50"
      ) +

      ggplot2::geom_line(
        linewidth = 0.8
      ) +

      ggplot2::geom_point(
        size = 2.4
      ) +

      ggplot2::labs(
        x = "Lag (days)",
        y = "Exact-lag DCF"
      )

  } else {

    plot_object <- ggplot2::ggplot(

      metric_data,

      ggplot2::aes(
        x = lag_days,
        y = correlation
      )
    ) +

      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dotted",
        linewidth = 0.4,
        color = "grey50"
      ) +

      ggplot2::geom_line(
        linewidth = 0.8
      ) +

      ggplot2::geom_point(
        size = 2.4
      ) +

      ggplot2::scale_y_continuous(
        limits = c(
          -1,
          1
        )
      ) +

      ggplot2::labs(
        x = "Lag (days)",
        y = "Pearson correlation"
      )
  }


  if (
    isTRUE(
      show_title
    )
  ) {

    plot_object <- plot_object +

      ggplot2::labs(
        title = metric
      )

  } else {

    plot_object <- plot_object +

      ggplot2::labs(
        title = NULL
      )
  }


  plot_object +

    ggplot2::theme_classic(
      base_size = 12
    ) +

    ggplot2::theme(

      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5
      ),

      axis.title = ggplot2::element_text(
        size = 11
      ),

      axis.text = ggplot2::element_text(
        size = 9
      )
    )
}


# ------------------------------------------------------------
#' Plot a DIVE heatmap
#'
#' Creates a heatmap of temporal association values across
#' alpha-diversity metrics and temporal lags.
#'
#' @param x A \code{dive_result} object.
#' @param show_title Logical. Add a plot title?
#'
#' @return A ggplot object.
#'
#' @export
# ------------------------------------------------------------

plot_dive_heatmap <- function(
    x,
    show_title = TRUE
) {

  .validate_dive_result(
    x
  )


  plot_data <- x$correlation


  if (
    identical(
      x$method,
      "DCF"
    )
  ) {

    plot_data$association <- plot_data$dcf

    legend_title <- "DCF"

  } else {

    plot_data$association <- plot_data$correlation

    legend_title <- "Correlation"
  }


  plot_data$metric <- factor(
    plot_data$metric,
    levels = rev(
      unique(
        plot_data$metric
      )
    )
  )


  heatmap <- ggplot2::ggplot(

    plot_data,

    ggplot2::aes(
      x = lag_days,
      y = metric,
      fill = association
    )
  ) +

    ggplot2::geom_tile() +

    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      name = legend_title
    ) +

    ggplot2::labs(
      x = "Lag (days)",
      y = "Alpha-diversity metric"
    ) +

    ggplot2::theme_classic(
      base_size = 11
    ) +

    ggplot2::theme(

      axis.text.y = ggplot2::element_text(
        size = 8
      ),

      legend.position = "right"
    )


  if (
    isTRUE(
      show_title
    )
  ) {

    heatmap <- heatmap +

      ggplot2::labs(
        title = paste0(
          x$settings$functionality_name,
          ": temporal association with alpha diversity"
        )
      ) +

      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          hjust = 0.5
        )
      )
  }


  heatmap
}


# ------------------------------------------------------------
#' Plot representative DIVE time series
#'
#' Produces separate time-series panels for selected
#' alpha-diversity metrics and ecosystem functionality.
#'
#' @param x A \code{dive_result} object.
#' @param metrics Character vector of alpha-diversity metrics.
#' @param metric_labels Optional named character vector providing
#' readable labels for the selected metrics.
#' @param ncol Number of facet columns.
#'
#' @return A ggplot object.
#'
#' @export
# ------------------------------------------------------------

plot_dive_timeseries <- function(
    x,
    metrics = c(
      "observed",
      "shannon",
      "ace",
      "simpson_lambda",
      "pielou"
    ),
    metric_labels = c(
      observed = "Richness (Observed)",
      shannon = "Diversity (Shannon)",
      ace = "Rarity (ACE)",
      simpson_lambda = "Dominance (Simpson lambda)",
      pielou = "Evenness (Pielou)"
    ),
    ncol = 2
) {

  .validate_dive_result(
    x
  )


  missing_metrics <- setdiff(
    metrics,
    colnames(
      x$alpha
    )
  )


  if (
    length(
      missing_metrics
    ) > 0L
  ) {

    stop(
      paste0(
        "The following metrics are not available in x$alpha: ",
        paste(
          missing_metrics,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  alpha_rows <- vector(
    mode = "list",
    length = length(
      metrics
    )
  )


  for (
    metric_index in seq_along(
      metrics
    )
  ) {

    metric_name <- metrics[
      metric_index
    ]


    if (
      metric_name %in% names(
        metric_labels
      )
    ) {

      display_name <- unname(
        metric_labels[
          metric_name
        ]
      )

    } else {

      display_name <- metric_name
    }


    alpha_rows[[metric_index]] <- data.frame(

      Date = x$dates,

      Series = display_name,

      Value = as.numeric(
        x$alpha[[metric_name]]
      ),

      stringsAsFactors = FALSE
    )
  }


  plot_data <- do.call(
    rbind,
    alpha_rows
  )


  functionality_name <-
    x$settings$functionality_name


  functionality_data <- data.frame(

    Date = x$dates,

    Series = functionality_name,

    Value = x$functionality,

    stringsAsFactors = FALSE
  )


  plot_data <- rbind(
    plot_data,
    functionality_data
  )


  series_order <- c(

    vapply(
      metrics,
      function(metric_name) {

        if (
          metric_name %in% names(
            metric_labels
          )
        ) {

          unname(
            metric_labels[
              metric_name
            ]
          )

        } else {

          metric_name
        }
      },
      character(1)
    ),

    functionality_name
  )


  plot_data$Series <- factor(
    plot_data$Series,
    levels = series_order
  )


  alpha_colors <- grDevices::hcl.colors(
    length(
      metrics
    ),
    palette = "Dark 3"
  )


  series_colors <- c(
    alpha_colors,
    "#000000"
  )


  names(
    series_colors
  ) <- series_order


  ggplot2::ggplot(

    plot_data,

    ggplot2::aes(
      x = Date,
      y = Value,
      color = Series,
      group = Series
    )
  ) +

    ggplot2::geom_line(
      linewidth = 0.9,
      na.rm = TRUE
    ) +

    ggplot2::geom_point(
      size = 2.1,
      na.rm = TRUE
    ) +

    ggplot2::facet_wrap(
      ~ Series,
      ncol = ncol,
      scales = "free_y"
    ) +

    ggplot2::scale_color_manual(
      values = series_colors,
      guide = "none"
    ) +

    ggplot2::scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b\n%Y",
      expand = ggplot2::expansion(
        mult = c(
          0.01,
          0.02
        )
      )
    ) +

    ggplot2::labs(
      x = "Sampling date",
      y = NULL,
      title = NULL
    ) +

    ggplot2::theme_classic(
      base_size = 12
    ) +

    ggplot2::theme(

      strip.background = ggplot2::element_blank(),

      strip.text = ggplot2::element_text(
        face = "bold",
        size = 10.5
      ),

      axis.text.x = ggplot2::element_text(
        size = 8.5
      ),

      axis.text.y = ggplot2::element_text(
        size = 9
      ),

      axis.title.x = ggplot2::element_text(
        size = 11,
        margin = ggplot2::margin(
          t = 8
        )
      ),

      panel.spacing = grid::unit(
        1,
        "lines"
      )
    )
}


# ------------------------------------------------------------
#' Plot a DIVE result
#'
#' @param x A \code{dive_result} object.
#' @param ... Additional arguments passed to
#' \code{plot_dive_heatmap()}.
#'
#' @return The input object invisibly.
#'
#' @export
# ------------------------------------------------------------

plot.dive_result <- function(
    x,
    ...
) {

  heatmap <- plot_dive_heatmap(
    x,
    ...
  )


  print(
    heatmap
  )


  invisible(
    x
  )
}
