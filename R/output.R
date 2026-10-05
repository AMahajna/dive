# ============================================================
# DIVE
# Output
# ============================================================


.settings_to_data_frame <- function(
    settings
) {

  data.frame(
    parameter = names(
      settings
    ),

    value = vapply(
      settings,
      function(x) {

        if (is.null(
          x
        )) {

          return(
            "NULL"
          )
        }


        paste(
          x,
          collapse = ", "
        )
      },
      character(1)
    ),

    stringsAsFactors = FALSE
  )
}


.write_dive_output <- function(
    result,
    sample_dates,
    output_dir
) {

  .ensure_directory(
    output_dir
  )


  alpha_output <- data.frame(
    Date = sample_dates,
    result$alpha,
    check.names = FALSE
  )


  utils::write.csv(
    alpha_output,
    file = file.path(
      output_dir,
      "alpha_diversity.csv"
    ),
    row.names = FALSE
  )


  utils::write.csv(
    result$sampling,
    file = file.path(
      output_dir,
      "sampling_summary.csv"
    ),
    row.names = FALSE
  )


  utils::write.csv(
    .settings_to_data_frame(
      result$settings
    ),
    file = file.path(
      output_dir,
      "analysis_settings.csv"
    ),
    row.names = FALSE
  )


  if (result$method == "DCF") {

    result_filename <-
      "dcf_exact_lag_results.csv"

  } else {

    result_filename <-
      "ccf_results.csv"
  }


  utils::write.csv(
    result$correlation,
    file = file.path(
      output_dir,
      result_filename
    ),
    row.names = FALSE
  )


  plot_directory <- file.path(
    output_dir,
    "correlation_plots"
  )


  .ensure_directory(
    plot_directory
  )


  for (metric in colnames(
    result$alpha
  )) {

    safe_metric <- .sanitize_filename(
      metric
    )


    p1 <- plot(
      result,
      metric = metric,
      show_title = TRUE
    )


    p2 <- plot(
      result,
      metric = metric,
      show_title = FALSE
    )


    prefix <- tolower(
      result$method
    )


    ggplot2::ggsave(
      filename = file.path(
        plot_directory,
        paste0(
          prefix,
          "_",
          safe_metric,
          "_with_title.png"
        )
      ),
      plot = p1,
      width = 7,
      height = 5,
      units = "in",
      dpi = 600
    )


    ggplot2::ggsave(
      filename = file.path(
        plot_directory,
        paste0(
          prefix,
          "_",
          safe_metric,
          "_no_title.png"
        )
      ),
      plot = p2,
      width = 7,
      height = 5,
      units = "in",
      dpi = 600
    )
  }


  heatmap_plot <- plot_dive_heatmap(
    result
  )


  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "dive_temporal_heatmap_all_alpha_metrics.png"
    ),
    plot = heatmap_plot,
    width = 10,
    height = max(
      6,
      2 +
        0.3 *
        ncol(
          result$alpha
        )
    ),
    units = "in",
    dpi = 600,
    limitsize = FALSE
  )


  invisible(
    result
  )
}
