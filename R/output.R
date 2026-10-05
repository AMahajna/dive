# ============================================================
# DIVE
# Output writing
# ============================================================


# ------------------------------------------------------------
# Write DIVE results to disk
# ------------------------------------------------------------

.write_dive_output <- function(
    result,
    output_dir
) {

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )


  # ----------------------------------------------------------
  # Alpha-diversity output
  # ----------------------------------------------------------

  alpha_output <- data.frame(

    Date = result$dates,

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


  # ----------------------------------------------------------
  # Functionality output
  # ----------------------------------------------------------

  functionality_output <- data.frame(

    Date = result$dates,

    functionality = result$functionality
  )


  utils::write.csv(

    functionality_output,

    file = file.path(
      output_dir,
      "functionality.csv"
    ),

    row.names = FALSE
  )


  # ----------------------------------------------------------
  # Correlation / DCF output
  # ----------------------------------------------------------

  correlation_filename <- if (
    identical(
      result$method,
      "DCF"
    )
  ) {

    "dcf_exact_lag_results.csv"

  } else {

    "ccf_results.csv"
  }


  utils::write.csv(

    result$correlation,

    file = file.path(
      output_dir,
      correlation_filename
    ),

    row.names = FALSE
  )


  # ----------------------------------------------------------
  # Sampling summary
  # ----------------------------------------------------------

  utils::write.csv(

    result$sampling$summary,

    file = file.path(
      output_dir,
      "sampling_summary.csv"
    ),

    row.names = FALSE
  )


  # ----------------------------------------------------------
  # Analysis settings
  # ----------------------------------------------------------

  settings_table <- data.frame(

    setting = c(
      "functionality_name",
      "method",
      "sampling_even",
      "max_lag",
      "rarefaction_iterations",
      "rarefaction_depth",
      "seed"
    ),

    value = c(
      result$settings$functionality_name,
      result$method,
      result$settings$sampling_even,
      result$settings$max_lag,
      result$settings$rarefaction_iterations,
      result$settings$rarefaction_depth,
      result$settings$seed
    ),

    stringsAsFactors = FALSE
  )


  utils::write.csv(

    settings_table,

    file = file.path(
      output_dir,
      "analysis_settings.csv"
    ),

    row.names = FALSE
  )


  # ----------------------------------------------------------
  # Figure directories
  # ----------------------------------------------------------

  figure_directory <- file.path(
    output_dir,
    "correlation_plots"
  )


  dir.create(
    figure_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )


  # ----------------------------------------------------------
  # Individual metric figures
  # ----------------------------------------------------------

  metric_names <- unique(
    result$correlation$metric
  )


  for (
    metric_name in metric_names
  ) {

    safe_metric_name <- .sanitize_filename(
      metric_name
    )


    plot_with_title <- .plot_metric_association(

      result = result,

      metric = metric_name,

      show_title = TRUE
    )


    plot_without_title <- .plot_metric_association(

      result = result,

      metric = metric_name,

      show_title = FALSE
    )


    ggplot2::ggsave(

      filename = file.path(
        figure_directory,
        paste0(
          safe_metric_name,
          "_with_title.png"
        )
      ),

      plot = plot_with_title,

      width = 7,

      height = 5,

      units = "in",

      dpi = 600,

      bg = "white"
    )


    ggplot2::ggsave(

      filename = file.path(
        figure_directory,
        paste0(
          safe_metric_name,
          "_without_title.png"
        )
      ),

      plot = plot_without_title,

      width = 7,

      height = 5,

      units = "in",

      dpi = 600,

      bg = "white"
    )
  }


  # ----------------------------------------------------------
  # Heatmap
  # ----------------------------------------------------------

  heatmap_with_title <- plot_dive_heatmap(

    result,

    show_title = TRUE
  )


  heatmap_without_title <- plot_dive_heatmap(

    result,

    show_title = FALSE
  )


  ggplot2::ggsave(

    filename = file.path(
      output_dir,
      "dive_temporal_heatmap_all_alpha_metrics.png"
    ),

    plot = heatmap_with_title,

    width = 11,

    height = 8,

    units = "in",

    dpi = 600,

    bg = "white"
  )


  ggplot2::ggsave(

    filename = file.path(
      output_dir,
      "dive_temporal_heatmap_all_alpha_metrics_no_title.png"
    ),

    plot = heatmap_without_title,

    width = 11,

    height = 8,

    units = "in",

    dpi = 600,

    bg = "white"
  )


  invisible(
    result
  )
}
