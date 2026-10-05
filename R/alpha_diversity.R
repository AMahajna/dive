# ============================================================
# DIVE
# Alpha-diversity calculations
# ============================================================


.calculate_alpha <- function(
    tse,
    rarefaction_depth = NULL,
    rarefaction_iterations = 50L
) {

  rarefaction_iterations <- .validate_positive_integer(
    rarefaction_iterations,
    "rarefaction_iterations"
  )


  assay_names <- SummarizedExperiment::assayNames(
    tse
  )


  if (!"counts" %in% assay_names) {

    stop(
      "`tse` must contain an assay named `counts`."
    )
  }


  counts <- SummarizedExperiment::assay(
    tse,
    "counts"
  )


  library_sizes <- colSums(
    counts,
    na.rm = TRUE
  )


  if (any(
    !is.finite(
      library_sizes
    )
  )) {

    stop(
      "Invalid library sizes were detected."
    )
  }


  if (any(
    library_sizes <= 0
  )) {

    stop(
      "Every sample must contain a positive library size."
    )
  }


  if (is.null(
    rarefaction_depth
  )) {

    rarefaction_depth <- floor(
      min(
        library_sizes
      )
    )

  } else {

    if (
      length(rarefaction_depth) != 1L ||
      !is.numeric(rarefaction_depth) ||
      !is.finite(rarefaction_depth) ||
      rarefaction_depth <= 0
    ) {

      stop(
        "`rarefaction_depth` must be NULL or one ",
        "positive number."
      )
    }


    rarefaction_depth <- floor(
      rarefaction_depth
    )


    if (rarefaction_depth > min(
      library_sizes
    )) {

      stop(
        "`rarefaction_depth` cannot exceed the smallest ",
        "library size."
      )
    }
  }


  alpha_indices <- c(
    "ace",
    "chao1",
    "hill",
    "observed",
    "coverage",
    "fisher",
    "gini_simpson",
    "inverse_simpson",
    "shannon",
    "log_modulo_skewness",
    "camargo",
    "pielou",
    "simpson_evenness",
    "evar",
    "bulla",
    "absolute",
    "dbp",
    "core_abundance",
    "gini",
    "dmn",
    "relative",
    "simpson_lambda"
  )


  tse_alpha <- mia::addAlpha(
    tse,
    assay.type = "counts",
    index = alpha_indices,
    name = alpha_indices,
    sample = rarefaction_depth,
    niter = rarefaction_iterations
  )


  sample_metadata <- as.data.frame(
    SummarizedExperiment::colData(
      tse_alpha
    )
  )


  missing_metrics <- setdiff(
    alpha_indices,
    colnames(
      sample_metadata
    )
  )


  if (length(
    missing_metrics
  ) > 0L) {

    stop(
      "The following metrics were not returned by ",
      "`mia::addAlpha()`: ",
      paste(
        missing_metrics,
        collapse = ", "
      )
    )
  }


  alpha <- sample_metadata[
    ,
    alpha_indices,
    drop = FALSE
  ]


  for (metric_name in alpha_indices) {

    alpha[[metric_name]] <- as.numeric(
      alpha[[metric_name]]
    )
  }


  if (nrow(
    alpha
  ) != ncol(
    tse
  )) {

    stop(
      "Alpha-diversity output does not contain one row ",
      "per sample."
    )
  }


  rownames(
    alpha
  ) <- colnames(
    tse
  )


  alpha
}
