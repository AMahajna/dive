# ============================================================
# DIVE
# Alpha-diversity calculations
# ============================================================


# ------------------------------------------------------------
# Alpha-diversity metrics used by DIVE
# ------------------------------------------------------------

.dive_alpha_indices <- function() {

  c(
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
}


# ------------------------------------------------------------
# Resolve rarefaction depth
# ------------------------------------------------------------

.resolve_rarefaction_depth <- function(
    tse,
    rarefaction_depth
) {

  counts <- SummarizedExperiment::assay(
    tse,
    "counts"
  )


  library_sizes <- colSums(
    counts,
    na.rm = TRUE
  )


  if (
    any(
      !is.finite(
        library_sizes
      )
    )
  ) {

    stop(
      "Non-finite library sizes were detected.",
      call. = FALSE
    )
  }


  if (
    any(
      library_sizes <= 0
    )
  ) {

    stop(
      "All samples must have positive library sizes.",
      call. = FALSE
    )
  }


  if (
    is.null(
      rarefaction_depth
    )
  ) {

    rarefaction_depth <- floor(
      min(
        library_sizes
      )
    )

  } else {

    rarefaction_depth <- .validate_positive_integer(
      rarefaction_depth,
      "rarefaction_depth"
    )
  }


  if (
    rarefaction_depth >
    min(
      library_sizes
    )
  ) {

    stop(
      paste0(
        "rarefaction_depth cannot exceed the smallest ",
        "sample library size."
      ),
      call. = FALSE
    )
  }


  as.integer(
    rarefaction_depth
  )
}


# ------------------------------------------------------------
# Calculate all DIVE alpha-diversity metrics
# ------------------------------------------------------------

.calculate_alpha <- function(
    tse,
    rarefaction_depth = NULL,
    rarefaction_iterations = 50L
) {

  rarefaction_iterations <- .validate_positive_integer(
    rarefaction_iterations,
    "rarefaction_iterations"
  )


  resolved_depth <- .resolve_rarefaction_depth(
    tse = tse,
    rarefaction_depth = rarefaction_depth
  )


  alpha_indices <- .dive_alpha_indices()


  tse_alpha <- mia::addAlpha(
    tse,
    assay.type = "counts",
    index = alpha_indices,
    name = alpha_indices,
    sample = resolved_depth,
    niter = rarefaction_iterations
  )


  alpha_metadata <- as.data.frame(
    SummarizedExperiment::colData(
      tse_alpha
    )
  )


  missing_metrics <- setdiff(
    alpha_indices,
    colnames(
      alpha_metadata
    )
  )


  if (
    length(
      missing_metrics
    ) > 0L
  ) {

    stop(
      paste0(
        "The following alpha-diversity metrics were not returned by mia::addAlpha(): ",
        paste(
          missing_metrics,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  alpha <- alpha_metadata[
    ,
    alpha_indices,
    drop = FALSE
  ]


  for (
    metric_name in alpha_indices
  ) {

    alpha[[metric_name]] <- as.numeric(
      alpha[[metric_name]]
    )
  }


  if (
    nrow(
      alpha
    ) != ncol(
      tse
    )
  ) {

    stop(
      paste0(
        "The alpha-diversity result does not contain one row ",
        "for every sample."
      ),
      call. = FALSE
    )
  }


  attr(
    alpha,
    "rarefaction_depth"
  ) <- resolved_depth


  alpha
}
