# ============================================================
# DIVE
# S3 methods for dive_result objects
# ============================================================


# ------------------------------------------------------------
#' Print a DIVE result
#'
#' @param x A \code{dive_result} object.
#' @param ... Additional arguments.
#'
#' @return The object invisibly.
#'
#' @export
# ------------------------------------------------------------

print.dive_result <- function(
    x,
    ...
) {

  .validate_dive_result(
    x
  )


  cat(
    "\nDIVE analysis\n"
  )


  cat(
    "-------------\n"
  )


  cat(
    "Functionality:",
    x$settings$functionality_name,
    "\n"
  )


  cat(
    "Temporal method:",
    if (
      identical(
        x$method,
        "DCF"
      )
    ) {
      "Exact-lag DCF"
    } else {
      "CCF"
    },
    "\n"
  )


  cat(
    "Samples:",
    length(
      x$dates
    ),
    "\n"
  )


  cat(
    "Date range:",
    format(
      min(
        x$dates
      )
    ),
    "to",
    format(
      max(
        x$dates
      )
    ),
    "\n"
  )


  cat(
    "Alpha-diversity metrics:",
    ncol(
      x$alpha
    ),
    "\n"
  )


  cat(
    "Maximum lag:",
    x$settings$max_lag,
    if (
      identical(
        x$method,
        "DCF"
      )
    ) {
      "days"
    } else {
      "sampling intervals"
    },
    "\n"
  )


  invisible(
    x
  )
}


# ------------------------------------------------------------
#' Summarize a DIVE result
#'
#' @param object A \code{dive_result} object.
#' @param ... Additional arguments.
#'
#' @return A summary object.
#'
#' @export
# ------------------------------------------------------------

summary.dive_result <- function(
    object,
    ...
) {

  .validate_dive_result(
    object
  )


  correlation_table <- object$correlation


  if (
    identical(
      object$method,
      "DCF"
    )
  ) {

    association_values <-
      correlation_table$dcf

  } else {

    association_values <-
      correlation_table$correlation
  }


  association_table <- correlation_table


  association_table$absolute_association <-
    abs(
      association_values
    )


  association_table <- association_table[
    is.finite(
      association_table$absolute_association
    ),
    ,
    drop = FALSE
  ]


  strongest_rows <- do.call(

    rbind,

    lapply(

      split(
        association_table,
        association_table$metric
      ),

      function(metric_table) {

        metric_table[
          which.max(
            metric_table$absolute_association
          ),
          ,
          drop = FALSE
        ]
      }
    )
  )


  rownames(
    strongest_rows
  ) <- NULL


  summary_object <- list(

    functionality_name =
      object$settings$functionality_name,

    method =
      object$method,

    n_samples =
      length(
        object$dates
      ),

    first_date =
      min(
        object$dates
      ),

    last_date =
      max(
        object$dates
      ),

    n_metrics =
      ncol(
        object$alpha
      ),

    strongest_association =
      strongest_rows
  )


  class(
    summary_object
  ) <- "summary.dive_result"


  summary_object
}


# ------------------------------------------------------------
#' @export
# ------------------------------------------------------------

print.summary.dive_result <- function(
    x,
    ...
) {

  cat(
    "\nDIVE summary\n"
  )


  cat(
    "------------\n"
  )


  cat(
    "Functionality:",
    x$functionality_name,
    "\n"
  )


  cat(
    "Method:",
    x$method,
    "\n"
  )


  cat(
    "Samples:",
    x$n_samples,
    "\n"
  )


  cat(
    "Alpha-diversity metrics:",
    x$n_metrics,
    "\n"
  )


  cat(
    "Study period:",
    format(
      x$first_date
    ),
    "to",
    format(
      x$last_date
    ),
    "\n\n"
  )


  cat(
    "Largest absolute exploratory association for each metric:\n\n"
  )


  print(
    x$strongest_association,
    row.names = FALSE
  )


  invisible(
    x
  )
}
