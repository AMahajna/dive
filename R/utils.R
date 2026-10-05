# ============================================================
# DIVE
# General utilities
# ============================================================


.ensure_directory <- function(
    directory
) {

  if (!dir.exists(
    directory
  )) {

    dir.create(
      directory,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }


  invisible(
    directory
  )
}


.validate_positive_integer <- function(
    x,
    name
) {

  if (
    length(x) != 1L ||
    !is.numeric(x) ||
    !is.finite(x) ||
    x <= 0 ||
    x != floor(x)
  ) {

    stop(
      "`",
      name,
      "` must be one positive integer."
    )
  }


  as.integer(
    x
  )
}


.validate_dive_result <- function(
    result
) {

  if (!inherits(
    result,
    "dive_result"
  )) {

    stop(
      "Object must inherit from class `dive_result`."
    )
  }


  required_components <- c(
    "alpha",
    "correlation",
    "settings",
    "sampling",
    "method"
  )


  missing_components <- setdiff(
    required_components,
    names(
      result
    )
  )


  if (length(
    missing_components
  ) > 0L) {

    stop(
      "Invalid DIVE result. Missing components: ",
      paste(
        missing_components,
        collapse = ", "
      )
    )
  }


  invisible(
    TRUE
  )
}


.sanitize_filename <- function(
    x
) {

  x <- gsub(
    "[^A-Za-z0-9_-]+",
    "_",
    x
  )


  x <- gsub(
    "_+",
    "_",
    x
  )


  x <- gsub(
    "^_|_$",
    "",
    x
  )


  x
}
