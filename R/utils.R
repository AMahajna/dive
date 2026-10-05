# ============================================================
# DIVE
# Internal utility functions
# ============================================================


# ------------------------------------------------------------
# Check whether a value is a single finite number
# ------------------------------------------------------------

.is_single_number <- function(x) {

  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x)
}


# ------------------------------------------------------------
# Validate a positive integer-like argument
# ------------------------------------------------------------

.validate_positive_integer <- function(
    x,
    argument_name
) {

  if (
    !.is_single_number(x) ||
    x < 1 ||
    x != floor(x)
  ) {

    stop(
      argument_name,
      " must be a positive integer.",
      call. = FALSE
    )
  }

  as.integer(x)
}


# ------------------------------------------------------------
# Validate a non-negative number
# ------------------------------------------------------------

.validate_nonnegative_number <- function(
    x,
    argument_name
) {

  if (
    !.is_single_number(x) ||
    x < 0
  ) {

    stop(
      argument_name,
      " must be a non-negative number.",
      call. = FALSE
    )
  }

  as.numeric(x)
}


# ------------------------------------------------------------
# Safe standard deviation
# ------------------------------------------------------------

.safe_sd <- function(x) {

  x <- x[
    is.finite(x)
  ]

  if (length(x) < 2L) {

    return(
      NA_real_
    )
  }

  stats::sd(
    x
  )
}


# ------------------------------------------------------------
# Standardize a numeric vector
# ------------------------------------------------------------

.standardize_vector <- function(x) {

  x <- as.numeric(
    x
  )

  x_mean <- mean(
    x,
    na.rm = TRUE
  )

  x_sd <- stats::sd(
    x,
    na.rm = TRUE
  )

  if (
    !is.finite(x_sd) ||
    x_sd == 0
  ) {

    return(
      rep(
        NA_real_,
        length(x)
      )
    )
  }

  (
    x - x_mean
  ) / x_sd
}


# ------------------------------------------------------------
# Make strings safe for filenames
# ------------------------------------------------------------

.sanitize_filename <- function(x) {

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


# ------------------------------------------------------------
# Validate DIVE result object
# ------------------------------------------------------------

.validate_dive_result <- function(x) {

  if (
    !inherits(
      x,
      "dive_result"
    )
  ) {

    stop(
      "The supplied object must be a 'dive_result' object.",
      call. = FALSE
    )
  }

  invisible(
    TRUE
  )
}
