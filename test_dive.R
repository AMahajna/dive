# ============================================================
# DIVE TEST SCRIPT
# ============================================================

# ------------------------------------------------------------
# 1. Load the local DIVE package
# ------------------------------------------------------------

devtools::load_all()

# Confirm the main functions are available
stopifnot(
  exists("dive"),
  exists("plot_dive_heatmap"),
  exists("plot_dive_null")
)


# ------------------------------------------------------------
# 2. Load the TSE object
# ------------------------------------------------------------

loaded_objects <- load(
  "data/tse_function.RData"
)

print(
  loaded_objects
)

# If the object inside the RData file is not called tse_function,
# automatically use the first loaded object.
if (!exists("tse_function")) {

  if (length(loaded_objects) == 1L) {

    tse_function <- get(
      loaded_objects[[1]]
    )

  } else {

    stop(
      "The RData file contains multiple objects and none is named ",
      "`tse_function`. Please inspect `loaded_objects`."
    )
  }
}


# ------------------------------------------------------------
# 3. Load removal-efficiency data
# ------------------------------------------------------------

removal_data <- readxl::read_excel(
  "data/Removal_Efficiency.xlsx"
)

print(
  colnames(removal_data)
)


# ------------------------------------------------------------
# 4. Check Date column in TSE
# ------------------------------------------------------------

if (
  !"Date" %in%
  colnames(
    SummarizedExperiment::colData(
      tse_function
    )
  )
) {
  stop(
    "`colData(tse_function)` does not contain a column named `Date`."
  )
}

if (
  !inherits(
    SummarizedExperiment::colData(
      tse_function
    )$Date,
    "Date"
  )
) {

  SummarizedExperiment::colData(
    tse_function
  )$Date <- as.Date(
    SummarizedExperiment::colData(
      tse_function
    )$Date
  )
}


# ------------------------------------------------------------
# 5. Prepare BOD functionality
# ------------------------------------------------------------

if (
  !"BOD_removal" %in%
  colnames(
    removal_data
  )
) {
  stop(
    "`Removal_Efficiency.xlsx` does not contain `BOD_removal`."
  )
}

# Prefer matching by Date when the Excel file contains Date
if (
  "Date" %in%
  colnames(
    removal_data
  )
) {

  removal_data$Date <- as.Date(
    removal_data$Date
  )

  tse_dates <-
    SummarizedExperiment::colData(
      tse_function
    )$Date

  match_index <- match(
    tse_dates,
    removal_data$Date
  )

  if (
    anyNA(
      match_index
    )
  ) {
    stop(
      "At least one TSE sampling date was not found in ",
      "`Removal_Efficiency.xlsx`."
    )
  }

  functionality <-
    removal_data$BOD_removal[
      match_index
    ]

} else {

  functionality <-
    removal_data$BOD_removal
}


# ------------------------------------------------------------
# 6. Check sample numbers
# ------------------------------------------------------------

cat(
  "Number of TSE samples:",
  ncol(tse_function),
  "\n"
)

cat(
  "Number of BOD observations:",
  length(functionality),
  "\n"
)

if (
  length(functionality) !=
  ncol(tse_function)
) {
  stop(
    "The number of BOD measurements does not match ",
    "the number of TSE samples."
  )
}


# ------------------------------------------------------------
# 7. Inspect Date-BOD matching
# ------------------------------------------------------------

input_check <- data.frame(
  Date =
    SummarizedExperiment::colData(
      tse_function
    )$Date,
  BOD_removal =
    functionality
)

print(
  input_check
)


# ------------------------------------------------------------
# 8. Run DIVE
# ------------------------------------------------------------

result <- dive(
  tse = tse_function,
  functionality = functionality,
  sampling_even = 0,
  output_dir = "test_output",
  rarefaction_iterations = 10,
  rarefaction_depth = NULL,
  max_lag = 60,
  dcf_bin_width = 21,
  n_permutations = 99,
  block_length = NULL,
  seed = 123
)


# ------------------------------------------------------------
# 9. Inspect returned DIVE object
# ------------------------------------------------------------

cat(
  "\nDIVE finished successfully.\n"
)

print(
  names(result)
)

cat(
  "\nSampling summary:\n"
)

print(
  result$sampling
)

cat(
  "\nAnalysis settings:\n"
)

print(
  result$settings
)

cat(
  "\nAlpha-diversity metrics:\n"
)

print(
  colnames(
    result$alpha
  )
)

cat(
  "\nFirst rows of alpha-diversity results:\n"
)

print(
  head(
    result$alpha
  )
)

cat(
  "\nTemporal correlation results:\n"
)

print(
  result$correlation
)

cat(
  "\nGlobal temporal robustness p-value:\n"
)

print(
  result$robustness$global_p_value
)


# ------------------------------------------------------------
# 10. Show strongest DCF relationships
# ------------------------------------------------------------

if (
  "dcf" %in%
  colnames(
    result$correlation
  )
) {

  strongest_results <-
    result$correlation[
      order(
        -abs(
          result$correlation$dcf
        )
      ),
    ]

  cat(
    "\nStrongest DCF relationships:\n"
  )

  print(
    head(
      strongest_results,
      20
    )
  )
}


# ------------------------------------------------------------
# 11. Plot DIVE results
# ------------------------------------------------------------

print(
  plot(result)
)

print(
  plot_dive_heatmap(
    result
  )
)

print(
  plot_dive_null(
    result
  )
)


# ------------------------------------------------------------
# 12. Show files created by DIVE
# ------------------------------------------------------------

cat(
  "\nFiles created in test_output:\n"
)

print(
  list.files(
    "test_output",
    recursive = TRUE
  )
)
