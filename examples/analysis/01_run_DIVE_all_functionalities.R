# ============================================================
# DIVE worked example
#
# Run DIVE separately for:
#
#   BOD removal
#   Nitrogen removal
#   Phosphorus removal
#
# Sampling dates are stored in tse_function.
#
# The functionality columns are assumed to be in exactly the
# same sample order as tse_function.
# ============================================================


library(DIVE)
library(readxl)
library(SummarizedExperiment)


# ============================================================
# 1. Load the TreeSummarizedExperiment
# ============================================================

load(
  "examples/data/tse_function.RData"
)


# ============================================================
# 2. Load ecosystem-functionality measurements
# ============================================================

removal_data <- readxl::read_excel(
  "examples/data/Removal_Efficiency.xlsx"
)


# ============================================================
# 3. Define variables to analyze
# ============================================================

functionality_variables <- c(
  "BOD_removal",
  "N_removal",
  "P_removal"
)


functionality_labels <- c(

  BOD_removal =
    "BOD removal",

  N_removal =
    "Nitrogen removal",

  P_removal =
    "Phosphorus removal"
)


output_directories <- c(

  BOD_removal =
    "examples/results/BOD",

  N_removal =
    "examples/results/N_removal",

  P_removal =
    "examples/results/P_removal"
)


# ============================================================
# 4. Check sample numbers
# ============================================================

n_samples <- ncol(
  tse_function
)


if (
  nrow(
    removal_data
  ) != n_samples
) {

  stop(
    paste0(
      "The number of rows in Removal_Efficiency.xlsx does not ",
      "match the number of samples in tse_function."
    )
  )
}


# ============================================================
# 5. Run DIVE
# ============================================================

dive_results <- list()


for (
  functionality_variable in functionality_variables
) {

  cat(
    "\n========================================\n"
  )


  cat(
    "Running DIVE for: ",
    functionality_labels[
      functionality_variable
    ],
    "\n",
    sep = ""
  )


  cat(
    "========================================\n"
  )


  functionality <- as.numeric(

    gsub(
      "%",
      "",
      as.character(
        removal_data[[
          functionality_variable
        ]]
      )
    )
  )


  if (
    anyNA(
      functionality
    )
  ) {

    stop(
      paste0(
        "Missing or non-numeric values were found in ",
        functionality_variable,
        "."
      )
    )
  }


  dive_results[[
    functionality_variable
  ]] <- dive(

    tse =
      tse_function,

    functionality =
      functionality,

    sampling_even =
      0,

    functionality_name =
      functionality_labels[
        functionality_variable
      ],

    output_dir =
      output_directories[
        functionality_variable
      ],

    rarefaction_iterations =
      100,

    rarefaction_depth =
      NULL,

    max_lag =
      63,

    seed =
      123
  )
}


# ============================================================
# 6. Inspect results
# ============================================================

print(
  dive_results$BOD_removal
)


summary(
  dive_results$BOD_removal
)


print(
  dive_results$N_removal
)


print(
  dive_results$P_removal
)


cat(
  "\nAll DIVE analyses completed successfully.\n"
)
