functionality_variables <- c(
  "N_removal",
  "BOD_removal",
  "P_removal"
)

dive_results <- list()

for (functionality_name in functionality_variables) {

  functionality <- as.numeric(
    removal_data[[functionality_name]]
  )

  dive_results[[functionality_name]] <- dive(
    tse = tse_function,
    functionality = functionality,
    sampling_even = 0,
    output_dir = paste0(
      "DIVE_",
      functionality_name,
      "_exact_lag_results"
    ),
    rarefaction_iterations = 100,
    rarefaction_depth = NULL,
    max_lag = 63,
    seed = 123
  )
}
