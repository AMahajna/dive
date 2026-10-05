# ============================================================
# DIVE example visualization
#
# Six-panel time series:
#
#   Richness
#   Diversity
#   Rarity
#   Dominance
#   Evenness
#   BOD removal
# ============================================================


library(DIVE)
library(readxl)


# ============================================================
# 1. Load example data
# ============================================================

load(
  "examples/data/tse_function.RData"
)


removal_data <- readxl::read_excel(
  "examples/data/Removal_Efficiency.xlsx"
)


BOD_removal <- as.numeric(

  gsub(
    "%",
    "",
    as.character(
      removal_data$BOD_removal
    )
  )
)


# ============================================================
# 2. Run DIVE
# ============================================================

bod_result <- dive(

  tse =
    tse_function,

  functionality =
    BOD_removal,

  sampling_even =
    0,

  functionality_name =
    "BOD removal",

  output_dir =
    "examples/results/BOD",

  rarefaction_iterations =
    100,

  rarefaction_depth =
    NULL,

  max_lag =
    63,

  seed =
    123
)


# ============================================================
# 3. Create the six-panel figure
# ============================================================

time_series_plot <- plot_dive_timeseries(

  bod_result,

  metrics = c(
    "observed",
    "shannon",
    "ace",
    "simpson_lambda",
    "pielou"
  ),

  ncol = 2
)


print(
  time_series_plot
)


# ============================================================
# 4. Save at 600 DPI
# ============================================================

ggplot2::ggsave(

  filename =
    "examples/visualizations/BOD_alpha_diversity_timeseries.png",

  plot =
    time_series_plot,

  width =
    11,

  height =
    11,

  units =
    "in",

  dpi =
    600,

  bg =
    "white"
)
