# ============================================================
# DIVE
# Six-panel time-series figure
#
# Panels:
#   1. Richness (Observed)
#   2. Diversity (Shannon)
#   3. Rarity (ACE)
#   4. Dominance (Simpson lambda)
#   5. Evenness (Pielou)
#   6. BOD removal
#
# Dates are taken ONLY from tse_function
#
# Output:
#   PNG
#   600 DPI
#   No overall title
# ============================================================


library(mia)
library(SummarizedExperiment)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)


# ============================================================
# 1. Get sampling dates from tse_function
# ============================================================

sample_dates <- SummarizedExperiment::colData(
  tse_function
)$Date


if (!inherits(
  sample_dates,
  "Date"
)) {

  sample_dates <- as.Date(
    sample_dates
  )
}


# ============================================================
# 2. Read BOD removal data
#
# Assumption:
# BOD values are already in exactly the same sample order
# as tse_function.
# ============================================================

removal_data <- readxl::read_excel(
  "data/Removal_Efficiency.xlsx"
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


if (length(
  BOD_removal
) != length(
  sample_dates
)) {

  stop(
    "The number of BOD removal values does not match ",
    "the number of samples in tse_function."
  )
}


if (anyNA(
  BOD_removal
)) {

  stop(
    "BOD_removal contains missing or non-numeric values."
  )
}


# ============================================================
# 3. Select representative alpha-diversity metrics
# ============================================================

alpha_metrics <- c(
  "observed",
  "shannon",
  "ace",
  "simpson_lambda",
  "pielou"
)


# ============================================================
# 4. Determine rarefaction depth
# ============================================================

counts <- SummarizedExperiment::assay(
  tse_function,
  "counts"
)


library_sizes <- colSums(
  counts,
  na.rm = TRUE
)


if (any(
  library_sizes <= 0
)) {

  stop(
    "At least one sample has a zero or negative library size."
  )
}


rarefaction_depth <- floor(
  min(
    library_sizes
  )
)


cat(
  "Rarefaction depth:",
  rarefaction_depth,
  "\n"
)


# ============================================================
# 5. Calculate alpha diversity
# ============================================================

set.seed(
  123
)


tse_alpha <- mia::addAlpha(
  tse_function,
  assay.type = "counts",
  index = alpha_metrics,
  name = alpha_metrics,
  sample = rarefaction_depth,
  niter = 100
)


# ============================================================
# 6. Extract alpha-diversity values
# ============================================================

alpha_data <- as.data.frame(
  SummarizedExperiment::colData(
    tse_alpha
  )
)


missing_metrics <- setdiff(
  alpha_metrics,
  colnames(
    alpha_data
  )
)


if (length(
  missing_metrics
) > 0L) {

  stop(
    "The following alpha-diversity metrics were not returned: ",
    paste(
      missing_metrics,
      collapse = ", "
    )
  )
}


alpha_data <- alpha_data[
  ,
  alpha_metrics,
  drop = FALSE
]


for (metric_name in alpha_metrics) {

  alpha_data[[metric_name]] <- as.numeric(
    alpha_data[[metric_name]]
  )
}


# ============================================================
# 7. Build combined wide data frame
# ============================================================

plot_data <- data.frame(
  Date = sample_dates,

  `Richness (Observed)` =
    alpha_data$observed,

  `Diversity (Shannon)` =
    alpha_data$shannon,

  `Rarity (ACE)` =
    alpha_data$ace,

  `Dominance (Simpson lambda)` =
    alpha_data$simpson_lambda,

  `Evenness (Pielou)` =
    alpha_data$pielou,

  `BOD removal` =
    BOD_removal,

  check.names = FALSE
)


# ============================================================
# 8. Sort chronologically
# ============================================================

plot_data <- plot_data[
  order(
    plot_data$Date
  ),
  ,
  drop = FALSE
]


# ============================================================
# 9. Convert to long format
# ============================================================

plot_long <- plot_data %>%

  tidyr::pivot_longer(
    cols = c(
      `Richness (Observed)`,
      `Diversity (Shannon)`,
      `Rarity (ACE)`,
      `Dominance (Simpson lambda)`,
      `Evenness (Pielou)`,
      `BOD removal`
    ),
    names_to = "Metric",
    values_to = "Value"
  )


# ============================================================
# 10. Set panel order
# ============================================================

plot_long$Metric <- factor(
  plot_long$Metric,
  levels = c(
    "Richness (Observed)",
    "Diversity (Shannon)",
    "Rarity (ACE)",
    "Dominance (Simpson lambda)",
    "Evenness (Pielou)",
    "BOD removal"
  )
)


# ============================================================
# 11. Define colors
# ============================================================

metric_colors <- c(
  "Richness (Observed)" = "#0072B2",
  "Diversity (Shannon)" = "#009E73",
  "Rarity (ACE)" = "#D55E00",
  "Dominance (Simpson lambda)" = "#CC79A7",
  "Evenness (Pielou)" = "#E69F00",
  "BOD removal" = "#000000"
)


# ============================================================
# 12. Create six-panel figure
#
# scales = "free_y" is important because every metric has
# a different numerical scale.
# ============================================================

p <- ggplot(
  plot_long,
  aes(
    x = Date,
    y = Value,
    color = Metric,
    group = Metric
  )
) +

  geom_line(
    linewidth = 0.9,
    na.rm = TRUE
  ) +

  geom_point(
    size = 2.2,
    na.rm = TRUE
  ) +

  facet_wrap(
    ~ Metric,
    ncol = 2,
    scales = "free_y"
  ) +

  scale_color_manual(
    values = metric_colors,
    guide = "none"
  ) +

  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y",
    expand = expansion(
      mult = c(
        0.01,
        0.02
      )
    )
  ) +

  labs(
    x = "Sampling date",
    y = NULL
  ) +

  theme_classic(
    base_size = 12
  ) +

  theme(

    # No overall title
    plot.title = element_blank(),

    # Panel labels
    strip.background = element_blank(),

    strip.text = element_text(
      face = "bold",
      size = 11
    ),

    # X axis
    axis.text.x = element_text(
      size = 8.5
    ),

    axis.title.x = element_text(
      size = 12,
      margin = margin(
        t = 10
      )
    ),

    # Y axis
    axis.text.y = element_text(
      size = 9
    ),

    # Spacing between panels
    panel.spacing = grid::unit(
      1.1,
      "lines"
    ),

    # Margins
    plot.margin = margin(
      10,
      10,
      10,
      10
    )
  )


# ============================================================
# 13. Display figure
# ============================================================

print(
  p
)


# ============================================================
# 14. Save as 600 DPI PNG
# ============================================================

ggplot2::ggsave(
  filename = "DIVE_alpha_diversity_BOD_six_panel_time_series.png",
  plot = p,
  width = 11,
  height = 11,
  units = "in",
  dpi = 600,
  bg = "white"
)
