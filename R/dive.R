
#' Diversity-Informed Valuation of Ecosystem functionality
#'
#' @param tse abundance data of features in community (format: summarized experiment with n sample)
#' @param functionality ecosystem performance measure (format: dataframe dimensions n*1)
#' @param output_dir directory to print cross correlations function in
#'
#' @return dataframe containing varying alpha diversity metrics
#' @export
#'
#' @examples dive(tse, functionality, output_dir = "figures")

dive <- function(tse, functionality, output_dir = "figures") {
  suppressMessages({
    suppressWarnings({
      #Compute alpha diversity
      #Richness
      tse <- mia::estimateRichness(tse,
                                   assay.type = "counts",
                                   index =   c("ace", "chao1", "hill", "observed"),
                                   name=  c("ace", "chao1", "hill", "observed"))

      #Diversity
      tse <- mia::estimateDiversity(tse,
                                    assay.type = "counts",
                                    index =  c("coverage", "fisher", "gini_simpson", "inverse_simpson",
                                               "shannon"),
                                    name =  c("coverage", "fisher", "gini_simpson", "inverse_simpson",
                                              "shannon"))
      #Evenness
      tse <- mia::estimateEvenness(tse,
                                   assay.type = "counts",
                                   index=c("camargo", "pielou", "simpson_evenness", "evar", "bulla"),
                                   name = c("camargo", "pielou", "simpson_evenness", "evar", "bulla"))
      #Dominance
      tse <- mia::estimateDominance(tse,
                                    assay.type = "counts",
                                    index=c("absolute", "dbp", "core_abundance", "gini", "dmn", "relative",
                                            "simpson_lambda"),
                                    name = c("absolute", "dbp", "core_abundance", "gini", "dmn", "relative",
                                             "simpson_lambda"))
      ##Divergence
      tse <- mia::addDivergence(tse)

      ##Rarity
      tse <- mia::estimateDiversity(tse,
                                    assay.type = "counts",
                                    index = "log_modulo_skewness")

      #Create alpha diversity data-frame
      alpha = as.data.frame(SummarizedExperiment::colData(tse))
      selected_alpha <- c("ace", "chao1", "hill", "observed",
                          "coverage", "fisher", "gini_simpson", "inverse_simpson",
                          "shannon","camargo", "pielou", "simpson_evenness", "evar",
                          "bulla","absolute", "dbp", "core_abundance", "gini", "dmn", "relative",
                          "simpson_lambda","divergence","log_modulo_skewness")
      alpha = alpha[ ,selected_alpha]

      #Create 'figures' folder if it doesn't exist
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }

      #Plot Cross-correlation funcitons as save in figures folder
      plot_list <- list()
      for (i in 1:length(alpha)){
        #Add name of the alpha diversity used in CCF
        n=colnames(alpha)
        text = paste("Cross-correlation function for", n[i] , "and removal efficiency")
        #CCF
        plot <- forecast::ggCcf(alpha[ ,i],
                                functionality,
                                type = "correlation",
                                na.action = stats::na.contiguous) +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_continuous(limits = c(-4, 0), breaks = seq(-4, 0, 1)) +
          ggplot2::scale_y_continuous(limits = c(-0.45, 0.45), breaks = seq(-0.45, 0.45, 0.1)) +
          ggplot2::labs(title = text,
               x = "Lag", y = "Correlation Coefficient") +
          ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
        plot_list[[i]] <- plot
        #ggplotly(plot)
        #Save plot
        cff_name = paste('figures/','ccf_', n[i],".png")
        grDevices::png(cff_name ,units = 'in',width=5, height=5, res=1000)
        print(plot)
        grDevices::dev.off()
      }

    })
  })
  #Return alpha diversity measures
  return(alpha)
}


