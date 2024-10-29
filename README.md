# DIVE: Diversity-Informed Valuation of Ecosystem functioning
an algorithmic reductionist framework that connects microbiome structure with ecosystem functioning through cross-correlation analysis. 
DIVE calculates the cross-correlation between two variables—biodiversity as indicated by the varying alpha diversity metrics and ecosystem functioning
as defined by the user.
The alpha-diversity metric that correlated most significantly with the ecosystem functioning measure is attributed to the microbiome structure that most effectively
enhances the ecosystem functioning. 

install and load package: 
remotes::install_github("AMahajna/dive")
library(dive) 

call: 
alpha = dive(tse, ecosystem_functioning, output_dir = "figures")

where: 
tse - is your Tree Summarized Experiment (n samples) 

ecosystem_functioning - is a measure of ecosystem functioning defined by the user (dataframe with dimesnions of n*1) 

alpha - is a dataframe of the different alpha diversity metrics calculated for the respective tse 

output_dir - is the name of directory in which to save the resulting cross-correlational functions (default = "figures") 


