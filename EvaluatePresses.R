#---------------------------------------------------------
# Author: Robert Wildermuth, rwildermuth@umassd.edu
# Created: 2/23/2021

# Description:


#---------------------------------------------------------

### QPress QNMs
########################################################################
# All code was originally obtained from  Melbourne-Thomas et al. 2012. #
########################################################################
# Adapted from Jon Reum's instructions for IEA QNM Workshop
# by Robert Wildermuth, 8/29/2018

# Download and install the package "QPress" if you haven't already
# Depending on your machine, you may have to go to www.rforge.net/QPress and download and install by hand
#install.packages("https://www.rforge.net/QPress/snapshot/QPress_0.21.tar.gz")
#install.packages('QPress',,'http://www.rforge.net/')

#devtools::install_github("SWotherspoon/QPress", build_vignettes = TRUE)
#Load the library
library(QPress)
library(tidyverse)

EvaluatePresses <- function(edges, perturb, scenario, nSims){
  
  
  # Save output to compare scenarios
  # 5. Build the function that will sample link weights 
  
  s <- community.sampler(edges)
  
  # 6.  Do we have validation criteria? Do we want to filter out matricies that are able to reproduce a known behavior?
  #     For now, we don't, so leave perturb and monitor as NA. This builds a function 
  
  press.val <- press.validate(edges,
                              perturb=NA,
                              monitor=NA )
  
  # 7. Build the function to define the perturbation scenario
  
  scenarioFxn <- press.impact(edges = edges, perturb=perturb)
  
  # 8. Simulate response of the community! 
  
  #n.sims <- 10000  #should take about 10 seconds, if longer, there might have build a digraph that nearly always leads to an unstable community. If so, consider including more negative feedbacks, or setting the A diagonal to -1 
  results <- 0
  i <- 0
  
  while(i < nSims) {
    
    ## Randomly choose edges to retain
    #z <- s$select(runif(1))
    ## Sample community matrix
    W <- s$community()
    
    ## Check press condition and stability
    if(!(press.val(W) && stable.community(W))) next
    
    ## Monitor impact post press
    imp <- scenarioFxn(W)
    
    results <- results + outer(signum(imp, epsilon=1.0E-5),-1:1,'==')  #signum classifies the predicted response to -1, 0, 1. Values less abs(epsilon) are rounded down to zero. 
    
    i <- i+1
  }
  
  ## Print results
  rownames(results) <- paste(levels(edges$From), 
                             rep(scenario, each = length(levels(edges$From))),
                             sep = ".")
  colnames(results) <- c('-','0','+')

  results <- as.data.frame(results) %>% mutate(node = levels(edges$From),
                                               scenario = scenario)
  return(results)
}
