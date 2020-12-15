#---------------------------------------------------------
# Author: Robert Wildermuth, rwildermuth@umassd.edu
# Created: 9/16/2020
# Last Modified: 9/16/2020

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

###########################################################
# Take a look at the Blue King Crab QNM. 
#     A. Download and install the freeware program Dia (https://sourceforge.net/projects/dia-installer/)
#     B. Open up the Dia file
#     C. Links terminating in an arrow indicate a positive effect of a node (origin) on another (arrow terminal)
#        Links terminating in a dot indicate a negative effect.  
#        Links that are solid are "certain" to occur; dashed are "uncertain", but if they do occur their sign is known.
#     D. We can add new nodes, move links around here, etc.. The saved .dia file is loaded into R and analyzed.
###############################################################

#--------------------------------------------------------------------------

# 1. Load in the Simple Example dia to make an edge list
edges <- model.dia("ExSimple.dia")
edges <- enforce.limitation(edges)
## Examine unweighted adjacency matrix
A <- adjacency.matrix(edges, labels=TRUE)

# Take a peak at the adjacency matrix
A

#Visualize A using:
adjacency.image(edges) # RW: this doesn't match -> labling on left inverted

# 2. Build a set of stable matricies, 
#We could add additional validation criteria to filter out matricies that don't reproduce a known system behavior 
sim<- system.simulate(n.sims=10000, edges=edges, 
                      sampler = community.sampler(edges), validators = NULL) 
#The sim object contains the inverse community matrcies, their corresponding edge weights, and a few other things.... 

#Look at the proportion of stable matricies when drawing edge weights from uniform distributions

sim$stable / sim$total #

#----------------------------------------------------------------
# 5. Build the function that will sample link weights 

s <- community.sampler(edges)

# 6.  Do we have validation criteria? Do we want to filter out matricies that are able to reproduce a known behavior?
#     For now, we don't, so leave perturb and monitor as NA. This builds a function 

press.val <- press.validate(edges,
                            perturb=NA,
                            monitor=NA )

# 7. Build the function to define the perturbation scenario

#impact <- press.impact(edges, perturb=c("CommercialGroundfishFishery"=1))
#impact  <- press.impact(edges, perturb=c("BottomTemperature"=1))
#impact  <- press.impact(edges, perturb=c("BottomTemperature"=1,"CommercialGroundfishFishery"=1))

# 8. Simulate response of the community! 

#use 10000 simulations
impact <- press.impact(edges, perturb=c("Temperature"=1,"Fishery"=-1))
  
  n.sims <- 10000  #should take about 10 seconds, if longer, there might have build a digraph that nearly always leads to an unstable community. If so, consider including more negative feedbacks, or setting the A diagonal to -1 
  results <- 0
  i <- 0
  
  while(i < n.sims) {
    
    ## Randomly choose edges to retain
    #z <- s$select(runif(1))
    ## Sample community matrix
    W <- s$community()
    
    ## Check press condition and stability
    if(!(press.val(W) && stable.community(W))) next
    
    ## Monitor impact post press
    imp <- impact(W)
    
    results <- results + outer(signum(imp, epsilon=1.0E-5),-1:1,'==')  #signum classifies the predicted response to -1, 0, 1. Values less abs(epsilon) are rounded down to zero. 
    i <- i+1
  }
  
  ## Print results
  rownames(results) <- levels(edges$From)
  colnames(results) <- c('-','0','+')
  
  ## Plot outcomes
  library(RColorBrewer)
  pal <- brewer.pal(n=5,"RdBu")[4:2]
  opar <- par(mar=c(5,10,1,1)+0.1)
  prop <- results/rowSums(results)
  r <- colSums(t(prop)*(-1:1))
  barplot(t(prop[order(r),]),
          horiz=T,cex.names=0.8,cex.axis=0.8,las=2,
          border=F,col=pal,xlab="Proportion")
  par(opar)




#----------------------------------------------------------------
  

# 3. Interactively expore how the nodes respond to different press scenarios

impact.barplot(sim)

# Look at how the community responds when nodes are pressed one at a time.
imptable<- impact.table(sim)

#Which node perturbations have similar community outcomes?

# Similarity of effects among pressed nodes (similarity of distrubances)
imp_dist<- dist(t(imptable))
plot(hclust(imp_dist), main="Perturbation similarity (Euclidean distance)", hang=-1)

# Similarity of effects across pressed nodes (similarity of reactions)
imp_dist<- dist(imptable)
plot(hclust(imp_dist), main="Node similarity across perturbations (Euclidean distance)", hang=-1)

