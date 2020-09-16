#---------------------------------------------------------
# Author: Robert Wildermuth, rwildermuth@umassd.edu
# Created: 9/1/2020
# Last Modified: 9/1/2020

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

# #-------------------------------------------------------------------------------
# # First create a Dia file to convert from mental modeler CSV to an "edge list" for QPress
# # Taken from: https://github.com/NOAA-EDAB/QNM/blob/master/looping_qpress_Rpath.R
# #function to create signed digraph from Mental Modeler 
# MM2Qpress <- function(data){
#   
#   mental.sheet <- as.data.table(data)
#   names(mental.sheet)[1] <- 'Box'
#   n <- nrow(mental.sheet)
#   box.names <- names(mental.sheet)[which(names(mental.sheet) != 'Box')]
#   model <- c()
#   for(i in 1:n){
#     pos <- which(mental.sheet[i, .SD, .SDcol = box.names] > 0)
#     if(length(pos) > 0){
#       pos.interaction <- paste(mental.sheet[i, Box], '->', 
#                                names(mental.sheet[i, pos + 1, with = F]), 
#                                sep = '')
#       model <- append(model, pos.interaction)
#     }
#     
#     neg <- which(mental.sheet[i, .SD, .SDcol = box.names] < 0)
#     if(length(neg) > 0){
#       neg.interaction <- paste(mental.sheet[i, Box], '-*', 
#                                names(mental.sheet[i, neg + 1, with = F]), 
#                                sep = '')
#       model <- append(model, neg.interaction)
#     }
#   }
#   return(model)
# }
# #function to create qnm models for qpress from signed digraphs
# make.qnm<-function(modmat){
#   q<-MM2Qpress(modmat)
#   qnm<-dput(q)
#   qnm.mod<-parse.digraph(qnm)
#   qnm.model<-enforce.limitation(qnm.mod)
# }
# 
# # Use previously defined community matrix
# adjacencyMat <- read.csv("Seafloor.csv")
# 
# qpressBI <- make.qnm(adjacencyMat)
# 
# #write.dia(qpressBI, "BearIsland.dia")

#--------------------------------------------------------------------------

# 1. Load in the Seafloor Habitat dia to make an edge list
edges <- model.dia("Seafloor.dia")
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

