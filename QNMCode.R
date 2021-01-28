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

# 1. Load in the Habitat dia to make an edge list
#edges <- model.dia("KelpForest_20210127.dia")
edges <- model.dia("Seafloor_20210127.dia")

edges <- enforce.limitation(edges)
## Examine unweighted adjacency matrix
A <- adjacency.matrix(edges, labels=TRUE)

# Take a peak at the adjacency matrix
View(A)

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

#-------------------------------------------------------------
# Save output to compare scenarios
# 5. Build the function that will sample link weights 

s <- community.sampler(edges)

# 6.  Do we have validation criteria? Do we want to filter out matricies that are able to reproduce a known behavior?
#     For now, we don't, so leave perturb and monitor as NA. This builds a function 

press.val <- press.validate(edges,
                            perturb=NA,
                            monitor=NA )

# 7. Build the function to define the perturbation scenario

enso <- press.impact(edges, perturb=c("ENSO"=1))
acidification  <- press.impact(edges, perturb=c("OA"=1))
warming  <- press.impact(edges, perturb=c("SeafloorTemperature"=1))
# warming  <- press.impact(edges, perturb=c("SeaSurfaceTemp"=1))
incFishing  <- press.impact(edges, perturb=c("Fishing"=1))
decFishing  <- press.impact(edges, perturb=c("Fishing"=-1))
# incFishing  <- press.impact(edges, perturb=c("RecreationalFishing"=1))
# decFishing  <- press.impact(edges, perturb=c("RecreationalFishing"=-1))
windfarm  <- press.impact(edges, perturb = c("Fishing"=-1, "RockHabitat"=-1, "CoralsSponges"=1))
# windfarm  <- press.impact(edges, perturb = c("RecreationalFishing"=-1, "RockyReef"=-1, "Sedimentation"=1))

# 8. Simulate response of the community! 

n.sims <- 10000  #should take about 10 seconds, if longer, there might have build a digraph that nearly always leads to an unstable community. If so, consider including more negative feedbacks, or setting the A diagonal to -1 
resENSO <- resOA <- resWarm <- resIncFish <- resDecFish <- resWindfarm <- 0
i <- 0

while(i < n.sims) {
  
  ## Randomly choose edges to retain
  #z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check press condition and stability
  if(!(press.val(W) && stable.community(W))) next
  
  ## Monitor impact post press
  impENSO <- enso(W)
  impOA <- acidification(W)
  impWarm <- warming(W)
  impIncFish <- incFishing(W)
  impDecFish <- decFishing(W)
  impWindfarm <- windfarm(W)
  
  resENSO <- resENSO + outer(signum(impENSO, epsilon=1.0E-5),-1:1,'==')  #signum classifies the predicted response to -1, 0, 1. Values less abs(epsilon) are rounded down to zero. 
  resOA <- resOA + outer(signum(impOA, epsilon=1.0E-5),-1:1,'==')
  resWarm <- resWarm + outer(signum(impWarm, epsilon=1.0E-5),-1:1,'==')
  resIncFish <- resIncFish + outer(signum(impIncFish, epsilon=1.0E-5),-1:1,'==')
  resDecFish <- resDecFish + outer(signum(impDecFish, epsilon=1.0E-5),-1:1,'==')
  resWindfarm <- resWindfarm + outer(signum(impWindfarm, epsilon=1.0E-5),-1:1,'==')
  
  i <- i+1
}

results <- rbind(resENSO, resOA, resWarm, resIncFish, resDecFish, resWindfarm)
## Print results
rownames(results) <- paste(levels(edges$From), 
                           rep(c("ENSO", "OA", "Warm", "IncFish", "DecFish", "Windfarm"), 
                               each = length(levels(edges$From))),
                           sep = ".")
colnames(results) <- c('-','0','+')

## Plot outcomes
library(RColorBrewer)
# pal <- brewer.pal(n=5,"RdBu")[4:2]
# opar <- par(mar=c(5,10,1,1)+0.1)
# prop <- results/rowSums(results)
# r <- colSums(t(prop)*(-1:1))
# barplot(t(prop[order(r),]),
#         horiz=T,cex.names=0.8,cex.axis=0.8,las=2,
#         border=F,col=pal,xlab="Proportion")
# par(opar)

pal <- rev(brewer.pal(n=11,"RdBu"))
prop <- results/rowSums(results)
# if all outcomes unaffected (0) make prop of +'s = 0.5
prop[prop[,'0']==1, '+'] <- 0.5
cutMap <- cut(prop[,'+'], breaks = 11)
par(mar=c(1.1,7,16,1.1))
plot(x = rep(1:length(levels(edges$From)), 6), y = c(rep(6, length(levels(edges$From))),
                                                     rep(5, length(levels(edges$From))), 
                                                     rep(4,length(levels(edges$From))), 
                                                     rep(3, length(levels(edges$From))), 
                                                     rep(2,length(levels(edges$From))), 
                                                     rep(1,length(levels(edges$From)))), 
     pch = 22, bg = pal[as.numeric(cutMap)], cex = 5, xlab = "", ylab = "", xaxt='n', yaxt='n',
     ylim = c(0.5, 6.5), bty = 'n')
axis(side = 2,
     at = 6:1, las = 2,
     labels =c("ENSO", "OA", "Warm", "IncFish", "DecFish", "Windfarm"))
#axis(3, at=1:length(levels(edges$From)), labels=FALSE, tck=0)
text(x=1:length(levels(edges$From)), y= 7,# par("usr")[2]+0.5,
     labels=levels(edges$From), srt=-45, pos = 2, offset = -0.25, xpd=NA, cex = 1.5)
mtext(text = "Kelp Forest Habitat", side = 3, line = 10, cex = 2)
# add effect lables
text(x = rep(1:length(levels(edges$From)), 6), y = c(rep(6, length(levels(edges$From))),
                                                     rep(5, length(levels(edges$From))), 
                                                     rep(4,length(levels(edges$From))), 
                                                     rep(3, length(levels(edges$From))), 
                                                     rep(2,length(levels(edges$From))), 
                                                     rep(1,length(levels(edges$From)))),
     labels = ifelse(prop[,'-'] > 0.5, yes = "-", no = ""), cex = 2)
text(x = rep(1:length(levels(edges$From)), 6), y = c(rep(6, length(levels(edges$From))),
                                                     rep(5, length(levels(edges$From))), 
                                                     rep(4,length(levels(edges$From))), 
                                                     rep(3, length(levels(edges$From))), 
                                                     rep(2,length(levels(edges$From))), 
                                                     rep(1,length(levels(edges$From)))),
     labels = ifelse(prop[,'0'] == 1, yes = "X", no = ""), cex = 2)
text(x = rep(1:length(levels(edges$From)), 6), y = c(rep(6, length(levels(edges$From))),
                                                     rep(5, length(levels(edges$From))), 
                                                     rep(4,length(levels(edges$From))), 
                                                     rep(3, length(levels(edges$From))), 
                                                     rep(2,length(levels(edges$From))), 
                                                     rep(1,length(levels(edges$From)))),
     labels = ifelse(prop[,'+'] > 0.5, yes = "+", no = ""), cex = 2)