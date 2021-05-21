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
library(RColorBrewer)
library(ggplot2)
library(hrbrthemes)
#install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
library(ggrepel)

source("EvaluatePresses.R")

# Load and inspect networks ------------------------------------------------

# 1. Load in the Habitat dia to make an edge list
KelpEdges <- model.dia("KelpForest_20210127.dia")
SeafloorEdges <- model.dia("Seafloor_20210127.dia")

KelpEdges <- enforce.limitation(KelpEdges)
SeafloorEdges <- enforce.limitation(SeafloorEdges)

## Examine unweighted adjacency matrix
KelpMat <- adjacency.matrix(KelpEdges, labels=TRUE)
SeafloorMat <- adjacency.matrix(SeafloorEdges, labels=TRUE)

# Take a peak at the adjacency matrix
View(KelpMat)
View(SeafloorMat)

#Visualize adjacency matrix using:
adjacency.image(KelpEdges) # RW: this doesn't match -> labling on left inverted
adjacency.image(SeafloorEdges)

# Call QNM Simulation function --------------------------------------------

nSims <- 10000

# Kelp Forest

# 2. Build a set of stable matricies, 
#We could add additional validation criteria to filter out matricies that don't reproduce a known system behavior 
sim<- system.simulate(n.sims=nSims, edges=KelpEdges, 
                      sampler = community.sampler(KelpEdges), validators = NULL) 
#The sim object contains the inverse community matrcies, their corresponding edge weights, and a few other things.... 

# Report the proportion of stable matricies when drawing edge weights from uniform distributions
cat("Proportion of stable vs proposed matrices")
print(sim$stable / sim$total) #

resENSO <- EvaluatePresses(edges = KelpEdges, perturb = c("ENSO"=1), scenario = "ENSO", nSims = nSims)
resOA  <- EvaluatePresses(edges = KelpEdges, perturb=c("OA"=1), scenario = "OA", nSims = nSims)
resWarm  <- EvaluatePresses(edges = KelpEdges, perturb=c("SeaSurfaceTemp"=1), scenario = "Warm", nSims = nSims)
resClimChng  <- EvaluatePresses(edges = KelpEdges, perturb=c("OA"=1, "SeaSurfaceTemp"=1), scenario = "CC", nSims = nSims)
resIncFish  <- EvaluatePresses(edges = KelpEdges, perturb=c("RecreationalFishing"=1), scenario = "IncFish", nSims = nSims)
resDecFish  <- EvaluatePresses(edges = KelpEdges, perturb=c("RecreationalFishing"=-1), scenario = "DecFish", nSims = nSims)
resWindfarm  <- EvaluatePresses(edges = KelpEdges, 
                                perturb = c("DetritusBacteria" = 1, # increased from biofouling
                                            "RecreationalFishing"= 1, # may attract fishers 
                                            "ForageFishes"=1, # attraction of schooling fish to structures
                                            "RockyReef" = -1, # removal or covering of natural habitat area
                                            "CurrentsEddiesPlumes"=1), # increased mixing of water column
                                scenario = "Wind", nSims = nSims)
resAquaculture  <- EvaluatePresses(edges = KelpEdges, 
                                perturb = c("DetritusBacteria" = 1, # increased from biofouling
                                            "Nutrients" = 1, # increased from feed and waste
                                            "Sedimentation" = 1, # increased nutrient loading in sediments
                                            "RecreationalFishing"= -1, #  anglers avoiding buffer around fishfarm
                                            "ForageFishes"= 1, # attraction of schooling fish to structures
                                            "Salmon" = -1), # attraction of marine mammals and birds
                                scenario = "Aquacult", nSims = nSims)
resMining  <- EvaluatePresses(edges = KelpEdges, 
                                perturb = c("Sedimentation" = 1, # increased from advected resuspended of sediment
                                            "RecreationalFishing"= -1, # potential displacement of fishing 
                                            "BenthicInvertebrates" = -1, # repeated mortality events for infauna and epibenthos
                                            "RockyReef" = -1, # damage to hard habitats from machinary
                                            "Hypoxia"=1), # increased hypoxia in divits in seafloor
                                scenario = "Mining", nSims = nSims)
# Scenarios with new uses + climate change
resWindCC  <- EvaluatePresses(edges = KelpEdges, 
                                perturb = c("OA"=1, "SeaSurfaceTemp"=1, # climate change
                                            "DetritusBacteria" = 1, # increased from biofouling
                                            "RecreationalFishing"= 1, # may attract fishers 
                                            "ForageFishes"=1, # attraction of schooling fish to structures
                                            "RockyReef" = -1, # removal or covering of natural habitat area
                                            "CurrentsEddiesPlumes"=1), # increased mixing of water column
                                scenario = "WindCC", nSims = nSims)
resAquaCC  <- EvaluatePresses(edges = KelpEdges, 
                                   perturb = c("OA"=1, "SeaSurfaceTemp"=1, # climate change
                                               "DetritusBacteria" = 1, # increased from biofouling
                                               "Nutrients" = 1, # increased from feed and waste
                                               "Sedimentation" = 1, # increased nutrient loading in sediments
                                               "RecreationalFishing"= -1, #  anglers avoiding buffer around fishfarm
                                               "ForageFishes"= 1, # attraction of schooling fish to structures
                                               "Salmon" = -1), # attraction of marine mammals and birds
                                   scenario = "AquacultCC", nSims = nSims)
resMineCC  <- EvaluatePresses(edges = KelpEdges, 
                              perturb = c("OA"=1, "SeaSurfaceTemp"=1, # climate change
                                          "Sedimentation" = 1, # increased from advected resuspended of sediment
                                          "RecreationalFishing"= -1, # potential displacement of fishing 
                                          "BenthicInvertebrates" = -1, # repeated mortality events for infauna and epibenthos
                                          "RockyReef" = -1, # damage to hard habitats from machinary
                                          "Hypoxia"=1), # increased hypoxia in divits in seafloor
                              scenario = "MiningCC", nSims = nSims)

resultsKelp <- rbind(resENSO, resOA, resWarm, resIncFish, resDecFish, 
                     resWindfarm, resAquaculture, resMining, resClimChng,
                     resWindCC, resAquaCC, resMineCC)

# save(resultsKelp, file = "resultsKelp_20210307.RData")

# Seafloor
resENSO <- EvaluatePresses(edges = SeafloorEdges, perturb = c("ENSO"=1), scenario = "ENSO", nSims = nSims)
resOA  <- EvaluatePresses(edges = SeafloorEdges, perturb=c("OA"=1), scenario = "OA", nSims = nSims)
resWarm  <- EvaluatePresses(edges = SeafloorEdges, perturb=c("SeafloorTemperature"=1), scenario = "Warm", nSims = nSims)
resClimChng <- EvaluatePresses(edges = SeafloorEdges, perturb=c("OA"=1, "SeafloorTemperature"=1), scenario = "CC", nSims = nSims)
resIncFish  <- EvaluatePresses(edges = SeafloorEdges, perturb=c("Fishing"=1), scenario = "IncFish", nSims = nSims)
resDecFish  <- EvaluatePresses(edges = SeafloorEdges, perturb=c("Fishing"=-1), scenario = "DecFish", nSims = nSims)
resWindfarm  <- EvaluatePresses(edges = SeafloorEdges, 
                                perturb = c("Fishing"=-1, # Restriction of trawling in wind leases
                                            "RockHabitat"=-1, # removal or covering of natural habitat area
                                            "SoftHabitat" = -1, # repeated disturbance during storms and maintenance
                                            "SmallPrey" = 1, # attraction of schooling fish to structures
                                            "CoralsSponges"=1), # new substrate to colonize
                                scenario = "Wind", nSims = nSims)
resAquaculture  <- EvaluatePresses(edges = SeafloorEdges, 
                                perturb = c("Fishing"=-1, # Restriction of trawling in fish farms
                                            "DetritusBacteria" = 1, # increased from biofouling
                                            "SoftHabitat" = -1, # increased nutrient loading in sediments
                                            "Pollution" = 1, # Buildup of heavy metals and persistant pollutants in sediment
                                            "SmallPrey" = 1, # attraction of schooling fish to structures
                                            "CoralsSponges"=1), # new substrate to colonize
                                scenario = "Aquacult", nSims = nSims)
resMining  <- EvaluatePresses(edges = SeafloorEdges, 
                                   perturb = c("Fishing"=-1, # Restriction of trawling in mining locations
                                               "RockHabitat" = -1, # damage to hard habitats from machinary
                                               "SoftHabitat" = -1, # Change in shape and structure, repeated disturbance
                                               "SmallPrey" = -1, # repeated mortality events for infauna and epibenthos
                                               "Hypoxia"=1), # increased hypoxia in divits in seafloor
                                   scenario = "Mining", nSims = nSims)
# Scenarios with new uses + climate change
resWindCC <- EvaluatePresses(edges = SeafloorEdges, 
                             perturb = c("OA"=1, "SeafloorTemperature"=1, # Climate change
                                         "Fishing"=-1, # Restriction of trawling in wind leases
                                         "RockHabitat"=-1, # removal or covering of natural habitat area
                                         "SoftHabitat" = -1, # repeated disturbance during storms and maintenance
                                         "SmallPrey" = 1, # attraction of schooling fish to structures
                                         "CoralsSponges"=1), # new substrate to colonize
                             scenario = "WindCC", nSims = nSims)
resAquaCC  <- EvaluatePresses(edges = SeafloorEdges, 
                                   perturb = c("OA"=1, "SeafloorTemperature"=1, # Climate change
                                               "Fishing"=-1, # Restriction of trawling in fish farms
                                               "DetritusBacteria" = 1, # increased from biofouling
                                               "SoftHabitat" = -1, # increased nutrient loading in sediments
                                               "Pollution" = 1, # Buildup of heavy metals and persistant pollutants in sediment
                                               "SmallPrey" = 1, # attraction of schooling fish to structures
                                               "CoralsSponges"=1), # new substrate to colonize
                                   scenario = "AquacultCC", nSims = nSims)
resMineCC  <- EvaluatePresses(edges = SeafloorEdges, 
                              perturb = c("OA"=1, "SeafloorTemperature"=1, # Climate change
                                          "Fishing"=-1, # Restriction of trawling in mining locations
                                          "RockHabitat" = -1, # damage to hard habitats from machinary
                                          "SoftHabitat" = -1, # Change in shape and structure, repeated disturbance
                                          "SmallPrey" = -1, # repeated mortality events for infauna and epibenthos
                                          "Hypoxia"=1), # increased hypoxia in divits in seafloor
                              scenario = "MiningCC", nSims = nSims)

resultsSeafloor <- rbind(resENSO, resOA, resWarm, resIncFish, resDecFish, 
                         resWindfarm, resAquaculture, resMining, resClimChng,
                         resWindCC, resAquaCC, resMineCC)

# save(resultsSeafloor, file = "resultsSeafloor_20210307.RData")


# Investigate links and connectivity --------------------------------------

linksKelp <- rbind(table(KelpEdges[,1]), table(KelpEdges[,2]))
colSums(linksKelp)

linksSeafloor <- rbind(table(SeafloorEdges[,1]), table(SeafloorEdges[,2]))
colSums(linksSeafloor)

# Visualize results -------------------------------------------------------


# 2. Build a set of stable matricies, 
#We could add additional validation criteria to filter out matricies that don't reproduce a known system behavior 
sim<- system.simulate(n.sims=10000, edges=SeafloorEdges, 
                      sampler = community.sampler(SeafloorEdges), validators = NULL) 
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



## Plot outcomes
load(file = "resultsSeafloor_20210307.RData")
load(file = "resultsKelp_20210307.RData")
# pal <- brewer.pal(n=5,"RdBu")[4:2]
# opar <- par(mar=c(5,10,1,1)+0.1)
# prop <- results/rowSums(results)
# r <- colSums(t(prop)*(-1:1))
# barplot(t(prop[order(r),]),
#         horiz=T,cex.names=0.8,cex.axis=0.8,las=2,
#         border=F,col=pal,xlab="Proportion")
# par(opar)

pal <- rev(brewer.pal(n=11,"RdBu"))
pal[6] <- "grey"
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
# text(x = rep(1:length(levels(edges$From)), 6), y = c(rep(6, length(levels(edges$From))),
#                                                      rep(5, length(levels(edges$From))), 
#                                                      rep(4,length(levels(edges$From))), 
#                                                      rep(3, length(levels(edges$From))), 
#                                                      rep(2,length(levels(edges$From))), 
#                                                      rep(1,length(levels(edges$From)))),
#      labels = ifelse(prop[,'0'] == 1, yes = "X", no = ""), cex = 2)
text(x = rep(1:length(levels(edges$From)), 6), y = c(rep(6, length(levels(edges$From))),
                                                     rep(5, length(levels(edges$From))), 
                                                     rep(4,length(levels(edges$From))), 
                                                     rep(3, length(levels(edges$From))), 
                                                     rep(2,length(levels(edges$From))), 
                                                     rep(1,length(levels(edges$From)))),
     labels = ifelse(prop[,'+'] > 0.5, yes = "+", no = ""), cex = 2)

# Plot output as waffle plots

pal2 <- rev(brewer.pal(n=3,"RdBu"))
prop <- results/rowSums(results)

#waffle(parts = results[1,], rows = 100, size = 0.02, colors = pal2)
#waffle(parts = results["RockyReef.ENSO",], rows = 100, size = 0.025, colors = pal2) = facet_wrap(~ node)

#results <- as.data.frame(results)
#results$node <- gsub(x = rownames(results), pattern = ".ENSO", replacement = "")
resultsKelp <- resultsKelp %>% pivot_longer(cols = c("-", "0", "+"), names_to = "response", values_to = "vals") %>%
                  mutate(myColor = case_when(response == "-" ~ pal2[1],
                                             response == "0" ~ pal2[2],
                                             response == "+" ~ pal2[3]),
                         response = factor(response, levels = c("-", "0", "+")))


resultsKelp %>% ggplot(aes(fill = response, values = vals)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = "response", colour = NULL, title = "Kelp Forest Scenarios") +
  scale_fill_brewer(palette = "RdBu") +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() + 
  geom_waffle( color = "white", size= 0.2, make_proportional = TRUE, n_rows = 10) +
  facet_grid(node ~ scenario, switch = "y") + 
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(panel.spacing = unit(0.001, units = "snpc"))

resultsSeafloor <- resultsSeafloor %>% pivot_longer(cols = c("-", "0", "+"), names_to = "response", values_to = "vals") %>%
                      mutate(myColor = case_when(response == "-" ~ pal2[1],
                                                 response == "0" ~ pal2[2],
                                                 response == "+" ~ pal2[3]),
                             response = factor(response, levels = c("-", "0", "+")))

resultsSeafloor %>% ggplot(aes(fill = response, values = vals)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = "response", colour = NULL, title = "Seafloor Scenarios") +
  scale_fill_brewer(palette = "RdBu") +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() + 
  geom_waffle( color = "white", size= 0.2, make_proportional = TRUE, n_rows = 10) +
  facet_grid(node ~ scenario, switch = "y") + 
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(panel.spacing = unit(0.001, units = "snpc"))

# ------------------------
# plot nodes along a scale for simple visualization

resultsKelp$system <- "Kelp Forest"
resultsSeafloor$system <- "Seafloor"

results <- rbind(resultsSeafloor, resultsKelp)

results$prop <- results$`+`/(results$`+` + results$`-`)

test1 <- results %>% filter(scenario == "OA")

scl <- data.frame(scl = seq(0,1, by= 0.01),
                  system = rep(c("Kelp Forest", "Seafloor"), each = 101))
ggplot() + geom_tile(data = scl, aes(x = scl, y= 1, fill = scl)) +
  #scale_y_continuous(limits=c(0,2),breaks=1) +
  facet_wrap(~system, nrow = 1) +
  scale_fill_gradient(low = "#1F78B4", high = "#E31A1C") + 
  theme_minimal() +
  ggtitle("Ocean Acidification") +
  # add the node labels
  ggrepel::geom_label_repel(data = test1, mapping = aes(x=prop, y=0.5, label = node),
                            nudge_y = 2, 
                            direction = "y", 
                            ylim = c(1.5, 4),
                            #hjust = 0,
                            max.overlaps = Inf) +
  ylim(0.5, 4) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(name = "Positive Simulation Proportion", limits = c(0,1), 
                     breaks = seq(0, 1, by = 0.2),
                     expand = expansion(mult = 0.25))

# Plot systems together with and without Climate Change
test2 <- results %>% filter(scenario %in% c("Aquacult", "AquacultCC"),
                            system == "Seafloor")

scl <- data.frame(scl = seq(0,1, by= 0.01),
                  scenario = rep(c("Aquacult", "AquacultCC"), each = 101))
ggplot() + geom_tile(data = scl, aes(x = scl, y= 1, fill = scl)) +
  #scale_y_continuous(limits=c(0,2),breaks=1) +
  facet_wrap(~scenario, nrow = 1) +
  scale_fill_gradient(low = "#1F78B4", high = "#E31A1C") + 
  theme_minimal() +
  ggtitle("Seafloor: Aquacutlure Baseline and Climate Change") +
  # add the node labels
  ggrepel::geom_label_repel(data = test2, mapping = aes(x=prop, y=0.5, label = node),
                            nudge_y = 2, 
                            direction = "y", 
                            ylim = c(1.5, 4),
                            #hjust = 0,
                            max.overlaps = Inf) +
  ylim(0.5, 4) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(name = "Positive Simulation Proportion", limits = c(0,1), 
                     breaks = seq(0, 1, by = 0.2),
                     expand = expansion(mult = 0.25))
