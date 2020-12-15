# Script to process pre-meeting survey data
library(tidyverse)
library(wordcloud)
library(RColorBrewer)

premeet <- read.csv("WA Dept. of Ecology Ocean Habitats Pre-Meeting Survey.csv", stringsAsFactors = FALSE)

#----------
# Get info about who experts are

# For now, just summarize types of roles
roles <- premeet$Select.any.and.all.roles.you.identify.with..you.can.select.more.than.one..
roles <- strsplit(roles, ";")
roles <- unlist(roles)

rolesTots <- data.frame(role = unique(roles), tot = NA)
for(r in 1:nrow(rolesTots)){
  rolesTots[r, "tot"] <- sum(str_count(roles, unique(roles)[r]))
}
View(rolesTots[order(rolesTots$tot), ])

# Years of experience
yrsExp <- premeet$How.many.years.have.you.lived..worked.or.otherwise.been.invested.in.the.WA.coast.
yrsExpSmry <- data.frame(role = unique(yrsExp), tot = NA)
for(r in 1:nrow(yrsExpSmry)){
  yrsExpSmry[r, "tot"] <- sum(str_count(yrsExp, unique(yrsExp)[r]))
}
View(yrsExpSmry[order(yrsExpSmry$tot), ])

premeet$What.is.your.gender
premeet$What.is.your.race.or.ethnicity.

# Summarize important and unimportant elements

## Seafloor
seafloorPhys <- premeet[, grepl("physical.drivers.in.the.seafloor", names(premeet), fixed = TRUE)]
elementNames <- names(premeet)[grepl("physical.drivers.in.the.seafloor", names(premeet), fixed = TRUE)]
elementNames <- sub("Below.is.a.list.of.the.system.components.and.attributes.that.are.physical.drivers.in.the.seafloor.habitat.conceptual.model.included.in.the.MSP..For.each.component.or.attribute..an.example.data.index.or.unit.is.provided.in.parentheses...",
                    "", elementNames, fixed = TRUE)
names(seafloorPhys) <- sapply(strsplit(elementNames, "..", fixed = TRUE), getElement, 1)
seafloorPhys$response <- 1:nrow(seafloorPhys)
seafloorPhys <- seafloorPhys %>% pivot_longer(!response, names_to = "element", values_to = "score")
# table(seafloorPhys[,-1])
# seafloorPhys %>% 
#   group_by(element) %>%
#   dplyr::summarize(VI = sum(score == "Very important"),
#                    FI = sum(score == "Fairly important"),
#                    SI = sum(score == "Somewhat important"),
#                    NI = sum(score == "Not important"),
#                    NO = sum(score == "No opinion"))

seafloorEco <- premeet[, grepl("attributes.that.are.in.the.seafloor", names(premeet), fixed = TRUE)]
elementNames <- names(premeet)[grepl("attributes.that.are.in.the.seafloor", names(premeet), fixed = TRUE)]
elementNames <- sub("Below.is.a.list.of.the.ecological.and.fisheries.components.and.attributes.that.are.in.the.seafloor.habitat.conceptual.model.included.in.the.MSP..For.each.component.or.attribute..an.example.data.index.or.unit.is.provided.in.parentheses....",
                    "", elementNames, fixed = TRUE)
names(seafloorEco) <- sapply(strsplit(elementNames, "..", fixed = TRUE), getElement, 1)
seafloorEco$response <- 1:nrow(seafloorEco)
seafloorEco <- seafloorEco %>% pivot_longer(!response, names_to = "element", values_to = "score")
# table(seafloorEco[,-1])
# seafloorEco %>% 
#   group_by(element) %>%
#   dplyr::summarize(VI = sum(score == "Very important"),
#                    FI = sum(score == "Fairly important"),
#                    SI = sum(score == "Somewhat important"),
#                    NI = sum(score == "Not important"),
#                    NO = sum(score == "No opinion"))

seafloorHD <- premeet[, grepl("human.activities.in.the.seafloor", names(premeet), fixed = TRUE)]
elementNames <- names(premeet)[grepl("human.activities.in.the.seafloor", names(premeet), fixed = TRUE)]
elementNames <- sub("Below.is.a.list.of.the.system.components.and.attributes.that.are.related.to.human.activities.in.the.seafloor.habitat.conceptual.model.included.in.the.MSP..For.each.component.or.attribute..an.example.data.index.or.unit.is.provided.in.parentheses....",
                    "", elementNames, fixed = TRUE)
names(seafloorHD) <- sapply(strsplit(elementNames, "..", fixed = TRUE), getElement, 1)
seafloorHD$response <- 1:nrow(seafloorHD)
seafloorHD <- seafloorHD %>% pivot_longer(!response, names_to = "element", values_to = "score")
# table(seafloorHD[,-1])
# seafloorHD %>% 
#   group_by(element) %>%
#   dplyr::summarize(VI = sum(score == "Very important"),
#                    FI = sum(score == "Fairly important"),
#                    SI = sum(score == "Somewhat important"),
#                    NI = sum(score == "Not important"),
#                    NO = sum(score == "No opinion"))

seafloorAll <- rbind(seafloorPhys, seafloorEco, seafloorHD)
freqVI <- seafloorAll %>%
  group_by(element) %>%
  dplyr::summarize(VI = sum(score == "Very important"),
                   FI = sum(score == "Fairly important"),
                   SI = sum(score == "Somewhat important"),
                   NI = sum(score == "Not important"),
                   NO = sum(score == "No opinion")) %>%
  #filter(NI != 0)
  #filter(NO != 0)
  filter(VI != 0)

wordcloud(words = freqVI$element, freq = freqVI$VI, min.freq = 1,
          scale = c(3,0.25), random.order = FALSE, rot.per = 0.2,
          colors = brewer.pal(7, "Dark2"))

seafloorAll %>%
  group_by(element) %>%
  dplyr::summarize(VI = sum(score == "Very important"),
                   FI = sum(score == "Fairly important"),
                   SI = sum(score == "Somewhat important"),
                   NI = sum(score == "Not important"),
                   NO = sum(score == "No opinion")) %>%
  filter(NI != 0)

## Kelp Forest
kelpPhys <- premeet[, grepl("physical.drivers.in.the.kelp", names(premeet), fixed = TRUE)]
elementNames <- names(premeet)[grepl("physical.drivers.in.the.kelp", names(premeet), fixed = TRUE)]
elementNames <- sub("Below.is.a.list.of.the.system.components.and.attributes.that.are.physical.drivers.in.the.kelp.forest.habitat.conceptual.model.included.in.the.MSP..For.each.component.or.attribute..an.example.data.index.or.unit.is.provided.in.parentheses....",
                    "", elementNames, fixed = TRUE)
names(kelpPhys) <- sapply(strsplit(elementNames, "..", fixed = TRUE), getElement, 1)
kelpPhys$response <- 1:nrow(kelpPhys)
kelpPhys <- kelpPhys %>% pivot_longer(!response, names_to = "element", values_to = "score")
# table(kelpPhys[,-1])
# kelpPhys %>% 
#   group_by(element) %>%
#   dplyr::summarize(VI = sum(score == "Very important"),
#                    FI = sum(score == "Fairly important"),
#                    SI = sum(score == "Somewhat important"),
#                    NI = sum(score == "Not important"),
#                    NO = sum(score == "No opinion"))

kelpEco <- premeet[, grepl("attributes.that.are.in.the.kelp", names(premeet), fixed = TRUE)]
elementNames <- names(premeet)[grepl("attributes.that.are.in.the.kelp", names(premeet), fixed = TRUE)]
elementNames <- sub("Below.is.a.list.of.ecological.and.fisheries.components.and.attributes.that.are.in.the.kelp.forest.habitat.conceptual.model.included.in.the.MSP..For.each.component.or.attribute..an.example.data.index.or.unit.is.provided.in.parentheses....",
                    "", elementNames, fixed = TRUE)
names(kelpEco) <- sapply(strsplit(elementNames, "..", fixed = TRUE), getElement, 1)
kelpEco$response <- 1:nrow(kelpEco)
kelpEco <- kelpEco %>% pivot_longer(!response, names_to = "element", values_to = "score")
# table(kelpEco[,-1])
# kelpEco %>% 
#   group_by(element) %>%
#   dplyr::summarize(VI = sum(score == "Very important"),
#                    FI = sum(score == "Fairly important"),
#                    SI = sum(score == "Somewhat important"),
#                    NI = sum(score == "Not important"),
#                    NO = sum(score == "No opinion"))

kelpHD <- premeet[, grepl("human.activities.in.the.kelp", names(premeet), fixed = TRUE)]
elementNames <- names(premeet)[grepl("human.activities.in.the.kelp", names(premeet), fixed = TRUE)]
elementNames <- sub("Below.is.a.list.of.the.system.components.and.attributes.that.are.related.to.human.activities.in.the.kelp.forest.habitat.conceptual.model.included.in.the.MSP..For.each.component.or.attribute..an.example.data.index.or.unit.is.provided.in.parentheses....",
                    "", elementNames, fixed = TRUE)
names(kelpHD) <- sapply(strsplit(elementNames, "..", fixed = TRUE), getElement, 1)
kelpHD$response <- 1:nrow(kelpHD)
kelpHD <- kelpHD %>% pivot_longer(!response, names_to = "element", values_to = "score")
# table(kelpHD[,-1])
# kelpHD %>% 
#   group_by(element) %>%
#   dplyr::summarize(VI = sum(score == "Very important"),
#                    FI = sum(score == "Fairly important"),
#                    SI = sum(score == "Somewhat important"),
#                    NI = sum(score == "Not important"),
#                    NO = sum(score == "No opinion"))

kelpAll <- rbind(kelpPhys, kelpEco, kelpHD)
freqVI <- kelpAll %>%
  group_by(element) %>%
  dplyr::summarize(VI = sum(score == "Very important"),
                   FI = sum(score == "Fairly important"),
                   SI = sum(score == "Somewhat important"),
                   NI = sum(score == "Not important"),
                   NO = sum(score == "No opinion")) %>%
  #filter(NI != 0)
  #filter(NO != 0)
  filter(VI != 0)

wordcloud(words = freqVI$element, freq = freqVI$VI, min.freq = 1,
          scale = c(3,0.25), random.order = FALSE, rot.per = 0.2,
          colors = brewer.pal(7, "Dark2"))

kelpAll %>%
  group_by(element) %>%
  dplyr::summarize(VI = sum(score == "Very important"),
                   FI = sum(score == "Fairly important"),
                   SI = sum(score == "Somewhat important"),
                   NI = sum(score == "Not important"),
                   NO = sum(score == "No opinion")) %>%
  filter(NI != 0)

#-----------------------------------------------------
# Get total ranks for elements using the rank conversion:
# Very important = 3
# Fairly important = 2
# Somewhat important = 1
# No opinion = 0
# Not important = -2

# Note: one respondent didn't rank any elements

kelpAll <- kelpAll %>% mutate(rank = case_when(score == "Very important" ~ 3,
                                               score == "Fairly important" ~ 2,
                                               score == "Somewhat important" ~ 1,
                                               score == "No opinion" ~ 0,
                                               score == "Not important" ~ -1))

seafloorAll <- seafloorAll %>% mutate(rank = case_when(score == "Very important" ~ 3,
                                               score == "Fairly important" ~ 2,
                                               score == "Somewhat important" ~ 1,
                                               score == "No opinion" ~ 0,
                                               score == "Not important" ~ -1))

# Summarize the total ranks and min and max ranks for each element
kelpSmry <- kelpAll %>% group_by(element) %>%
              dplyr::summarize(totRank = sum(rank, na.rm = TRUE),
                               minRank = min(rank, na.rm = TRUE),
                               maxRank = max(rank, na.rm = TRUE))
View(kelpSmry)
# Everything marked as very important at least once. Currents, Eddies & Plumes; ENSO; 
# Forage Fishes; Marine Snow; Phytoplankton; Sea Otters all marked as Not important AND Very important.
# Top 5 most highly ranked elements are within 6 pts: Kelp, Sea Urchins, Rocky Reef, Nutrients, SST
# Marine Snow only ranked as important once

seafloorSmry <- seafloorAll %>% group_by(element) %>%
                  dplyr::summarize(totRank = sum(rank, na.rm = TRUE),
                                   minRank = min(rank, na.rm = TRUE),
                                   maxRank = max(rank, na.rm = TRUE))
View(seafloorSmry)
# Everything marked as very important at least once. ENSO; Forage Fishes; Marine Snow;
# Mid-Water Rockfishes all marked as Not important AND Very important.
# Currents, Eddies & Plumes; ENSO and Mid-Water Rockfishes ranked lowest (22 to 23)
# Top 2 ranked elements all had minimum rank of Fairly important: Dissolved Oxygen, Fishing
# followed by Crabs & Shrimp, Upwelling, Benthic Inverts, and Phytoplankton

# look at text comments
survComments <- premeet[, c("Is.there.an.important.component.or.attribute.of.the.seafloor.habitat.system.not.included.in.the.list.above.", 
                            "If.there.is.a.component.or.attribute.missing..please.provide.it.here.with.justification.and.any.resources.or.links.to.monitoring.data.related.to.this.missing.component.",
                            "Is.there.an.important.component.or.attribute.of.the.kelp.forest.habitat.system.not.included.in.the.list.above.", 
                            "If.there.is.a.component.or.attribute.missing..please.provide.it.here.with.justification.and.any.resources.or.links.to.monitoring.data.related.to.this.missing.component..1"
                            )]
names(survComments) <- c("seafloorYN", "seafloorComment", "kelpYN", "kelpComment")
survComments %>% filter(seafloorYN == "Yes") %>%
                  select(seafloorComment)
survComments %>% filter(kelpYN == "Yes") %>%
                  select(kelpComment)
