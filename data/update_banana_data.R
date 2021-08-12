

library(tidyverse)
library(magrittr)

bananadt <- readRDS("data/bananadt.rds")

# crosses type
bananadt$CrossType <- ifelse(substr(bananadt$FemaleGenotype,1,2) == "T.", "Back-cross",
                           ifelse(substr(bananadt$FemaleGenotype,1,2) == "NM", "Back-cross",
                                  ifelse(substr(bananadt$MaleGenotype,1,2) == "T.", "Back-cross",
                                         ifelse(substr(bananadt$MaleGenotype,1,2) == "NM", "Back-cross", "Bi-parental"))))

# info
accession_info <- readRDS("data/accession_info.rds")

# replace accessions with links
accession_info$germplasmName <- as.character(accession_info$germplasmName)

accession_info <- accession_info %>%
  dplyr::rename(
    "FemaleGenotype" = "germplasmName",
    "FemalePloidy"  = "value"
  )

banana <- bananadt %>%
  dplyr::left_join(accession_info) %>%
  .[!duplicated(.$Crossnumber),]
banana$FemaleGenotype <- ifelse(!is.na(banana$germplasmPUI), banana$germplasmPUI, banana$FemaleGenotype)
banana$germplasmPUI <- NULL

accession_info <- accession_info %>%
  dplyr::rename(
    "MaleGenotype" = "FemaleGenotype",
    "MalePloidy"  = "FemalePloidy"
  )
banana <- banana %>%
  dplyr::left_join(accession_info)%>%
  .[!duplicated(.$Crossnumber),]
banana$MaleGenotype <- ifelse(!is.na(banana$germplasmPUI), banana$germplasmPUI, banana$MaleGenotype)
banana$germplasmPUI <- NULL

banana[,c("Location", "Crossnumber", "TrialName", "CrossType", "FemalePloidy", "MalePloidy")] %<>%
  mutate_all(as.factor)
banana <- banana %>%
  dplyr::select("Location","Crossnumber", "TrialName", contains("Female"), Cycle, contains("Male"), "CrossType", everything())

saveRDS(banana, "data/banana.rds")
