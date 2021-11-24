library(openxlsx)
spe_dat <- read.xlsx("Plant composition data.xlsx", sheet = "Original data")
names(spe_dat)
check <- spe_dat$Genera.species

library(Taxonstand)
checklist <- TPL(check)

newchecklist <- unique(checklist)
dim(newchecklist)
newchecklist <- newchecklist[c("Taxon", "Typo", "New.Genus", "New.Species", 
                               "Taxonomic.status")]
write.xlsx(newchecklist, "newchecklist2.xlsx")
dim(newchecklist)

