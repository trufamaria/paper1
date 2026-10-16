# Builds data/items.csv (item-level data used by scripts 07-10) from the SPSS file.
# Run from the results/ folder: setwd("results")
suppressMessages({library(haven)})
d <- read_sav("../data/Merged_BR_US_Updated.sav")
d <- d[nchar(as.character(d$Age)) <= 3, ]      # drops implausible age entries
d <- d[d$Gender %in% c(1, 2), ]                 # men and women only
d <- as.data.frame(zap_labels(d))
d$Country <- ifelse(d$Is_Br == 1, "US", "Brazil")   # Is_Br == 1 is the US in this file
items <- c(paste0("RD",1:5), paste0("Misg",1:10), paste0("SPV",1:4), "WEPV",
  paste0("Chaos_",1:7), "SDO_1","SDO2","SDO3","SDOD4", paste0("AT",1:4),
  paste0("Racism_",3:6), "PPl2","Ppl3_","Ppl4","Ppl5", paste0("Trust",1:3),
  "PSE1","PSE2", paste0("Belief",1:4), paste0("Share",1:4), "Pol_Tend", "Country")
write.csv(d[, items], "../data/items.csv", row.names = FALSE)
cat("rows:", nrow(d), "\n"); print(table(d$Country))
