rm(list = ls())


#FIXME set to your own path here
#setwd("<< Insert path on local computer >>")

# load data
predation <- read.table(file = "predation.txt", header = TRUE,
  sep = "\t")  #<-- XXX important to include tab-separated

str(predation)


################################################################################

# How many nests depredated?
sum(predation$Depred)
  nrow(predation)  #<-- total number of nests
  

# Make a table with observed instances of predation by site type
predTab <- with(predation, table(HumDist, Depred))
  ## calculate percent nests depredated
  round(100 * predTab[, 2] / rowSums(predTab), 0)
  

set.seed(1142)  #<-- set to replicate exact p-value from Monte Carlo simulation
chisq.test(x = predTab, simulate.p.value = TRUE, B = 10000)


