rm(list = ls())


#FIXME set to your own path here
#setwd("<< Insert path on local computer >>")

# Get basic site data
sites <- read.table("site_latlon_DATA.txt", header = TRUE, sep = "\t")

################################################################################
# Pull in downloaded StreamCAT data:
## https://www.epa.gov/waterdata/waters-geoviewer


streamCATyr <- "2017"

# Make an object to store streamCAT data for each site
scd <- data.frame(siteAbbr = sites$siteAbbr)
scd[, c("MnAnthroImpervCatch", "PcntCatchDvlpd", "AvgDensRdKmSqCatch", "MnHouseKmSqCatch", "Mn2010PopKmSqCatch")] <- matrix(0,
                                        nrow = nrow(sites), ncol = 5)


# Variables to use
### 1 (167) Mean imperviousness of anthropogenic surfaces within catchment.

### 2a (337) Percentage of the local catchment area classified as developed, high intensity land use.
#### 2b (341) Percentage of the local catchment area classified as developed, low intensity land use.
#### 2c (345) Percentage of the local catchment area classified as developed, medium intensity land use.
#### 2d (349) Percentage of the local catchment area classified as developed, open space land use.

### 3 (391) Average density of roads per square kilometer within the local catchment (kilometer of road/square kilometer).

### 4 (413) Mean of all housing units per square kilometer values within the local catchment.

### 5 (417) Mean of all 2010 population per square kilometer values within the local catchment.
comidColNms <- paste0("streamCAT_comid", seq.int(3))
rw <- 1  #<-- track which iteration through the for loop/row of scd currently on
for(s in 1:nrow(sites)){
  # go through each streamCAT segment associated with EACH site
  ## in case site equidistant between 2 streamCAT segments
  s_comidCols <- comidColNms[!is.na(sites[s, comidColNms])] 
  s_subscd <- data.frame(Metric.Value = rep(0, 8), Metric.Description = rep(0, 8))
  for(i in s_comidCols){
    # make the filename from the site information
    s_fnm <- paste(paste0("comid", sites[s, i]), streamCATyr, sep = "_")
    # read in streamCAT data  
    s_scd <- read.csv(paste0("./streamCAT/", s_fnm, ".csv"),
      header = TRUE, skip = 9) 
    ## grab subset of variables (row indices from original csv file)      
    s_subscd[, 1] <- s_subscd[, 1] +
           s_scd[c(167, 337, 341, 345, 349, 391, 413, 417) - 10, "Metric.Value"]
  }  #<-- end for i
  s_subscd[, 2] <- s_scd[c(167, 337, 341, 345, 349, 391, 413, 417) - 10,
                                                         "Metric.Description"]
  s_subscd[, 1] <- s_subscd[, 1] / length(s_comidCols)  #<-- taking average
                                                    
  ## add percentage classified as developed while putting values in to `scd`
  scd[rw, 2:6] <- s_subscd[c(1,2,6:8), "Metric.Value"]  
    scd[rw, 3] <- scd[rw, 3] + sum(s_subscd[3:5, "Metric.Value"])
  rw <- rw + 1  
}




################################################################################

# principal components analysis

## Use `scd` object just created
pca_scd <- prcomp( ~ MnAnthroImpervCatch + PcntCatchDvlpd + AvgDensRdKmSqCatch +
    MnHouseKmSqCatch + Mn2010PopKmSqCatch,
  data = scd,
  center = TRUE, scale. = TRUE)

summary(pca_scd)

# Multiply by negative 1 to aid in interpretation
## positive value means more human activity/disturbance and negative more "natural"
pca_scd$rotation * -1

# Scree plot
plot(pca_scd)
## instead plot as proportions of variance explained by each PC
barplot(summary(pca_scd)$importance["Proportion of Variance", ], main = "",
  ylab = "Variance (proportion)", ylim = c(0, 1))


  
# first grab PC variables and add to the scd object
## Only keep PC1 and PC2
### Multiply by negative 1 to aid in interpretation
### positive value means more human activity/disturbance and negative more "natural"

scd[, paste0("PC", seq(2))] <- pca_scd$x[, 1:2] * -1

## Sort scd by PC1 score
srt_scd <- scd[order(scd$PC1), ]


# Now to add PC variables to data
## Match row/site in scd to site location data
##XXX FIXME commented out so don't do, but assumes microhabitat data doesn't have PC scores already

#microSiteIn_scd <- match(microhabitat$siteAbbr, scd$siteAbbr)
#microhabitat[, paste0("urbPC", seq(2))] <- scd[microSiteIn_scd, paste0("PC", seq(2))]


