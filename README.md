# Caldwell_turtle_nest-choice-predation

[![DOI](https://zenodo.org/badge/600135781.svg)](https://zenodo.org/badge/latestdoi/600135781)

Version controlled and editable source for the data and code supporting the paper
__"Consistent nest site selection by turtles across habitats with varying levels of human disturbance"__ by _Molly Folkerts Caldwell, Jorge E. López-Pérez, Daniel A. Warner, and Matthew E. Wolak_.

## Data

### Data citation

If you use these data, please cite the publication (preference)

>M. Folkerts Caldwell, J.E. López-Pérez, D.A. Warner, and M.E. Wolak. 2023. Consistent
nest site selection by turtles across habitats with varying levels of human disturbance.
_Diversity_

or data package 

>Matthew E. Wolak. (2023). qgevoeco/Caldwell_turtle_nest-choice-predation: Initial release v1.0.0 (v1.0.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7630637

Data were also collected from the StreamCat Dataset and are located in this repositories `./streamCAT` folder. For information, please see the associated metadata [https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset-readme](https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset-readme). To cite the StreamCat data:

>Hill, Ryan A., Marc H. Weber, Scott G. Leibowitz, Anthony R. Olsen, and Darren J. Thornbrugh, 2016. The Stream-Catchment (StreamCat) Dataset: A Database of Watershed Metrics for the Conterminous United States. Journal of the American Water Resources Association (JAWRA) 52:120-128. DOI: 10.1111/1752-1688.12372.


### Data metadata

Column header descriptions for the datasets analyzed in the paper:

#### "microhabitat.txt" file

A tab-separated text file consisting of:

  - `SiteType` integer representing the categorization of site by human activit and/or disturbance. Values of `0` represent Low, `1` represent Intermediate, and `2` represent high human activity and/or disturbance at the site. See also the character representation in `HumDist`.
  
  - `siteAbbr` character string giving an abbreviated site name.
  
  - `SiteName` character string for the name of each study site.
  
  - `NestRand` integer that indicates whether a nest observation was `0` a natural or `1` a randomly positioned artificial nest.
  
  - `HumDist` character string representing the categorization of site by human activity and/or disturbance. See also the integer representation in `SiteType`.
  
  - `NestCluster` character grouping natural nests and associated artificial nests.
  
  - `ibdepth` numeric variable indicating the depth of the iButton placement in each nest.
  
  - `Canopy` numeric indicating percent canopy openness.
  
  - `DistTW` numeric indicating the distance between each nest and the water in meters.
  
  - `Slope` numeric indicating the slope of the ground at each nest in degrees.
  
  - `dailyMean_C` numeric indicating the averaged daily mean nest temperature in degrees Celsius.
  
  - `dailyMax_C` numeric indicating the averaged daily maximum nest temperature in degrees Celsius.
  
  - `dailyMin_C` numeric indicating the averaged daily minimum nest temperature in degrees Celsius.
  
  - `range` numeric indicating the difference between averaged daily maximum and minimum nest temperature in degrees Celsius.
  
  - `urbPC1` numeric indicating the principal components analysis score for each observation on the first principal components axis (unitless).   
    
  - `urbPC2` numeric indicating the principal components analysis score for each observation on the second principal components axis (unitless). Note, this was not used in any analyses. 

#### "predation.txt" file

A tab-separated text file consisting of:
  
  - `NestID` integer giving unique identity label to each experimental nest.
  
  - `SiteName` character string for the name of each study site.
  
  - `Depred` integer that indicates if a nest was `1` depredated or `0` not.
  
  - `SiteType` integer representing the categorization of site by human activit and/or disturbance. Values of `0` represent Low, `1` represent Intermediate, and `2` represent high human activity and/or disturbance at the site. See also the character representation in `HumDist`.

  - `HumDist` character string representing the categorization of site by human activity and/or disturbance. See also the integer representation in `SiteType`.
  
  - `RoundCat` character indicating which round (1-3) of the experiment in which each nest occurred.

#### "site_latlon_DATA.txt" file

A tab-separated text file consisting of:

  - `Site` character string for the name of each study site.
  
  - `siteAbbr` character string giving an abbreviated site name.
  
  - `lat` and `lon` numeric latitude and longitude GPS coordinates. The Notasulga site intentionally has NA values to protect the privacy of the landowners.
  
  - `streamCAT_comid1`, `streamCAT_comid2`, and ``streamCAT_comid3` integer Stream CAT stream segment unique identifiers. Where a site is between stream segments, multiple segments are attributed to the site and the average is taken.
  
  - `Notes` character string with notes about the sites.
  

## Changes
For ease of reference, an overview of significant changes to be noted below. Tag comments with commits or issues, where appropriate.


