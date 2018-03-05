########## INSERT MICROSOFT DISCLAIMER STATEMENT AND AUTHORS HERE

### Data set: NYPD Stop and Frisk dataset with heuristic risk score from Goel et al. and Weapon Possession actual outcome
### Source 1: https://www1.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page
### Source 2: https://5harad.com/data/sqf.RData (Goel's website)
### Note that this risk score was proposed by an academic research group, not NYPD
### References for cleaning the data set and creating the risk score:
### - Sharad Goel, Justin M. Rao, Ravi Shroff. Precinct or Prejudice? Understanding Racial Disparities in New York City's Stop-and-Frisk Policy. Annals of Applied Statistics 2016
### - Elaine Angelino, Nicholas Larus-Stone, Daniel Alabi, Margo Seltzer, Cynthia Rudin. Learning Certifiably Optimal Rule Lists. KDD 2017


########## Read data
datafile = sqf.RData
load("raw/sqf.RData")
prefix = "stopfrisk"

# Indicate variables that will be the labels
labels = c("heuristic","found.weapon")
labelnames = c("score","outcome")

# Indicate variables that are true categorical variables
truefactors = "precinct"  

# Indicate imputation method
impute = "mean"

########## Source helper functions
source("utils.R")

########## Subset to only cpw crime (following Goel et al.) and 2009-2010
### Risk scoring model was trained on 2009-2010, then predictions on 2011-2012 data was used in the rest of the analysis in the Goel paper)
data = stops[stops$suspected.crime=="cpw" & (stops$year==2009 | stops$year==2010) & !is.na(stops$suspected.crime) & !is.na(stops$year),]

########## Create heuristic risk score
data$heuristic = 3*data$stopped.bc.object+1*data$stopped.bc.bulge+1*data$additional.sights

########## Clean up date time information
### Create month, day of week, time of day binned into disjoint 4 hour blocks
library(lubridate) # for month function
monthofyear = month(data$date)
data$monthofyear = factor(monthofyear,levels=sort(unique(monthofyear)),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
data$dayofweek = weekdays(as.Date(data$date))
timeofday = as.numeric(hm(data$time)) 
breaks = seq(0,24,4)*60*60
temp = sapply(timeofday,function(x) findInterval(x,breaks))
data$timeofday = factor(temp,levels=sort(unique(temp)),labels=c("0-4","4-8","8-12","12-16","16-20","20-24"))

########## Drop variables
### Administrative variables
drop = c("year","serial","suspect.dob","lat","lon","xcoord","ycoord","id")
### Variables already cleaned up into other variables
drop = c(drop,"date","time")
### Variables that will leak info into label
drop = c(drop,"arrested","arrested.reason","summons.issued","frisked","searched")
drop = c(drop,names(data)[grepl("force",names(data))])
drop = c(drop,names(data)[grepl("frisked",names(data))])
drop = c(drop,names(data)[grepl("searched",names(data))])
drop = c(drop,names(data)[grepl("found",names(data))])
### Variables with too many missing values and imputation likely not meaningful
drop = c(drop,"officer.verbal","officer.shield","suspect.hair","stop.length")
### Don't accidentally drop label
drop = setdiff(drop,labels)
data = drop_columns(data,drop)

########## Drop observations missing key variables
keep = complete.cases(data.frame(data$suspect.sex,data$suspect.race,data$suspect.weight))
data = data[keep,]

##########  Relevel ordinal variables
data$dayofweek = factor(data$dayofweek,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data$suspect.eye = factor(data$suspect.eye,levels=c("black","blue","brown","gray","green","hazel","maroon","pink","violet","other","two different","unknown"))

########## Process and output data
### Missing data is imputed according to method specified by "impute" variable
### Observations with missing values for labels are dropped
### Useless variables (variable completely duplicated, or only has missing values, or only has one unique value) are dropped
### Unused categorical levels are dropped
### Some ML methods are sensitive to the scale of the label so large numeric labels are scaled down
data = process_data(data,prefix,labels,postfix,impute,truefactors)


### Now you should have two csv files with the same features but one has the risk scores label and the other has the actual outcomes label.
### Please email ht395@cornell.edu if you face problems running this script.