########## INSERT MICROSOFT DISCLAIMER STATEMENT AND AUTHORS HERE

### Data set: Chicago Police "Strategic Subject List" risk score and "Party-To-Violence" actual outcome
### Source: https://data.cityofchicago.org/Public-Safety/Strategic-Subject-List/4aki-r3np

########## Read data
datafile = "Strategic_Subject_List.csv"
data = read.csv(datafile,header=T)
prefix = "police"

# Indicate variables that will be the labels
labels = c("SSL_SCORE","partytoviolence")
labelnames = c("score","outcome")

# Indicate variables that are true categorical variables
truefactors = c("gang_affiliation","police_district_last_contact","residence_district_last_arrest")

# Indicate imputation method
impute = "drop"

# Indicate code(s) for missing data
missing = c("", " ", "X")

########## Source helper functions
source("utils.R")

########## Code missing values
data = code_NAs(data,missing)

########## Clean up variables
data$stoporder = factor(ifelse(is.na(data$STOP_ORDER_NO),"N","Y"))
data$parolee = factor(ifelse(is.na(data$PAROLEE_I),"N","Y"))
data$partytoviolence = ifelse(!is.na(data$SSL_LAST_PTV_DATE),"Y","N")   

########## Select variables to keep
### Not kept because already cleaned up into other variables: STOP_ORDER_NO, PAROLEE_I
### Not kept because not baseline variables, would leak info into label: TRAP_STATUS, SSL_LAST_PTV_DATE, LATEST_DATE, LATEST_WEAPON_ARR_DATE, LATEST_NARCOTIC_ARR_DATE, LATEST_DOMESTIC_ARR_DATE, CPD_ARREST_I
### Not kept because information is duplicated in other variables: AGE_GROUP, AGE_TO, AGE_CURR, RAW_SSL_SCORE, HEAT_SCORE, RAW_HEAT_SCORE
### Not kept, because not informative (e.g. all obs have same value for this var): STATUS_I
### Not kept, because too many missing values and imputation likely not meaningful: DOMESTIC_ARR_CNT, WEAPONS_ARR_CNT, NARCOTICS_ARR_CNT, MAJORITY_DIST, DLST, IDOC_RES_CITY, IDOC_RES_STATE_CODE, IDOC_RES_ZIP_CODE, IDOC_CPD_DIST, TRAP_FLAGS, SSL_FLAGS, LATITUDe, LONGITUDE, COMMUNITY_AREA, CENSUS_TRACT, LOCATION
keep = c("PREDICTOR_RAT_AGE_AT_LATEST_ARREST","PREDICTOR_RAT_VICTIM_SHOOTING_INCIDENTS","PREDICTOR_RAT_VICTIM_BATTERY_OR_ASSAULT",
         "PREDICTOR_RAT_ARRESTS_VIOLENT_OFFENSES","PREDICTOR_RAT_GANG_AFFILIATION","PREDICTOR_RAT_NARCOTIC_ARRESTS","PREDICTOR_RAT_TREND_IN_CRIMINAL_ACTIVITY",
         "PREDICTOR_RAT_UUW_ARRESTS","SEX_CODE_CD","RACE_CODE_CD","LATEST_DIST","LATEST_DIST_RES","WEAPON_I", "DRUG_I","stoporder","parolee")
data = data[,c(keep,labels)]

########## Relevel ordinal variables
data$age_last_arrest = factor(data$age_last_arrest,levels=c("less than 20","20-30","30-40","40-50","50-60","60-70","70-80"))

########## Process and output data
### Missing data is imputed according to method specified by "impute" variable
### Observations with missing values for labels are dropped
### Useless variables (variable completely duplicated, or only has missing values, or only has one unique value) are dropped
### Unused categorical levels are dropped
### Some ML methods are sensitive to the scale of the label so large numeric labels are scaled down
data = process_data(data,prefix,labels,postfix,impute,truefactors)


### Now you should have two csv files with the same features but one has the risk scores label and the other has the actual outcomes label.
### Please email ht395@cornell.edu if you face problems running this script.