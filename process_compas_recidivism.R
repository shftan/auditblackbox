########## INSERT MICROSOFT DISCLAIMER STATEMENT AND AUTHORS HERE

### Data set: ProPublica COMPAS risk score and recidivism actual outcome
### Source: https://www.propublica.org/datastore/dataset/compas-recidivism-risk-score-data-and-analysis

########## Read data
datafile = "compas-scores-two-years.csv"
data = read.csv(datafile,header=T)
prefix = "recid"

# Indicate variables that will be the labels
labels = c("decile_score","two_year_recid")
labelnames = c("score","outcome")

# Indicate variables that are true categorical variables
truefactors = c("gang_affiliation","police_district_last_contact","residence_district_last_arrest")

# Indicate imputation method
impute = "none"

########## Source helper functions
source("utils.R")

########## Subset data like ProPublica 
keep = which((data$days_b_screening_arrest <= 30) & (data$days_b_screening_arrest >= -30) & (data$is_recid != -1) & (data$c_charge_degree != "O") & (data$score_text != 'N/A'))
data = data[keep,]  

########## Create prison length of stay variable
data$length_of_stay <- as.numeric(as.Date(data$c_jail_out) - as.Date(data$c_jail_in))

########## Code categories
data$c_charge_desc = as.character(data$c_charge_desc)
data$c_charge_desc[data$c_charge_desc==""] = "unknown"

########## Drop and keep variables
## Drop duplicate variables
drop = c("priors_count.1","decile_score.1")  #identical to their no .1 counterparts
data = drop_columns(data,drop)
## Drop variables related to violent recidivism
keep = c("age","race","sex","priors_count","length_of_stay","c_charge_degree","c_charge_desc")
data = data[,c(keep,labels)]

########## Process and output data
### Missing data is imputed according to method specified by "impute" variable
### Observations with missing values for labels are dropped
### Useless variables (variable completely duplicated, or only has missing values, or only has one unique value) are dropped
### Unused categorical levels are dropped
### Some ML methods are sensitive to the scale of the label so large numeric labels are scaled down
data = process_data(data,prefix,labels,postfix,impute)


### Now you should have two csv files with the same features but one has the risk scores label and the other has the actual outcomes label.
### Please email ht395@cornell.edu if you face problems running this script.