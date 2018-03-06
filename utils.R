########## This file contains helper functions for data wrangling
########## Please email ht395@cornell.edu if you face problems or have questions about this file. 


# Main function to process and output data
process_data = function(data,prefix,labels,postfix,impute,truefactors=NULL)
{
  if (length(postfix)!=length(labels))
  {
    print("postfix must have same length as labels")
    return
  }
  
  # Drop observations where labels are missing
  keep = complete.cases(data[,labels])
  data = data[keep,]
  
  # Ensure that variables are correctly labeled as factors / numbers
  data = check_type(data,truefactors)
  
  # Remove useless variables
  data = remove_useless_vars(data)
  
  # Examine missing-ness
  print_summary(data)
  data = fix_NAs(data,impute)
  get_NA_columns(data,0)
  
  # Final removal of useless variables 
  data = remove_useless_vars(data)
  
  # Remove unused factor levels 
  data = remove_unused_levels(data)

  # Scale down large numeric labels
  data = scale_down_label(data,labels)
  
  # Output files
  for (i in 1:length(labels))
  {
    data_out = change_label(data,labels,i)
    filename = paste(prefix,postfix[i],sep="_")
    # Output csv
    write.csv(data_out,paste(filename,".csv",sep=""),quote=F,row.names=F)
    print("**************************************************")
  }
  print(paste(filename,"files outputted"))
  return(data)
}

## Function to select one of risk scores and actual outcomes label to output
# Convention used: 
# - high risk scores -> higher probability of negative outcome (e.g. loan default, recidivism) happening
# - minority class of actual outcomes label is the negative outcome
library(plyr) # for mapvalues function
change_label = function(data,labels,k)
{
  data_out = data
  for (i in 1:length(labels))
  {
    data_out[,labels[i]] = NULL
  }
  print(paste("label:",labels[k]))
  labelk = data[,labels[k]]
  if (length(unique(labelk))==2)   # binary classification
  { 
    # Set majority class to 0 and minority to 1
    labelk = mapvalues(labelk,names(sort(table(labelk))),c(1,0))
    labelk = factor(labelk)
    print(table(labelk))
  }
  else print(summary(labelk))
  data_out = data.frame(data_out,labelk)
  names(data_out)[ncol(data_out)] = labels[k]
  return(data_out)
}

# Function to drop columns
drop_columns = function(data,drop)
{
  if (length(drop)>0)
  {
    for (i in drop)
    {
      data[,i] = NULL
    }
  }
  return(data)
}

# Function to convert numeric factor to numeric
numfac2num = function(col)
{
  return(as.numeric(as.character(col)))
}

# Function to replace numbers with characters
num2fac = function(col,orig,new)
{
  colchar = as.character(col)
  if (length(orig)!=length(new)) stop("replacement must be of same length as original")
  for (i in 1:length(orig))
  {
    colchar[col==orig[i]]=new[i]
  }
  return(factor(colchar))
}

# Function to code missing values as NAs
code_NAs = function(data,NAcode)
{
  for (i in 1:ncol(data))
  {
    if (class(data[,i])=="Date")   # date parsing functions will deal with NA dates
    {
      next
    }
    missing = data[,i]%in%NAcode
    if (sum(missing,na.rm=T)>0)
    {
      data[missing,i] = NA
      if (class(data[,i])=="factor")
      {
        if (is_numeric(data[,i])) # Convert numeric factor to numeric
        {
          data[,i] = numfac2num(data[,i])
        }
        else  # Refactor levels to remove NA level
        {
          data[,i] = factor(data[,i])
        }
      }
    }
  }
  return(data)
}

# Function to remove useless variables
remove_useless_vars = function(data)
{
  # Duplicates
  dup = get_dup_columns(data)
  # Completely missing
  missing = get_NA_columns(data,1)
  # Have only one unique value
  oneval = get_oneval_columns(data)
  drop = unique(c(dup,oneval,missing))
  data = drop_columns(data,drop)
  return(data)
}

# Function to re-factor factors to remove any unused levels
remove_unused_levels = function(data)
{
  for (i in 1:ncol(data))
  {
    if (class(data[,i])=="factor") data[,i] = factor(data[,i])
  }
  return(data)
}

# Function to scale down large numeric label
scale_down_label = function(data,labels,const=1000)
{
  vars = names(data)
  for (i in 1:ncol(data))
  {
    if (class(data[,i])!="factor" & (vars[i]%in%labels))
    {
      if (median(data[,i])>const)
      {
        data[,i] = data[,i]/const
        #names(data)[i] = paste(vars[i],"_divide",const,sep="")
      } 
    }
  }
  return(data)
}

# Function to ensure variables are coded as they should be (numeric as numeric, categorical as categorical)
check_type = function(data,truefactors)
{
  vars = names(data)
  for (i in 1:ncol(data))
  {
    if (class(data[,i])=="factor" & is_numeric(data[,i]))  # Convert numeric factor to numeric
    {
      data[,i] = numfac2num(data[,i])
    }
    else if (class(data[,i])!="factor" & !is_numeric(data[,i]))  # Convert non-factor categorical to factor
    {
      data[,i] = factor(data[,i])
    }
    if (!is.null(truefactors))
    {
      if (vars[i]%in%truefactors)
      {
        data[,i] = factor(data[,i])
      }
    }
  }
  return(data)
}

# Function to describe NAs
get_NA_columns = function(data,dropthreshold)
{
  if (dropthreshold<0 | dropthreshold>1) stop("dropthreshold must be between 0 and 1")
  vars = names(data)
  drop = vector("character")
  dropthreshold = max(nrow(data)*dropthreshold,1)
  for (i in 1:ncol(data))
  {
    missing = is.na(data[,i])
    # Report number of NAs
    if (sum(missing)>=dropthreshold) print(paste(vars[i],"NA for",sum(missing),"obs"))
    # Add variable to drop list if more than acceptable amount of NAs
    if (sum(missing)>=dropthreshold) drop = c(drop,vars[i])
  }
  print("**************************************************")
  return(drop)
}

# Function to get variables that are identical to some other variable
library(digest) # for the hashing function, digest
get_dup_columns = function(data)
{
  dups = duplicated(sapply(data, digest))
  vars = names(data)
  drop = vars[dups]
  return(drop)
}

# Function to get variables that only have one value for all observations (hence useless)
get_oneval_columns = function(data)
{
  vars = names(data)
  drop = vector("character")
  for (i in 1:ncol(data))
  {
    if (length(unique(data[,i]))==1) drop = c(drop,vars[i])
  }
  return(drop)
}

# Function to code missing values as NAs
code_NAs = function(data,NAcode)
{
  for (i in 1:ncol(data))
  {
    if (class(data[,i])=="Date")   # date parsing functions will deal with NA dates
    {
      next
    }
    missing = data[,i]%in%NAcode
    if (sum(missing,na.rm=T)>0)
    {
      data[missing,i] = NA
      if (class(data[,i])=="factor")
      {
        if (is_numeric(data[,i])) # Convert numeric factor to numeric
        {
          data[,i] = numfac2num(data[,i])
        }
        else  # Refactor levels to remove NA level
        {
          data[,i] = factor(data[,i])
        }
      }
    }
  }
  return(data)
}

# Function to drop or impute NAs
library(randomForest) #for na.roughfix
library(missForest)  # for missForest
fix_NAs = function(data,impute)
{
  if (impute=="drop")
  {
    # Keep only complete cases
    keep = complete.cases(data)
    data = data[keep,]
    print(paste(sum(!keep),"obs dropped because of NAs"))
  }
  else if (impute=="mean")
  {
    # Impute mean of numerical, mode of categorical
    data = na.roughfix(data) 
  }
  else if (impute=="predict")
  {
    # Impute using random forest (remove large factors first, forest can't handle >53 categories)
    tmp = split_large(data)
    data_large = tmp[[1]]
    data_nolarge = tmp[[2]]
    tmp = missForest(data_nolarge)
    data = tmp$ximp
    data = data.frame(data,data_large)
  }
  else if (impute=="none") 1   # do nothing
  else print("Imputation options: drop, mean, predict, none")
  return(data)
}


########## Please email ht395@cornell.edu if you face problems or have questions about this file. 