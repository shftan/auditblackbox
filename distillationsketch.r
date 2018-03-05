########## INSERT MICROSOFT DISCLAIMER STATEMENT AND AUTHORS HERE

########## Sketch of distillation setup 

# Read in data
score = ###vector_of_length_n###
outcome = ###vector_of_length_n###
features = ###matrix of n observations and p features###

# Student model of teacher black-box risk scoring model
scorestudentmodel = ###model_predicting_score_from_features###

# Actual outcomes model
outcomemodel = ###model_predicting_outcome_from_features###

########## Calibrate risk scores and actual outcomes

### Calculate empirical probability
epsilon = 0.0001
fraction = table(score,outcome)/as.numeric(table(score))
actualscore = as.numeric(rownames(fraction))
actualprob = fraction[,2]
if (sum(actualprob==0)>0 & sum(actualprob==1)>0)
{
  actualprob[actualprob==0] = epsilon 
  actualprob[actualprob==1] = 1-epsilon 
} else if (sum(actualprob==0)>0) 
{
  actualprob = actualprob+epsilon
} else if (sum(actualprob==1)>0) 
{
  actualprob = actualprob-epsilon    
} 

### Plot on probability scale
plot(actualscore,actualprob)

### Plot on logit probability scale
plot(actualscore,logit(actualprob))
# We pick the logit scale to be the scale on which to compare the risk scores and actual outcomes models
# If the relationship here is not linear, calibration is needed

### If needed, learn nonlinear transformation to calibrate
# We use smoothing splines, and tune the smoothing parameter until monotonicity is achieved
# This can be thought of as similar to isotonic regression but smoother because splines instead of step functions are used
mgcvmodel = gam(outcome~s(score),family="binomial")
spnow = mgcvmodel$sp
testing = data.frame(actualscore)
mgcvpredlogit = predict(mgcvmodel,newdata=testing,type="link")

### If not yet monotonic, tune sp until get monotonicity
if (min(mgcvpredlogit[2:length(mgcvpredlogit)]-mgcvpredlogit[1:(length(mgcvpredlogit)-1)])<0)
print("mgcv not yet monotonic, tune!!!")
tunesp = ### look at current sp value (spnow variable) and sweep a range of other values
mgcvmodel = gam(outcome~s(score),family="binomial",sp=spnow*tunesp)
mgcvpredlogit = predict(mgcvmodel,newdata=testing,type="link")

########## Calibrate risk scores and actual outcomes
