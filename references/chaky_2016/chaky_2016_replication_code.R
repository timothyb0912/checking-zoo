setwd("Dropbox/Replication Paper/Code:Data:Paper/")
require(readstata13)
require(foreign)
require(multiwayvcov)
require(mlogit)
require(xtable)
require(stargazer)
require(ggplot2)
require(car)
require(mgcv)
require(reshape2)
require(nnet)

#Load in replication data from B&W
county.data <- read.dta13("BW JOP county replication data.dta")
indiv.data <- read.dta("BW JOP individual replication data.dta")
state.data <- read.dta("BW JOP state replication data.dta")

#REPLICATION

#TABLE 1

#Table 1 Model 1 (Unweighted) - correct coefficients and standard errors (except intercept)

table1.model1 <- lm(Turnout ~ Unemploy_County_new + Unemploy_State + PcntBlack + ZPcntHSGrad +
                      AdjIncome + closeness + GubElection + SenElection +
                      Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr92 + Yr88 + Yr84 + Yr80 +  
                      factor(FIPS_County), data=county.data)
table1.model1$coefficients[1:9]

#SE
m1.vcovCL <- cluster.vcov(table1.model1, county.data$FIPS_County, df_correction = FALSE)
ses.m1u <- sqrt(diag(m1.vcovCL))
ses.m1u[1:13]

#Table 1 Model 1 (Weighted) - correct coefficients and standard errors (except intercept)

table1.model1w <- lm(Turnout ~ Unemploy_County_new + Unemploy_State + PcntBlack + ZPcntHSGrad +
                       AdjIncome + closeness + GubElection + SenElection +
                       Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr92 + Yr88 + Yr84 + Yr80 +  
                       factor(FIPS_County), data=county.data, weights=as.numeric(pop))
table1.model1w$coefficients[1:9]

#SE
m1w.vcovCL <- cluster.vcov(table1.model1w, county.data$FIPS_County, df_correction = TRUE)
ses.m1w <- sqrt(diag(m1w.vcovCL))
ses.m1w[1:9]

#Table 1 Model 2 (Unweighted) - correct coefficients and standard errors (except intercept)

table1.model2u <- lm(Turnout ~ Unemploy_County_new + Unemploy_State + PcntBlack + ZPcntHSGrad +
                       AdjIncome + closeness + GubElection + SenElection + Turnout_Lag +
                       Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr92 + Yr88 + Yr84 + Yr80 +  
                       factor(FIPS_County), data=county.data)
table1.model2u$coefficients[1:10]

#SE
m2u.vcovCL <- cluster.vcov(table1.model2u, county.data$FIPS_County, df_correction = FALSE)
ses.m2u <- sqrt(diag(m2u.vcovCL))
ses.m2u[1:10]

#Table 1 Model 2 (Weighted) - correct coefficients and standard errors (except intercept)

table1.model2w <- lm(Turnout ~ Unemploy_County_new + Unemploy_State + PcntBlack + ZPcntHSGrad +
                       AdjIncome + closeness + GubElection + SenElection + Turnout_Lag +
                       Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr92 + Yr88 + Yr84 + Yr80 +  
                       factor(FIPS_County), data=county.data, weights=as.numeric(pop))
table1.model2w$coefficients[1:10]

#SE
m2w.vcovCL <- cluster.vcov(table1.model2w, county.data$FIPS_County, df_correction = TRUE)
ses.m2w <- sqrt(diag(m2w.vcovCL))
ses.m2w[1:10]

#########################################################################################################
#TABLE 2- Diff in Diff model of county turnout

#Model 1

t2model1 <- lm(Turnout_diff4 ~ Unemploy_County_diff4 + Unemploy_State_diff4 + Yr2008
               + Yr2004 + Yr2000 + Yr96 + Yr88 + Yr84 + Yr80 + Yr76 + factor(FIPS_County), data = county.data)
t2model1$coefficients[1:3]

#SE

t2m1.vcovCL <- cluster.vcov(t2model1, county.data$FIPS_County, df_correction = FALSE)
ses.t2m1 <- sqrt(diag(t2m1.vcovCL))
ses.t2m1[1:3]

#Intercept p-value

#Test Statistic
t_stat.t2m1 <- (4.09143865 - 0)/0.07437852 # 55.00834
t_stat.t2m1

#p-value
pvalue.t2m1 <- 2*(1 - pnorm(abs(t_stat.t2m1), 0, 1)) # 0
pvalue.t2m1

#Model 2

t2model2 <- lm(Turnout_diff4 ~ Unemploy_County_diff4 + Unemploy_State_diff4 + PcntBlack + ZPcntHSGrad + AdjIncome
               + closeness + GubElection + SenElection + Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr88 + Yr84 + Yr80 + Yr76 
               + factor(FIPS_County), data = county.data)
t2model2$coefficients[1:25]

t2m2.vcovCL <- cluster.vcov(t2model2, county.data$FIPS_County, df_correction = FALSE)
ses.t2m2 <- sqrt(diag(t2m2.vcovCL))
ses.t2m2[1:13]

#Intercept Test Statistic
t_stat.t2m2 <- (2.485957452 - 0)/0.775492363 # 3.20565
t_stat.t2m2

#Intercept p-value
pvalue.t2m2 <- 2*(1 - pnorm(abs(t_stat.t2m2), 0, 1)) # 0.0013
pvalue.t2m2

#################################################################################################
#TABLE 3

new.state.data <- state.data[ which(state.data$presyear==0 
                                    & state.data$year >= '1978'), ]

# Column 1 (Quality challengers: All incumbents)
# share of House incumbents who faced challengers with prior elective experience 
t3challenger.all <- lm(quality_share_new ~ prior_ue + s_black + college + pci_0000 + SenElection 
                       + factor(year) + factor(fips_state), data=new.state.data)
summary(t3challenger.all)
t3challenger.all$coefficients[1:25]

#stan errors slightly off
t3challenger.all.vcovCL <- cluster.vcov(t3challenger.all, new.state.data$fips_state, df_correction=TRUE)
ses <- sqrt(diag(t3challenger.all.vcovCL))
ses[1:12]

#table 3, column 2 - Quality challengers - only incumbents from Pres party

t3challenger.inc <- lm(share_qual_out2 ~ prior_ue + s_black + college + pci_0000 + SenElection 
                       + factor(year) + factor(fips_state), data=new.state.data)
t3challenger.inc$coefficients[1:25]

#stan errors slightly off
t3challenger.inc.vcovCL <- cluster.vcov(t3challenger.inc, new.state.data$fips_state, df_correction=TRUE)
ses <- sqrt(diag(t3challenger.inc.vcovCL))
ses[1:12]

#Table 3 - column 3 - Open seats (all seats)
#share of House seats in the state that become open

t3seats.all <- lm(open_all_share ~ prior_ue + s_black + college + pci_0000 + SenElection 
                  + factor(year) + factor(fips_state), data=new.state.data)
t3seats.all$coefficients[1:25]

#stan errors slightly off
t3seats.all.vcovCL <- cluster.vcov(t3seats.all, new.state.data$fips_state, df_correction=FALSE)
ses <- sqrt(diag(t3seats.all.vcovCL))
ses[1:12]

#Table 3 - column 4 - Only seats held by President's party

t3seats.pp <- lm(share_open ~ prior_ue + s_black + college + pci_0000 + SenElection 
                 + factor(year) + factor(fips_state), data=new.state.data)
t3seats.pp$coefficients[1:25]

#stan errors slightly off
t3seats.pp.vcovCL <- cluster.vcov(t3seats.pp, new.state.data$fips_state, df_correction=FALSE)
ses <- sqrt(diag(t3seats.pp.vcovCL))
ses[1:12]

#Table 3 column 5 - Party Transfers, both parties

t3transfers.both <- lm(partyspending ~ prior_ue + s_black + college + pci_0000 + SenElection 
                       + factor(year) + factor(fips_state), data=new.state.data)
t3transfers.both$coefficients[1:25]

#stan errors slightly off
t3transfers.both.vcovCL <- cluster.vcov(t3transfers.both, new.state.data$fips_state, df_correction=FALSE)
ses <- sqrt(diag(t3transfers.both.vcovCL))
ses[1:12]

#table 3 - column 6 - party transfers, only party not in white house

t3transfers.notWH <- lm(outparty_spend ~ prior_ue + s_black + college + pci_0000 + SenElection 
                        + factor(year) + factor(fips_state), data=new.state.data)
t3transfers.notWH$coefficients[1:25]

t3transfers.notWH.vcovCL <- cluster.vcov(t3transfers.notWH, new.state.data$fips_state, df_correction=FALSE)
ses <- sqrt(diag(t3transfers.notWH.vcovCL))
ses[1:12]


###############################################################################################  
#TABLE 4

state.data.t4 <- state.data[ which(state.data$presyear==0 
                                   & state.data$year >= '1980'
                                   & state.data$GubElection == 1), ]

#The regression they ran, without year fixed effects
table4.1 <- lm(vep ~ uerate + incparty + uerate*incparty + s_black + college + SenElection
               + factor(fips_state), data=state.data.t4)
summary(table4.1)

t41.vcovCL <- cluster.vcov(table4.1, state.data.t4$fips_state, df_correction = TRUE)
ses.t41 <- sqrt(diag(t41.vcovCL))
ses.t41

##########################################################################################
#TABLE 5 - correct coefficients and standard errors (except intercept)

#Convert educ and income variables to numeric
indiv.data1 <- indiv.data
indiv.data1$educ <- as.numeric(indiv.data1$educ)
indiv.data1$income <- as.numeric(indiv.data1$income)

indiv.data.ml <- mlogit.data(indiv.data1, choice="partyvote", shape="wide")

table5 <- mlogit(partyvote ~ 0 | uerate + totalspend_voter_inf + democrat + republican + black + hisp +
                   other + female + married + age + educ + income + incomedk + unemployed + factor(fips_state),
                 data=indiv.data.ml, reflevel="Abstain")
summary(table5)

#SE

#Remove NA's from dataset for SE
indiv.data2 <- na.omit(indiv.data)
nrow(indiv.data2)

cl.mlogit   <- function(fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- length(coefficients(fm))
  dfc <- (M/(M-1))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat.=crossprod(uj)/N)
  ses <- sqrt(diag(vcovCL))
  coeftest <- coeftest(fm, vcovCL) 
  return(newList <- list("summary" = coeftest, "vcovCL" = vcovCL, "ses" = ses))
}

ses.t51 <- cl.mlogit(table5, indiv.data2$fips_state)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#EXTENSIONS

#TABLE 4 (need to run Table 4 replication section that to run this section)

#Re-calculate Table 4 regression with year fixed effects
table4 <- lm(vep ~ uerate + incparty + uerate*incparty + s_black + college + SenElection
             + factor(fips_state) + factor(year), data=state.data.t4)
summary(table4)
nrow(state.data.t4) #289 observations - 4 in regression = 285 total obs (matches Stata)
coefs.t4 <- table4$coefficients

#SE
t4.vcovCL <- cluster.vcov(table4, state.data.t4$fips_state, df_correction = TRUE)
ses.t4 <- sqrt(diag(t4.vcovCL))
ses.t4

stargazer(table4.1, table4,  se=list(ses.t41, ses.t4), notes = c("Ordinary least squares models.", "Standard errors clustered by county."))

#Interaction plot (from Anton's code - http://causalloop.blogspot.com/2013/06/marginal-effect-plots-for-interaction.html)

interaction_plot_binary <- function(model, effect, moderator, interaction, varcov="default", conf=.95, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient", factor_labels=c(0,1)){
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- c(0,1)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = 1.3
  min_y = -0.4
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(-.5, 1.5), xlab=xlabel, ylab=ylabel, main=title, cex.main=.95,  xaxt="n")
  
  # Plot points of estimated effects
  points(x=x_2, y=delta_1, pch=16)
  
  # Plot lines of confidence intervals
  lines(x=c(x_2[1], x_2[1]), y=c(upper_bound[1], lower_bound[1]), lty=1, col="blue")
  points(x=c(x_2[1], x_2[1]), y=c(upper_bound[1], lower_bound[1]), pch=c(25,24), bg="blue", col="blue")
  lines(x=c(x_2[2], x_2[2]), y=c(upper_bound[2], lower_bound[2]), lty=1, col="red")
  points(x=c(x_2[2], x_2[2]), y=c(upper_bound[2], lower_bound[2]), pch=c(25,24), bg="red", col="red")
  
  # Label the axis
  axis(side=1, at=c(0,1), labels=factor_labels)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3)
  
}

#With year fixed effects
interaction_plot_binary(model=table4, effect="uerate", moderator="incparty", interaction="uerate:incpartyR", varcov=t4.vcovCL, conf=.95,  title="Marginal Effects of Unemployment \nRate and Party on Turnout (w/Year FEs)", xlabel="Incumbent Party", ylabel="Estimated marginal coefficient", factor_labels=c("Democrat","Republican"))

#Without year fixed effects (the model presented in B&W)
interaction_plot_binary(model=table4.1, effect="uerate", moderator="incparty", interaction="uerate:incpartyR", varcov=t4.vcovCL, conf=.95,  title="Marginal Effects of Unemployment \nRate and Party on Turnout (no Year FEs)", xlabel="Incumbent Party", ylabel="Estimated marginal coefficient", factor_labels=c("Democrat","Republican"))


#########################################################################################################
#TABLE 5

#Read in ANES, cut to necessary variables/years and make CSV (reduced CSV included, read in below)

  #anes <- read.dta13("anes_timeseries_cdf_stata12.dta")
  #anes.0 <- data.frame(anes$VCF0004, anes$VCF0101, anes$VCF0110, anes$VCF0901a, anes$VCF0114,
                    #anes$VCF0105b, anes$VCF0104, anes$VCF0147, anes$VCF0303, anes$VCF0116, anes$VCF9025)
  #colnames(anes.0) <- c("VCF0004", "VCF0101", "VCF0110", "VCF0901a", "VCF0114", "VCF0105b",
                      #"VCF0104", "VCF0147", "VCF0303", "VCF0116", "VCF9025")
  #anes.1 <- subset(anes.0, anes$VCF0004 == 1978 | anes$VCF0004 == 1982 | anes$VCF0004 == 1986 |
                    #anes$VCF0004 == 1990 | anes$VCF0004 == 1994 | anes$VCF0004 == 1998) 
  #write.csv(anes.1, "anes-reduced.csv")

anes.2 <- read.csv("anes-reduced.csv")
anes.2 <- anes.2[,-1]
head(anes.2)
#Transform/rename variables
year <- anes.2$VCF0004 
age <- anes.2$VCF0101
educ <- anes.2$VCF0110
fips_state <- anes.2$VCF0901a
income <- anes.2$VCF0114

incomedk <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0114[i] == "0. DK; NA; refused to answer; no Pre IW"){
    incomedk[i] <- 1
  } else{
    incomedk[i] <- 0
  }
}

black <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0105b[i] == "2. Black non-Hispanic"){
    black[i] <- 1
  } else{
    black[i] <- 0
  }
}

hisp <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0105b[i] == "3. Hispanic"){
    hisp[i] <- 1
  } else{
    hisp[i] <- 0
  }
}

other <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0105b[i] == "4. Other or multiple races, non-Hispanic"){
    other[i] <- 1
  } else{
    other[i] <- 0
  }
}

female <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0104[i] == "2. Female"){
    female[i] <- 1
  } else{
    female[i] <- 0
  }
}

married <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0147[i] == "1. Married"){
    married[i] <- 1
  } else{
    married[i] <- 0
  }
}

democrat <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0303[i] == "1. Democrats (including leaners)"){
    democrat[i] <- 1
  } else{
    democrat[i] <- 0
  }
}

republican <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0303[i] == "3. Republicans (including leaners)"){
    republican[i] <- 1
  } else{
    republican[i] <- 0
  }
}


unemployed <- rep(NA, nrow(anes.2))
for(i in 1:nrow(anes.2)){
  if(anes.2$VCF0116[i] == "4. Unemployed"){
    unemployed[i] <- 1
  } else{
    unemployed[i] <- 0
  }
}


partyvote <- as.numeric(anes.2$VCF9025)
partyvote[which(partyvote == 2)] <- "Democrat"
partyvote[which(partyvote == 3)] <- "Republican"
partyvote[which(partyvote == 5)] <- "Abstain"
partyvote[which(partyvote == 1)] <- NA
partyvote[which(partyvote == 4)] <- NA

anes.reduced <- data.frame(year, age, educ, fips_state, black, hisp, other, female, married,
                           republican, democrat, unemployed, partyvote, income, incomedk)

head(anes.reduced)

#Read in unemployment rate data
uerate.bls <- read.csv("uerate-bls.csv")
colnames(uerate.bls) <- c("fips_state", c(1976:2010))
uerate.bls.melt <- melt(uerate.bls, id = "fips_state")
colnames(uerate.bls.melt) <- c("fips_state", "year", "uerate")

#Merge uerate.bls.melt into ANES
anes.reduced.uemerge <- merge(anes.reduced, uerate.bls.melt, by = c("fips_state", "year"))

#Read in campaign expenditures
campaign.spend <- read.csv("CampaignSpend.csv")
campaign.spend <- data.frame(campaign.spend$Year, campaign.spend$State, campaign.spend$X.Spent.2001.)
colnames(campaign.spend) <- c("year", "state", "totalspend")
campaign.spend$state <- as.character(campaign.spend$state)

#Read FIPS state codes (for merge)
states <- read.csv("fips-state.csv")
states$state_abb <- as.character(states$state_abb)

#Add fips_state codes to campaign expenditures
campaign.spend$fips_state <- c()
for(i in 1:nrow(campaign.spend)){
  campaign.spend$fips_state[i] <- states$fips_state[which(states$state_abb == campaign.spend$state[i])]
}

#Add voting eligible population
vep <- read.csv("1980-2014 November General Election - Turnout Rates.csv")

#Add fips_state codes to vep
vep$fips_state <- c()
vep$State[which(vep$State == "United States")] <- NA
vep$State[which(vep$State == "District of Columbia")] <- NA
vep$State[which(vep$State == "United States (Excl. Louisiana)")] <- NA
vep <- vep[which(vep$State != "United States"),]
vep <- vep[which(vep$State != "District of Columbia"),]
vep <- vep[which(vep$State != "United States (Excl. Louisiana)"),]
vep$State <- as.character(vep$State)

for(i in 1:nrow(vep)){
  vep$fips_state[i] <- states$fips_state[which(states$state == vep$State[i])]
}

vep1 <- data.frame(vep$fips_state, vep$Year, vep$Voting.Eligible.Population..VEP.)
colnames(vep1) <- c("fips_state", "year", "vep")

#Merge vep to campaign expenditures
campaign.spend.vep <- merge(campaign.spend, vep1, by = c("fips_state", "year"))

#Divide totalspend by vep
campaign.spend.vep$totalspend_voter_inf <- campaign.spend.vep$totalspend/campaign.spend.vep$vep

#Merge campaign expenditures with ANES
anes.reduced.camp.merge <- merge(anes.reduced.uemerge, campaign.spend.vep, by = c("fips_state", "year"))

#Replicate Table 5 with new dataset/remove 1994
anes.data <- subset(anes.reduced.camp.merge, anes.reduced.camp.merge$year != "1994")

#Cleaning
anes.data$totalspend_voter_inf <- as.numeric(anes.data$totalspend_voter_inf)
anes.data$age[which(anes.data$age == 0)] <- NA
anes.data$educ[which(anes.data$educ == "0. DK; NA; no Pre IW; short-form 'new' Cross Section")] <- NA
anes.data$income[which(anes.data$income == "0. DK; NA; refused to answer; no Pre IW")] <- NA
anes.data$incomedk <- as.numeric(anes.data$incomedk)
anes.data$uerate <- anes.data$uerate/100

#Remove NA's from dataset for robust SE function and set educ/income as numeric
anes.data2 <- na.omit(anes.data)
anes.data$educ <- as.numeric(anes.data$educ)
anes.data$income <- as.numeric(anes.data$income)
anes.data.ml <- mlogit.data(anes.data, choice="partyvote", shape="wide")

#Run multinomial logit as close to Table 5 model as possible
table5.anes <- mlogit(partyvote ~ 0 | uerate + totalspend_voter_inf + democrat + republican + black + hisp +
                        other + female + married + age + educ + income + unemployed + factor(fips_state),
                      data=anes.data.ml, reflevel="Abstain")
summary(table5.anes)

ses.t5a <- cl.mlogit(table5.anes, anes.data2$fips_state)

#Model with year fixed effects
table5.anes1 <- mlogit(partyvote ~ 0 | uerate + totalspend_voter_inf + democrat + republican + black + hisp +
                         other + female + married + age + educ + income + unemployed + factor(year),
                       data=anes.data.ml, reflevel="Abstain")
summary(table5.anes1)
ses.t5a1 <- cl.mlogit(table5.anes1, anes.data2$fips_state)

#Compare Table 5, model from Table 5 using our data, and replacing state with year fixed effects in model
stargazer(table5, table5.anes, table5.anes1, se=list(ses.t51$ses, ses.t5a$ses, ses.t5a1$ses))

#########################################################################################
#Add in incumbency Variables
incumbent <- read.csv("incumbent.csv")
incumbent$Area <- as.character(incumbent$Area)
states$state <- as.character(states$state)
incumbent$year <- incumbent$raceYear

#Add fips_state codes to incumbent
incumbent$fips_state <- c()
for(i in 1:nrow(incumbent)){
  incumbent$fips_state[i] <- states$fips_state[which(states$state == incumbent$Area[i])]
}

#Create binary indicator for incumbent for each party
incumbent$RepInc[incumbent$RepStatus == "Incumbent"] <- 1
incumbent$RepInc[incumbent$RepStatus == "Challenger"] <- 0
incumbent$DemInc[incumbent$DemStatus == "Incumbent"] <- 1
incumbent$DemInc[incumbent$DemStatus == "Challenger"] <- 0

#Reduce to needed columns
incumbent <- data.frame(incumbent$year, incumbent$fips_state, incumbent$RepInc, incumbent$DemInc)
colnames(incumbent) <- c("year", "fips_state", "rep_status", "dem_status")

#Merge incumbent with ANES
anes.inc <- merge(anes.data, incumbent, by = c("fips_state", "year"))
nrow(anes.inc)

anes.inc.ml <- mlogit.data(anes.inc, choice="partyvote", shape="wide")

#Multinomial logit with incumbency control and year fixed effects
table5.anes.inc2 <- mlogit(partyvote ~ 0 | uerate + totalspend_voter_inf + democrat + republican + black + hisp +
                             other + female + married + age + educ + income + unemployed + factor(year)
                           + rep_status + dem_status,
                           data=anes.inc.ml, reflevel="Abstain")
summary(table5.anes.inc2)
anes.inc2 <- na.omit(anes.inc)
ses.t5ai <- cl.mlogit(table5.anes.inc2, anes.inc2$fips_state)

#Compare to model without incumbency control
stargazer(table5.anes1, table5.anes.inc2, se=list(ses.t5a1$ses, ses.t5ai$ses))

########################################################################################################################
#calculating predicted probabilities across diff values of uerate - using authors' original model from paper 
#Table 5, multnomial logit with state FEs and clustered SEs at state level


#running clustered SE function with table 5 - calculating simulated betas with clustered vcov object
require(mlogit)

indiv.data1 <- indiv.data
indiv.data1$educ <- as.numeric(indiv.data1$educ)
indiv.data1$income <- as.numeric(indiv.data1$income)
head(indiv.data1)
indiv.data.ml <- mlogit.data(indiv.data1, choice="partyvote", shape="wide")
head(indiv.data.ml)

logit3 <- mlogit(partyvote ~ 0 | uerate + totalspend_voter_inf + democrat + republican + black + hisp +
                   other + female + married + age + income + educ + incomedk + unemployed + factor(fips_state), 
                 data=indiv.data.ml, reflevel = "Abstain")
summary(logit3)


#SE - getting clustered SE vcov matrix 

#Remove NA's from dataset for SE function
indiv.data1 <- na.omit(indiv.data1)
nrow(indiv.data1)


require(sandwich, quietly = TRUE)
require(lmtest, quietly = TRUE)

# setting cluster to correct variable

cluster <- indiv.data1$fips_state

M <- length(unique(cluster))
N <- length(cluster)
K <- length(coefficients(logit3))
dfc <- (M/(M-1))
uj  <- apply(estfun(logit3),2, function(x) tapply(x, cluster, sum));
vcovCL <- dfc*sandwich(logit3, meat.=crossprod(uj)/N)
coeftest(logit3, vcovCL)

cov.mat <- vcovCL
dim(cov.mat)


#generating betas 

require(MASS)
sim.betas <- mvrnorm(10000, as.vector(coef(logit3)), cov.mat)
dim(sim.betas)
head(sim.betas)

#splitting Dem and Rep betas for each covariate - want 16 in each vector, the last beta for each vector is the 
#coefficient for the state of interest (in our case, Michigan (fips code = 26))
dem.betas <- sim.betas[, c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 57)] #16 covs
rep.betas <- sim.betas[, c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 58)] #16 covs
dim(dem.betas)
dim(rep.betas)
#both 10000 x 16 - good, what we want


#making vector of covariates held at median - making voter republican 
#keeping fips_state at 1 bc we've set the covariate above to that of MI
head(sim.betas)
voter1 <- with(indiv.data1, c(uerate = median(uerate), totalspend_voter_inf = median(totalspend_voter_inf), democrat = 0, republican = 1, 
                              black = median(black), hisp = median(hisp), other = median(other), female = median(female), married = median(married),
                              age = median(age), income = median(income), educ = median(educ), incomedk = median(incomedk), 
                              uemployed = median(unemployed), fips_state = 1))


voter1 <- as.matrix(as.numeric(c(1, voter1)))
#binding a 1 to our voter1 vector for intercept
voter1
dim(voter1)
#16 x 1 - good

#equations for predicted probs 

p.abstain <- 1/(exp(t(voter1)%*%t(dem.betas)) + exp(t(voter1)%*%t(rep.betas)) + 1)
p.dem <- exp(t(voter1)%*%t(dem.betas))/(exp(t(voter1)%*%t(dem.betas)) + exp(t(voter1)%*%t(rep.betas)) + 1)
p.rep <- exp(t(voter1)%*%t(rep.betas))/(exp(t(voter1)%*%t(dem.betas)) + exp(t(voter1)%*%t(rep.betas)) + 1)

p.abstain
mean(p.abstain)
p.dem
mean(p.dem)
p.rep
mean(p.rep)

plot(density(p.rep))


#calculating predicted probabilities for Abstaining for Republican voter (MI)
ue.vals <- seq(0, .2, .01)
ue.vals
p.abstain.rep <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  voter1.new <- voter1
  voter1.new[2] <- ue.vals[i]
  p.abstain.rep[,i] <- 1/(exp(t(voter1.new)%*%t(dem.betas)) + exp(t(voter1.new)%*%t(rep.betas)) + 1)
}

head(p.abstain.rep)


#calculating predicted probabilities for Democrat vote

ue.vals <- seq(0, .2, .01)
ue.vals
p.dem.rep <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  voter1.new <- voter1
  voter1.new[2] <- ue.vals[i]
  p.dem.rep[,i] <- exp(t(voter1.new)%*%t(dem.betas))/(exp(t(voter1.new)%*%t(dem.betas)) + exp(t(voter1.new)%*%t(rep.betas)) + 1)
}

head(p.dem.rep)


#calculating predicted probabilities for voting for a Republican 

ue.vals <- seq(0, .2, .01)
ue.vals
p.rep.rep <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  voter1.new <- voter1
  voter1.new[2] <- ue.vals[i]
  p.rep.rep[,i] <- exp(t(voter1.new)%*%t(rep.betas))/(exp(t(voter1.new)%*%t(dem.betas)) + exp(t(voter1.new)%*%t(rep.betas)) + 1)
}

head(p.rep.rep)


#combined plot of pred probs for partyvote, across uerate, for Republican voter 


plot(ue.vals, apply(p.abstain.rep,2,mean), ylim=c(0,1), main = "Pred. Prob. of Party Vote Across UE Rates (R)", type = 'p', pch = 16, ylab = "Probability of Vote", xlab = "State-level Unemployment Rate")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.abstain.rep, 2, quantile, .025),
         y1 = apply(p.abstain.rep, 2, quantile, .975), lwd = 2)
par(new = TRUE)
plot(ue.vals, apply(p.dem.rep,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Blue")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.dem.rep, 2, quantile, .025),
         y1 = apply(p.dem.rep, 2, quantile, .975), lwd = 2, col = "Blue")
par(new = TRUE)
plot(ue.vals, apply(p.rep.rep,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Red")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.rep.rep, 2, quantile, .025),
         y1 = apply(p.rep.rep, 2, quantile, .975), lwd = 2, col = "Red")


#doing same thing - calculating predicted probs for Democratic voter (MI)

voter2 <- with(indiv.data1, c(uerate = median(uerate), totalspend_voter_inf = median(totalspend_voter_inf), democrat = 1, republican = 0, 
                              black = median(black), hisp = median(hisp), other = median(other), female = median(female), married = median(married),
                              age = median(age), income = median(income), educ = median(educ), incomedk = median(incomedk), 
                              uemployed = median(unemployed), fips_state = 1))


voter2 <- as.matrix(as.numeric(c(1, voter2)))

voter2
dim(voter2)

#calculating predicted probabilities for Abstaining (will do separate graphs for each level of party vote)
ue.vals <- seq(0, .2, .01)
ue.vals
p.abstain.dem <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  voter2.new <- voter2
  voter2.new[2] <- ue.vals[i]
  p.abstain.dem[,i] <- 1/(exp(t(voter2.new)%*%t(dem.betas)) + exp(t(voter2.new)%*%t(rep.betas)) + 1)
}

head(p.abstain.dem)

#calculating predicted probabilities for Democrat vote

ue.vals <- seq(0, .2, .01)
ue.vals
p.dem.dem <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  voter2.new <- voter2
  voter2.new[2] <- ue.vals[i]
  p.dem.dem[,i] <- exp(t(voter2.new)%*%t(dem.betas))/(exp(t(voter2.new)%*%t(dem.betas)) + exp(t(voter2.new)%*%t(rep.betas)) + 1)
}

head(p.dem.dem)

#calculating predicted probabilities for voting for a Republican 

ue.vals <- seq(0, .2, .01)
ue.vals
p.rep.dem <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  voter2.new <- voter2
  voter2.new[2] <- ue.vals[i]
  p.rep.dem[,i] <- exp(t(voter2.new)%*%t(rep.betas))/(exp(t(voter2.new)%*%t(dem.betas)) + exp(t(voter2.new)%*%t(rep.betas)) + 1)
}

head(p.rep.dem)


#combining plots for Dem voter

plot(ue.vals, apply(p.abstain.dem,2,mean), ylim=c(0,1), main = "Pred. Prob. of Party Vote Across UE Rate (D)", type = 'p', pch = 16, ylab = "Probability of Vote", xlab = "State-level Unemployment Rate")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.abstain.dem, 2, quantile, .025),
         y1 = apply(p.abstain.dem, 2, quantile, .975), lwd = 2)
par(new = TRUE)
plot(ue.vals, apply(p.dem.dem,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Blue")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.dem.dem, 2, quantile, .025),
         y1 = apply(p.dem.dem, 2, quantile, .975), lwd = 2, col = "Blue")
par(new = TRUE)
plot(ue.vals, apply(p.rep.dem,2,mean), ylim=c(0,1), type = 'p', pch = 16, col = "Red", ann = FALSE)
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.rep.dem, 2, quantile, .025),
         y1 = apply(p.rep.dem, 2, quantile, .975), lwd = 2, col = "Red")



####################################################################################################################
#running logit model without 1994 - totalspend_voter_inf is our variable
#removed state FEs and replaced with year FEs (1982, 1986, 1990, 1998)
#clustered SEs at fips_state level

anes.data.sh <- subset(anes.data, anes.data$year != "1994")
anes.data.sh.ml <- mlogit.data(anes.data.sh, choice="partyvote", shape="wide")

table5.anes.sh <- mlogit(partyvote ~ 0 | uerate + totalspend_voter_inf + democrat + republican + black + hisp +
                         other + female + married + age + educ + income + unemployed + factor(year),
                       data=anes.data.sh.ml, reflevel="Abstain")
summary(table5.anes.sh)
anes.data.sh <- na.omit(anes.data.sh)
require(sandwich, quietly = TRUE)
require(lmtest, quietly = TRUE)

# setting cluster to correct variable

cluster <- anes.data.sh$fips_state

M <- length(unique(cluster))
N <- length(cluster)
K <- length(coefficients(table5.anes.sh))
dfc <- (M/(M-1))
uj  <- apply(estfun(table5.anes.sh),2, function(x) tapply(x, cluster, sum));
vcovCL2 <- dfc*sandwich(table5.anes.sh, meat.=crossprod(uj)/N)
coeftest(table5.anes.sh, vcovCL2)

cov.mat2 <- vcovCL2
dim(cov.mat2)
#saved var-cov matrix to be used in sim betas 


head(anes.data.sh)
#vector of covariate values for reference voter - will this work with factored vars? what to do with year?
table(anes.data.sh$year)
anes.data.sh$year[anes.data.sh$year == "1982"] <- 1
anes.data.sh$year[anes.data.sh$year == "1986"] <- 2
anes.data.sh$year[anes.data.sh$year == "1990"] <- 3
anes.data.sh$year[anes.data.sh$year == "1998"] <- 4
median(anes.data.sh$year)
#median is 1986

#predicted probabiltiies for year = 1986 (2) - Republican voter
v1 <- with(anes.data.sh, c(uerate = median(uerate), totalspend_voter_inf = median(totalspend_voter_inf), democrat = 0, republican = 1, 
                         black = median(black), hisp = median(hisp), other = median(other), female = median(female), married = median(married),
                         age = median(age), educ = median(educ), income = median(income), uemployed = median(unemployed),
                         year = 1))


v1 <- as.matrix(as.numeric(c(1, v1)))

v1
dim(v1)



#simulating betas using coefs from logit and ses from clustered logit model

require(MASS)
sim.betas2 <- mvrnorm(10000, as.vector(coef(table5.anes.sh)), cov.mat2)
dim(sim.betas2)
head(sim.betas2)


#splitting Dem and Rep betas for simulation - 29 and 30 are coefs for 1986 -
#will have to change when looking at other years 

dem.betas2 <- sim.betas2[, c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)] #15 covs
rep.betas2 <- sim.betas2[, c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)] #15 covs
dim(dem.betas2)
dim(rep.betas2)

#equations for predicted probabilities
p.abstain2 <- 1/(exp(t(v1)%*%t(dem.betas2)) + exp(t(v1)%*%t(rep.betas2)) + 1)
p.dem2 <- exp(t(v1)%*%t(dem.betas2))/(exp(t(v1)%*%t(dem.betas2)) + exp(t(v1)%*%t(rep.betas2)) + 1)
p.rep2 <- exp(t(v1)%*%t(rep.betas2))/(exp(t(v1)%*%t(dem.betas2)) + exp(t(v1)%*%t(rep.betas2)) + 1)

p.abstain2
mean(p.abstain2)
p.dem2
mean(p.dem2)
p.rep2
mean(p.rep2)

#check distribution of probabiltiies for each outcome level
plot(density(p.rep2))

#########################################################################################################
#calculating predicted probabilties for each level of partyvote for Rep voter in 1986 (v1)

ue.vals <- seq(0, .2, .01)
ue.vals
p.abstain.rep2 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v1.new <- v1
  v1.new[2] <- ue.vals[i]
  p.abstain.rep2[,i] <- 1/(exp(t(v1.new)%*%t(dem.betas2)) + exp(t(v1.new)%*%t(rep.betas2)) + 1)
}

head(p.abstain.rep2)


#calculating predicted probabilities for Democrat vote

ue.vals <- seq(0, .2, .01)
ue.vals
p.dem.rep2 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v1.new <- v1
  v1.new[2] <- ue.vals[i]
  p.dem.rep2[,i] <- exp(t(v1.new)%*%t(dem.betas2))/(exp(t(v1.new)%*%t(dem.betas2)) + exp(t(v1.new)%*%t(rep.betas2)) + 1)
}

head(p.dem.rep2)


#calculating predicted probabilities for voting for a Republican 

ue.vals <- seq(0, .2, .01)
ue.vals
p.rep.rep2 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v1.new <- v1
  v1.new[2] <- ue.vals[i]
  p.rep.rep2[,i] <- exp(t(v1.new)%*%t(rep.betas2))/(exp(t(v1.new)%*%t(dem.betas2)) + exp(t(v1.new)%*%t(rep.betas2)) + 1)
}

head(p.rep.rep2)


#combined plot of pred probs for partyvote, across uerate, for Republican voter (year = 1986)


plot(ue.vals, apply(p.abstain.rep2,2,mean), ylim=c(0,1), main = "Pred. Prob. of Party Vote Across UE Rate 1986 (R)", type = 'p', pch = 16, ylab = "Probability of Vote", xlab = "State-level Unemployment Rate")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.abstain.rep2, 2, quantile, .025),
         y1 = apply(p.abstain.rep2, 2, quantile, .975), lwd = 2)
par(new = TRUE)
plot(ue.vals, apply(p.dem.rep2,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Blue")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.dem.rep2, 2, quantile, .025),
         y1 = apply(p.dem.rep2, 2, quantile, .975), lwd = 2, col = "Blue")
par(new = TRUE)
plot(ue.vals, apply(p.rep.rep2,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Red")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.rep.rep2, 2, quantile, .025),
         y1 = apply(p.rep.rep2, 2, quantile, .975), lwd = 2, col = "Red")


##########################################################################################################
#same thing for Democratic voter (year = 1986 = 2)

v2 <- with(anes.data.sh, c(uerate = median(uerate), totalspend_voter_inf = median(totalspend_voter_inf), democrat = 1, republican = 0, 
                         black = median(black), hisp = median(hisp), other = median(other), female = median(female), married = median(married),
                         age = median(age), educ = median(educ), income = median(income), uemployed = median(unemployed),
                         year = 1))


v2 <- as.matrix(as.numeric(c(1, v2)))

v2
dim(v2)

#calculating predicted probabilities for Abstaining 
ue.vals <- seq(0, .2, .01)
ue.vals
p.abstain.dem2 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v2.new <- v2
  v2.new[2] <- ue.vals[i]
  p.abstain.dem2[,i] <- 1/(exp(t(v2.new)%*%t(dem.betas2)) + exp(t(v2.new)%*%t(rep.betas2)) + 1)
}

head(p.abstain.dem2)


#calculating predicted probabilities for Democrat vote

ue.vals <- seq(0, .2, .01)
ue.vals
p.dem.dem2 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v2.new <- v2
  v2.new[2] <- ue.vals[i]
  p.dem.dem2[,i] <- exp(t(v2.new)%*%t(dem.betas2))/(exp(t(v2.new)%*%t(dem.betas2)) + exp(t(v2.new)%*%t(rep.betas2)) + 1)
}

head(p.dem.dem2)


#calculating predicted probabilities for voting for a Republican 

ue.vals <- seq(0, .2, .01)
ue.vals
p.rep.dem2 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v2.new <- v2
  v2.new[2] <- ue.vals[i]
  p.rep.dem2[,i] <- exp(t(v2.new)%*%t(rep.betas2))/(exp(t(v2.new)%*%t(dem.betas2)) + exp(t(v2.new)%*%t(rep.betas2)) + 1)
}

head(p.rep.dem2)


#combined plot of pred probs for partyvote, across uerate, for Democratic voter, year = 1986

plot(ue.vals, apply(p.abstain.dem2,2,mean), ylim=c(0,1), main = "Pred. Prob. of Party Vote Across UE Rate 1986 (D)", type = 'p', pch = 16, ylab = "Probability of Vote", xlab = "State-level Unemployment Rate")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.abstain.dem2, 2, quantile, .025),
         y1 = apply(p.abstain.dem2, 2, quantile, .975), lwd = 2)
par(new = TRUE)
plot(ue.vals, apply(p.dem.dem2,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Blue")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.dem.dem2, 2, quantile, .025),
         y1 = apply(p.dem.dem2, 2, quantile, .975), lwd = 2, col = "Blue")
par(new = TRUE)
plot(ue.vals, apply(p.rep.dem2,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Red")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.rep.dem2, 2, quantile, .025),
         y1 = apply(p.rep.dem2, 2, quantile, .975), lwd = 2, col = "Red")


##############################################################################################################################
#repeating steps for 1990 (year = 3) - Rep voter first 

#splitting Dem and Rep betas for simulation - 31 and 32 are coefs for 1990 

dem.betas3 <- sim.betas2[, c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 31)] #15 covs
rep.betas3 <- sim.betas2[, c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 32)] #15 covs
dim(dem.betas3)
dim(rep.betas3)



v3 <- with(anes.data.sh, c(uerate = median(uerate), totalspend_voter_inf = median(totalspend_voter_inf), democrat = 0, republican = 1, 
                         black = median(black), hisp = median(hisp), other = median(other), female = median(female), married = median(married),
                         age = median(age), educ = median(educ), income = median(income), uemployed = median(unemployed),
                         year = 1))

v3 <- as.matrix(as.numeric(c(1, v3)))

v3
dim(v3)

#equations for predicted probabilities
p.abstain3 <- 1/(exp(t(v3)%*%t(dem.betas3)) + exp(t(v3)%*%t(rep.betas3)) + 1)
p.dem3 <- exp(t(v3)%*%t(dem.betas3))/(exp(t(v3)%*%t(dem.betas3)) + exp(t(v3)%*%t(rep.betas3)) + 1)
p.rep3 <- exp(t(v3)%*%t(rep.betas3))/(exp(t(v3)%*%t(dem.betas3)) + exp(t(v3)%*%t(rep.betas3)) + 1)

p.abstain3
mean(p.abstain3)
p.dem3
mean(p.dem3)
p.rep3
mean(p.rep3)

#check distribution of probabiltiies for each outcome level
plot(density(p.abstain3))

#########################################################################################################
#calculating predicted probabilties for each level of partyvote for Rep voter in 1990 

ue.vals <- seq(0, .2, .01)
ue.vals
p.abstain.rep3 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v3.new <- v3
  v3.new[2] <- ue.vals[i]
  p.abstain.rep3[,i] <- 1/(exp(t(v3.new)%*%t(dem.betas3)) + exp(t(v3.new)%*%t(rep.betas3)) + 1)
}

head(p.abstain.rep3)


#calculating predicted probabilities for Democrat vote

ue.vals <- seq(0, .2, .01)
ue.vals
p.dem.rep3 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v3.new <- v3
  v3.new[2] <- ue.vals[i]
  p.dem.rep3[,i] <- exp(t(v3.new)%*%t(dem.betas3))/(exp(t(v3.new)%*%t(dem.betas3)) + exp(t(v3.new)%*%t(rep.betas3)) + 1)
}

head(p.dem.rep3)


#calculating predicted probabilities for voting for a Republican 

ue.vals <- seq(0, .2, .01)
ue.vals
p.rep.rep3 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v3.new <- v3
  v3.new[2] <- ue.vals[i]
  p.rep.rep3[,i] <- exp(t(v3.new)%*%t(rep.betas3))/(exp(t(v3.new)%*%t(dem.betas3)) + exp(t(v3.new)%*%t(rep.betas3)) + 1)
}

head(p.rep.rep3)


#combined plot of pred probs for partyvote, across uerate, for Republican voter (year = 1990)


plot(ue.vals, apply(p.abstain.rep3,2,mean), ylim=c(0,1), main = "Pred. Prob. of Party Vote Across UE Rate 1990 (R)", type = 'p', pch = 16, ylab = "Probability of Vote", xlab = "State-level Unemployment Rate")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.abstain.rep3, 2, quantile, .025),
         y1 = apply(p.abstain.rep3, 2, quantile, .975), lwd = 2)
par(new = TRUE)
plot(ue.vals, apply(p.dem.rep3,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Blue")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.dem.rep3, 2, quantile, .025),
         y1 = apply(p.dem.rep3, 2, quantile, .975), lwd = 2, col = "Blue")
par(new = TRUE)
plot(ue.vals, apply(p.rep.rep3,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Red")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.rep.rep3, 2, quantile, .025),
         y1 = apply(p.rep.rep3, 2, quantile, .975), lwd = 2, col = "Red")


######################################################################################################
#predicted probabilities for Democratic voter, year = 1990 (3)


v4 <- with(anes.data.sh, c(uerate = median(uerate), totalspend_voter_inf = median(totalspend_voter_inf), democrat = 1, republican = 0, 
                           black = median(black), hisp = median(hisp), other = median(other), female = median(female), married = median(married),
                           age = median(age), educ = median(educ), income = median(income), uemployed = median(unemployed),
                           year = 1))

v4 <- as.matrix(as.numeric(c(1, v4)))

v4
dim(v4)

########################################################################################################
#calculating predicted probabitliies 
ue.vals <- seq(0, .2, .01)
ue.vals
p.abstain.dem3 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v4.new <- v4
  v4.new[2] <- ue.vals[i]
  p.abstain.dem3[,i] <- 1/(exp(t(v4.new)%*%t(dem.betas3)) + exp(t(v4.new)%*%t(rep.betas3)) + 1)
}

head(p.abstain.dem3)


#calculating predicted probabilities for Democrat vote

ue.vals <- seq(0, .2, .01)
ue.vals
p.dem.dem3 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v4.new <- v4
  v4.new[2] <- ue.vals[i]
  p.dem.dem3[,i] <- exp(t(v4.new)%*%t(dem.betas3))/(exp(t(v4.new)%*%t(dem.betas3)) + exp(t(v4.new)%*%t(rep.betas3)) + 1)
}

head(p.dem.dem3)


#calculating predicted probabilities for voting for a Republican 

ue.vals <- seq(0, .2, .01)
ue.vals
p.rep.dem3 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v4.new <- v4
  v4.new[2] <- ue.vals[i]
  p.rep.dem3[,i] <- exp(t(v4.new)%*%t(rep.betas3))/(exp(t(v4.new)%*%t(dem.betas3)) + exp(t(v4.new)%*%t(rep.betas3)) + 1)
}

head(p.rep.dem3)


#combined plot of pred probs for partyvote, across uerate, for Democratic voter (year = 1990)


plot(ue.vals, apply(p.abstain.dem3,2,mean), ylim=c(0,1), main = "Pred. Prob. of Party Vote Across UE Rates 1990 (D)", type = 'p', pch = 16, ylab = "Probability of Vote", xlab = "State-level Unemployment Rate")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.abstain.dem3, 2, quantile, .025),
         y1 = apply(p.abstain.dem3, 2, quantile, .975), lwd = 2)
par(new = TRUE)
plot(ue.vals, apply(p.dem.dem3,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Blue")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.dem.dem3, 2, quantile, .025),
         y1 = apply(p.dem.dem3, 2, quantile, .975), lwd = 2, col = "Blue")
par(new = TRUE)
plot(ue.vals, apply(p.rep.dem3,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Red")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.rep.dem3, 2, quantile, .025),
         y1 = apply(p.rep.dem3, 2, quantile, .975), lwd = 2, col = "Red")


########################################################################################################
#Same thing for year = 1998 - calculating predicted probabilities partyvote outcomes


#splitting Dem and Rep betas for simulation - 31 and 32 are coefs for 1998

dem.betas4 <- sim.betas2[, c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 33)] #15 covs
rep.betas4 <- sim.betas2[, c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 34)] #15 covs
dim(dem.betas4)
dim(rep.betas4)

#starting with Republican voter (year = 1998 = 4)

v5 <- with(anes.data.sh, c(uerate = median(uerate), totalspend_voter_inf = median(totalspend_voter_inf), democrat = 0, republican = 1, 
                           black = median(black), hisp = median(hisp), other = median(other), female = median(female), married = median(married),
                           age = median(age), educ = median(educ), income = median(income), uemployed = median(unemployed),
                           year = 1))

v5 <- as.matrix(as.numeric(c(1, v5)))

v5
dim(v5)

#equations for predicted probabilities
p.abstain4 <- 1/(exp(t(v5)%*%t(dem.betas4)) + exp(t(v5)%*%t(rep.betas4)) + 1)
p.dem4 <- exp(t(v5)%*%t(dem.betas4))/(exp(t(v5)%*%t(dem.betas4)) + exp(t(v5)%*%t(rep.betas4)) + 1)
p.rep4 <- exp(t(v5)%*%t(rep.betas4))/(exp(t(v5)%*%t(dem.betas4)) + exp(t(v5)%*%t(rep.betas4)) + 1)

p.abstain4
mean(p.abstain4)
p.dem4
mean(p.dem4)
p.rep4
mean(p.rep4)

#check distribution of probabiltiies for each outcome level
plot(density(p.rep4))

#########################################################################################################
#calculating predicted probabilties for each level of partyvote for Rep voter in 1990 

ue.vals <- seq(0, .2, .01)
ue.vals
p.abstain.rep4 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v5.new <- v5
  v5.new[2] <- ue.vals[i]
  p.abstain.rep4[,i] <- 1/(exp(t(v5.new)%*%t(dem.betas4)) + exp(t(v5.new)%*%t(rep.betas4)) + 1)
}

head(p.abstain.rep4)


#calculating predicted probabilities for Democrat vote

ue.vals <- seq(0, .2, .01)
ue.vals
p.dem.rep4 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v5.new <- v5
  v5.new[2] <- ue.vals[i]
  p.dem.rep4[,i] <- exp(t(v5.new)%*%t(dem.betas4))/(exp(t(v5.new)%*%t(dem.betas4)) + exp(t(v5.new)%*%t(rep.betas4)) + 1)
}

head(p.dem.rep4)


#calculating predicted probabilities for voting for a Republican 

ue.vals <- seq(0, .2, .01)
ue.vals
p.rep.rep4 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v5.new <- v5
  v5.new[2] <- ue.vals[i]
  p.rep.rep4[,i] <- exp(t(v5.new)%*%t(rep.betas4))/(exp(t(v5.new)%*%t(dem.betas4)) + exp(t(v5.new)%*%t(rep.betas4)) + 1)
}

head(p.rep.rep4)


#combined plot of pred probs for partyvote, across uerate, for Republican voter (year = 1990)


plot(ue.vals, apply(p.abstain.rep4,2,mean), ylim=c(0,1), main = "Pred. Prob. of Party Vote Across UE Rates 1998 (R)", type = 'p', pch = 16, ylab = "Probability of Vote", xlab = "State-level Unemployment Rate")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.abstain.rep4, 2, quantile, .025),
         y1 = apply(p.abstain.rep4, 2, quantile, .975), lwd = 2)
par(new = TRUE)
plot(ue.vals, apply(p.dem.rep4,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Blue")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.dem.rep4, 2, quantile, .025),
         y1 = apply(p.dem.rep4, 2, quantile, .975), lwd = 2, col = "Blue")
par(new = TRUE)
plot(ue.vals, apply(p.rep.rep4,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Red")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.rep.rep4, 2, quantile, .025),
         y1 = apply(p.rep.rep4, 2, quantile, .975), lwd = 2, col = "Red")


######################################################################################################
#predicted probabilities for Democratic voter, year = 1998 (4)


v6 <- with(anes.data.sh, c(uerate = median(uerate), totalspend_voter_inf = median(totalspend_voter_inf), democrat = 1, republican = 0, 
                           black = median(black), hisp = median(hisp), other = median(other), female = median(female), married = median(married),
                           age = median(age), educ = median(educ), income = median(income), uemployed = median(unemployed),
                           year = 1))

v6 <- as.matrix(as.numeric(c(1, v6)))

v6
dim(v6)

########################################################################################################
#calculating predicted probabitliies 
ue.vals <- seq(0, .2, .01)
ue.vals
p.abstain.dem4 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v6.new <- v6
  v6.new[2] <- ue.vals[i]
  p.abstain.dem4[,i] <- 1/(exp(t(v6.new)%*%t(dem.betas4)) + exp(t(v6.new)%*%t(rep.betas4)) + 1)
}

head(p.abstain.dem4)


#calculating predicted probabilities for Democrat vote

ue.vals <- seq(0, .2, .01)
ue.vals
p.dem.dem4 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v6.new <- v6
  v6.new[2] <- ue.vals[i]
  p.dem.dem4[,i] <- exp(t(v6.new)%*%t(dem.betas4))/(exp(t(v6.new)%*%t(dem.betas4)) + exp(t(v6.new)%*%t(rep.betas4)) + 1)
}

head(p.dem.dem4)


#calculating predicted probabilities for voting for a Republican 

ue.vals <- seq(0, .2, .01)
ue.vals
p.rep.dem4 <- matrix(data = NA, ncol= length(ue.vals), nrow = 10000)

for(i in 1:length(ue.vals)){
  v6.new <- v6
  v6.new[2] <- ue.vals[i]
  p.rep.dem4[,i] <- exp(t(v6.new)%*%t(rep.betas4))/(exp(t(v6.new)%*%t(dem.betas4)) + exp(t(v6.new)%*%t(rep.betas4)) + 1)
}

head(p.rep.dem4)


#combined plot of pred probs for partyvote, across uerate, for Democratic voter (year = 1990)


plot(ue.vals, apply(p.abstain.dem4,2,mean), ylim=c(0,1), main = "Pred. Prob. of Party Vote Across UE Rates 1998 (D)", type = 'p', pch = 16, ylab = "Probability of Vote", xlab = "State-level Unemployment Rate")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.abstain.dem4, 2, quantile, .025),
         y1 = apply(p.abstain.dem4, 2, quantile, .975), lwd = 2)
par(new = TRUE)
plot(ue.vals, apply(p.dem.dem4,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Blue")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.dem.dem4, 2, quantile, .025),
         y1 = apply(p.dem.dem4, 2, quantile, .975), lwd = 2, col = "Blue")
par(new = TRUE)
plot(ue.vals, apply(p.rep.dem4,2,mean), ylim=c(0,1), type = 'p', pch = 16, ann = FALSE, col = "Red")
segments(x0 = ue.vals, x1 = ue.vals,
         y0 = apply(p.rep.dem4, 2, quantile, .025),
         y1 = apply(p.rep.dem4, 2, quantile, .975), lwd = 2, col = "Red")


###############################################################################################################################
#running anes model with year FEs and interaction between UE rate and pid (new variable)


anes.data.sh2 <- anes.data.sh
anes.data.sh2$pid <- c()
anes.data.sh2$pid[anes.data.sh2$republican == 0 & anes.data.sh2$democrat == 0] <- "Ind"
anes.data.sh2$pid[anes.data.sh2$democrat == 1] <- "Dem"
anes.data.sh2$pid[anes.data.sh2$republican == 1] <- "Rep"
anes.data.sh2$educ <- as.numeric(anes.data.sh2$educ)
anes.data.sh2$income <- as.numeric(anes.data.sh2$income)
anes.data.sh2.ml <- mlogit.data(anes.data.sh2, choice="partyvote", shape="wide")
head(anes.data.sh2)
anes.data.sh2$pid <- as.factor(anes.data.sh2$pid)

anes.data.sh2$pid = factor(anes.data.sh2$pid, c("Ind","Dem","Rep"))
levels(anes.data.sh2$pid)
anes.data.sh2$pid <- relevel(anes.data.sh2$pid, ref = "Ind")

table5.anes.sh2 <- mlogit(partyvote ~ 0 | uerate + totalspend_voter_inf + pid + pid:uerate + black + hisp +
                           other + female + married + age + educ + income + unemployed + factor(year),
                         data=anes.data.sh2.ml, reflevel="Abstain")
summary(table5.anes.sh2)

anes.data.sh2 <- na.omit(anes.data.sh2)
ses.t5sh2 <- cl.mlogit(table5.anes.sh2, anes.data.sh2$fips_state)
stargazer(table5.anes.sh2, se=list(ses.t5sh2$ses))


