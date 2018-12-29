## ----Cleanup, include=FALSE----------------------------------------------
library(ggplot2) ## for plotting
library(psych) ## for Cronbach
library(reshape)
library(mclogit)
library(car)
library(ascii)
library(scales) ## for squish!
library(R2WinBUGS)
library(rjags)
library(snow)
library(dclone)

cl <- makeCluster(2, type = "SOCK")
parLoadModule(cl, "glm")



dat <- read.csv("data/data_used_in_modelling_20140429.csv",header=TRUE)
dat <- subset(dat,thepost %in% c("LJA","LAO"))
dat  <- ddply(dat,.(occasion),transform,nChoices = length(occasion))
dat$experience[is.na(dat$experience)] <- mean(dat$experience,na.rm=TRUE)
dat$sqrtPastPartyMatch <- sqrt(abs(dat$PastPartyMatch)) * sign(dat$PastPartyMatch)

for (i in c("SpecialtyChan","SpecialtyPDA")) {
	tmp <- dat[,i]
	tmp[is.na(tmp)] <- 0
	dat[,i] <- tmp
}

## ----Setup, include=FALSE, results="hide", warning=FALSE-----------------
## This chunk is for setting nice options for the output. Notice how
## we create both png and pdf files by default, so that we can easily process
## to both HTML and LaTeX/PDF files later.
opts_chunk$set(fig.path='figures/paper-', cache.path='cache/report-', dev=c("png","pdf"), fig.width=14, fig.height=7, fig.show='hold', fig.lp="fig:", cache=FALSE, par=TRUE, echo=FALSE, results="hide", message=FALSE, warning=FALSE)
knit_hooks$set(par=function(before, options, envir){
if (before && options$fig.show!='none') par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
}, crop=hook_pdfcrop) 

## ----skippers, include=FALSE---------------------------------------------
high.court.date<-as.numeric(as.character(dat$HCtDate))-1000
high.court.date[high.court.date<0]<-NA

min.date<-as.matrix(dat[,c("JUG1BEG","JUG2BEG","JUG3BEG","JUG4BEG","JUG5BEG","JUG6BEG","JUG7BEG","JUG8BEG","JUG9BEG","JUG10BEG")],na.rm=T)
min.date[min.date==0]<-NA
min.date[min.date<0]<-NA ## missing codes
min.date<-pmin(min.date[,"JUG1BEG"],min.date[,"JUG2BEG"],min.date[,"JUG3BEG"],min.date[,"JUG4BEG"],min.date[,"JUG5BEG"],min.date[,"JUG6BEG"],min.date[,"JUG7BEG"],min.date[,"JUG8BEG"],min.date[,"JUG9BEG"],min.date[,"JUG10BEG"],high.court.date,na.rm=TRUE)

ca.skippers<-which((min.date+1000)==dat$CAorDivD)
lao.skippers<-which((min.date+1000)==dat$LAODate)

ca.skippers<-unique(dat[ca.skippers,c("ThomasID","JNAME","HCtDate","CAorDivD","LAODate")])
lao.skippers<-unique(dat[lao.skippers,c("ThomasID","JNAME","HCtDate","CAorDivD","LAODate")])

## ----apptsbytime, fig.lp="fig:", fig.cap="Appointments by time and party",fig.width=7,fig.height=3.5,dpi=300----
appt.plot <- ggplot(subset(dat,chosen==1),aes(x=as.Date(thedate),y=LC.Party,color=LC.Party,shape=LC.Party)) + 
	scale_x_date("Date") + scale_y_discrete("Party") + 
	geom_point(position=position_jitter(width=0,height=0.1)) + theme_classic() + 
	scale_colour_manual(values=c("blue4","brown4","darkgoldenrod2")) + 
	theme(legend.position="none")
print(appt.plot)

## ----cronbach,include=FALSE----------------------------------------------
judge.df <- unique(dat[,c("ThomasID","fstat1","fstat2","fstat4","fstat5")])
judge.mat <- sapply(judge.df[,-1],as.numeric)
judge.mat <- judge.mat[!apply(judge.mat,1,function(x)all(is.na(x))),]
my.cronbach <- psych:::alpha(judge.mat,na.rm=TRUE)


## ----summarydf,results="asis"--------------------------------------------

dat$experience <- dat$experience / 365.25
dat$experience <- log(dat$experience)
dat$clubbishness <- log1p(dat$CLUBTOTL)
summary.df<-dat[,c("clarendon","FamilyStatus","clubbishness",
	"education","lawdegree",
	"experience","fftjudpos.superior",
	"SpecialtyPDA","SpecialtyChan",
	"same.party.b","PastPartyMatch")]

plot.df <- melt(summary.df)
plot.df$variable<-car:::recode(plot.df$variable,
	"'education'='Education';
	'clarendon'='Clarendon';
	'clubbishness'='Clubbishness';
	'FamilyStatus'='Family status';
	'lawdegree'='Law degree';
	'experience'='Experience (years)';
	'fftjudpos.superior'='High-flier';
	'SpecialtyPDA'='Specialty PDA';
	'SpecialtyChan'='Specalty Chancery';
	'same.party.b' = 'Same party';
	'PastPartyMatch' = 'Past party match'")

plot.df <- ddply(plot.df,.(variable),function(df) {
	data.frame(N = sum(!is.na(df$value)),
		Mean = mean(df$value,na.rm=T),
		SD = sd(df$value,na.rm=T),
		Min = min(df$value,na.rm=T),
		Max = max(df$value,na.rm=T))
})
names(plot.df)[1] <- "Variable"


## ----sparklines,fig.lp="fig:", fig.cap="Sparklines and summary statistics",fig.width=7,fig.height=7,dpi=300----
summary.df<-dat[,c("thedate","clarendon","FamilyStatus","clubbishness",
	"education","lawdegree","FirstAge",
	"experience","fftjudpos.superior",
	"SpecialtyPDA","SpecialtyChan",
	"same.party.b","PastPartyMatch")]

plot.df <- aggregate(summary.df[,-1],list(Date=summary.df$thedate),mean,na.rm=T)
plot.df$Date <- as.Date(plot.df$Date)

bandline<-function(df, low.col, high.col, axis=FALSE,index.date = TRUE){

	my.xrange <- range(df[,1],na.rm=TRUE)
    my.xlim <- my.xrange
	my.xlim[2] <- my.xlim[2] + 0.25 * diff(my.xlim)

    par(mfcol=c(ncol(df)-1, 1),oma=c(2,10,2,0),xpd=NA)

    for(i in 2:ncol(df)){
		xbar <- mean(df[,i],na.rm=T)
		my.sd <- sd(df[,i],na.rm=T)
		xhi <- xbar + my.sd
		xlo <- xbar - my.sd
		
		my.ylim <- range(df[,i],na.rm=TRUE)
        ifelse(i==1, par(mar=c(0,3,3,3)), 
                    ifelse(i==ncol(df), par(mar=c(1,3,0,3)), 
                                          par(mar=c(0,3,0,3))))
        plot(NA, axes=F, bty="n", xlim=my.xlim, ylim=my.ylim, xaxs="i",ylab="",xlab="")
        rect(my.xrange[1],xlo, my.xrange[2], xhi, col="grey80", border=NA)
        abline(h=mean(dat[,i],na.rm=T),col="white", lwd=2)
        lines(df[,1], df[,i])

        points(df[which.max(df[,i]),1], max(df[,i],na.rm=T), bg=high.col, pch=21,cex=1.5)
        points(df[which.min(df[,i]),1], min(df[,i],na.rm=T), bg=low.col, pch=21,cex=1.5)
		axis(2,labels=FALSE)
        if(axis==TRUE){
			if(i==ncol(df)){
				if (index.date==TRUE) {
					# axis(1)
					axis(1,
						at=pretty(df[,1]),
						labels = format(as.Date(pretty(df[,1]),origin=as.Date("1970-01-01")),"%Y"))
				}
			}
 		}
        mtext(names(df)[i], side=2, line=1, las = 1)
		## Add summary stats
		## mean, sd
		text(my.xrange[2]+0.025 * diff(my.xrange),xbar,
			adj = c(0,0),
			labels = substitute( bar(x) == res,list(res=round(xbar,2))))
		text(my.xrange[2]+0.025 * diff(my.xrange),xbar,
			adj = c(0,1),
			labels = substitute( sigma == res,list(res=round(my.sd,2))))
		## min, max
		text(my.xrange[2]+0.15 * diff(my.xrange),xbar,
			adj = c(0,0),
			labels = substitute( symbol("\267") == res,list(res=round(xlo,2))))
		text(my.xrange[2]+0.15 * diff(my.xrange),xbar,
			adj = c(0,1),
			labels = substitute( symbol("\260") == res,list(res=round(xhi,2))))

    }
}
names(plot.df) <- c("Date","Clarendon","Family status","Clubbishness","Education","Law degree","Age at first F/T pos.","Experience",
	"High-flier","PDA specialty","Chancery specialty","Same party","Past party match")
bandline(plot.df, "black", "white", axis=TRUE)


## ----modelling,echo=FALSE,include=FALSE,results='hide'-------------------
get.pcp<-function(mod) {
	preds<-data.frame(y=mod$y,yhat=mod$fitted.values,group=mod$s)
	preds<-ddply(preds,.(group),function(df) {
		df$thepred<-0
		df$thepred[which.max(df$yhat)]<-1
		df
	})
	preds<-preds[preds$y==1,]
	PCP <- sum(preds$y == preds$thepred)/length(preds$y)
	PCP <- round(PCP * 100,2)

	PCP
}

dat$thepost<-factor(dat$thepost,
	levels=c("LJA","LAO"),
	ordered=FALSE)

dat$hc4lao<-as.numeric(dat$hcjudge) * as.numeric(dat$thepost=="LAO")


### Create a simple model for LJA appointments only
### We then create the interaction more easily using update(...)
mod.simple <-mclogit(cbind(chosen,occasion)~
	clarendon+clubbishness + FamilyStatus + 
	education + lawdegree + 
	experience + FirstAge +	fftjudpos.superior+ 
	SpecialtyPDA+SpecialtyChan+
	same.party.b + PastPartyMatch,
	data=subset(dat,thepost=="LJA"))

mod.inter <-update(mod.simple,.~(.)*thepost+hc4lao,
	data=dat)

the.pcp <- get.pcp(mod.inter)

## ----cljags,eval=TRUE,echo=FALSE,include=FALSE,results='hide',cache=TRUE----


#my.iter <- 1000
my.iter <- 2e4
my.thin <- my.iter/1000
model <- function() {
 	for (the.occasion in 1:nOccasions) {
		for (the.judge in start[the.occasion]:stop[the.occasion] ) {
			log(phi[the.judge]) <- inprod(X[the.judge,],beta)  
			p[the.judge] <- phi[the.judge] / sum(phi[start[the.occasion]:stop[the.occasion]])
		}
		y[start[the.occasion]:stop[the.occasion]] ~ dmulti(p[start[the.occasion]:stop[the.occasion]], 1)
	}
 
	beta[1:nVars]  ~ dmnorm(b0[1:nVars],B0[1:nVars,1:nVars])
}

write.model(model,"model.bug")

dat <- dat[order(dat$occasion),]
dat$the.row <- 1:nrow(dat)

### Set up the model matrix
mf <- model.matrix(formula(mod.inter),data=dat)
### Drop intercept
mf <- mf[,-1]
### Remove "thepostLAO"
mf <- mf[,-which(colnames(mf)=="thepostLAO")]
### Deal with FirstAge 
mf[,"FirstAge"] <- as.vector(scale(mf[,"FirstAge"]))
forJags <- list(nOccasions = length(unique(dat$occasion)),
	start = aggregate(dat$the.row,list(occasion=dat$occasion),min)[,2],
	stop = aggregate(dat$the.row,list(occasion=dat$occasion),max)[,2],
	X = mf,
	y = dat$chosen,
	nVars = ncol(mf),
	b0 = rep(0,ncol(mf)),
	B0 = diag(.001,ncol(mf)))



#jags <- jags.model('model.bug',
#                   data = forJags,
#                   n.chains = 3,
#					inits = list(beta=coef(mod.inter)*runif(1,.5,1.5)),
#                   n.adapt = 500)
#
#system.time(update(jags, my.iter))
#
#system.time(out <- coda.samples(jags,
#             c('beta', 'p'),
#             my.iter,thin=my.thin))
#
initfunc <- function() {
list(beta=coef(mod.inter)*runif(1,.5,1.5))
}

system.time(jags.out <- jags.parfit(cl,
	data = forJags,
	model = 'model.bug',
	params = c('beta', 'p'),
	n.adapt=500,
	inits = initfunc,
	#updated.model=TRUE,
	n.update=my.iter,
	n.iter=my.iter,thin=my.thin,n.chains=2)) 

coef.holder <- summary(jags.out[,1:25])
	
### Check the match
plot(coef.holder$statistics[,1],coef(mod.inter),type="n")
text(coef.holder$statistics[,1],coef(mod.inter),names(coef(mod.inter)))

my.coef.gelman <- gelman.diag(jags.out[,1:25])
my.coef.geweke <- geweke.diag(jags.out[,1:25])

## ----hausmantest,echo=FALSE,include=FALSE,results='hide',cache=TRUE------

hausman.pvals <- vector("list",20)
for (i in 1:20) {
	dat2 <- ddply(dat,.(occasion),function(df) {
		kill.list <- sample(x= which(df$chosen==0),
			size = round(nrow(df)*.1))
		df <- df[-kill.list,]
		return(df)
	})

	## Taken from Achim Zeileis' post at 
	## https://stat.ethz.ch/pipermail/r-help/2011-January/265671.html

	mod.restricted <- update(mod.inter,data = dat2)
	cf_diff <- coef(mod.inter) - coef(mod.restricted)
   vc_diff <- vcov(mod.inter) - vcov(mod.restricted)
   x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
   hausman.pvals[[i]] <- pchisq(x2_diff, df = 2, lower.tail = FALSE)

}

hausman.pvals <- unlist(hausman.pvals)	

## ----modellingout,echo=FALSE,results='asis'------------------------------
### Extract the relevant items
beta <- coef(mod.inter)
ses <- summary(mod.inter)$coef[,2]
stars <- symnum(summary(mod.inter)$coef[,"Pr(>|z|)"], corr = FALSE,
                    cutpoints = c(0,  .001,.01,.05, .1, 1),
                    symbols = c("***","**","*","."," "))
stars <- as.character(stars)

### Build a data frame manually
out <- data.frame(Variable = names(beta)[1:13],
	Estimate = c(beta[1:12],NA),
	StdErr = c(ses[1:12],NA),
	Sig = c(stars[1:12],NA),
	Estimate = c(beta[c(14:25,13)]),
	StdErr = c(ses[c(14:25,13)]),
	Sig = c(stars[c(14:25,13)]))

### Now rbind on the summary statistics
summary.stats.names <- c("Log-likelihood","% correctly predicted","N (appointments)","N (appointment-judge dyads)","Avg. p-value for rejecting IIA assumption")
summary.stats <- c(as.numeric(logLik(mod.inter)),
	the.pcp,
	sum(mod.inter$y),
	length(mod.inter$y),
	mean(hausman.pvals))

extras <- data.frame(Variable = summary.stats.names,
	Estimate = summary.stats)
out <- merge(out,extras,
	all=TRUE,sort=FALSE)

### Make it pretty
var.recode.string <- c("'SpecialtyPDA'='Family specialty (-)';
	'clarendon'='Clarendon private school (+)';
	'clubbishness'='Clubbishness (+)';
	'FamilyStatus'='Family status (+)';
	'education'='Education (+)';
	'lawdegree'='Law degree (~)';
	'experience'='Experience (+)';
	'FirstAge'='Age at first f/t jud. appt. (-)';
	'fftjudpos.superior'= 'High-flier (+)';
	'SpecialtyChan'='Chancery specialty' ;	
	'same.party.b'='Same Party as Lord Chancellor (+)';
	'PastPartyMatch'='Past Party Match (+)';
	'hc4lao'='High Court judge, LAO vacancy (-)'")

out$Variable<-car:::recode(out$Variable,var.recode.string)
names(out) <- c("Variable (expectation)",rep(c("Estimate","SE","sig."),2))
print(ascii(out,
	include.rownames=FALSE,
	caption = "Conditional logit models",
	tgroup = c("","Main effect","LAO vacancy"),
	n.tgroup = c(1,3,3)),
	type="pandoc")

## ----cljagsout,echo=FALSE,include=TRUE,results='asis',cache=FALSE--------
### 
### ORs for everything
### But first, scale the coefficient for Age
jags.out[[1]][,7] <- jags.out[[1]][,7] / sd(dat$FirstAge)
### And the interaction
jags.out[[1]][,20] <- jags.out[[1]][,20] / sd(dat$FirstAge)
main <- jags.out[[1]][,1:12]
interax <- jags.out[[1]][,1:12] + jags.out[[1]][,14:25]
#summary(main)
#summary(interax)

out.df <- data.frame(`One unit change in...` = names(coef(mod.inter)[1:12]),
	OR = round(exp(apply(main,2,mean)),2),
	CI = paste0("[",round(exp(apply(main,2,quantile,prob=0.025)),2),",",
		round(exp(apply(main,2,quantile,prob=0.975)),2),"]"),
	OR = round(exp(apply(interax,2,mean)),2),
	CI = paste0("[",round(exp(apply(interax,2,quantile,prob=0.025)),2),",",
		round(exp(apply(interax,2,quantile,prob=0.975)),2),"]"),
	check.names=FALSE
)

var.recode.string <- gsub("(+","(>1",var.recode.string,fixed=TRUE)
var.recode.string <- gsub("(-","(<1",var.recode.string,fixed=TRUE	)

out.df$`One unit change in...` <- car:::recode(out.df$`One unit change in...`,
	var.recode.string)

	print(ascii(out.df,
		include.rownames=FALSE,
		caption = "Odds-ratios",
		tgroup = c("","Main effect","LAO vacancy"),
		n.tgroup = c(1,2,2)),
		type="pandoc")


## ----robustness,eval=TRUE,echo=FALSE,include=FALSE,results='hide'--------

dat$thedate <- as.Date(as.character(dat$thedate))
dat$ApptDate <- as.Date(as.character(dat$ApptDate))

mod.post1924 <- update(mod.inter,
	data=subset(dat,thedate>as.Date("1924-01-01")))
mod.cons <- update(mod.simple,.~(.)*thepost+(.)*I(LC.Party=="Conservative") + hc4lao,
	data = dat)

dat$grace <- as.numeric(as.numeric(dat$thedate-dat$ApptDate)>365)
mod.grace<-update(mod.inter,
	data=subset(dat,grace==TRUE))

smry.1<-summary(mod.inter)$coef
smry.2<-summary(mod.post1924)$coef
smry.3<-summary(mod.cons)$coef
smry.4<-summary(mod.grace)$coef

smry.1<-data.frame(Model="Baseline",Variable=rownames(smry.1),
	Estimate=smry.1[,"Estimate"],
	Hi=smry.1[,"Estimate"] + 1.96 * smry.1[,"Std. Error"],
	Lo=smry.1[,"Estimate"] - 1.96 * smry.1[,"Std. Error"])

smry.2<-data.frame(Model="Post-1924",Variable=rownames(smry.2),
	Estimate=smry.2[,"Estimate"],
	Hi=smry.2[,"Estimate"] + 1.96 * smry.2[,"Std. Error"],
	Lo=smry.2[,"Estimate"] - 1.96 * smry.2[,"Std. Error"])

smry.3<-data.frame(Model="Conservative",Variable=rownames(smry.3),
	Estimate=smry.3[,"Estimate"],
	Hi=smry.3[,"Estimate"] + 1.96 * smry.3[,"Std. Error"],
	Lo=smry.3[,"Estimate"] - 1.96 * smry.3[,"Std. Error"])

smry.4<-data.frame(Model="Grace",Variable=rownames(smry.4),
	Estimate=smry.4[,"Estimate"],
	Hi=smry.4[,"Estimate"] + 1.96 * smry.4[,"Std. Error"],
	Lo=smry.4[,"Estimate"] - 1.96 * smry.4[,"Std. Error"])

plot.df<-rbind(smry.1,smry.2,smry.3,smry.4)
plot.df$Condition<-"LJA"
plot.df$Condition[grepl(":thepostLAO",plot.df$Variable)]<-"LAO"
plot.df$Condition[grepl("hc4lao",plot.df$Variable)]<-"LAO"
plot.df$Condition[grepl("Conservative",plot.df$Variable)]<-"Conservative"
plot.df$Variable<-gsub(":.*","",plot.df$Variable)
plot.df<-plot.df[plot.df$Condition!="Conservative",]
var.recode.string <- c("'SpecialtyPDA'='Family specialty';
	'clarendon'='Clarendon private school';
	'clubbishness'='Clubbishness';
	'FamilyStatus'='Family status';
	'education'='Education';
	'lawdegree'='Law degree';
	'experience'='Experience';
	'FirstAge'='Age at first f/t jud. appt.';
	'fftjudpos.superior'= 'High-flier';
	'SpecialtyChan'='Chancery specialty';	
	'same.party.b'='Same Party as Lord Chancellor';
	'PastPartyMatch'='Past Party Match';
	'hc4lao'='High Court judge, LAO vacancy'")

plot.df$Variable<-car:::recode(plot.df$Variable,var.recode.string)
plot.df$Variable<-factor(plot.df$Variable,
	levels=c("Clarendon private school",
		"Clubbishness",
		"Family status",
		"Education",
		"Law degree",
		"Experience",
		"Age at first f/t jud. appt.",
		"High-flier",
		"Chancery specialty",
		"Family specialty",
		"Same Party as Lord Chancellor",
		"Past Party Match",
		"High Court judge, LAO vacancy"),ordered=TRUE)

plot.df$Condition <- factor(plot.df$Condition,
	levels = c("LJA","LAO"),
	ordered = TRUE)
coef.plot<- ggplot(plot.df,aes(x=Variable,y=Estimate,ymin=Lo,ymax=Hi,shape=Model,color=Model)) + 
	geom_pointrange(width=.2,position=position_dodge(width=0.9)) + 
	facet_grid(~Condition) + theme_bw() + 
	geom_hline(yintercept=0,alpha=0.5) + # coord_flip() + 
	scale_y_continuous(limits=c(-3,3),oob=squish) + 
	scale_x_discrete("") +
	theme(legend.position="bottom",
		legend.direction = "horizontal",
#		legend.text = element_text(angle=90),
		strip.text.x=element_text(angle=90),
		axis.text.x=element_text(angle=90),
		axis.text.y=element_text(angle=90)) 

just.fails<-round(summary(mod.post1924)$coef["same.party.b",4],2)


## ----robustnessout, fig.lp="fig:", fig.cap="Robustness checks",fig.width=7,fig.height=10,dpi=300----
print(coef.plot)

## ----cormat,echo=FALSE,results= 'asis'-----------------------------------
cormat <- as.matrix(cor(summary.df[,-1],use="pairwise"))
var.recode.string <- c("'SpecialtyPDA'='Family specialty';
	'clarendon'='Clarendon private school';
	'clubbishness'='Clubbishness';
	'FamilyStatus'='Family status';
	'education'='Education';
	'lawdegree'='Law degree';
	'experience'='Experience';
	'FirstAge'='Age at first f/t jud. appt.';
	'fftjudpos.superior'= 'High-flier';
	'SpecialtyChan'='Chancery specialty';	
	'same.party.b'='Same Party as Lord Chancellor';
	'PastPartyMatch'='Past Party Match';
	'hc4lao'='High Court judge, LAO vacancy'")

colnames(cormat)  <-car:::recode(colnames(cormat),var.recode.string)
rownames(cormat)  <-car:::recode(rownames(cormat),var.recode.string)

print(ascii(cormat[,1:7],
	include.rownames=TRUE,
	include.colnames = TRUE,
	caption = "*Table A1:* Correlation matrix of independent variables"),type="pandoc")

print(ascii(cormat[,8:ncol(cormat)],
	include.rownames=TRUE,
	include.colnames = TRUE,
	caption = "*Table A1 (cont.):* Correlation matrix of independent variables"),type="pandoc")



