library(ggplot2)
library(survival)
library(rms)
options(stringsAsFactors = FALSE)

dirout = "McKay_etal_2021"

#########################
# Functions
#########################

getstats <- function(dataset, cat12, cond1, cond2, stat1) {
x1 = dim(dataset[dataset[,c(cat12)] == cond1, ])[1]
print(paste("Number", cond1, ":", x1 ))
y1 = dim(dataset[dataset[,c(cat12)] == cond2, ])[1]
print(paste("Number", cond2, ":", y1 ))
	x2 = mean(dataset[dataset[,c(cat12)] == cond1, c(stat1)])
print(paste("Mean", cond1, ":", x2 ))
y2 = mean(dataset[dataset[,c(cat12)] == cond2, c(stat1)])
print(paste("Mean", cond2, ":", y2 ))
x3 =  median(dataset[dataset[,c(cat12)] == cond1, c(stat1)])
print(paste("Median", cond1, ":", x3 ))
y3 = median(dataset[dataset[,c(cat12)] == cond2, c(stat1)])
print(paste("Median", cond2, ":", y3 ))
}


#########################
# Organizing and Cleaning Data
#########################

N= 2

# Loading data
df2 = read.table("lifespandata.csv", sep = ',', header = TRUE)
df1 = read.table("firstlifespandata.csv", sep = ',', header = TRUE)

# Making sure these are factors (and getting rid of comp/52split levels)
df1$FeedingScheme = as.factor(df1$FeedingScheme)
df2$FeedingScheme = as.factor(df2$FeedingScheme)
df2 <- within(df2, FeedingScheme <- relevel(FeedingScheme, ref = 2))

df1$Sex = as.factor(df1$Sex)
df2 = df2[df2$Sex != "",]
df2$Sex = as.factor(df2$Sex)

# just one of these animals, remove hatch date
df1 = df1[df1$HatchDate != "2019-04-05",]
# these are all AL, so remove since confounded
df2 = df2[df2$HatchDate != "2019-10-09", ]
df1$HatchDate = as.factor(df1$HatchDate)
df2$HatchDate = as.factor(df2$HatchDate)

# Males only
dfm1 = df1[df1$Sex == "m",]
dfm2 = df2[df2$Sex == "m",]

# Females only
dff1 = df1[df1$Sex == "f",]
dff2 = df2[df2$Sex == "f",]

# AL or DR only for cohort 1
df1AL = df1[df1$FeedingScheme == "AL",]
df1AL = df1AL[df1AL$Sex != "" ,]
df1DR = df1[df1$FeedingScheme == "DR",]
df1DR = df1DR[df1DR $Sex != "" ,]

# AL or DR only for cohort 2
df2AL = df2[df2$FeedingScheme == "AL",]
df2AL = df2AL[df2AL$Sex != "" ,]
df2DR = df2[df2$FeedingScheme == "DR",]
df2DR = df2DR[df2DR $Sex != "" ,]

#########################
# males, second cohort
#########################

lrank = survdiff(Surv(Lifespan_days, Observed) ~ FeedingScheme, data = dfm2, rho = 1)
pval = pchisq(lrank $chisq, length(lrank$n)-1, lower.tail = FALSE)

getstats(dfm2, "FeedingScheme", "DR", "AL", "Lifespan_days")

saveout = paste("~/", dirout, "/Fig4/SecondCohortMaleFeedingDRvsAL.pdf", sep = "")
pdf(file = saveout, compress = FALSE)

plot(survfit(Surv(Lifespan_days, Observed)~ FeedingScheme,data = dfm2), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
	#xlim = c(0,250), 
legend(
  "topright",
  legend=levels(factor(dfm2$FeedingScheme)),
  col= seq_along(levels(factor(dfm2$FeedingScheme))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()

#########################
# females, second cohort
#########################

lrank = survdiff(Surv(Lifespan_days, Observed) ~ FeedingScheme, data = dff2, rho = 1)
pval = pchisq(lrank $chisq, length(lrank$n)-1, lower.tail = FALSE)

getstats(dff2, "FeedingScheme", "DR", "AL", "Lifespan_days")

saveout = paste("~/", dirout, "/Fig4/SecondCohortFemaleFeedingDRvsAL.pdf", sep = "")
pdf(file = saveout, compress = FALSE)

plot(survfit(Surv(Lifespan_days, Observed)~ FeedingScheme,data = dff2), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
legend(
  "topright",
  legend=levels(factor(dff2$FeedingScheme)),
  col= seq_along(levels(factor(dff2$FeedingScheme))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()

#########################
# females vs males, AL, second cohort
#########################

lrank = survdiff(Surv(Lifespan_days, Observed) ~ Sex, data = df2AL, rho = 1)
pval = pchisq(lrank$chisq, length(lrank$n)-1, lower.tail = FALSE)

getstats(df2AL, "Sex", "m", "f", "Lifespan_days")

saveout = paste("~/", dirout, "/Fig4/MalesVsFemales_secondcohortALonly.pdf", sep = "")
pdf(file = saveout, compress = FALSE)

plot(survfit(Surv(Lifespan_days, Observed)~ Sex,data =df2AL), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
	#xlim = c(0,250), 
legend(
  "topright",
  legend=levels(factor(df2AL$Sex)),
  col= seq_along(levels(factor(df2AL $Sex))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()

lrank = survdiff(Surv(Lifespan_days, Observed) ~ Sex, data = df2DR, rho = 1)
pval = pchisq(lrank $chisq, length(lrank$n)-1, lower.tail = FALSE)

saveout = paste("~/", dirout, "/Fig4/MalesVsFemales_secondcohortDRonly.pdf", sep = "")
pdf(file = saveout, compress = FALSE)

# stats
summary(df2DR[df2DR$Sex == "m",])
summary(df2DR[df2DR$Sex == "f",])


plot(survfit(Surv(Lifespan_days, Observed)~ Sex,data =df2DR), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
	#xlim = c(0,250), 
legend(
  "topright",
  legend=levels(factor(df2DR $Sex)),
  col= seq_along(levels(factor(df2DR $Sex))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()


#########################
# Gompertz Curve fitting (only second cohort, males AL vs DR for now)
#########################

# using flexsurv to test gompertz
library(flexsurv)

# need to make columns match between
dfm1 = dfm1[,names(dfm1) %in% c("Sex", "Observed", "FeedingScheme", "Lifespan_weeks", "Lifespan_days", "Lifespan_3weeks")]
dfm2 = dfm2[,names(dfm2) %in% c("Sex", "Observed", "FeedingScheme", "Lifespan_weeks", "Lifespan_days", "Lifespan_3weeks")]
dfm = rbind(dfm1, dfm2)

dfmal = dfm[dfm$FeedingScheme == "AL",]
dfmdr = dfm[dfm$FeedingScheme == "DR",]
AL <- flexsurvreg(Surv(Lifespan_3weeks, Observed) ~ 1, data = dfmal, dist="gompertz")

DR <- flexsurvreg(Surv(Lifespan_3weeks, Observed) ~ 1, data = dfmdr, dist="gompertz")

# based upon this, need to export these back for plotting in julia:
# 6.15.20
# shape/RoA: AL1 = 0.3388 > DR = 0.1457
# rate/IMR/frailty: AL1 = 0.0385 < DR = 0.0557
write.table(dfmal,file = "male_al.csv", sep= ",")
write.table(dfmdr,file = "male_dr.csv", sep= ",")


#########################
# Supplmentary, first cohort
#########################

# testing using cox proportional hazards model on first cohort with baseline being male AL
df1$Sex = relevel(df1$Sex, "m")

coxph(Surv(Lifespan_days, Observed) ~ FeedingScheme + Sex, data = df1)

lrank = survdiff(Surv(Lifespan_days, Observed) ~ FeedingScheme, data = dfm1, rho = 1)
pval = pchisq(lrank $chisq, length(lrank$n)-1, lower.tail = FALSE)

getstats(dfm1, "FeedingScheme", "AL", "DR", "Lifespan_days")

saveout = paste("~/", dirout, "/Fig4/FirstCohortMaleFeedingDRvsAL.pdf", sep = "")
pdf(file = saveout, compress = FALSE)

plot(survfit(Surv(Lifespan_days, Observed)~ FeedingScheme,data = dfm1), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
	#xlim = c(0,250), 
legend(
  "topright",
  legend=levels(factor(dfm1$FeedingScheme)),
  col= seq_along(levels(factor(dfm1$FeedingScheme))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()


lrank = survdiff(Surv(Lifespan_days, Observed) ~ FeedingScheme, data = dff1, rho = 1)
pval = pchisq(lrank $chisq, length(lrank$n)-1, lower.tail = FALSE)

getstats(dff1, "FeedingScheme", "AL", "DR", "Lifespan_days")

saveout = paste("~/", dirout, "/Fig4/FirstCohortFemaleFeedingDRvsAL.pdf", sep = "")
pdf(file = saveout, compress = FALSE)

plot(survfit(Surv(Lifespan_days, Observed)~ FeedingScheme,data = dff1), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
legend(
  "topright",
  legend=levels(factor(dff1$FeedingScheme)),
  col= seq_along(levels(factor(dff1$FeedingScheme))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()

lrank = survdiff(Surv(Lifespan_days, Observed) ~ Sex, data = df1AL, rho = 1)
pval = pchisq(lrank$chisq, length(lrank$n)-1, lower.tail = FALSE)

getstats(df1AL, "Sex", "m", "f", "Lifespan_days")

saveout = paste("~/", dirout, "/Fig4/FirstCohort_MalesVsFemales_ALonly.pdf", sep = "")
pdf(file = saveout, compress = FALSE)

plot(survfit(Surv(Lifespan_days, Observed)~ Sex,data =df1AL), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
	#xlim = c(0,250), 
legend(
  "topright",
  legend=levels(factor(df1AL$Sex)),
  col= seq_along(levels(factor(df1AL $Sex))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()

lrank = survdiff(Surv(Lifespan_days, Observed) ~ Sex, data = df1DR, rho = 1)
pval = pchisq(lrank $chisq, length(lrank$n)-1, lower.tail = FALSE)

getstats(df1DR, "Sex", "m", "f", "Lifespan_days")

saveout = paste("~/", dirout, "/Fig4/FirstCohort_MalesVsFemales_DRonly.pdf", sep = "")
pdf(file = saveout, compress = FALSE)

plot(survfit(Surv(Lifespan_days, Observed)~ Sex,data =df1DR), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
	#xlim = c(0,250), 
legend(
  "topright",
  legend=levels(factor(df1DR $Sex)),
  col= seq_along(levels(factor(df1DR $Sex))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()




#########################
# Supplmentary, both cohorts
#########################
a = c("Number", "HatchDate", "Lifespan_days", "Observed", "FeedingScheme")
dfm = rbind(dfm1[,a], dfm2[,a])

lrank = survdiff(Surv(Lifespan_days, Observed) ~ FeedingScheme, data = dfm, rho = 1)
pval = pchisq(lrank $chisq, length(lrank$n)-1, lower.tail = FALSE)

getstats(dfm, "FeedingScheme", "AL", "DR", "Lifespan_days")

saveout = paste("~/", dirout, "/Fig4/BothCohortsMaleFeedingDRvsAL.pdf", sep = "")
pdf(file = saveout, compress = FALSE)


plot(survfit(Surv(Lifespan_days, Observed)~ FeedingScheme,data = dfm), 
	xlab = "Days",
	ylab = "Survival"
	,col = 1:N,
	lwd = 2.6666,
	cex.lab = 1.66,
	cex.axis = 1.33,
	main = pval
	)
	#xlim = c(0,250), 
legend(
  "topright",
  legend=levels(factor(dfm$FeedingScheme)),
  col= seq_along(levels(factor(dfm$FeedingScheme))),
  lty = 1,
  horiz=FALSE,
  bty='n')

dev.off()
