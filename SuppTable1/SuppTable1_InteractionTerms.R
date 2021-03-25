library(ggplot2)
library(survival)
library(rms)
options(stringsAsFactors = FALSE)

datadir = "yourdatafolder"

#########################
# Organizing and Cleaning Data
#########################

N= 2

# Loading data (NOTE: need to run the code in Code/Fig4 first)
df2 = read.table("lifespandata.csv", sep = ""), sep = ',', header = TRUE)
df1 = read.table("firstlifespandata.csv", sep = ""), sep = ',', header = TRUE)

# Making sure these are factors 
df1$FeedingScheme = as.factor(df1$FeedingScheme)
df2$FeedingScheme = as.factor(df2$FeedingScheme)
df2 <- within(df2, FeedingScheme <- relevel(FeedingScheme, ref = 2))

df1$Sex = as.factor(df1$Sex)
df2 = df2[df2$Sex != "",]
df2$Sex = as.factor(df2$Sex)

df1 = df1[df1$HatchDate != "2019-04-05",]
df2 = df2[df2$HatchDate != "2019-10-09", ]
df1$HatchDate = as.factor(df1$HatchDate)
df2$HatchDate = as.factor(df2$HatchDate)

# Males only
dfm1 = df1[df1$Sex == "m",]
dfm2 = df2[df2$Sex == "m",]

# Females only
dff1 = df1[df1$Sex == "f",]
dff2 = df2[df2$Sex == "f",]

# AL or DR only for cohort 2
df2AL = df2[df2$FeedingScheme == "AL",]
df2AL = df2AL[df2AL$Sex != "" ,]
df2DR = df2[df2$FeedingScheme == "DR",]
df2DR = df2DR[df2DR $Sex != "" ,]

#########################
# Testing for interaction terms
#########################

# need to make columns match between
df1 = df1[,names(df1) %in% c("Sex", "Observed", "FeedingScheme", "Lifespan_weeks", "Lifespan_days", "HatchDate")]
df2 = df2[,names(df2) %in% c("Sex", "Observed", "FeedingScheme", "Lifespan_weeks", "Lifespan_days", "HatchDate")]
df = rbind(df1, df2)

# relevel on female, AL
df2$FeedingScheme = relevel(df2$FeedingScheme, "AL")
df2$Sex = relevel(df2$Sex, "m")

coxfit_df2 = coxph(Surv(Lifespan_days, Observed) ~ FeedingScheme + Sex  + HatchDate + Sex:FeedingScheme, data = df2)
summary(coxfit_df2)



















