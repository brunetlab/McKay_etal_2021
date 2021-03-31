library(ggplot2)

datadir = "yourdatafolder"

readin = paste(datadir, "/Source Data 10.csv", sep = "")
df = read.table(readin, sep= ",", header = TRUE)
df = df[df$deviation >= 0,]

numfeeders = length(unique(df$feeder))

# loading in deviations from ideal (3 or 7) feedings
p <- ggplot(data = df,aes(df$deviation)) + geom_histogram() +  theme_classic(base_size=16)
ggsave("SuppFig1A.pdf")

# create table
total = length(df$deviation)
# getting counts
vals = summary(as.factor(df$deviation))
percent = vals/total







