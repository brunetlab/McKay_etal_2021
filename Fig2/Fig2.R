library(ggplot2)

dirout = "McKay_etal_2021"
datadir = "yourdatafolder"

readin = paste(datadir, "/Source Data 1.csv", sep = "")
df30days = read.table(readin, sep= ",", header = TRUE)

saveout = paste("~/", dirout, "/Fig2/Fig2A.pdf", sep ="")
pdf(file = saveout, compress = FALSE)

p <- ggplot(df30days)
p + geom_point(aes(Day, NumberTimesFed)) + ylim(0,8) +  theme_classic(base_size = 16)

dev.off()


# loading in deviations from ideal (3 or 7) feedings for 41 feeders over 2281 days worth of feedings
readin = paste(datadir, "/Source Data 2.csv", sep = "")
freqd = read.table(readin, header = TRUE, sep = ",")
p <- ggplot(data = freqd,aes(freqd$MissedFeedings)) + geom_histogram() +  theme_classic(base_size=16)
saveout = paste("~/" , dirout , "/Fig2/Fig2B.pdf", sep ="")
ggsave(saveout)


# pulling in percentage deviation from the mean
devf = read.table("rawdeviations_percent.csv", header = TRUE, sep = ",")

devf$category = factor(devf$category, levels = c("Multiple People", "Single Person", "Automatic Feeder", "Single Automatic Feeder"))

p <- ggplot(devf, aes(x = category, y = deviation, color = category))+ geom_jitter( stat="identity", size = 1, width=0.15) + theme_classic() + geom_hline(yintercept = 0) + ylim(-0.8,0.8)
saveout = paste("~/", dirout, "/Fig2/Fig2D.pdf", sep ="")
ggsave(saveout)








