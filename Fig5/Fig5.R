library(ggplot2)
library(gridExtra)
library(data.table)

datadir = "yourdatafolder"

readin = paste(datadir, "/Training/allscoreslab.csv", sep = "")
data = read.table(readin, sep = ',', header = TRUE)
data = data[data$label != "middle",]

data$label = factor(data$label, c("young", "old"))

pd <- position_jitter(width = 0.1)
# Plot manual
ggplot(data, aes(x = label, y = manindex, color = label)) +  geom_boxplot()  +geom_point(position = pd) + theme_bw()
ggsave("manualindex_youngold.pdf")

# testing significance for manual:
wilcox.test(data$manindex ~ data$label)

# Plot auto
ggplot(data, aes(x = label, y = autoindex, color = label)) +  geom_boxplot()  +geom_point(position = pd) + theme_bw()
ggsave("autoindex_youngold.pdf")

# testing significance for manual:
wilcox.test(data$autoindex ~ data$label)
#pvalue = 0.004947


ggplot(data, aes(x = manindex, y= autoindex)) + geom_point() + geom_smooth(method = "lm", fill = NA) + theme_bw()
ggsave("autoVsmanindexscores.pdf")


#######################################
# startle response
#######################################
readin = paste(datadir, "/Training/filteredstartle.csv", sep = "")
sdata = read.table(readin, sep = ',', header = TRUE)
sdata = sdata[sdata$AgeCat != "middle",]
sdata$AgeCat = factor(sdata$AgeCat, c("young", "old"))

# plot proportion startle, horizontally bar
pd <- position_jitter(width = 0.1)
subsdata = sdata[,c(1,3)]
subsdata = as.data.frame(table(subsdata)) 
ggplot(subsdata, aes(x = AgeCat, y = Freq, fill = startleResponse)) + geom_bar(stat= "identity", position = 'stack', width = .5)  + theme_bw() + coord_flip() #+ theme(legend.position="none")
ggsave("binaryStartle.pdf")

# plot proportion startle
pd <- position_jitter(width = 0.1)
subsdata = sdata[,c(1,3)]
subsdata = as.data.frame(table(subsdata)) 
tsubsdata = data.table(subsdata)
tsubsdata[, percen := sum(Freq), by=list(AgeCat)]
tsubsdata[, percen := Freq/percen]
ggplot(tsubsdata, aes(x = AgeCat, y = percen, fill = startleResponse)) + geom_bar(stat= "identity", position = 'stack', width = .5)  + theme_bw() +  theme(legend.position="none")
ggsave("vertical_Proportion_binaryStartle.pdf")


# plot VidWhereStartled
sdata = sdata[sdata$VidWhereStartled != "NA", ]
sdata = sdata[complete.cases(sdata), ]
sdata$VidWhereStartled  = sdata$VidWhereStartled + 1
ggplot(sdata, aes(x = AgeCat, y = VidWhereStartled, color = AgeCat)) +  geom_boxplot()  +geom_point(position = pd) + theme_bw() 
ggsave("vidwherestartled.pdf")

# testing significance for manual:
#wilcox.test(sdata$VidWhereStartled~ sdata$AgeCat)
#pvalue = 1

# summary stats
summary(sdata)
sdata = sdata[sdata$startleResponse == 1,]
mean(sdata[sdata$AgeCat == "young", c("VidWhereStartled")])
mean(sdata[sdata$AgeCat == "old", c("VidWhereStartled")])


#######################################
# movement response
#######################################
readin = paste(datadir, "/Training/filteredcame2top.csv", sep = "")
mdata = read.table(readin, sep = ',', header = TRUE)
mdata = mdata[mdata$AgeCat != "middle",]
mdata$AgeCat = factor(mdata$AgeCat, c("young", "old"))

# plot proportion that came to top with food, horizontal bar
pd <- position_jitter(width = 0.1)
submdata = mdata[,c(1,3)]
submdata = as.data.frame(table(submdata)) 
ggplot(submdata, aes(x = AgeCat, y = Freq, fill = Came2topwithfood)) + geom_bar(stat= "identity", position = 'dodge', width = .5)  + theme_bw() + coord_flip()+ theme(legend.position="none")
ggsave("binaryCame2topwithfood.pdf")

# plot proportion that came to top with food, vertical bar
pd <- position_jitter(width = 0.1)
submdata = mdata[,c(1,3)]
submdata = as.data.frame(table(submdata)) 
tsubsdata = data.table(submdata)
tsubsdata[, percen := sum(Freq), by=list(AgeCat)]
tsubsdata[, percen := Freq/percen]
ggplot(tsubsdata, aes(x = AgeCat, y = percen, fill = Came2topwithfood)) + geom_bar(stat= "identity", position = 'dodge', width = .5)  + theme_bw() + theme(legend.position="none")
ggsave("vertical_Proportion_binaryCame2topwithfood.pdf")


# plot VidWhereCame2Top with food
mdata = mdata[mdata$VidWhereCame2Top != "NA", ]
mdata = mdata[complete.cases(mdata), ]
mdata$VidWhereCame2Top  = mdata$VidWhereCame2Top + 1
ggplot(mdata, aes(x = AgeCat, y = VidWhereCame2Top, color = AgeCat)) +  geom_boxplot()  +geom_point(position = pd) + theme_bw() 
ggsave("vidwherecame2top.pdf")

# testing significance for manual:
#wilcox.test(mdata$VidWhereCame2Top ~ mdata$AgeCat)
#pvalue = 0.2262

summary(mdata)
mean(mdata[mdata $AgeCat == "young", c("VidWhereCame2Top")])
mean(mdata[mdata $AgeCat == "old", c("VidWhereCame2Top")])

#######################################
# Compass/Rose Plot
#######################################
# first, individual
library(ggplot2)

data1= read.table("../../Data/Training/bins_first5_f3.csv", sep = ',', header = FALSE)
data2= read.table("../../Data/Training/bins_14to19_f3.csv", sep = ',', header = FALSE)

ggplot(data=data1,aes(x=V1,y=V2))+geom_bar(stat="identity")+ coord_polar(start = -1.79, direction = -1)+xlab("")+ylab("")+ ylim(0,30)

ggsave("First5trainingsf3_CompassPlot.pdf")


ggplot(data=data2,aes(x=V1,y=V2))+geom_bar(stat="identity") +coord_polar(start = -1.79, direction = -1)+xlab("")+ylab("") + ylim(0,30)
ggsave("Later14to19trainingsf3_CompassPlot.pdf")

# now, aggregated data
ypre= read.table("../../Data/Training/young_pretrain.csv", sep = ',', header = FALSE)
ypost = read.table("../../Data/Training/young_posttrain.csv", sep = ',', header = FALSE)
opre= read.table("../../Data/Training/old_pretrain.csv", sep = ',', header = FALSE)
opost = read.table("../../Data/Training/old_posttrain.csv", sep = ',', header = FALSE)

p1 = ggplot(data=ypre,aes(x=V1,y=V2))+geom_bar(stat="identity")+ coord_polar(start = -1.79, direction = -1)+xlab("")+ylab("")+ ylim(0,3) + theme(axis.text = element_blank(), axis.ticks = element_blank()) 

p2 = ggplot(data=ypost,aes(x=V1,y=V2))+geom_bar(stat="identity")+ coord_polar(start = -1.79, direction = -1)+xlab("")+ylab("")+ ylim(0,3) + theme(axis.text = element_blank(), axis.ticks = element_blank()) 

p3 = ggplot(data=opre,aes(x=V1,y=V2))+geom_bar(stat="identity")+ coord_polar(start = -1.79, direction = -1)+xlab("")+ylab("")+ ylim(0,3) + theme(axis.text = element_blank(), axis.ticks = element_blank()) 

p4 = ggplot(data=opost,aes(x=V1,y=V2))+geom_bar(stat="identity")+ coord_polar(start = -1.79, direction = -1)+xlab("")+ylab("")+ ylim(0,3) + theme(axis.text = element_blank(), axis.ticks = element_blank()) 

p = grid.arrange(p1,p2,p3,p4, nrow = 1)

ggsave("young_old_pretrain_posttrain_CompassPlot.pdf", p)




