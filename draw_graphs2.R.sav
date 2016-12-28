# Program for drawing percentage outside normal range graphs for weekend mortality paper
# input = all_lims.csv from Stata program above
# output  = graphs for each of the biomarkers, showing percentage
#outside ref range over time, split by day of the week

# Author: Amy Mason


# packages


library(ggplot2)

# load in data to R (only run if data changes, otherwise use loaded R dataset below)

#alldata<-read.csv("D:\\Weekend_graphs\\all_lims.csv")
#save (alldata,file="D:\\Weekend_graphs\\all_lims.Rda")


load("D:\\Weekend_graphs\\all_lims.Rda")

# draw graphs, labelled by biomarker

###### ALB

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$alb), c("year", "hour",
"admissionday", "alb")]

names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-35
highref<-50
title1<-"Albumin \n percentages outside reference range"
title2<-"Albumin \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\albumin_ref1.svg"
savename2<-"D:\\Weekend_graphs\\albumin_ref2.svg"

tempset$lower<-ifelse(tempset$result<lowref,1,0)
tempset$upper<-ifelse(tempset$result>highref,1,0)
tempset$outside<-pmax(tempset$lower, tempset$upper)

lower <-aggregate(tempset$lower, by=list(tempset$hour,
tempset$admissionday),mean)
names(lower) <- c("Hour", "Day", "lower")
lower$lower<-lower$lower*100

upper <-aggregate(tempset$upper, by=list(tempset$hour,
tempset$admissionday),mean )
names(upper) <- c("Hour", "Day", "upper")
upper$upper<-upper$upper*100

both <-aggregate(tempset$outside, by=list(tempset$hour,
tempset$admissionday),mean)
names(both) <- c("Hour", "Day", "both")
both$both <- both$both*100

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, both, by=c("Hour", "Day"))

temp2$Day2<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Day2, temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper, linetype="Above")) + geom_path(aes(y=lower, linetype="Below")) +
   # scale_y_continuous(limits = c(35, 50)) +
    scale_x_continuous(expand = c(0, 0)) +
    xlab ("Hour of Admission") +
    ylab (title1) +
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,savename)
dev.off()

ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=both))+
    scale_y_continuous(limits = c(5, 15), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    xlab ("Hour of Admission") +
    ylab (title2) +
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,savename2)
dev.off()
