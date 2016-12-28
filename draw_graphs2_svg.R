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

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-35
highref<-50
title1<-"Albumin \n percentages outside reference range"
title2<-"Albumin \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\albumin_ref1.svg"
savename2<-"D:\\Weekend_graphs\\albumin_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
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

###### ALK

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$alk), c("year", "hour",
"admissionday", "alk")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-95
highref<-320
title1<-"Alkaline Phosphatase \n percentages outside reference range"
title2<-"Alkaline Phosphatase \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\alk_ref1.svg"
savename2<-"D:\\Weekend_graphs\\alk_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#   scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### ALT

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$alt), c("year", "hour",
"admissionday", "alt")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-10
highref<-45
title1<-"Alanine Aminotransferase \n percentages outside reference range"
title2<-"Alanine Aminotransferase \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\alt_ref1.svg"
savename2<-"D:\\Weekend_graphs\\alt_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### AST

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$ast), c("year", "hour",
"admissionday", "ast")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-15
highref<-42
title1<-"Aspartate transaminase \n percentages outside reference range"
title2<-"Aspartate transaminase \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\ast_ref1.svg"
savename2<-"D:\\Weekend_graphs\\ast_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### BIL

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$bil), c("year", "hour",
"admissionday", "bil")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-3
highref<-7
title1<-"Bilirubin \n percentages outside reference range"
title2<-"Bilirubin \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\bil_ref1.svg"
savename2<-"D:\\Weekend_graphs\\bil_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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



##### CRT

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$crt), c("year", "hour",
"admissionday", "crt")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-54
highref<-150
title1<-"Creatinine \n percentages outside reference range"
title2<-"Creatinine \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\crt_ref1.svg"
savename2<-"D:\\Weekend_graphs\\crt_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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

##### CRP

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$crp), c("year", "hour",
"admissionday", "crp")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-0
highref<-8
title1<-"C-Reactive Protein\n percentages outside reference range"
title2<-"C-Reactive Protein \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\crp_ref1.svg"
savename2<-"D:\\Weekend_graphs\\crp_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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

##### EOS

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$eos), c("year", "hour",
"admissionday", "eos")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-0
highref<-0.5
title1<-"Eosinophils \n percentages outside reference range"
title2<-"Eosinophils \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\eos_ref1.svg"
savename2<-"D:\\Weekend_graphs\\eos_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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

##### HB

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$hb), c("year", "hour",
"admissionday", "hb")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-13
highref<-17
title1<-"Haemoglobin \n percentages outside reference range"
title2<-"Haemoglobin \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\hb_ref1.svg"
savename2<-"D:\\Weekend_graphs\\hb_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### LYM

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$lym), c("year", "hour",
"admissionday", "lym")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-1
highref<-4
title1<-"Lymphocytes \n percentages outside reference range"
title2<-"Lymphocytes \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\lym_ref1.svg"
savename2<-"D:\\Weekend_graphs\\lym_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### Mono

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$mono), c("year", "hour",
"admissionday", "mono")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-0.2
highref<-1
title1<-"Monocytes \n percentages outside reference range"
title2<-"Monocytes \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\mono_ref1.svg"
savename2<-"D:\\Weekend_graphs\\mono_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### NEU

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$neu), c("year", "hour",
"admissionday", "neu")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-2
highref<-7
title1<-"Neutrophils \n percentages outside reference range"
title2<-"Neutrophils \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\neu_ref1.svg"
savename2<-"D:\\Weekend_graphs\\neu_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### PHO

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$pho), c("year", "hour",
"admissionday", "pho")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-0.7
highref<-1.45
title1<-"Alkaline Phosphatase \n percentages outside reference range"
title2<-"Alkaline Phosphatase \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\pho_ref1.svg"
savename2<-"D:\\Weekend_graphs\\pho_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### PLT

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$plt), c("year", "hour",
"admissionday", "plt")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-150
highref<-400
title1<-"Platelets \n percentages outside reference range"
title2<-"Platelets \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\plt_ref1.svg"
savename2<-"D:\\Weekend_graphs\\plt_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### POT

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$pot), c("year", "hour",
"admissionday", "pot")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-3.5
highref<-5
title1<-"Potassium \n percentages outside reference range"
title2<-"Potassium \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\pot_ref1.svg"
savename2<-"D:\\Weekend_graphs\\pot_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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


##### SOD

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$sod), c("year", "hour",
"admissionday", "sod")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-135
highref<-145
title1<-"Sodium \n percentages outside reference range"
title2<-"Sodium \n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\sod_ref1.svg"
savename2<-"D:\\Weekend_graphs\\sod_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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



##### URE

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$ure), c("year", "hour",
"admissionday", "ure")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-2.5
highref<-6.7
title1<-"Urea \n percentages outside reference range"
title2<-"Urea\n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\ure_ref1.svg"
savename2<-"D:\\Weekend_graphs\\ure_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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

##### WCL

# select only people with non-empty alb results
tempset<- alldata[!is.na(alldata$wcl), c("year", "hour",
"admissionday", "wcl")]

#set variables to draw graph
names(tempset)<-c("year", "hour","admissionday", "result")
lowref<-4
highref<-11
title1<-"White Cell Count \n percentages outside reference range"
title2<-"White Cell Count\n percentage outside reference range"
savename <- "D:\\Weekend_graphs\\wcl_ref1.svg"
savename2<-"D:\\Weekend_graphs\\wcl_ref2.svg"

# set up variable for graph
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

# draw graph1
ggplot(temp2, aes(x=Hour, color=Day2)) +
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

# draw graph2
ggplot(temp2, aes(x=Hour, color=Day2)) +
    geom_path(aes(y=both))+
#    scale_y_continuous(limits = c(10, 25), expand = c(0, 0)) +
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
