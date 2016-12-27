# Program for drawing 10th and 90th percentile graphs for weekend mortality paper
# input = all_lims.csv from Stata program above
# output  = graphs for each of the biomarkers, showing ref range
# and 10/90th percentile over time, split by day of the week

# Author: Amy Mason


# packages

library(ggplot2)

# load in data to R (only run if data changes, otherwise use loaded R dataset below)

#alldata<-read.csv("D:\\Weekend_graphs\\all_lims.csv")
#save (alldata,file="D:\\Weekend_graphs\\all_lims.Rda")


load("D:\\Weekend_graphs\\all_lims.Rda")

# draw graphs, labelled by biomarker

###### ALB

tempset<- alldata[!is.na(alldata$alb), c("year", "hour",
"admissionday", "alb")]

lower <-aggregate(tempset$alb, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$alb, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$alb, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
    scale_y_continuous(limits = c(30, 50)) +
    xlab ("Hour of Admission") +
    ylab ("Albumin g/L \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 35, color="Reference range"))+
    geom_hline( aes(yintercept = 50,color="Reference range")) +
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))


dev.copy(svg,"D:\\Weekend_graphs\\albumin_centiles.svg")
dev.off()


###### ALK

tempset<- alldata[!is.na(alldata$alk), c("year", "hour",
"admissionday", "alk")]

lower <-aggregate(tempset$alk, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$alk, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$alk, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab ("Alkaline Phosphatase IU/L \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 95, color="Reference range"))+
    geom_hline( aes(yintercept = 320,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\alk_centiles.svg")
dev.off()



###### ALT

tempset<- alldata[!is.na(alldata$alt), c("year", "hour",
"admissionday", "alt")]

lower <-aggregate(tempset$alt, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$alt, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$alt, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab ("Alanine Aminotransferase IU/L \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 10, color="Reference range"))+
    geom_hline( aes(yintercept = 45,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\alt_centiles.svg")
dev.off()



###### AST

tempset<- alldata[!is.na(alldata$ast), c("year", "hour",
"admissionday", "ast")]

lower <-aggregate(tempset$ast, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$ast, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$ast, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab ("Aspartate transaminase IU/L \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 15, color="Reference range"))+
    geom_hline( aes(yintercept = 42,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\ast_centiles.svg")
dev.off()


###### BIL

tempset<- alldata[!is.na(alldata$bil), c("year", "hour",
"admissionday", "bil")]

lower <-aggregate(tempset$bil, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$bil, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$bil, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab ("Bilirubin umol/L \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 3, color="Reference range"))+
    geom_hline( aes(yintercept = 17,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\bil_centiles.svg")
dev.off()

###### CRT

tempset<- alldata[!is.na(alldata$crt), c("year", "hour",
"admissionday", "crt")]

lower <-aggregate(tempset$crt, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$crt, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$crt, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab ("Creatinine mmol/L \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 54, color="Reference range"))+
    geom_hline( aes(yintercept = 150,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\crt_centiles.svg")
dev.off()

###### CRT

tempset<- alldata[!is.na(alldata$crt), c("year", "hour",
"admissionday", "crt")]

lower <-aggregate(tempset$crt, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$crt, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$crt, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab ("Creatinine mmol/L \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 54, color="Reference range"))+
    geom_hline( aes(yintercept = 150,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\crt_centiles.svg")
dev.off()



###### CRP

tempset<- alldata[!is.na(alldata$crp), c("year", "hour",
"admissionday", "crp")]

lower <-aggregate(tempset$crp, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$crp, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$crp, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab ("C-Reactive Protein mg/L \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 0, color="Reference range"))+
    geom_hline( aes(yintercept = 8,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\crp_centiles.svg")
dev.off()



###### EOS

tempset<- alldata[!is.na(alldata$eos), c("year", "hour",
"admissionday", "eos")]

lower <-aggregate(tempset$eos, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$eos, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$eos, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab (expression(atop("Eosinophils 10"^9~"/L", "10th and 90th percentiles")))+
    geom_hline(aes(yintercept = 0, color="Reference range"))+
    geom_hline( aes(yintercept = 0.5,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\eos_centiles.svg")
dev.off()

###### HB

tempset<- alldata[!is.na(alldata$hb), c("year", "hour",
"admissionday", "hb")]

lower <-aggregate(tempset$hb, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$hb, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$hb, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
    ylab ("Haemoglobin g/DL \n 10th and 90th percentiles") +
    geom_hline(aes(yintercept = 13, color="Reference range"))+
    geom_hline( aes(yintercept = 17,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\hb_centiles.svg")
dev.off()


###### LYM

tempset<- alldata[!is.na(alldata$lym), c("year", "hour",
"admissionday", "lym")]

lower <-aggregate(tempset$lym, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.1))
names(lower) <- c("Hour", "Day", "lower")

upper <-aggregate(tempset$lym, by=list(tempset$hour,
tempset$admissionday),
   function(x) quantile(x, probs = 0.9))
names(upper) <- c("Hour", "Day", "upper")

median <-aggregate(tempset$lym, by=list(tempset$hour,
tempset$admissionday),
   function(x) median(x))
names(median) <- c("Hour", "Day", "median")

temp<-merge(upper, lower, by=c("Hour", "Day"))
temp2<-merge(temp, median, by=c("Hour", "Day"))

temp2$Day<-factor(temp2$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
temp2<-temp2[order(temp2$Hour),]


ggplot(temp2, aes(x=Hour, color=Day)) +
    geom_path(aes(y=upper)) + geom_path(aes(y=lower)) +
#    scale_y_continuous(limits = c(0, 65 )) +
    xlab ("Hour of Admission") +
       ylab (expression(atop("Lymphocytes 10"^9~"/L", "10th and 90th percentiles")))+
    geom_hline(aes(yintercept = 1, color="Reference range"))+
    geom_hline( aes(yintercept = 4,color="Reference range"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"))

dev.copy(svg,"D:\\Weekend_graphs\\lym_centiles.svg")
dev.off()
