library(dplyr)
library(ggplot2)
library(lubridate)
library(lattice)
library(hexbin)



#------||||| Unpacking file |||||-------
FileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
NameFile <- "Dataset.zip"

if (!file.exists(NameFile)) {
  download.file(FileUrl, NameFile, mode = "wb")
}

unzip(NameFile)

activity<-read.csv("activity.csv")


#------||||| Total steps made every day |||||-------

activity$date<-ymd(activity$date)
My_table<-data.frame(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
My_table$date<-rownames(My_table)
rownames(My_table)<-NULL
names(My_table)[[1]]<-"Total Steps"

dev.new()
par(mfcol = c(1,1))

ggplot(My_table,aes(y=My_table$`Total Steps`,x=My_table$date))+geom_bar(stat="identity",fill = "#FF6666") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")

dev.copy(png, file="plot1.png", height=480, width=480,units="px",bg="transparent")
dev.off()

dev.new()
par(mfcol = c(1,1))

qplot(My_table$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram") +  geom_histogram(color="black", fill="lightblue",
                                                                                                                             linetype="dashed")
dev.copy(png, file="plot1_1.png", height=480, width=480,units="px",bg="transparent")
dev.off()


#------||||| Mean and median steps made every day |||||-------

My_table2<-data.frame(round(tapply(activity$steps,activity$date,mean,na.rm=TRUE),2))
My_table2$date<-rownames(My_table2)
rownames(My_table2)<-NULL
names(My_table2)[[1]]<-"Mean Steps"
temp<-activity%>%select(date,steps) %>% group_by(date) %>% summarise(median(steps))
names(temp)[[2]]<-"Median Steps"
My_table2$median<-temp$`Median Steps`
My_table2<-My_table2 %>% select(date,`Mean Steps`,median)


#------||||| Average steps made every day |||||-------

My_table2$date<-as.Date(My_table2$date,format="%Y-%m-%d")

dev.new()
par(mfcol = c(1,1))

ggplot(My_table2,aes(x=My_table2$date,y=My_table2$`Mean Steps`))+geom_bar(stat="identity", fill = "#66ff66")+scale_x_date()+ylab("Mean Steps Every day")+xlab("Date")+ggtitle("Mean Steps by Date")

dev.copy(png, file="plot2.png", height=480, width=480,units="px",bg="transparent")
dev.off()


#------||||| Average steps made every day |||||-------

activity$interval<-factor(activity$interval)
My_table3<-aggregate(data=activity,steps~date+interval,FUN="mean")
My_table3<-aggregate(data=My_table3,steps~interval,FUN="max")



#------||||| Next step i change missing data  |||||-------
##||||| Before we start we need to understand what |||||             
##||||| are the distributions of missing values by date and interval |||||
##||||| we will use the mean value substitution strategy |||||

My_table4<-activity
My_table4$Missing<-is.na(My_table4$steps)
My_table4<-aggregate(data=My_table4,Missing~date+interval,FUN="sum")
My_table4.1<-data.frame(tapply(My_table4$Missing,My_table4$date,sum))
My_table4.1$date<-rownames(My_table4.1)
rownames(My_table4.1)<-NULL
names(My_table4.1)<-c("Missing","date")
My_table4.1$date<-as.Date(My_table4.1$date,format="%Y-%m-%d")
My_table4.2<-data.frame(tapply(My_table4$Missing,My_table4$interval,sum))
My_table4.2$date<-rownames(My_table4.2)
rownames(My_table4.2)<-NULL
names(My_table4.2)<-c("Missing","Interval")

dev.new()
par(mfrow=c(1,2))

plot(y=My_table4.1$Missing,x=My_table4.1$date,main="Missing Value Distribution by Date",pch=21, bg="red" )
plot(y=My_table4.2$Missing,x=My_table4.2$Interval,main="Missing Value Distribution by Interval",pch=21, bg="red" )

dev.copy(png, file="plot3.png", height=480, width=480,units="px",bg="transparent")
dev.off()

My_table4.3<-as.data.frame(My_table4.1) %>% select(date,Missing) %>% arrange(desc(Missing))
My_table4.3<-My_table4.3[which(My_table4.3$Missing!=0),]
My_table4.3$Weekday<-wday(My_table4.3$date,label=TRUE)
My_table4.4<-activity
My_table4.4$weekday<-wday(My_table4.4$date,label=TRUE)

My_table4.5<-aggregate(data=My_table4.4,steps~interval+weekday,FUN="mean",na.rm=TRUE)

My_table4.6<-merge(x=My_table4.4,y=My_table4.5,by.x=c("interval","weekday"),by.y=c("interval","weekday"),all.x=TRUE)

My_table4.6$Steps.Updated<-0
for (i in 1:dim(My_table4.6)[[1]]){
  if(is.na(My_table4.6[i,3])){My_table4.6[i,6]=My_table4.6[i,5]}
  else {My_table4.6[i,6]=My_table4.6[i,3]}
}

My_table4.6 <-My_table4.6  %>% select(date,weekday,interval,Steps.Updated)
names(My_table4.6)[[4]]<-"Steps"



#------||||| The total steps made every day after missing values  |||||-------


dev.new()
par(mfcol = c(1,1))

qplot(My_table4.6$Steps,geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count") +  geom_histogram(color="black", fill="lightblue",
                                                                                                                                        linetype="dashed")
dev.copy(png, file="plot4.png", height=480, width=480,units="px",bg="transparent")
dev.off()


#------||||| Compare the average steps made in 5-minute interval across weekdays and weekends  |||||-------

My_table5<-My_table4.6
levels(My_table5$weekday)<-c(1,2,3,4,5,6,7)
My_table5$WDWE<-My_table5$weekday %in% c(1,2,3,4,5)
My_table5.1<-aggregate(data=My_table5,Steps~interval+WDWE,mean,na.rm=TRUE)
My_table5.1$WDWE<-as.factor(My_table5.1$WDWE)
levels(My_table5.1$WDWE)<-c("Weekend","Weekday")

dev.new()
par(mfcol = c(1,1))

ggplot(data=My_table5.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")

dev.copy(png, file="plot5.png", height=480, width=480,units="px",bg="transparent")
dev.off()

My_table5.1$interval<-as.numeric(as.character(My_table5.1$interval))

dev.new()
par(mfcol = c(1,1))

xyplot(data=My_table5.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
hexbinplot(data=My_table5.1,Steps~interval|WDWE, aspect = 1, bins=50)

dev.copy(png, file="plot6.png", height=480, width=480,units="px",bg="transparent")
dev.off()
