#Uber Supply & Demand  Assignment 2018
setwd("C:/PGDDS/Uber Case Study")
library(dplyr)
library(tidyr)
library(lubridate) # for parse_date_time
library(ggplot2)
library(stringr)
library(grid) # for 2 plots
library(gridExtra)

# Reading the csv file from Source directory and getting "NA",""," " as NA strings
reqdata = read.csv("Uber Request Data.csv",na.strings = c("NA"," ",""))
View(reqdata)

# Counting the number of records in the data
reqdata_nrow<-nrow(reqdata)

# Checking for duplicated records
reqdata_unrow<-nrow(unique(reqdata))
if(reqdata_nrow==reqdata_unrow) {print("No duplicated Records")} else {print("Duplicated records.Need Cleaning")}

#Checking for NA values

na_req<-is.na(reqdata$Request.timestamp)
ifelse(sum(na_req)>0,"NA values present in Request timesatmap","No NA values present in Request timesatmap") 

na_drp<-is.na(reqdata$Drop.timestamp)
ifelse(sum(na_drp)>0,"NA values present in Drop timesatmap","No NA values present in Drop timesatmap") 

na_reqid<-is.na(reqdata$Request.id)
ifelse(sum(na_reqid)>0,"NA values present in Request ID","No NA values present in Request ID") 

na_drvid<-is.na(reqdata$Driver.id)
ifelse(sum(na_drvid)>0,"NA values present in Driver ID","No NA values present in Driver ID") 

na_Sts<-is.na(reqdata$Status)
ifelse(sum(na_Sts)>0,"NA values present in Status","No NA values present in Status") 

na_Pkup<-is.na(reqdata$Pickup.point)
ifelse(sum(na_Pkup)>0,"NA values present in Pickup Point","No NA values present in Pickup Point") 

#cleaning the request timestamp data
reqdata$Request.timestamp<-trimws(reqdata$Request.timestamp)
reqdata$Request.timestamp<-ifelse(str_count(reqdata$Request.timestamp)!=19,paste(reqdata$Request.timestamp,":00",sep=""),reqdata$Request.timestamp)

#cleaning the drop timestamp data
reqdata$Drop.timestamp<-trimws(reqdata$Drop.timestamp)
reqdata$Drop.timestamp<-ifelse(str_count(reqdata$Drop.timestamp)!=19,paste(reqdata$Drop.timestamp,":00",sep=""),reqdata$Drop.timestamp)

#cleaning the other data
reqdata$Status<-trimws(reqdata$Status)
reqdata$Pickup.point<-trimws(reqdata$Pickup.point)
reqdata$Driver.id<-trimws(reqdata$Driver.id)
reqdata$Request.id<-trimws(reqdata$Request.id)

#Using parse_date_time for parsing request and drop timestamps
reqdata$Request.timestamp=parse_date_time(reqdata$Request.timestamp,orders="dmy hms")
reqdata$Drop.timestamp=parse_date_time(reqdata$Drop.timestamp,orders="dmy hms")

# extracting the date, day of week,hour,minute,second from request timestamp

reqdata$Request.date = format(reqdata$Request.timestamp, "%d-%m-%Y")
reqdata$Request.dow=format(reqdata$Request.timestamp, "%A")
reqdata$Request.hour = format(reqdata$Request.timestamp, "%H")
reqdata$Request.minute = format(reqdata$Request.timestamp, "%M")
reqdata$Request.second = format(reqdata$Request.timestamp, "%S")

# extracting the date, day of week,hour,minute,second from drop timestamp

reqdata$Drop.date = format(reqdata$Drop.timestamp, "%d-%m-%Y")
reqdata$Drop.dow=format(reqdata$Drop.timestamp, "%A")
reqdata$Drop.hour = format(reqdata$Drop.timestamp, "%H")
reqdata$Drop.minute = format(reqdata$Drop.timestamp, "%M")
reqdata$Drop.second = format(reqdata$Drop.timestamp, "%S")


# Calculating the trip duration for classifying trips into short,medium,long
reqdata$trip_time_minutes<-as.numeric(reqdata$Drop.timestamp-reqdata$Request.timestamp)

# Determining whether service or not to take care of demand supply gap

reqdata$IsServiced<-ifelse(reqdata$Status=="Trip Completed","Serviced","Service Gap")


# Assigning five timeslots to the respective Requests

reqdata$TimeSlot = rep(1,nrow(reqdata))
reqdata$TimeSlot[ which( as.numeric(reqdata$Request.hour) < 5) ] = "Early Morning"
reqdata$TimeSlot[ which(as.numeric(reqdata$Request.hour) >= 5 & as.numeric(reqdata$Request.hour) < 11 ) ] = "Morning"
reqdata$TimeSlot[ which(as.numeric(reqdata$Request.hour) >= 11 & as.numeric(reqdata$Request.hour) < 17 ) ] = "Afternoon"
reqdata$TimeSlot[ which( as.numeric(reqdata$Request.hour) >= 17 & as.numeric(reqdata$Request.hour) < 22 ) ] = "Evening"
reqdata$TimeSlot[ which( as.numeric(reqdata$Request.hour) >= 22 ) ] = "Night"

View(reqdata)

#Ordering in Barcharts for Timeslot
reqdata$TimeSlot <- factor(reqdata$TimeSlot ,levels = c("Early Morning", "Morning", "Afternoon", "Evening","Night"))

#Ordering in Barcharts for week of the day
reqdata$Request.dow <- factor(reqdata$Request.dow ,levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday"))


#Plotting as per Pickup Point of the request to see the count 
ggplot(reqdata, aes(x = reqdata$Request.hour, fill = reqdata$Pickup.point )) + geom_bar()+labs(title = " Request Statistics Hourly",x = "Hour",y="Frequency",fill="Pickup Point")
ggplot(reqdata, aes(x = reqdata$TimeSlot, fill = reqdata$Pickup.point )) + geom_bar(width = 0.6)+labs(title = " Request Statistics per Timeslot",x = "Timeslot",y="Frequency",fill="Pickup Point")
ggplot(reqdata, aes(x = reqdata$Request.dow, fill = reqdata$Pickup.point )) + geom_bar(width = 0.6)+labs(title = " Request Statistics per Day of the Week",x = "Day of the week",y="Frequency",fill="Pickup Point")

#Plotting as per Request of the request to see the count 
ggplot(reqdata, aes(x = reqdata$Request.hour, fill = reqdata$Status )) + geom_bar()+labs(title = " Request Statistics Hourly",x = "Hour",y="Frequency",fill="Request Status")
ggplot(reqdata, aes(x = reqdata$TimeSlot, fill = reqdata$Status )) + geom_bar(width = 0.6)+labs(title = " Request Statistics per Timeslot",x = "Timeslot",y="Frequency",fill="Request Status")
ggplot(reqdata, aes(x = reqdata$Request.dow, fill = reqdata$Status )) + geom_bar(width = 0.6)+labs(title = " Request Statistics per Day of the Week",x = "Day of the week",y="Frequency",fill="Request Status")

ggplot(reqdata, aes(x = reqdata$Request.hour, fill = reqdata$Status )) + geom_bar(position="dodge")+labs(title = " Request Statistics Hourly",x = "Hour",y="Frequency",fill="Request Status")
ggplot(reqdata, aes(x = reqdata$TimeSlot, fill = reqdata$Status )) + geom_bar(position="dodge",width = 0.6)+labs(title = " Request Statistics per Timeslot",x = "Timeslot",y="Frequency",fill="Request Status")
ggplot(reqdata, aes(x = reqdata$Request.dow, fill = reqdata$Status )) + geom_bar(position="dodge",width = 0.6)+labs(title = " Request Statistics per Day of the Week",x = "Day of the week",y="Frequency",fill="Request Status")


# Filtering as per Pickup Point i.e Airport or City

reqdata_map1<-filter(reqdata,Pickup.point=="Airport")
reqdata_map2<-filter(reqdata,Pickup.point=="City")

#Ordering in Barcharts for week of the day
reqdata_map1$Request.dow <- factor(reqdata_map1$Request.dow ,levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday"))

#Ordering in Barcharts for Timeslot
reqdata_map1$TimeSlot <- factor(reqdata_map1$TimeSlot ,levels = c("Early Morning", "Morning", "Afternoon", "Evening","Night"))

#Ordering in Barcharts for week of the day
reqdata_map2$Request.dow <- factor(reqdata_map2$Request.dow ,levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday"))

#Ordering in Barcharts for Timeslot
reqdata_map2$TimeSlot <- factor(reqdata_map2$TimeSlot ,levels = c("Early Morning", "Morning", "Afternoon", "Evening","Night"))

# Request statistics per  Timeslot
plot1<-ggplot(reqdata_map1, aes(x = reqdata_map1$TimeSlot, fill = reqdata_map1$Status )  ) +  geom_bar(position="dodge")+ylim(0,1500)
plot2<-ggplot(reqdata_map2, aes(x = reqdata_map2$TimeSlot, fill = reqdata_map2$Status )  ) +  geom_bar(position="dodge")+ylim(0,1500)
grid.arrange(plot1+labs(title = "Pickup Point:Airport Request Statistics",x = "TimeSlot",y="Frequency",fill="Request Status"),plot2+labs(title = "Pickup Point:City Request Statistics",x = "TimeSlot",y="Frequency",fill="Request Status"),nrow=1)

# Request statistics per Hour
plot3<-ggplot(reqdata_map1, aes(x = reqdata_map1$Request.hour, fill = reqdata_map1$Status )  ) + geom_bar(position="dodge")+ylim(0,400)
plot4<-ggplot(reqdata_map2, aes(x = reqdata_map2$Request.hour, fill = reqdata_map2$Status )  ) + geom_bar(position="dodge")+ylim(0,400)
grid.arrange(plot3+labs(title = "Pickup Point:Airport Request Statistics",x = "Hour",y="Frequency",fill="Request Status"),plot4+labs(title = "Pickup Point:City Request Statistics",x = "Hour",y="Frequency",fill="Request Status"),nrow=1)

# Request statistics per Day of the week
plot5<-ggplot(reqdata_map1, aes(x = reqdata_map1$Request.dow, fill = reqdata_map1$Status )  ) + geom_bar(position="dodge")+ylim(0,500)
plot6<-ggplot(reqdata_map2, aes(x = reqdata_map2$Request.dow, fill = reqdata_map2$Status )  ) + geom_bar(position="dodge")+ylim(0,500)
grid.arrange(plot5+labs(title = "Pickup Point:Airport Request Statistics",x = "Day of week",y="Frequency",fill="Request Status"),plot6+labs(title = "Pickup Point:City Request Statistics",x = "Day of week",y="Frequency",fill="Request Status"),nrow=1)


## Service statistics per Timeslot

plot7<-ggplot(reqdata_map1, aes(x = reqdata_map1$TimeSlot, fill = reqdata_map1$IsServiced)  ) + geom_bar()
plot8<-ggplot(reqdata_map2, aes(x = reqdata_map2$TimeSlot, fill = reqdata_map2$IsServiced)  ) + geom_bar()
grid.arrange(plot7+labs(title = "Pickup Point:Airport Service Statistics",x = "TimeSlot",y="Frequency",fill="Service Status"),plot8+labs(title = "Pickup Point:City Service Statistics",x = "TimeSlot",y="Frequency",fill="Service Status"),nrow=1)

#Plotting  the demand suppply gap as (No of requests serviced(Trip Completed)/ Total No of Requests)

# Demand supply gap per Timeslot with Pickup point being Airport

reqdata_map1_gr<-group_by(reqdata_map1,TimeSlot)
nm1<-count(reqdata_map1_gr)
nm<-count(filter(reqdata_map1_gr,IsServiced=="Serviced"))
nm$percentge<-(nm$n*100)/nm1$n
barplot(100-nm$percentge,ylim=c(0,100),names.arg=nm$TimeSlot,xlab=" TimeSlot",ylab="Requests Not Serviced %",col="cyan", main=" Demand Supply Gap per Timeslot for Pickup Point : Airport",border="red")

# Demand supply gap per Timeslot with Pickup point being City

reqdata_map2_gr<-group_by(reqdata_map2,TimeSlot)
nm3<-count(reqdata_map2_gr)
nm2<-count(filter(reqdata_map2_gr,IsServiced=="Serviced"))
nm2$percentge<-(nm2$n*100)/nm3$n
barplot(100-nm2$percentge,ylim=c(0,100),names.arg=nm2$TimeSlot,xlab=" TimeSlot",ylab="Requests Not Serviced %",col="red", main="Demand Supply Gap per Timeslot for Pickup Point :City",border="black")

# Demand supply gap Overall per Timeslot 

reqdata_gr<-group_by(reqdata,TimeSlot)
nm5<-count(reqdata_gr)
nm4<-count(filter(reqdata_gr,IsServiced=="Serviced"))
nm4$percentge<-(nm4$n*100)/nm5$n
barplot(100-nm4$percentge,ylim=c(0,100),names.arg=nm4$TimeSlot,xlab=" TimeSlot",ylab="Requests Not Serviced %",col="green", main="Demand Supply Gap Overall per timeslot",border="red")



## Service statistics per Hour

plot9<-ggplot(reqdata_map1, aes(x = reqdata_map1$Request.hour, fill = reqdata_map1$IsServiced)  ) + geom_bar()
plot10<-ggplot(reqdata_map2, aes(x = reqdata_map2$Request.hour, fill = reqdata_map2$IsServiced)  ) + geom_bar()
grid.arrange(plot9+labs(title = "Pickup Point:Airport Service Statistics",x = " Hour",y="Frequency",fill="Service Status"),plot10+labs(title = "Pickup Point:City Service Statistics",x = "Hour",y="Frequency",fill="Service Status"),nrow=1)

#Plotting  the demand suppply gap as (No of requests serviced(Trip Completed)/ Total No of Requests)

# Demand supply gap per Hour with Pickup point being Airport

reqdata_map1_gr<-group_by(reqdata_map1,Request.hour)
nm7<-count(reqdata_map1_gr)
nm6<-count(filter(reqdata_map1_gr,IsServiced=="Serviced"))
nm6$percentge<-(nm6$n*100)/nm7$n
barplot(100-nm6$percentge,ylim=c(0,100),names.arg=nm6$Request.hour,xlab=" Hour",ylab="Requests Not Serviced %",col="pink", main="Demand Supply Gap per Hour for Pickup Point:Airport",border="red")

# Demand supply gap per Hour with Pickup point being City

reqdata_map2_gr<-group_by(reqdata_map2,Request.hour)
nm9<-count(reqdata_map2_gr)
nm8<-count(filter(reqdata_map2_gr,IsServiced=="Serviced"))
nm8$percentge<-(nm8$n*100)/nm9$n
barplot(100-nm8$percentge,ylim=c(0,100),names.arg=nm8$Request.hour,xlab="Hour",ylab="Requests Not Serviced %",col="purple", main="Demand Supply Gap per Hour for Pickup Point: City",border="black")

# Demand supply gap Overall per Hour 

reqdata_gr<-group_by(reqdata,Request.hour)
nm11<-count(reqdata_gr)
nm10<-count(filter(reqdata_gr,IsServiced=="Serviced"))
nm10$percentge<-(nm10$n*100)/nm11$n
barplot(100-nm10$percentge,ylim=c(0,100),names.arg=nm10$Request.hour,xlab="Hour",ylab="Requests Not Serviced %",col="orange", main="Demand Supply Gap per Hour for Hourly slots",border="red")

#Plotting  the demand suppply gap as (No of requests serviced(Trip Completed)/ Total No of Requests)

## Service statistics per Day of the week

plot11<-ggplot(reqdata_map1, aes(x = reqdata_map1$Request.dow, fill = reqdata_map1$IsServiced)  ) + geom_bar()
plot12<-ggplot(reqdata_map2, aes(x = reqdata_map2$Request.dow, fill = reqdata_map2$IsServiced)  ) + geom_bar()
grid.arrange(plot11+labs(title = "Pickup Point:Airport Service Statistics",x = "Day of week",y="Frequency",fill="Service Status"),plot12+labs(title = "Pickup Point:City Service Statistics",x = "Day of week",y="Frequency",fill="Service Status"),nrow=1)

# Demand supply gap per Day of the week with Pickup point being Airport

reqdata_map1_gr<-group_by(reqdata_map1,Request.dow)
nm13<-count(reqdata_map1_gr)
nm12<-count(filter(reqdata_map1_gr,IsServiced=="Serviced"))
nm12$percentge<-(nm12$n*100)/nm13$n
barplot(100-nm12$percentge,ylim=c(0,100),names.arg=nm12$Request.dow,xlab="Day of week",ylab="Requests Not Serviced %",col="blue", main="Demand Supply Gap per Day of Week for Pickup Point :Airport",border="red")

# Demand supply gap per Day of the week with Pickup point being City

reqdata_map2_gr<-group_by(reqdata_map2,Request.dow)
nm15<-count(reqdata_map2_gr)
nm14<-count(filter(reqdata_map2_gr,IsServiced=="Serviced"))
nm14$percentge<-(nm14$n*100)/nm15$n
barplot(100-nm14$percentge,ylim=c(0,100),names.arg=nm14$Request.dow,xlab="Day of week",ylab="Requests Not Serviced %",col="brown", main="Demand Supply Gap per Day of Week for Pickup Point :City",border="red")

# Demand supply gap Overall per Day of the week

reqdata_gr<-group_by(reqdata,Request.dow)
nm17<-count(reqdata_gr)
nm16<-count(filter(reqdata_gr,IsServiced=="Serviced"))
nm16$percentge<-(nm16$n*100)/nm17$n
barplot(100-nm16$percentge,ylim=c(0,100),names.arg=nm16$Request.dow,xlab="Day of week",ylab="Requests Not Serviced %",col="yellow", main="Demand Supply Gap per Day of Week Overall",border="red")


# Filtering all requests on "Trip Completed" field
reqdata_complete<-filter(reqdata,Status == "Trip Completed")
reqdata_complete_ap<-filter(reqdata,Status == "Trip Completed" & Pickup.point == "Airport")
reqdata_complete_ct<-filter(reqdata,Status == "Trip Completed" & Pickup.point == "City")

##Doing Analysis on Trip time

#Calculating Average Trip Time

avg_triptime<-mean(reqdata_complete$trip_time_minutes,na.RM=TRUE)
paste("Avg Trip Time in minutes is:",round(avg_triptime,digits=0),sep="")
avg_triptime_ap<-mean(reqdata_complete_ap$trip_time_minutes,na.RM=TRUE)
paste("Avg Trip Time from Airport in minutes is:",round(avg_triptime_ap,digits=0),sep="")
avg_triptime_ct<-mean(reqdata_complete_ct$trip_time_minutes,na.RM=TRUE)
paste("Avg Trip Time from City in minutes is:",round(avg_triptime_ct,digits=0),sep="")

#Calculating Median Trip Time

median_triptime<-median(reqdata_complete$trip_time_minutes)
median_triptime_ap<-median(reqdata_complete_ap$trip_time_minutes)
median_triptime_ct<-median(reqdata_complete_ct$trip_time_minutes)
paste("Median Trip Time in minutes is:",round(median_triptime,digits=0),sep="")
paste("Median Trip Time from Airport in minutes is:",round(median_triptime_ap,digits = 0),sep="")
paste("Median Trip Time from City in minutes is:",round(median_triptime_ct,digits=0),sep="")

#Calculating First Quartile of  Trip Time

firstquar_triptime<-quantile(reqdata_complete$trip_time_minutes,0.25)
firstquar_triptime_ap<-quantile(reqdata_complete_ap$trip_time_minutes,0.25)
firstquar_triptime_ct<-quantile(reqdata_complete_ct$trip_time_minutes,0.25)
paste("First quartile Trip Time in minutes is:",round(firstquar_triptime,digits=0),sep="")
paste("First quartile Trip Time from Airport in minutes is:",round(firstquar_triptime_ap,digits=0),sep="")
paste("First quartile Trip Time from City in minutes is:",round(firstquar_triptime_ct,digits=0),sep="")

#Calculating Third Quartile of  Trip Time

thirdquar_triptime<-quantile(reqdata_complete$trip_time_minutes,0.75)
thirdquar_triptime_ap<-quantile(reqdata_complete_ap$trip_time_minutes,0.75)
thirdquar_triptime_ct<-quantile(reqdata_complete_ct$trip_time_minutes,0.75)
paste("Third quartile Trip Time in minutes is:",round(thirdquar_triptime,digits=0),sep="")
paste("Third quartile Trip Time from Airport in minutes is:",round(thirdquar_triptime_ap,digits=0),sep="")
paste("Third quartile Trip Time from City in minutes is:",round(thirdquar_triptime_ct,digits=0),sep="")

# Creating a new column about trip duration. we classify into "Short","Medium","Long" trips
# Short(<=first quartile), Medium is (>first qat & <= third quartile), Long is (>third quartile)

reqdata_complete$trip_type = rep(1,nrow(reqdata_complete))
reqdata_complete$trip_type[which (reqdata_complete$trip_time_minutes<=round(firstquar_triptime,digits=0))]="Short"
reqdata_complete$trip_type[which (reqdata_complete$trip_time_minutes>round(firstquar_triptime,digits=0) & reqdata_complete$trip_time_minutes<=round(thirdquar_triptime,digits=0))]="Medium"
reqdata_complete$trip_type[which (reqdata_complete$trip_time_minutes>round(thirdquar_triptime,digits=0))]="Long"

ggplot(reqdata_complete, aes(x = reqdata_complete$TimeSlot, fill = reqdata_complete$trip_type )  ) +  geom_bar(position="dodge")+facet_wrap(~reqdata_complete$Pickup.point)+labs(title = "Pickup Point:Trip Duration Statistics",x = "Timeslot",y="Frequency",fill=" Trip Type")
ggplot(reqdata_complete, aes(x = reqdata_complete$Request.hour, fill = reqdata_complete$trip_type )  ) +  geom_bar(position="dodge")+facet_wrap(~reqdata_complete$Pickup.point)+labs(title = "Pickup Point:Trip Duration Statistics",x = "Timeslot",y="Frequency",fill=" Trip Type")

#Analyzing by driver ids

reqdata_complete_grp<-group_by(reqdata_complete,Driver.id)
reqdata_complete_grp_cnt<-count(reqdata_complete_grp,sort=TRUE)
View(reqdata_complete_grp_cnt)

best_perform_complete<-head(reqdata_complete_grp_cnt,1)
View(best_perform_complete)

reqdata_complete_ap_grp<-group_by(reqdata_complete_ap,Driver.id)
reqdata_complete_ap_grp_cnt<-count(reqdata_complete_ap_grp,sort=TRUE)
View(reqdata_complete_ap_grp_cnt)

reqdata_complete_ct_grp<-group_by(reqdata_complete_ct,Driver.id)
reqdata_complete_ct_grp_cnt<-count(reqdata_complete_ct_grp,sort=TRUE)
View(reqdata_complete_ct_grp_cnt)

reqdata_nt_complete=reqdata[which(reqdata$Status != "Trip Completed"),]
reqdata_nt_complete_temp<-group_by(reqdata_nt_complete,Driver.id)
reqdata_nt_complete_temp1<-count(reqdata_nt_complete_temp,sort=TRUE)
View(reqdata_nt_complete_temp1)


reqdata_nt_complete_tot<-filter(reqdata,Status == "Cancelled")
reqdata_nt_complete_ap<-filter(reqdata,Status == "Cancelled" & Pickup.point == "Airport")
reqdata_nt_complete_ct<-filter(reqdata,Status == "Cancelled" & Pickup.point == "City")


reqdata_nt_complete_tot_grp<-group_by(reqdata_nt_complete_tot,Driver.id)
reqdata_nt_complete_tot_grp_cnt<-count(reqdata_nt_complete_tot_grp,sort=TRUE)
View(reqdata_nt_complete_tot_grp_cnt)

worst_canceller<-head(reqdata_nt_complete_tot_grp_cnt,1)
View(worst_canceller)

reqdata_nt_complete_ap_grp<-group_by(reqdata_nt_complete_ap,Driver.id)
reqdata_nt_complete_ap_grp_cnt<-count(reqdata_nt_complete_ap_grp,sort=TRUE)
View(reqdata_nt_complete_ap_grp_cnt)

reqdata_nt_complete_ct_grp<-group_by(reqdata_nt_complete_ct,Driver.id)
reqdata_nt_complete_ct_grp_cnt<-count(reqdata_nt_complete_ct_grp,sort=TRUE)
View(reqdata_nt_complete_ct_grp_cnt)

# Investigating for Driver.id=22

reqdatacomplete_drv_22<-filter(reqdata_complete,Driver.id==best_perform_complete$Driver.id)
reqdatantcomplete_drv_22<-filter(reqdata_nt_complete,Driver.id==best_perform_complete$Driver.id)
View(reqdatacomplete_drv_22)
View(reqdatantcomplete_drv_22)

reqdatacomplete_drv_84<-filter(reqdata_complete,Driver.id==worst_canceller$Driver.id)
reqdatantcomplete_drv_84<-filter(reqdata_nt_complete,Driver.id==worst_canceller$Driver.id)
View(reqdatacomplete_drv_84)
View(reqdatantcomplete_drv_84)

# If we could convince the drivers who have underatken "Short" trips(<= 41 min)  by giving incentive to/from airport rather than cancelling. This is during the "Morning" "Afternoon" "Evening" slots
# Also more drivers travelling  towards the airport/city means lesser "No Cars avilable"
# Assuming a 25% decrease in "No Cars avilable" & "Cancelled"

reqdata_gr<-group_by(reqdata,TimeSlot)
nm105<-count(reqdata_gr)
nm104<-count(filter(reqdata_gr,IsServiced=="Serviced"))
nm106<-count(filter(reqdata_gr,Status=="Cancelled"|Status=="No Cars Available" ))
nm104$percentge<-((nm104$n+0.25*nm106$n)*100)/nm105$n
barplot(100-nm104$percentge,names.arg=nm104$TimeSlot,xlab=" TimeSlot",ylab="Requests  Not Serviced %",col="green", main="Service percent chart : Overall for timeslot after Incentive")

# If in Addition to above measure Uber increases fleet by 25%.
# Increase in drivers has probability for more cancellations though. 
# Increase in overhead costs
# Assuming a 20 % decrease in "No Cars avilable" & "Cancelled"
# Assuming the Impact of both measures would be 45% 

reqdata_gr<-group_by(reqdata,TimeSlot)
nm105<-count(reqdata_gr)
nm104<-count(filter(reqdata_gr,IsServiced=="Serviced"))
nm106<-count(filter(reqdata_gr,Status=="Cancelled"|Status=="No Cars Available" ))
nm104$percentge<-((nm104$n+0.45*nm106$n)*100)/nm105$n
barplot(100-nm104$percentge,ylim=c(0,100),names.arg=nm104$TimeSlot,xlab=" TimeSlot",ylab="Requests Not Serviced %",col="green", main="Demand Supply Gap per Timeslot : Overall   after Measures")
