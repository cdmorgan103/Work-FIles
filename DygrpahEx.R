library(dplyr)
library(lubridate)
library(reshape2)
library(xts)
library(dygraphs)

df<-read.csv("asdf.csv",header = T)

#removes whitespace
df$APPT.DATE...TIME<-trimws(df$APPT.DATE...TIME)

#format for time
df$AptTime<-gsub("17 ","2017 ",df$APPT.DATE...TIME)
#df$new<-gsub("\b/17","*/2017",df$APPT.DATE...TIME)
df$AptTime<-strptime(df$AptTime, "%m/%d/%Y %I:%M %p")
df$AptTime<-as.POSIXct(df$AptTime)
df$CheckInT<-strptime(df$CHECKIN_TIME, "%m/%d/%Y %I:%M:%S %p")
df$CheckInT<-as.POSIXct(df$CheckInT)

df$dif<-round(((df$AptTime)-(df$CheckInT))/60,1)

##mean time early
mean(as.numeric(df$dif),na.rm = T)
#% of patients early
sum(df$dif>=0,na.rm=T)/sum(df$dif!="na",na.rm=T)

#add column with checkin delay bias lef in data, and shift to 0 by adding a 180 val for patients 3 hours early
#patient at 240 val is 4 hours early, patient at 360 val is 2 hours late
#120 val gives 2 hour lead for import into flexsim to account for early arrivals
###allows for 4 hour early 2 hour late
df$zeroed<-240-df$dif

#creates count
df$count<-1

#change continuity of care to chpg
df$DEPARTMENT_NAME[which(df$DEPARTMENT_NAME == "DALcoca")] = "CHPGMD"

#creates a date value
df$date<-date(df$AptTime)
#creates hour field
df$apt.hour<-hour(df$AptTime)
#creates minute field and rounds to nearest 5 minutes
df$apt.mi<-minute(df$AptTime)

#id combo
df$HarAndCSN<-ifelse(is.na(df$HAR),paste("c",df$CSN),df$HAR)

#filters times <180 minutes early or -120/120 minutes late as potentially bad data
dff<-df[df$zeroed<360 & df$zeroed>0,]
dff<-df[df$apt.hour<=17&df$apt.hour>=7,]

#list of duplicated data
dup<-dff[(duplicated(dff[c(20,23)]) | duplicated(dff[c(20,23)], fromLast = T)), ]

#generates list of nonduplicated data
nondup<-dff[!(duplicated(dff[c(20,23)]) | duplicated(dff[c(20,23)], fromLast = T)), ]

#for sorting to delete duplicates DepartmentNames, creates a dept name to be sorted for deletion
LabRad<-c("DAL AIM","DAL LAB PAVILION","DAL PIC CT","DAL PIC DIAGNOSTICS","DAL PIC FLUOROSCOPY","DAL PIC MRI","DAL PIC SONOGRAPHY","DAL PULMONARY LAB")

#if a labrad val, append 1 to dept name for sorting
dup$DEPARTMENT_NAMESort<-if_else((dup$DEPARTMENT_NAME == LabRad),(paste("1",dup$DEPARTMENT_NAME)),paste(dup$DEPARTMENT_NAME))
#sorts the data
dup<-dup[order(dup$DEPARTMENT_NAMESort,na.last = T,decreasing = T),]
#eliminates duplicates in duplicate df with dept names for lab rad removed by preferences and clinic visits remian
dup<-dup[!duplicated(dup['HarAndCSN']),]

#binds duplicates filtered to nonduplicates
clean<-rbind(nondup,dup[,-24])
#removes NA's
clean<-clean[complete.cases(clean[ , 23]),]
#creates week day value
clean$apt.day<-wday(clean$date,label = T, abbr = T)
#creates week value
clean$week<-strftime(clean$date, format = "%V")
#eliminate 12/31 so we have consistent weekday counts for all days
clean<-clean[!(clean$date=="2017-12-31"),]
#gives month
clean$month<-strftime(clean$date,format = "%b")




#

#chpg, continuity of care, and complex care
#cp<-subset(clean,subset = (clean$DEPARTMENT_NAME == "CHPG MEDICAL DISTRICT" | clean$DEPARTMENT_NAME == "DAL CONTINUITY OF CARE" |  clean$DEPARTMENT_NAME == "PEMR COMPLEX CARE" ))
#remove chpg, dontinuity of care, and complex care
#so2<-subset(clean,subset = (clean$DEPARTMENT_NAME != "CHPG MEDICAL DISTRICT" & clean$DEPARTMENT_NAME != "DAL CONTINUITY OF CARE" &  clean$DEPARTMENT_NAME != "PEMR COMPLEX CARE" ))
so<-clean




#Filter week list
#monfilt<-c("01","03","22","36","52")
#tuefilt<-c(27)
#wedfilt<-c()
#thufilt<-c(47)
#frifilt<-c(47)


  #creates time for apointment val
    so$rdtime<-so$AptTime
    #strips time date values
    so$rdtime<-strptime(so$rdtime,format  = "%Y-%m-%d %H:%M:%S",tz = "America/Chicago")
    #rounds to nearest hour
    so$rdtime<-round_date(so$rdtime,"1 hour")
    #converts to posixct
    so$rdtime<-as.POSIXct(so$rdtime)

#creates imprt table,groups by rdtime, filters for operating times & Work DOW, exports as table with values of count
    imprt<-so %>%
      group_by(rdtime) %>%
      filter(apt.hour>=8 & apt.hour<=17)%>%
      filter(apt.day=="Mon" | apt.day=="Tue" | apt.day=="Wed" | apt.day=="Thu" | apt.day=="Fri") %>%
      summarize(count=sum(count,na.rm = F)) 
    imprt
    colnames(imprt)<-c("dt","count")
    


    #creates bef as a time sequence
    bef<-seq(from=as.POSIXct("2017-1-2 12:00", tz="UTC"),to=as.POSIXct("2017-12-29 12:00", tz="UTC"),by="day")
    #creates dataframe of bef
    befd<-data.frame(bef,0)
    #changes col names of befd
    colnames(befd)<-c("dt","count")
    #creates aft values
    aft<-seq(from=as.POSIXct("2017-1-2 24:00", tz="UTC"),to=as.POSIXct("2017-12-29 24:00", tz="UTC"),by="day")
    #creates aft dataframe
    aftd<-data.frame(aft,0)
    #renames columns
    colnames(aftd)<-c("dt","count")
    #binds together
    mac<-rbind(befd,aftd)
    
    imprt2<-rbind(imprt,mac)
    #imprt2$dt<-order(imprt2$dt)

    #magic=xts(x = imprt$count, order.by = imprt$dt)
    magic=xts(x = imprt2$count, order.by = imprt2$dt)
    
    
    dygraph(magic, main="2017 Specialty Volume (UNOFFICIAL)", ylab="Patients", xlab="Date/Time") %>%
      dyRangeSelector(height=50, fillColor = "grey") %>%
      dySeries(label = "Patient Volume") %>%
      dyLegend(width = 400) %>%
      dyOptions(stackedGraph = TRUE,connectSeparatedPoints = FALSE, colors="#5DADE2", fillAlpha=0.8) %>% 
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))    


    
    
    
    