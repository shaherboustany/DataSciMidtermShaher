############################################  MIDTERM PROJECT ##############################################

#Shaher Boustany

#packages
require(foreign)
require(tidyr)
require(plyr)
require(dplyr)
require(magrittr)
require(ggplot2)

#files loaded, after reading the readme.TXT file, these three files seem to have at least one column 
#of useful information towards the project question
#Osha.dbf - main table with company name, address, date of inspection, etc. 
#Viol.dbf - violations from each inspection. 
#Accid.dbf - details about accident victims

ACCID<-read.dbf("accid.DBF")
OSHA<-read.dbf("osha.dbf")
VIOL<-read.dbf("viol.DBF")

####################################STEP 1 Cleaning up the OSHA file #######################################
############################################################################################################

#After reading osha.txt, the metrics that I found important are 
#ACTIVITYNO, ESTABNAME,OPENDATE, SITEADD, SITEZIP,ACCID_,VIOLS_,HAZSUB_

OSHA <- select(OSHA, ACTIVITYNO,ESTABNAME,OPENDATE,SITEADD,SITEZIP,ACCID_,VIOLS_,HAZSUB_)
OSHA <-unique(OSHA) #making sure there are no duplicates

#Remove all rows that had no accidents, violations or hazardous substances
#(Assumption: Places with no accidents, violations or hazardous substances are safe)
OSHA<- filter(OSHA,!(ACCID_==0 & VIOLS_==0 & HAZSUB_==0))
### Be careful when using this table, it is very messy and some company names have been repeated
### when using this OSHA file to join or merge, make sure to use activity number to reduce errors
### missing data is starting to cause problems with this data, we will need to get rid of all the 
### missing data we can not control. 



####################################STEP 2 Cleaning up the ACCID file #######################################
#############################################################################################################
#When reading the ACCID file, the ACC.dbf is referenced after reading the files
#since the accidents are classified in degrees 1 2 and 3, the many variables 


#Cleaning up ACCID table. After reading the ACCID.txt, the metrics that I found important are
#ACCIDNO,DEGREE,NATURE,BODYPART,SOURCE,EVENT,ENVIRON,HUMAN,TASK

ACCID<-select(ACCID,NAME,AGE,ACTIVITYNO,DEGREE,NATURE,BODYPART,SOURCE,EVENT,ENVIRON,HUMAN,TASK)

#the more information is beeing presented, the more missing information is becoming a problem with 
#the data. looking at the current ACCID table, some rows are duplicated without any name or age, so it is
#unclear whether it is a mistake or if more than one person was affected by accident
#in some of the rows, the name and age are shown and the all the rest of the entries in the columns are the 
#same. This means that more than one person was affected by the accident.
#for simplification purposes. And to be able to clean up this data, we will make an assumption
#that the gravity of the accident is the same whether one or more people were affected.
#we therefore need to get rid of these duplicates in the file. 

ACCID<-select(ACCID,ACTIVITYNO,DEGREE,NATURE,BODYPART,SOURCE,EVENT,ENVIRON,HUMAN,TASK)
ACCID<-filter(ACCID,!DEGREE==0)
#since the current ACCID contains duplicates of ACTIVITYNO (since the same activity can have different accidents)
#lets make a new variable called ACCNO, this one will contain a list of activity numbers
ACCNO<- select(ACCID,ACTIVITYNO)
ACCNO<-unique(ACCNO)
#lets double check we have no duplicates
#since number of obs does not change, we have no duplicates. 

#Now, lets take ACCID and sort them by degrees. 
ACCID<- arrange(ACCID,DEGREE)
#All degrees 0 are have no information in the simplified ACCID2 file I made nor in the initial ACCID.dbf 
#file. We can drop this variable. This missing information plays a role in skewing the results
#but they are out of our control. 

ACCID<-filter(ACCID,!DEGREE==0)


#now we can classify the accidents by degrees
#then we can figure out how many times each company is shown in the table
#we need an OSHA file containing needed info + ACCID_ which shows how many accidents happened
#at each location

OSHAacc<-select(OSHA,ACTIVITYNO,ESTABNAME,SITEADD,SITEZIP,ACCID_)

###degree 1 = fatality

ACCID.DEATH <- filter(ACCID,DEGREE==1)

#trying to figure out how many accidents of degree 1 each company was subject to
accdeath<-count(ACCID.DEATH,ACTIVITYNO)
colnames(accdeath)[2] <- "ACC_DEG_1"
#checking for duplicates
accdeath<-unique(accdeath)

#now lets merge with accdeath with ACCNO
#Then we can merge ACCNO with OSHAacc
ACCNO<-merge(x=ACCNO,y=accdeath,by="ACTIVITYNO",all.x=TRUE)
FINALACCID<-merge(x=ACCNO,y=OSHAacc,by="ACTIVITYNO",all.x=TRUE)
rm(ACCNO)
rm(accdeath)
rm(ACCID.DEATH)
rm(OSHAacc)

#####degree 2 = hospitalization (same process as degree 1)
ACCID.HOSP <-filter(ACCID,DEGREE==2)
acchosp<-count(ACCID.HOSP,ACTIVITYNO)
colnames(acchosp)[2] <- "ACC_DEG_2"
acchosp<-unique(acchosp)

#now lets merge with FINALACCID

FINALACCID<-merge(x=FINALACCID,y=acchosp,by="ACTIVITYNO",all.x=TRUE)
rm(ACCID.HOSP)
rm(acchosp)

#######degree 3 = no hospitalization
ACCID.NOHOSP <- filter(ACCID,DEGREE==3)

acc.nohosp<-count(ACCID.NOHOSP,ACTIVITYNO)
colnames(acc.nohosp)[2] <- "ACC_DEG_3"


#now lets merge with FINALACCID

FINALACCID<-merge(x=FINALACCID,y=acc.nohosp,by="ACTIVITYNO",all.x=TRUE)

#Re arrange ACCDEG_1
FINALACCID<-select(FINALACCID,ACTIVITYNO,ESTABNAME,SITEADD,SITEZIP,ACCID_,ACC_DEG_1,ACC_DEG_2,ACC_DEG_3)
#This table gives all the companies with documented incidents, how many total accidents occured,
#and how many out of these accidents are of degree 1, 2 and 3. 
rm(acc.nohosp)
rm(ACCID.NOHOSP)

#make all NAs to 0
FINALACCID[is.na(FINALACCID)] <- 0

#rename columns
colnames(FINALACCID)<- c("ACTIVITY_ID","COMPANY_NAME","COMPANY_ADDRESS","COMPANY_ZIP","TOTAL_ACC","ACC_DEG_1","ACC_DEG_2","ACC_DEG_3")

#for analysis purposes, lets look at the top 30 companies with the most accidents
TOPTENACC<-arrange(FINALACCID,desc(ACCID_))
TOPTENACC<- TOPTENACC[1:30,]




####################################STEP 3 Cleaning up the VIOL file #######################################
############################################################################################################


#Cleaning up VIOL table. After reading the viol.txt, the metrics that I found important are
# VIOLTYPE, GRAVITY, ISSUE DATE, ACTIVUTY NUMBER, INSTANCES. 
#looking at VIOL, specifically at the DELETE column shows that dleted violations are marked with an X 
#so lets start by removing these rows
VIOL <- filter(VIOL, is.na(DELETE))


#taking the important metrics
VIOL<- select(VIOL, ACTIVITYNO,ISSUEDATE,CITATION,ITEMNO,VIOLTYPE,GRAVITY,INSTANCES)
#making sure there are no duplicates
VIOL <-unique(VIOL)
# first we want to categorize the table into its Violation types
VIOL.types<-select(VIOL,ACTIVITYNO,VIOLTYPE,INSTANCES)
VIOL.types<- group_by(VIOL.types,ACTIVITYNO) #group by activity numbers
VIOL.types<-group_by(VIOL.types,VIOLTYPE) #group by violation types
#we need to spread the date to know how many instances of each violation we have
#?aggregate
VIOL.types<-aggregate(VIOL.types$INSTANCES,list(ACTIVITYNO = VIOL.types$ACTIVITYNO, VIOLTYPE = VIOL.types$VIOLTYPE),sum)
VIOL.types<-spread(VIOL.types,VIOLTYPE,x)
VIOL.types[is.na(VIOL.types)] <- 0 #removing NA's so we can sum the values and get the total number of 
#violations per Activity number

VIOL.types$TotalVIOLS<-((as.numeric(VIOL.types$O)+as.numeric(VIOL.types$R)+as.numeric(VIOL.types$S)+as.numeric(VIOL.types$U)+as.numeric(VIOL.types$W)))

#we need an OSHA name file for violations (same procedure as in ACCID)
OSHAviol<-select(OSHA,ACTIVITYNO,ESTABNAME,SITEADD,SITEZIP)
FINALVIOL<-merge(x=VIOL.types,y=OSHAviol,by="ACTIVITYNO",all.x=TRUE)
rm(VIOL.types)
rm(OSHAviol)
#rearrange +rename columns
FINALVIOL<-select(FINALVIOL,ACTIVITYNO,ESTABNAME,SITEADD,SITEZIP,TotalVIOLS,S,O,W,R,U)
colnames(FINALVIOL)<- c("ACTIVITY_ID","COMPANY_NAME","COMPANY_ADDRESS","COMPANY_ZIP","TOTAL_VIOLS","SERIOUS_VIOLS","OTHER_VIOLS","WILLFULL_VIOLS","REPEATED_VIOLS","UNCLASSIFIED_VIOLS")


#lets check if all the accidents in step 2 have a entry for violation
s<-(FINALACCID$ACTIVITY_ID %in% FINALVIOL$ACTIVITY_ID)
d<-which(s==TRUE)
#it looks like aproximately 300 out of the 1500 accidents do not have a violation. 
#This table will join the compnaies that have both violations and accidents. 

ACCIDVIOL<- inner_join(FINALACCID,FINALVIOL)

rm(VIOL)
rm(OSHA)

######################################## STEP 4 GRAPHICS     #############################################################

ZIP=read.csv2("ZIP.csv",sep=",",stringsAsFactors=T)#reading files containing US zip codes
ZIP<-filter(ZIP,State=="MA") #filtering MA
ZIP<-select(ZIP,Zipcode,City) #taking out columns that are not needed
colnames(ZIP)<-c("COMPANY_ZIP","CITY") #renaming columns
#in this case taking companies that have both accidents and violations
#(ASSUMPTION: companies with a lot of accidents have violations)
ACCIDVIOL.zip<-select(ACCIDVIOL,COMPANY_NAME,COMPANY_ZIP,TOTAL_ACC,TOTAL_VIOLS)
ACCIDVIOL.zip$COMPANY_ZIP<- as.integer(as.character(ACCIDVIOL.zip$COMPANY_ZIP))
ACCIDVIOL.zip<- inner_join(ACCIDVIOL.zip,ZIP) #joining zip codes to get cities
rm(ZIP)

ACCIDVIOL.zip<-group_by(ACCIDVIOL.zip,CITY)
CITY_ACC_VIOL<-select(ACCIDVIOL.zip,CITY,TOTAL_ACC,TOTAL_VIOLS)
CITY_ACC_VIOL$CITY<- as.character(CITY_ACC_VIOL$CITY)
#playing around trying to group them into one table, this might be useful for analysis

test1<-select(CITY_ACC_VIOL,CITY,TOTAL_ACC)
test1<-aggregate(test1$TOTAL_ACC,list(city=test1$CITY),sum)
colnames(test1)<-c("CITY","TOTAL_ACC")
test1<-arrange(test1,-test1$TOTAL_ACC)
test2<-select(CITY_ACC_VIOL,CITY,TOTAL_VIOLS)
test2<-aggregate(test2$TOTAL_VIOLS,list(city=test2$CITY),sum)
colnames(test2)<-c("CITY","TOTAL_VIOLS")
test2<-arrange(test2,-test2$TOTAL_VIOLS)

#barplot accidents
accibar<- test1[1:10,]
require(ggplot2)
ggplot(accibar)+aes(x=CITY,y=TOTAL_ACC)+geom_col()+coord_flip()

######################################## UNDER CONSTRUCTION (trying to map) ###############################################
###########################################################################################################################
################TEST
#install.packages("mapproj")
#install.packages("ggmap")
#install.packages("DeducerSpatial")
#require(maps)
#require(ggmap)
#map("state", "MASSACHUSETTS")
#data(us.cities)
#map.cities(us.cities, country = "MA")
#TEST<- select(FINALACCID,SITEZIP,ACCID_)
#TEST$SITEZIP<-as.numeric(TEST$SITEZIP)
#TEST$ACCID_<-as.numeric(TEST$ACCID_)

#TEST$colorBuckets <- as.numeric(cut(TEST$SITEZIP, c(0, 2, 4, 6, 8, 
#                                                   10, 100)))

#colorsmatched <- TEST$colorBuckets[match(TEST$SITEZIP,TEST$SITEZIP)]
#map(TEST, col = colors[colorsmatched], fill = TRUE, resolution = 0, 
#   lty = 0, projection = "polyconic")
