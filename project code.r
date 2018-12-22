
library(ggplot2)
library(arules)
library(arulesViz)
detach(package:tm, unload=TRUE)
library(e1071)
library(gridExtra)
library(kernlab)
library(zipcode)
library(ggmap)

#######################################################################################################################################################
# Reading Data from the Computer
#######################################################################################################################################################
#a <- read.csv('C:/Users/admin/Desktop/Raghav/Syracuse University/Semester 1/687 - Applied Data Science/Project/IST687-data/IST687-data/out-201404.csv',na.strings=c("","NA"))
#b <- read.csv('C:/Users/admin/Desktop/Raghav/Syracuse University/Semester 1/687 - Applied Data Science/Project/IST687-data/IST687-data/out-201405.csv',na.strings=c("","NA"))

a <- read.csv('C:/Users/admin/Desktop/Raghav/Syracuse University/Semester 1/687 - Applied Data Science/Project/IST687-data/IST687-data/realData201404.csv',na.strings=c("","NA"))
b <- read.csv('C:/Users/admin/Desktop/Raghav/Syracuse University/Semester 1/687 - Applied Data Science/Project/IST687-data/IST687-data/realData201405.csv',na.strings=c("","NA"))
d <- read.csv('C:/Users/admin/Desktop/Raghav/Syracuse University/Semester 1/687 - Applied Data Science/Project/IST687-data/IST687-data/realData201412.csv',na.strings=c("","NA"))
e <- read.csv('C:/Users/admin/Desktop/Raghav/Syracuse University/Semester 1/687 - Applied Data Science/Project/IST687-data/IST687-data/realData201501.csv',na.strings=c("","NA"))
c <- rbind(a,b,d,e)

realData <- data.frame(c,stringsAsFactors = FALSE)
#subset2 <- a[which(a$Country_PL=="United States"),]
View(realData)

#realData <- realData[-c(1:18,20:21,24:47,49:63,65:69,71:80,82:85,90:108,110:125,128:136,148:166,169,177,178,180:186,188:193,195:197,199:201,209,211,212,217,218,228:231,233:237)]
str(realData)
#View(realData)

#summary(realData$Property.Longitude_PL)
#gp <- ggplot(realData)+ borders("world", colour="black", fill="grey") + geom_point(aes(x=realData$Property.Longitude_PL,y= realData$Property.Latitude_PL))
#gp


realData <- realData[which(realData$Country_PL=="United States"),]
realData <- realData[!is.na(realData$Likelihood_Recommend_H),]

g0 <- ggplot(realData, aes(x=realData$NPS_Type)) + geom_bar(fill="blue", color = "green") 
g0 <- g0 + xlab("NPS Type") + ylab("Frequency")
g0 <- g0 + ggtitle("NPS Type Overall")
g0


##################################################################################################
# What is the purpose of Visit ?
##################################################################################################
leisure <- length(which(realData$POV_CODE_C=="LEISURE"))
Business <- length(which(realData$POV_CODE_C=="BUSINESS"))
leisurePer <- leisure/sum(leisure,Business)*100
BusinessPer <- Business/sum(leisure,Business)*100

povTable <- data.frame(tapply(realData$POV_CODE_C,list(realData$State_PL,realData$POV_CODE_C),length))
povTable$states <- rownames(povTable)
povTable <- povTable[!is.na(povTable$BUSINESS),]
povTable$lp <- -povTable$LEISURE/(povTable$LEISURE+povTable$BUSINESS) * 100
povTable$bp <- povTable$BUSINESS/(povTable$LEISURE+povTable$BUSINESS) * 100

# Exact Number of people travelling to different States w.r.t purpose
g <- ggplot(data=povTable,position="dodge")+geom_bar(aes(x=povTable$states,y=povTable$BUSINESS, fill="Business"), stat = "identity",width=0.8)
g <- g + geom_bar(aes(x=povTable$states,y=povTable$LEISURE, fill="Leisure"), stat = "identity",width=0.5)
g <- g + theme(axis.text.x = element_text(angle = 90,hjust = 1))
g <- g +ggtitle("Travelling for Business Vs Leisure") + xlab("City")+ylab("Percentage")
g

length(which(realData$State_PL=="Maine"))
length(which(realData$State_PL=="Arkansas"& realData$POV_CODE_C=="LEISURE"& realData$NPS_Type=="Promoter"))



# Percentage of people travelling to different states w.r.t purpose
gt <- ggplot(data=povTable,aes(fill=Purpose_of_Visit))+geom_bar(aes(x=povTable$states,y=povTable$bp, fill="Business"), stat = "identity")
gt <- gt + geom_bar(aes(x=povTable$states,y=povTable$lp, fill="Leisure"), stat = "identity")
gt <- gt + theme(axis.text.x = element_text(angle = 90,hjust = 1))
gt <- gt +ggtitle("Travelling for Business Vs Leisure") + xlab("State")+ylab("Percentage")
gt

#gn <- ggplot(data=realData,aes(x=realData$State_PL,y = (..count..)/sum(..count..)))+geom_bar(aes(fill=realData$POV_CODE_C),position = "dodge")
#gn <- gn + theme(axis.text.x = element_text(angle = 90,hjust = 1))
#gn <- gn +ggtitle("Travelling for Business Vs Leisure") + xlab("City")+ylab("Percentage")
#gn


################################################################################################
# Which hotel has the lowest and highest NPS in terms of Business Crowd and why ?
################################################################################################
#realData <- realData[!is.na(realData$Age_Range_H),]
leisTable <- data.frame(realData[which(realData$POV_CODE_C=="LEISURE"),])
busiTable <- data.frame(realData[which(realData$POV_CODE_C=="BUSINESS"),],stringsAsFactors = FALSE)

# Count the number of NPS TYPE in each parameter
busdTable <- data.frame(busiTable$Guest_Room_H,busiTable$Tranquility_H,busiTable$Condition_Hotel_H,busiTable$Customer_SVC_H,busiTable$Staff_Cared_H)
#busdTable <- round(busdTable,0)
#busdTable$busiTable.Guest_Room_H<-ifelse(busdTable$busiTable.Guest_Room_H< 7,"Low_Room",ifelse(busdTable$busiTable.Guest_Room_H >6 & busdTable$busiTable.Guest_Room_H<9,"Medium_Room",ifelse(busdTable$busiTable.Guest_Room_H>8,"High_Room",0)))  
#busdTable$busiTable.Tranquility_H<-ifelse(busdTable$busiTable.Tranquility_H< 7,"Low_Tran",ifelse(busdTable$busiTable.Tranquility_H >6 & busdTable$busiTable.Tranquility_H<9,"Medium_Tran",ifelse(busdTable$busiTable.Tranquility_H>8,"High_Tran",0)))
#busdTable$busiTable.Condition_Hotel_H<-ifelse(busdTable$busiTable.Condition_Hotel_H< 7,"Low_Hotel",ifelse(busdTable$busiTable.Condition_Hotel_H >6 & busdTable$busiTable.Condition_Hotel_H<9,"Medium_Hotel",ifelse(busdTable$busiTable.Condition_Hotel_H>8,"High_Hotel",0)))
#busdTable$busiTable.Customer_SVC_H<-ifelse(busdTable$busiTable.Customer_SVC_H< 7,"Low_Cust",ifelse(busdTable$busiTable.Customer_SVC_H >6 & busdTable$busiTable.Customer_SVC_H<9,"Medium_Cust",ifelse(busdTable$busiTable.Customer_SVC_H>8,"High_Cust",0)))
#busdTable$busiTable.Staff_Cared_H<-ifelse(busdTable$busiTable.Staff_Cared_H< 7,"Low_Staff",ifelse(busdTable$busiTable.Staff_Cared_H >6 & busdTable$busiTable.Staff_Cared_H<9,"Medium_Staff",ifelse(busdTable$busiTable.Staff_Cared_H>8,"High_Staff",0)))
busdTable[busdTable==0|busdTable==1|busdTable==2|busdTable==3|busdTable==4|busdTable==5|busdTable==6] <- "Low"
busdTable[busdTable==7|busdTable==8] <- "Medium"
busdTable[busdTable==9|busdTable==10] <- "High"
str(busdTable)
busdTable$NPS <- busiTable$NPS_Type
busdTable <- na.omit(busdTable)

colnames(busdTable) <- c("GR","Tr","HC","CS","SC","NPS")
busdTable$GR <- as.factor(busdTable$GR)
busdTable$Tr <- as.factor(busdTable$Tr)
busdTable$HC <- as.factor(busdTable$HC)
busdTable$CS<- as.factor(busdTable$CS)
busdTable$SC<- as.factor(busdTable$SC)
#busdTable$IS <- factor(busdTable$IS)
#busdTable$CH <- factor(busdTable$CH)
#busdTable$FB <- factor(busdTable$FB)
#busdTable$NPS <- factor(busdTable$NPS)

#tryDe <- busdTable[busdTable$NPS=="Detractor",]
#tryPro <- busdTable[busdTable$NPS=="Promoter",]

busdTable <- data.frame(busdTable, stringsAsFactors = TRUE)

abuTable <- data.frame(busiTable$OFFER_FLG_R,busiTable$Mini.Bar_PL,busiTable$Spa_PL,busiTable$Restaurant_PL,busiTable$Golf_PL,busiTable$Fitness.Center_PL,busiTable$Conference_PL,busiTable$Casino_PL,busiTable$Business.Center_PL,busiTable$NPS_Type)
colnames(abuTable) <- c("Offer","Mini Bar","Spa","Restaurant","Golf","Fitness Center","Conference Room","Casino","Business Center","NPS")
#apriori rules
# 
rules <- apriori(abuTable, parameter=list(support=0.002,confidence=0.7),appearance = list(rhs="NPS=Promoter"))

subset1 <- head(sort(rules,by="confidence"),100)
plot(subset1)
inspect(subset1)

#rules2 <- apriori(abuTable, parameter=list(support=0.0001,confidence=0.0),appearance = list(rhs="NPS=Detractor"))
#subset2 <- head(sort(rules2,by="count"),200)
#plot(subset2)
#inspect(subset2)

#inspect(rules2)
#plot(rules2, measure=c("support","lift"), shading="confidence")


# Tr does not matter much


GR <- tapply(busdTable$NPS,busdTable$GR,length)
Tr <- tapply(busdTable$Tr,busdTable$Tr,length)
HC <- tapply(busdTable$HC,busdTable$HC,length)
CS <- tapply(busdTable$CS,busdTable$CS,length)
SC <- tapply(busdTable$SC,busdTable$SC,length)
#IS <- tapply(busdTable$busiTable.Internet_Sat_H,busdTable$busiTable.Internet_Sat_H,length)
#CH <- tapply(busdTable$busiTable.Check_In_H,busdTable$busiTable.Check_In_H,length)
#FH <- tapply(busdTable$busiTable.F.B_Overall_Experience_H,busdTable$busiTable.F.B_Overall_Experience_H,length)

#g1 <- ggplot(busiTable, aes(x=busiTable$NPS_Type, y=(..count..)/sum(..count..))) + geom_bar(fill="light green", color = "dark green") 
#g1 <- g1 + xlab("NPS Type") + ylab("Frequency")
#g1 <- g1 + ggtitle("NPS in Business")
#g1

g1 <- ggplot(busiTable, aes(x=busiTable$NPS_Type)) + geom_bar(fill="light green", color = "dark green") 
g1 <- g1 + xlab("NPS Type") + ylab("Frequency")
g1 <- g1 + ggtitle("NPS Type in Business")
g1


#busiTable$NPS_Type <- as.numeric(busiTable$NPS_Type)
#str(busiTable$NPS_Type)
#View(busiTable)



# Convert each parameter into its NPS form
name <- c("Guest Room","Tranquility","Hotel Condition","Customer Service","Staff Caring")

cdf <- rbind(GR,Tr,HC,CS,SC)
cdf <- cbind(name,cdf)
cdf <- data.frame(cdf,stringsAsFactors = FALSE)
cdf$Detractor <- as.numeric(cdf$Detractor)
cdf$Passive <- as.numeric(cdf$Passive)
cdf$Promotor <- as.numeric(cdf$Promotor)

cdf$Dp <- (cdf$Detractor/(cdf$Detractor+cdf$Passive+cdf$Promotor)) * 100
cdf$Pap <- (cdf$Passive/(cdf$Detractor+cdf$Passive+cdf$Promotor)) * 100
cdf$Prp <- (cdf$Promotor/(cdf$Detractor+cdf$Passive+cdf$Promotor)) * 100
cdf <- cdf[-2:-4]
str(cdf)
rownames(cdf) <- c(1:nrow(cdf))
colnames(cdf) <- c("Names","Dp","Pap","Prp")

ggplot(cdf,aes(x=cdf$Names, group=1)) + geom_line(aes(y=cdf$Prp, color = "Promotor")) + geom_line(aes(y=cdf$Pap, color= "Passive")) + geom_line(aes(y=cdf$Dp,color= "Detractor"))

busP <- data.frame(tapply(busiTable$NPS_Type,list(busiTable$State_PL,busiTable$NPS_Type),length))
busP$state <- rownames(busP)
busP <- busP[!is.na(busP$Detractor),]
row.names(busP) <- c(1:nrow(busP))
busP$NPS <- (busP$Promoter/(busP$Detractor+busP$Passive+busP$Promoter)-busP$Detractor/(busP$Detractor+busP$Passive+busP$Promoter)) * 100
#View(busP)

g3 <- ggplot(busP,aes(x=state,y=NPS)) + geom_bar(aes(fill=NPS),stat = "identity") 
g3 <- g3 + theme(axis.text.x = element_text(angle = 90,hjust = 1))
g3 <- g3 + ggtitle("Net Promotor Score for Business")
g3


#busP[which.min(busP$NPS),4]

invesState <- busiTable[which(busiTable$State_PL=="California"),]
#View(invesState)

# Nothing:: 0.2145
cas <- invesState[!is.na(invesState$F.B_Overall_Experience_H),]
summary(cas$F.B_Overall_Experience_H)
FB <- lm(cas$Likelihood_Recommend_H~cas$F.B_Overall_Experience_H,data = cas)
summary(FB)

# Nothing:: 0.2342
ca <- invesState[!is.na(invesState$Check_In_H),]
summary(ca$Check_In_H)
CH <- lm(Likelihood_Recommend_H~Check_In_H,data = ca)
summary(CH)

############IMPORTANT######################################################

# MAJOR CONTRIBUTOR TOWARDS LIKELIHOOD TO RECOMEND
### A little :: 0.4242
c <- invesState[!is.na(invesState$Staff_Cared_H),]
summary(c$Staff_Cared_H)
SC <- lm(Likelihood_Recommend_H~Staff_Cared_H,data = c)
summary(SC)

# ANOTHER MAJOR CONTRIBUTOR TOWARDS CUSTOMER SATISFACTION
### A little Better:: 0.5001
co <- invesState[!is.na(invesState$Customer_SVC_H),]
summary(co$Customer_SVC_H)
CS <- lm(Likelihood_Recommend_H~Customer_SVC_H,data = co)
summary(CS)

# ANOTHER IMPORTANT FACTOR
### Good :: 0.5209
o <- invesState[!is.na(invesState$Condition_Hotel_H),]
summary(o$Condition_Hotel_H)
HC <- lm(Likelihood_Recommend_H~Condition_Hotel_H,data = o)
summary(HC)

# Important
## Fairly relevant : 0.498
com <- invesState[!is.na(invesState$Staff_Cared_H),]
o <- com[!is.na(com$Customer_SVC_H),]
summary(o$Customer_SVC_H)
summary(o$Staff_Cared_H)
SC <- lm(Likelihood_Recommend_H~Staff_Cared_H+Customer_SVC_H,data = o)
summary(SC)

### Great:: 0.6312
sf <- invesState[!is.na(invesState$Condition_Hotel_H),]
h <- sf[!is.na(sf$Customer_SVC_H),]
summary(h$Customer_SVC_H)
summary(h$Condition_Hotel_H)
SC <- lm(Likelihood_Recommend_H~Condition_Hotel_H+Customer_SVC_H,data = h)
summary(SC)


# Important
#### Great :: 0.6081
com <- invesState[!is.na(invesState$Staff_Cared_H),]
o <- com[!is.na(com$Condition_Hotel_H),]
co <- o[!is.na(o$Customer_SVC_H),]
ACOM <- lm(Likelihood_Recommend_H~Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = co)
summary(ACOM)

# Important
##### Great :: 0.6312
l <- invesState[!is.na(invesState$Condition_Hotel_H),]
h <- l[!is.na(l$Customer_SVC_H),]
OM <- lm(Likelihood_Recommend_H~Condition_Hotel_H+Customer_SVC_H,data = h)
summary(OM)
#View(co)
##########################################################################################

### R Squared :: 0.3787
Tr <- invesState[!is.na(invesState$Tranquility_H),]
summary(Tr$Tranquility_H)
TR <- lm(Likelihood_Recommend_H~Tranquility_H,data = Tr)
summary(TR)

### R Squared:: 0.08724
In <- invesState[!is.na(invesState$Internet_Sat_H),]
summary(In$Internet_Sat_H)
Int <- lm(Likelihood_Recommend_H~Internet_Sat_H,data = In)
summary(Int)

# Good :: 0.5446
GR <- invesState[!is.na(invesState$Guest_Room_H),]
summary(GR$Guest_Room_H)
GRO <- lm(Likelihood_Recommend_H~Guest_Room_H,data = GR)
summary(GRO)

# Good :: 0.601
G <- invesState[!is.na(invesState$Guest_Room_H),]
hc <- G[!is.na(G$Condition_Hotel_H),]
summary(hc$Guest_Room_H)
summary(hc$Condition_Hotel_H)
RO <- lm(Likelihood_Recommend_H~Guest_Room_H+Condition_Hotel_H,data = hc)
summary(RO)

## Great :: 0.6628
gu <- invesState[!is.na(invesState$Guest_Room_H),]
ser <- gu[!is.na(gu$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6195
gu <- invesState[!is.na(invesState$Guest_Room_H),]
ser <- gu[!is.na(gu$Staff_Cared_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H,data = ser)
summary(co)


## Great:: 0.6435
gu <- invesState[!is.na(invesState$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Staff_Cared_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6802
gu <- invesState[!is.na(invesState$Guest_Room_H),]
staff <- gu[!is.na(gu$Condition_Hotel_H),]
ser <- staff[!is.na(staff$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Staff_Cared_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Condition_Hotel_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.637
gu <- invesState[!is.na(invesState$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Staff_Cared_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Condition_Hotel_H,data = ser)
summary(co)

# Great :: 0.6621
gu <- invesState[!is.na(invesState$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
te <-  ser[!is.na(ser$Customer_SVC_H),]
summary(te$Guest_Room_H)
summary(te$Staff_Cared_H)
summary(te$Staff_Cared_H)
summary(te$Condition_Hotel_H)
summary(te$Tranquility_H)
co <- lm(Likelihood_Recommend_H~ Guest_Room_H+Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H+Tranquility_H,data = te)
summary(co)

gu <- invesState[!is.na(invesState$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
tran <- ser[!is.na(ser$Tranquility_H),]
te <-  tran[!is.na(tran$Customer_SVC_H),]
summary(te$Guest_Room_H)
summary(te$Staff_Cared_H)
summary(te$Staff_Cared_H)
summary(te$Condition_Hotel_H)
summary(te$Tranquility_H)
ci <- lm(Likelihood_Recommend_H~ Tranquility_H +Guest_Room_H+Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H+Tranquility_H,data = te)
summary(ci)




# Important
OS <- invesState[!is.na(invesState$Overall_Sat_H),]
summary(OS$Overall_Sat_H)
SO <- lm(Likelihood_Recommend_H~Overall_Sat_H,data = OS)
summary(SO)


gn <- ggplot(data=busP,aes(x=busP$state,y=busP$NPS)) + geom_bar(fill="White",color="Black",stat = "identity")
gn <- gn + theme(axis.text.x = element_text(angle = 90,hjust = 1))
gn <- gn +ggtitle("Statewise NPS") + xlab("States")+ylab("NPS")
gn
########################################################################################
########################################################################################

# Study the State New York and California During the placement on East Coast and West Coast
# New has less customers and still has lower NPS while Cali has more guest, still better NPS
#######################################################################################

invesState2 <- busiTable[which(busiTable$State_PL=="New York"),]
#View(invesState2)

## Not really:: 0.2167
cas <- invesState2[!is.na(invesState2$F.B_Overall_Experience_H),]
summary(cas$F.B_Overall_Experience_H)
FB <- lm(cas$Likelihood_Recommend_H~cas$F.B_Overall_Experience_H,data = cas)
summary(FB)

## Not Really:: 0.2453
ca <- invesState2[!is.na(invesState2$Check_In_H),]
summary(ca$Check_In_H)
CH <- lm(Likelihood_Recommend_H~Check_In_H,data = ca)
summary(CH)

############IMPORTANT######################################################

## Okay:: 0.4601
c <- invesState2[!is.na(invesState2$Staff_Cared_H),]
summary(c$Staff_Cared_H)
SC <- lm(Likelihood_Recommend_H~Staff_Cared_H,data = c)
summary(SC)

## Good:: 0.5407
co <- invesState2[!is.na(invesState2$Customer_SVC_H),]
summary(co$Customer_SVC_H)
CS <- lm(Likelihood_Recommend_H~Customer_SVC_H,data = co)
summary(CS)

## Good:: 0.5482
o <- invesState2[!is.na(invesState2$Condition_Hotel_H),]
summary(o$Condition_Hotel_H)
HC <- lm(Likelihood_Recommend_H~Condition_Hotel_H,data = o)
summary(HC)

###############################################################
## Fairly relevant : 0.5481
com <- invesState2[!is.na(invesState2$Staff_Cared_H),]
o <- com[!is.na(com$Customer_SVC_H),]
summary(o$Customer_SVC_H)
summary(o$Staff_Cared_H)
SC <- lm(Likelihood_Recommend_H~Staff_Cared_H+Customer_SVC_H,data = o)
summary(SC)

### Great:: 0.6589
sf <- invesState2[!is.na(invesState2$Condition_Hotel_H),]
h <- sf[!is.na(sf$Customer_SVC_H),]
summary(h$Customer_SVC_H)
summary(h$Condition_Hotel_H)
SC <- lm(Likelihood_Recommend_H~Condition_Hotel_H+Customer_SVC_H,data = h)
summary(SC)

#### Great :: 0.6487
com <- invesState2[!is.na(invesState2$Staff_Cared_H),]
o <- com[!is.na(com$Condition_Hotel_H),]
co <- o[!is.na(o$Customer_SVC_H),]
ACOM <- lm(Likelihood_Recommend_H~Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = co)
summary(ACOM)

##### Great :: 0.6589
l <- invesState2[!is.na(invesState2$Condition_Hotel_H),]
h <- l[!is.na(l$Customer_SVC_H),]
OM <- lm(Likelihood_Recommend_H~Condition_Hotel_H+Customer_SVC_H,data = h)
summary(OM)
##############################################################################################

## Okay:: 0.3937
Tr <- invesState2[!is.na(invesState2$Tranquility_H),]
summary(Tr$Tranquility_H)
TR <- lm(Likelihood_Recommend_H~Tranquility_H,data = Tr)
summary(TR)

## Not Good:: 0.09425
In2 <- invesState2[!is.na(invesState2$Internet_Sat_H),]
summary(In2$Internet_Sat_H)
Int2 <- lm(Likelihood_Recommend_H~Internet_Sat_H,data = In2)
summary(Int2)

## Good:: 0.5744
GR <- invesState2[!is.na(invesState2$Guest_Room_H),]
summary(GR$Guest_Room_H)
GRO <- lm(Likelihood_Recommend_H~Guest_Room_H,data = GR)
summary(GRO)
####################################################################

# Good :: 0.6298
G <- invesState2[!is.na(invesState2$Guest_Room_H),]
hc <- G[!is.na(G$Condition_Hotel_H),]
summary(hc$Guest_Room_H)
summary(hc$Condition_Hotel_H)
RO <- lm(Likelihood_Recommend_H~Guest_Room_H+Condition_Hotel_H,data = hc)
summary(RO)


## Great :: 0.698
gu <- invesState2[!is.na(invesState2$Guest_Room_H),]
ser <- gu[!is.na(gu$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6602
gu <- invesState2[!is.na(invesState2$Guest_Room_H),]
ser <- gu[!is.na(gu$Staff_Cared_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H,data = ser)
summary(co)


## Great:: 0.6868
gu <- invesState2[!is.na(invesState2$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Staff_Cared_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.7108
gu <- invesState2[!is.na(invesState2$Guest_Room_H),]
staff <- gu[!is.na(gu$Condition_Hotel_H),]
ser <- staff[!is.na(staff$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Condition_Hotel_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Condition_Hotel_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6731
gu <- invesState2[!is.na(invesState2$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Condition_Hotel_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Condition_Hotel_H,data = ser)
summary(co)


gu <- invesState2[!is.na(invesState2$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
te <-  ser[!is.na(ser$Customer_SVC_H),]
summary(te$Guest_Room_H)
summary(te$Staff_Cared_H)
summary(te$Customer_SVC_H)
summary(te$Condition_Hotel_H)
summary(te$Tranquility_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = te)
summary(co)




# Great :: 0.6945
gu <- invesState2[!is.na(invesState2$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
tran <- ser[!is.na(ser$Tranquility_H),] 
te <-  tran[!is.na(tran$Customer_SVC_H),]
summary(te$Guest_Room_H)
summary(te$Staff_Cared_H)
summary(te$Customer_SVC_H)
summary(te$Condition_Hotel_H)
summary(te$Tranquility_H)
co <- lm(Likelihood_Recommend_H~Tranquility_H+Guest_Room_H+Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = te)
summary(co)

############ Whole Business  # 65.78
gu <- busiTable[!is.na(busiTable$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
tran <- ser[!is.na(ser$Tranquility_H),] 
te <-  tran[!is.na(tran$Customer_SVC_H),]
summary(te$Guest_Room_H)
summary(te$Staff_Cared_H)
summary(te$Customer_SVC_H)
summary(te$Condition_Hotel_H)
summary(te$Tranquility_H)
co <- lm(Likelihood_Recommend_H~Tranquility_H+Guest_Room_H+Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = te)
summary(co)

fd <- data.frame(summary(co)$coefficients)
fd <- fd[c(-1,-7),]
fd$parameters <- rownames(fd)
rownames(fd) <- c(1:nrow(fd))

Ps <- ggplot(fd,aes(x=parameters,y=Estimate,fill=Value)) + geom_bar(stat = "identity",aes(fill=Estimate))
Ps <- Ps + theme(axis.text.x = element_text(angle = 90,hjust = 1))
Ps <- Ps + ggtitle("Estimate for Linear Modelling in Business") + xlab("Parameters") + ylab("Value")
Ps <- Ps + scale_fill_gradient(low="light yellow",high="light green")
Ps


####################################################################
# Important
OS <- invesState2[!is.na(invesState2$Overall_Sat_H),]
summary(OS$Overall_Sat_H)
SO <- lm(Likelihood_Recommend_H~Overall_Sat_H,data = OS)
summary(SO)

################################################################################################
# Study the same states New York and Californai  for leisure
################################################################################################


leisP <- data.frame(tapply(leisTable$NPS_Type,list(leisTable$State_PL,leisTable$NPS_Type),length))
leisP$state <- rownames(leisP)
leisP <- leisP[!is.na(leisP$Detractor),]
row.names(leisP) <- c(1:nrow(leisP))
leisP$NPS <- (leisP$Promoter/(leisP$Detractor+leisP$Passive+leisP$Promoter)-leisP$Detractor/(leisP$Detractor+leisP$Passive+leisP$Promoter)) * 100
View(leisP)

gn <- ggplot(data=leisP,aes(x=leisP$state,y=leisP$NPS)) + geom_bar(aes(fill=NPS),color="Black",stat = "identity")
gn <- gn + theme(axis.text.x = element_text(angle = 90,hjust = 1))
gn <- gn +ggtitle("Statewise NPS For Leisure") + xlab("States")+ylab("NPS")
gn

g2 <- ggplot(leisTable, aes(x=leisTable$NPS_Type)) + geom_bar(fill="red", color = "dark green") 
g2 <- g2 + xlab("NPS Type") + ylab("Frequency")
g2 <- g2 + ggtitle("NPS Type in Leisure")
g2

# Study the State
invesStateL1 <- leisTable[which(leisTable$State_PL=="California"),]

## Okay:: 0.2428
cas <- invesStateL1[!is.na(invesStateL1$F.B_Overall_Experience_H),]
summary(cas$F.B_Overall_Experience_H)
FB <- lm(cas$Likelihood_Recommend_H~cas$F.B_Overall_Experience_H,data = cas)
summary(FB)

## Nothing :: 0.1924
ca <- invesStateL1[!is.na(invesStateL1$Check_In_H),]
summary(ca$Check_In_H)
CH <- lm(Likelihood_Recommend_H~Check_In_H,data = ca)
summary(CH)

## Good:: 0.4533
c <- invesStateL1[!is.na(invesStateL1$Staff_Cared_H),]
summary(c$Staff_Cared_H)
SC <- lm(Likelihood_Recommend_H~Staff_Cared_H,data = c)
summary(SC)

## Good :: 0.5485
co <- invesStateL1[!is.na(invesStateL1$Customer_SVC_H),]
summary(co$Customer_SVC_H)
CS <- lm(Likelihood_Recommend_H~Customer_SVC_H,data = co)
summary(CS)

## Good:: 0.5227
o <- invesStateL1[!is.na(invesStateL1$Condition_Hotel_H),]
summary(o$Condition_Hotel_H)
HC <- lm(Likelihood_Recommend_H~Condition_Hotel_H,data = o)
summary(HC)

## Good:: 0.5709
com <- invesStateL1[!is.na(invesStateL1$Staff_Cared_H),]
o <- com[!is.na(com$Condition_Hotel_H),]
SC <- lm(Likelihood_Recommend_H~Staff_Cared_H+Condition_Hotel_H,data = o)
summary(SC)

## Great:: 0.6085
com <- invesStateL1[!is.na(invesStateL1$Staff_Cared_H),]
o <- com[!is.na(com$Condition_Hotel_H),]
co <- o[!is.na(o$Customer_SVC_H),]
ACOM <- lm(Likelihood_Recommend_H~Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = co)
summary(ACOM)

## Great:: 0.6335
l <- invesStateL1[!is.na(invesStateL1$Condition_Hotel_H),]
h <- l[!is.na(l$Customer_SVC_H),]
OM <- lm(Likelihood_Recommend_H~Condition_Hotel_H+Customer_SVC_H,data = h)
summary(OM)

## Good:: 0.5312
S <- invesStateL1[!is.na(invesStateL1$Staff_Cared_H),]
SV <- S[!is.na(S$Customer_SVC_H),]
OM <- lm(Likelihood_Recommend_H~Staff_Cared_H+Customer_SVC_H,data = SV)
summary(OM)

#View(co)
##########################################################################################

## Okay:: 0.3188
Tr <- invesStateL1[!is.na(invesStateL1$Tranquility_H),]
summary(Tr$Tranquility_H)
TR <- lm(Likelihood_Recommend_H~Tranquility_H,data = Tr)
summary(TR)

## Nothing:: 0.142
InL1 <- invesStateL1[!is.na(invesStateL1$Internet_Sat_H),]
summary(InL1$Internet_Sat_H)
IntL1 <- lm(Likelihood_Recommend_H~Internet_Sat_H,data = InL1)
summary(IntL1)

## Good:: 0.5491
GR <- invesStateL1[!is.na(invesStateL1$Guest_Room_H),]
summary(GR$Guest_Room_H)
GRO <- lm(Likelihood_Recommend_H~Guest_Room_H,data = GR)
summary(GRO)

# Good :: 0.6003
G <- invesStateL1[!is.na(invesStateL1$Guest_Room_H),]
hc <- G[!is.na(G$Condition_Hotel_H),]
summary(hc$Guest_Room_H)
summary(hc$Condition_Hotel_H)
RO <- lm(Likelihood_Recommend_H~Guest_Room_H+Condition_Hotel_H,data = hc)
summary(RO)


## Great :: 0.6842
gu <- invesStateL1[!is.na(invesStateL1$Guest_Room_H),]
ser <- gu[!is.na(gu$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6348
gu <- invesStateL1[!is.na(invesStateL1$Guest_Room_H),]
ser <- gu[!is.na(gu$Staff_Cared_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H,data = ser)
summary(co)


## Great:: 0.6647
gu <- invesStateL1[!is.na(invesStateL1$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6977
gu <- invesStateL1[!is.na(invesStateL1$Guest_Room_H),]
staff <- gu[!is.na(gu$Condition_Hotel_H),]
ser <- staff[!is.na(staff$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Condition_Hotel_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Condition_Hotel_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6408
gu <- invesStateL1[!is.na(invesStateL1$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Condition_Hotel_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Condition_Hotel_H,data = ser)
summary(co)

# Great :: 0.6674
gu <- invesStateL1[!is.na(invesStateL1$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
te <-  ser[!is.na(ser$Customer_SVC_H),]
tran <- te[!is.na(te$Tranquility_H),]
summary(te$Guest_Room_H)
summary(te$Staff_Cared_H)
summary(te$Customer_SVC_H)
summary(te$Condition_Hotel_H)
co <- lm(Likelihood_Recommend_H~Tranquility_H+Guest_Room_H+Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = tran)
summary(co)


OS <- invesStateL1[!is.na(invesStateL1$Overall_Sat_H),]
summary(OS$Overall_Sat_H)
SO <- lm(Likelihood_Recommend_H~Overall_Sat_H,data = OS)
summary(SO)


########################################################################################
########################################################################################

# Next State
#######################################################################################

invesStateL2 <- leisTable[which(leisTable$State_PL=="New York"),]

######## NOPE ################# MAJOR CONTRIBUTOR #######################################
## Nothing:: 0.2646
cas <- invesStateL2[!is.na(invesStateL2$F.B_Overall_Experience_H),]
summary(cas$F.B_Overall_Experience_H)
FB <- lm(cas$Likelihood_Recommend_H~cas$F.B_Overall_Experience_H,data = cas)
summary(FB)

## Nothing:: 0.3174
ca <- invesStateL2[!is.na(invesStateL2$Check_In_H),]
summary(ca$Check_In_H)
CH <- lm(Likelihood_Recommend_H~Check_In_H,data = ca)
summary(CH)

## Good:: 0.4502
c <- invesStateL2[!is.na(invesStateL2$Staff_Cared_H),]
summary(c$Staff_Cared_H)
SC <- lm(Likelihood_Recommend_H~Staff_Cared_H,data = c)
summary(SC)

## Good:: 0.5147
co <- invesStateL2[!is.na(invesStateL2$Customer_SVC_H),]
summary(co$Customer_SVC_H)
CS <- lm(Likelihood_Recommend_H~Customer_SVC_H,data = co)
summary(CS)

## Good:: 0.5548
o <- invesStateL2[!is.na(invesStateL2$Condition_Hotel_H),]
summary(o$Condition_Hotel_H)
HC <- lm(Likelihood_Recommend_H~Condition_Hotel_H,data = o)
summary(HC)

## Good:: 0.5812
com <- invesStateL2[!is.na(invesStateL2$Staff_Cared_H),]
o <- com[!is.na(com$Condition_Hotel_H),]
SC <- lm(Likelihood_Recommend_H~Staff_Cared_H+Condition_Hotel_H,data = o)
summary(SC)

## Great:: 0.6267
com <- invesStateL2[!is.na(invesStateL2$Staff_Cared_H),]
o <- com[!is.na(com$Condition_Hotel_H),]
co <- o[!is.na(o$Customer_SVC_H),]
ACOM <- lm(Likelihood_Recommend_H~Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = co)
summary(ACOM)

## Great:: 0.6642
l <- invesStateL2[!is.na(invesStateL2$Condition_Hotel_H),]
h <- l[!is.na(l$Customer_SVC_H),]
OM <- lm(Likelihood_Recommend_H~Condition_Hotel_H+Customer_SVC_H,data = h)
summary(OM)

## Good:: 0.5336
S <- invesStateL2[!is.na(invesStateL2$Staff_Cared_H),]
SV <- S[!is.na(S$Customer_SVC_H),]
OM <- lm(Likelihood_Recommend_H~Staff_Cared_H+Customer_SVC_H,data = SV)
summary(OM)

#View(co)
##########################################################################################

## Okay:: 0.3692
Tr <- invesStateL2[!is.na(invesStateL2$Tranquility_H),]
summary(Tr$Tranquility_H)
TR <- lm(Likelihood_Recommend_H~Tranquility_H,data = Tr)
summary(TR)

## Nothing:: 0.1229
InL1 <- invesStateL2[!is.na(invesStateL2$Internet_Sat_H),]
summary(InL1$Internet_Sat_H)
IntL1 <- lm(Likelihood_Recommend_H~Internet_Sat_H,data = InL1)
summary(IntL1)

## Good:: 0.6198
GR <- invesStateL2[!is.na(invesStateL2$Guest_Room_H),]
summary(GR$Guest_Room_H)
GRO <- lm(Likelihood_Recommend_H~Guest_Room_H,data = GR)
summary(GRO)

# Good :: 0.6531
G <- invesStateL2[!is.na(invesStateL2$Guest_Room_H),]
hc <- G[!is.na(G$Condition_Hotel_H),]
summary(hc$Guest_Room_H)
summary(hc$Condition_Hotel_H)
RO <- lm(Likelihood_Recommend_H~Guest_Room_H+Condition_Hotel_H,data = hc)
summary(RO)


## Great :: 0.7283
gu <- invesStateL2[!is.na(invesStateL2$Guest_Room_H),]
ser <- gu[!is.na(gu$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6737
gu <- invesStateL2[!is.na(invesStateL2$Guest_Room_H),]
ser <- gu[!is.na(gu$Staff_Cared_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H,data = ser)
summary(co)


## Great:: 0.6969
gu <- invesStateL2[!is.na(invesStateL2$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.7345
gu <- invesStateL2[!is.na(invesStateL2$Guest_Room_H),]
staff <- gu[!is.na(gu$Condition_Hotel_H),]
ser <- staff[!is.na(staff$Customer_SVC_H),]
summary(ser$Guest_Room_H)
summary(ser$Condition_Hotel_H)
summary(ser$Customer_SVC_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Condition_Hotel_H+Customer_SVC_H,data = ser)
summary(co)

## Great:: 0.6751
gu <- invesStateL2[!is.na(invesStateL2$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
summary(ser$Guest_Room_H)
summary(ser$Staff_Cared_H)
summary(ser$Condition_Hotel_H)
co <- lm(Likelihood_Recommend_H~Guest_Room_H+Staff_Cared_H+Condition_Hotel_H,data = ser)
summary(co)

# Great :: 0.6974
gu <- invesStateL2[!is.na(invesStateL2$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
te <-  ser[!is.na(ser$Customer_SVC_H),]
tran <- te[!is.na(te$Tranquility_H),]
summary(te$Guest_Room_H)
summary(te$Staff_Cared_H)
summary(te$Customer_SVC_H)
summary(te$Condition_Hotel_H)
co <- lm(Likelihood_Recommend_H~Tranquility_H+Guest_Room_H+Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = tran)
summary(co)


########### For Leisure ## 0.6609

gu <- leisTable[!is.na(leisTable$Guest_Room_H),]
staff <- gu[!is.na(gu$Staff_Cared_H),]
ser <- staff[!is.na(staff$Condition_Hotel_H),]
tran <- ser[!is.na(ser$Tranquility_H),]
te <-  tran[!is.na(tran$Customer_SVC_H),]
summary(te$Guest_Room_H)
summary(te$Staff_Cared_H)
summary(te$Customer_SVC_H)
summary(te$Tranquility_H)
summary(te$Condition_Hotel_H)
cok <- lm(Likelihood_Recommend_H~Tranquility_H+Guest_Room_H+Staff_Cared_H+Condition_Hotel_H+Customer_SVC_H,data = te)
summary(cok)


#sd <- data.frame(summary(FB)$Coefficients,summary(CH)$Coefficients,summary(SC)$Coefficients,summary(CS)$Coefficients,summary(HC)$Coefficients,summary(GR)$Coefficients)
sd <- data.frame(summary(cok)$coefficients)
sd <- sd[c(-1,-7),]
sd$parameters <- rownames(sd)
rownames(sd) <- c(1:nrow(sd))

Es <- ggplot(sd,aes(x=sd$parameters,y=sd$Estimate,fill=Value)) + geom_bar(stat = "identity",aes(fill=sd$Estimate))
Es <- Es + theme(axis.text.x = element_text(angle = 90,hjust = 1))
Es <- Es + ggtitle("Estimate for Linear Modelling in Leisure") + xlab("Parameters") + ylab("Value")
Es <- Es + scale_fill_gradient(low="grey",high="light green")
Es


# Important
OS <- invesStateL2[!is.na(invesStateL2$Overall_Sat_H),]
summary(OS$Overall_Sat_H)
SO <- lm(Likelihood_Recommend_H~Overall_Sat_H,data = OS)
summary(SO)

##########################################################################################


leisdTable <- data.frame(leisTable$Guest_Room_H,leisTable$Tranquility_H,leisTable$Condition_Hotel_H,leisTable$Customer_SVC_H,leisTable$Staff_Cared_H)

leisdTable[leisdTable==0|leisdTable==1|leisdTable==2|leisdTable==3|leisdTable==4|leisdTable==5|leisdTable==6] <- "Low"
leisdTable[leisdTable==7|leisdTable==8] <- "Medium"
leisdTable[leisdTable==9|leisdTable==10] <- "High"
View(leisdTable)
leisdTable$NPS <- leisTable$NPS_Type
leisdTable <- na.omit(leisdTable)

colnames(leisdTable) <- c("GR","Tr","HC","CS","SC","NPS")
leisdTable$GR <- as.factor(leisdTable$GR)
leisdTable$Tr <- as.factor(leisdTable$Tr)
leisdTable$HC <- as.factor(leisdTable$HC)
leisdTable$CS<- as.factor(leisdTable$CS)
leisdTable$SC<- as.factor(leisdTable$SC)

###############################################################apriori rules
aleiTable <- data.frame(leisTable$OFFER_FLG_R,leisTable$Mini.Bar_PL,leisTable$Spa_PL,leisTable$Restaurant_PL,leisTable$Golf_PL,leisTable$Fitness.Center_PL,leisTable$Conference_PL,leisTable$Casino_PL,leisTable$Business.Center_PL,leisTable$NPS_Type)
colnames(aleiTable) <- c("Offer","Mini Bar","Spa","Restaurant","Golf","Fitness Center","Conference Room","Casino","Business Center","NPS")
#apriori rules
# 
rulesL <- apriori(aleiTable, parameter=list(support=0.002,confidence=0.7),appearance = list(rhs="NPS=Promoter"))

subsetL <- head(sort(rulesL,by="confidence"),200)
plot(subsetL)
inspect(subsetL)




#rules <- apriori(leisdTable, parameter=list(support=0.002,confidence=0.7),appearance = list(rhs="NPS=Promoter"))
#inspect(rules)
#plot(rules, measure=c("support","lift"), shading="confidence")

#rules2 <- apriori(leisdTable, parameter=list(support=0.002,confidence=0.7),appearance = list(rhs="NPS=Detractor"))
#rules
#inspect(rules2)
#plot(rules2, measure=c("support","lift"), shading="confidence")


##########################################################################################

#View(leisTable)
#plot(realData$POV_CODE_C,realData$Likelihood_Recommend_H)
#fSw <- lm(formula = like$Likelihood_Recommend_H~like$POV_CODE_C,data=like)
#summary(fSw)
# r Squared: 0.00021 ::adjusted r squared:0.00025

age <- realData[!is.na(realData$Age_Range_H),]
age <- realData[which(realData$Age_Range_H!=""),]
summary(realData$Age_Range_H)

fSw <- lm(formula = Likelihood_Recommend_H~Age_Range_H,data=age)
summary(fSw)

stay <- realData[!is.na(realData$LENGTH_OF_STAY_C),]
#summary(stay$LENGTH_OF_STAY_C)
fSw <- lm(formula = Likelihood_Recommend_H~LENGTH_OF_STAY_C,data=stay)
summary(fSw)



CN <- realData[!is.na(realData$Shuttle.Service_PL),]
summary(CN$Casino_PL)
fSw <- lm(formula = Likelihood_Recommend_H~Shuttle.Service_PL,data=CN)
summary(fSw)

RTD <- realData[!is.na(realData$Overall_Sat_H),]
summary(RTD$Overall_Sat_H)
fSw <- lm(formula = Likelihood_Recommend_H~Overall_Sat_H,data=RTD)
summary(fSw)

OS <- realData[!is.na(realData$Guest_Room_H),]
summary(OS$Guest_Room_H)
fSw <- lm(formula = Likelihood_Recommend_H~Guest_Room_H,data=OS)
summary(fSw)

TQ <- realData[!is.na(realData$Tranquility_H),]
summary(TQ$Tranquility_H)
fSw <- lm(formula = Likelihood_Recommend_H~Tranquility_H,data=TQ)
summary(fSw)

HC <- realData[!is.na(realData$Condition_Hotel_H),]
summary(HC$Condition_Hotel_H)
fSw <- lm(formula = Likelihood_Recommend_H~Condition_Hotel_H,data=HC)
summary(fSw)

CA <- realData[!is.na(realData$Condition_Hotel_H),]
CA <- CA[!is.na(CA$Customer_SVC_H),]
summary(CS$Staff_Cared_H)
fS <- lm(formula = Likelihood_Recommend_H~Guest_Room_H+Customer_SVC_H+Condition_Hotel_H,data=realData)
summary(fS)

F2 <- realData[!is.na(realData$Staff_Cared_H),]
F2 <- F2[!is.na(realData$Condition_Hotel_H),]
F2 <- F2[!is.na(realData$Customer_SVC_H),]
F2 <- F2[!is.na(realData$Guest_Room_H),]
F2 <- F2[!is.na(realData$Tranquility_H),]

fSw <- lm(formula = Likelihood_Recommend_H~Staff_Cared_H+Customer_SVC_H+Condition_Hotel_H+Guest_Room_H+Tranquility_H,data=F2)
summary(fSw)

rd <- data.frame(summary(fSw)$coefficients)
rd <- rd[c(-1,-7),]
rd$parameters <- rownames(rd)
rownames(rd) <- c(1:nrow(rd))

Rs <- ggplot(rd,aes(x=parameters,y=Estimate,fill=Value)) + geom_bar(stat = "identity",aes(fill=Estimate))
Rs <- Rs + theme(axis.text.x = element_text(angle = 90,hjust = 1))
Rs <- Rs + ggtitle("Estimate for Linear Modelling OverAll") + xlab("Parameters") + ylab("Value")
Rs <- Rs + scale_fill_gradient(low="grey",high="sky blue")
Rs

grid.arrange(arrangeGrob(Rs,ncol=1,nrow=1,respect = TRUE),arrangeGrob(Ps,Es,ncol=2,nrow = 1,respect = TRUE))


SC <- realData[!is.na(realData$Staff_Cared_H),]
summary(SC$Staff_Cared_H)
fSw <- lm(formula = Likelihood_Recommend_H~Staff_Cared_H,data=SC)
summary(fSw)

IS <- realData[!is.na(realData$Internet_Sat_H),]
summary(IS$Internet_Sat_H)
fSw <- lm(formula = Likelihood_Recommend_H~Internet_Sat_H,data=IS)
summary(fSw)

CI <- realData[!is.na(realData$Check_In_H),]
summary(CI$Check_In_H)
fSw <- lm(formula = Likelihood_Recommend_H~Check_In_H,data=CI)
summary(fSw)

FB <- realData[!is.na(realData$F.B_Overall_Experience_H),]
summary(FB$F.B_Overall_Experience_H)
fSw <- lm(formula = Likelihood_Recommend_H~F.B_Overall_Experience_H,data=FB)
summary(fSw)

summary(realData$State_PL)


#abline(fSw)

#write.csv(realData,'realData201501.csv')
################################################################################################
# SVM
################################################################################################rand_index <- sample(1:dim(aq)[1])
#sampleTable <- busdTable
#View(sampleTable)
#sampleTable <- na.omit(sampleTable)
#rand_index <- sample(1:dim(sampleTable)[1])
#create2_3 <- floor(2 * dim(sampleTable)[1]/3)
#create2_3
#train_data <- sampleTable[rand_index[1:create2_3],]
#test_data <- sampleTable[rand_index[create2_3:dim(sampleTable)[1]],]

#sampleTable2 <- leisdTable
#View(sampleTable2)
#sampleTable2 <- na.omit(sampleTable2)
#rand_index2 <- sample(1:dim(sampleTable2)[1])
#create2_3_2 <- floor(2 * dim(sampleTable2)[1]/3)
#create2_3_2
#train_data2 <- sampleTable2[rand_index2[1:create2_3_2],]
#test_data2 <- sampleTable2[rand_index2[create2_3_2:dim(sampleTable2)[1]],]



#View(train_data)
# 0.1896
#t1 <- ksvm(train_data$NPS~.,data=train_data,kernel="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
#t1
# 0.1737
#t2 <- ksvm(train_data2$NPS~.,data=train_data2,kernel="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
#t2

#n <- naiveBayes(train_data$NPS~.,data=train_data)
#n

#n2 <- naiveBayes(train_data2$NPS~.,data=train_data2)
#n2

#t1p <- predict(t1,test_data,type="votes")

#error <- test_data$realData.Guest_Room_H - t1p 
#e <- sqrt(sum(error^2)/(nrow(test_data)-1))

grid.arrange(arrangeGrob(g1,g2,ncol=2,nrow = 1),arrangeGrob(g0,ncol=1,nrow=1))

# NPS Score for Business and Leisure
grid.arrange(gn,g3)


gc <- ggplot()
gc <- gc + geom_bar(aes(x=busiTable$NPS_Type,y=(..count..)/sum(..count..),fill="Business"),width = 0.8) 
gc <- gc + geom_bar(aes(x=leisTable$NPS_Type,y=(..count..)/sum(..count..),fill="Leisure"), width = 0.7) 
gc <- gc + geom_bar(aes(x=realData$NPS_Type,y=(..count..)/sum(..count..), fill="Overall"), width = 0.6)
gc <- gc + xlab("NPS Type") + ylab("Percentage")
gc <- gc + ggtitle("Comparison of Overall NPS Type with Business and Leisure ")
gc

us <- map_data("state")
us

#d <- data.frame(tapply(realData$Likelihood_Recommend_H,realData$State_PL,mean))
#d$State <- rownames(d)
#colnames(d) <- c("like","State")
#str(d)
#summary(busiTable$State_PL)

busP$state <- tolower(busP$state)
#busiTable$Likelihood_Recommend_H <- as.numeric(busiTable$Likelihood_Recommend_H)
#str(busiTable)


gh <- ggplot(busP,aes(map_id = state)) 
gh <- gh + geom_map(map= us,aes(fill=busP$NPS), color="black")
#gh <- gh + geom_point(aes(x=realData$Property.Longitude_PL,y=realData$Property.Latitude_PL))
gh <- gh + expand_limits(x=us$long,y=us$lat) + coord_map() + ggtitle("NPS for each state based on business purpose")
gh

leisP$state <- tolower(leisP$state)
#leisTable$Likelihood_Recommend_H <- as.numeric(leisTable$Likelihood_Recommend_H)

gl <- ggplot(leisP,aes(map_id = state)) 
gl <- gl + geom_map(map= us,aes(fill=leisP$NPS), color="black")
#gh <- gh + geom_point(aes(x=realData$Property.Longitude_PL,y=realData$Property.Latitude_PL))
gl <- gl + expand_limits(x=us$long,y=us$lat) + coord_map() + ggtitle("NPS for each state based for leisure purpose")
gl

#realData$State_PL <- tolower(realData$State_PL)
#gp <- ggplot(realData,aes(map_id = State_PL)) 
#gp <- gp + geom_map(map= us,fill="white", color="black")
#gp <- gp + geom_point(aes(x=realData$Property.Longitude_PL,y=realData$Property.Latitude_PL))
#gp <- gp + expand_limits(x=us$long,y=us$lat) + coord_map() + ggtitle("NPS Type for each state based for leisure purpose") + xlim(-130,-60)
#gp



realP <- data.frame(tapply(realData$NPS_Type,list(realData$State_PL,realData$NPS_Type),length))
realP$state <- rownames(realP)
realP <- realP[!is.na(realP$Detractor),]
row.names(realP) <- c(1:nrow(realP))
realP$NPS <- (realP$Promoter/(realP$Detractor+realP$Passive+realP$Promoter)-realP$Detractor/(realP$Detractor+realP$Passive+realP$Promoter)) * 100


realP$state <- tolower(realP$state)

rl <- ggplot(realP,aes(map_id = state)) 
rl <- rl + geom_map(map= us,aes(fill=realP$NPS), color="black")
rl <- rl + expand_limits(x=us$long,y=us$lat) + coord_map() + ggtitle("NPS for each state")
rl

grid.arrange(arrangeGrob(rl,ncol=1,nrow=1),arrangeGrob(gl,gh,ncol=2,nrow = 1))

ty <- ggplot(realP,aes(x=realP$state,y=realP$NPS,fill=Value))
ty <- ty + geom_bar(aes(fill=realP$NPS),stat = "identity") 
ty <- ty + theme(axis.text.x = element_text(angle = 90,hjust = 1))
ty <- ty + ggtitle("Net Promotor Score for each State") + xlab("State") + ylab("Net Promotor Score")
ty

ggplot(realData,aes(x=realData$Age_Range_H)) + geom_bar() + xlab("Age") + ggtitle("Overall Population")
ggplot(busiTable,aes(x=busiTable$Age_Range_H)) +geom_bar()+ xlab("Age") + ggtitle("Customers for Business")
ggplot(leisTable,aes(x=leisTable$Age_Range_H)) + geom_bar()+ xlab("Age") + ggtitle("Customers for Leisure")

ggplot(realData,aes(x=realData$GROUPS_VS_FIT_R)) + geom_bar() + xlab("Group") + ggtitle("Overall Population")
ggplot(busiTable,aes(x=busiTable$GROUPS_VS_FIT_R)) +geom_bar()+ xlab("Age") + ggtitle("Customers for Business")
ggplot(leisTable,aes(x=leisTable$GROUPS_VS_FIT_R)) + geom_bar()+ xlab("Age") + ggtitle("Customers for Leisure")





RTable <- data.frame(realData$Guest_Room_H,realData$Tranquility_H,realData$Condition_Hotel_H,realData$Customer_SVC_H,realData$Staff_Cared_H)
RTable$NPS <- realData$NPS_Type
RTable <- na.omit(RTable)

RTable[RTable==0|RTable==1|RTable==2|RTable==3|RTable==4|RTable==5|RTable==6] <- "Low"
RTable[RTable==7|RTable==8] <- "Medium"
RTable[RTable==9|RTable==10] <- "High"
#View(RTable)

colnames(RTable) <- c("GR","Tr","HC","CS","SC","NPS")
#RTable$GR <- as.factor(RTable$GR)
#RTable$Tr <- as.factor(RTable$Tr)
#RTable$HC <- as.factor(RTable$HC)
#RTable$CS<- as.factor(RTable$CS)
#RTable$SC<- as.factor(RTable$SC)

###############################################################apriori rules
areaTable <- data.frame(realData$OFFER_FLG_R,realData$Mini.Bar_PL,realData$Spa_PL,realData$Restaurant_PL,realData$Golf_PL,realData$Fitness.Center_PL,realData$Conference_PL,realData$Casino_PL,realData$Business.Center_PL,realData$NPS_Type)
colnames(areaTable) <- c("Offer","Mini Bar","Spa","Restaurant","Golf","Fitness Center","Conference Room","Casino","Business Center","NPS")
#apriori rules
# 
rulesrea <- apriori(areaTable, parameter=list(support=0.002,confidence=0.7),appearance = list(rhs="NPS=Promoter"))

subsetrea <- head(sort(rulesrea,by="confidence"),200)
plot(subsetrea)
inspect(subsetrea)



#rulesR <- apriori(RTable, parameter=list(support=0.002,confidence=0.8),appearance = list(rhs="NPS=Promoter"))
#inspect(rulesR)
#plot(rulesR, measure=c("support","lift"), shading="confidence")

rulesR2 <- apriori(RTable, parameter=list(support=0.002,confidence=0.7),appearance = list(rhs="NPS=Detractor"))
inspect(rulesR2)
plot(rulesR2, measure=c("support","lift"), shading="confidence")


sampleTable3 <- RTable
View(sampleTable2)
sampleTable3 <- na.omit(sampleTable3)
rand_index3 <- sample(1:dim(sampleTable3)[1])
create2_3_3 <- floor(2 * dim(sampleTable3)[1]/3)
create2_3_3
train_data3 <- sampleTable3[rand_index3[1:create2_3_3],]
test_data3 <- sampleTable3[rand_index3[create2_3_3:dim(sampleTable3)[1]],]



# 0.180073
#t3 <- ksvm(train_data3$NPS~.,data=train_data3,kernel="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
#t3

t4 <- naiveBayes(train_data3$NPS~.,data=train_data3)
t4



