#notes:

#Libraries
library("readxl")
library("writexl")
library("lme4")
library("ggplot2")
library("dfoptim")
library("optimx")
library("ggfortify")
library("jtools")
library("ggstance")
library("broom")
library("broom.mixed")
library("ggpubr")

#Global Variables

##file information

###Observation data

FilePath_Master <- "C:\\Users\\Daniel\\Documents\\University Assignments\\Honours Project\\HonoursProj\\"
FileName_OBS <- "AllSightings_2017_2018_2019_Daniel.xlsx"
FirstWinter_FileSheetNumber <- 1 ## sheet 1 is winter of 2017-2018 (1st winter)
SecondWinter_FileSheetNumber <- 2 ## sheet 2 is winter of 2018-2019 (2nd winter)
FileName_Outpout_OBS <- "Shag Workbook.xlsx"

###Reproduction Data

FileName_RS <- "RS_Data.xlsx" 
FileSheetNumber <- 3 ## the number of the sheet being imported
FileName_Outpout_RS <- "Shag GLMM.xlsx" 

##global static variables
WinterWeight <- 1.1 #strength of 'depth of winter' weighting
DoW1 <- 10+1 #first time period weighted higher !!! +1 to adjust column selection correctly
DoW2 <- 16+1 #last time period weighted higher !!! +1 to adjust column selection correctly
QualificationRequirement <- 1 #shags need to qualify at least this many times to considered in further analyses
toolate1stw <- as.Date("01/12/2017",format="%d/%m/%Y") #Shags that haven't been seen in Peterhead at all before this date are not likely to be wintering there
toolate2ndw <- as.Date("01/12/2018",format="%d/%m/%Y") #Shags that haven't been seen in Peterhead at all before this date are not likely to be wintering there

##Location names
ImportantSites <- "Boddam Buchanhaven Peterhead Scotstown" ## these sites are 'relevant'

#-#-#-#

#Custom functions

##Overdispersion test function

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

##bootstrapping 95% CI functions
###lower 

l95ci <- function(V){
  a <- numeric(10000)
  for(BSI in 1:length(a)) {
    a[BSI] <- mean(sample(V, replace = TRUE))
  }
  
  return(quantile(a, 0.025))
}

###higher
h95ci <- function(V){
  a <- numeric(10000)
  for(BSI in 1:length(a)) {
    a[BSI] <- mean(sample(V, replace = TRUE))
  }
  
  return(quantile(a, 0.975))
}

#-#-#-#

##Importing and quick formatting dataset of winter sightings
###1st Winter
FirstWinterMaster <- read_excel(paste(FilePath_Master, FileName_OBS, sep = ""), sheet = FirstWinter_FileSheetNumber, col_names = TRUE)
FirstWinterMaster$Date <- as.Date(FirstWinterMaster$Date) ##default is timezone date
FirstWinterMaster <- subset(FirstWinterMaster, !is.na(Darvic) & !is.na(Location) & !is.na(Date) & Date > '2017-01-01') ## remove empty rows if any, and also too early dates
FirstWinterMaster <- FirstWinterMaster[order(FirstWinterMaster$Date),]


###2nd Winter
SecondWinterMaster <- read_excel(paste(FilePath_Master, FileName_OBS, sep = ""), sheet = SecondWinter_FileSheetNumber, col_names = TRUE)
SecondWinterMaster$Date <- as.Date(SecondWinterMaster$Date) ##default is timezone date
SecondWinterMaster <- subset(SecondWinterMaster, !is.na(Darvic) & !is.na(Location) & !is.na(Date) & Date > '2018-01-01') ## remove empty rows if any, and also too early dates
SecondWinterMaster <- SecondWinterMaster[order(SecondWinterMaster$Date),]

#creating column to check if observation was at relevant site

###1st Winter relevant site
FirstWinterMaster$RelevantSite <- 0

for(r in 1:nrow(FirstWinterMaster)) {
  ifelse(grepl(FirstWinterMaster$Location[r], ImportantSites, fixed = FALSE), FirstWinterMaster$RelevantSite[r] <- 1, FirstWinterMaster$RelevantSite[r] <- 0)
  
}

###2nd Winter relevant site
SecondWinterMaster$RelevantSite <- 0

for(r in 1:nrow(SecondWinterMaster)) {
  ifelse(grepl(SecondWinterMaster$Location[r], ImportantSites, fixed = FALSE), SecondWinterMaster$RelevantSite[r] <- 1, SecondWinterMaster$RelevantSite[r] <- 0)
  
}

#-#-#-#

#Dropping shags where the first winter sighting in Peterhead was in December or later
#Dropping rows that should not be used in points evaluation (only of birds that have been seen in Peterhead). Non-Peterhead sightings before the 1st Peterhead date are just 'pre-arrival'

##1st winter

PracDF <- subset(FirstWinterMaster, Date >= as.Date("01/09/2017",format="%d/%m/%Y") & RelevantSite == 1)
PracDF <- PracDF[order(PracDF$Date),]
PracDF <- PracDF[!duplicated(PracDF["Darvic"]),] # This dataframe now has only sighting per bird, the first one in Peterhead

###shag dropping
ShagsToDrop <- c()

for(p in 1:nrow(PracDF)) {
  DarvicCode <- PracDF$Darvic[p]
  if(isTRUE(PracDF$Date[p] >= toolate1stw)) {
    ShagsToDrop <- append(ShagsToDrop, DarvicCode)
    
  }
} 

FirstWinterMaster <- subset(FirstWinterMaster, !(FirstWinterMaster$Darvic %in% ShagsToDrop)) # drop the shags where their first winter Peterhead date was in December or late

###row dropping
RowsToDrop <- c() #indeces of rows that should not be used for the scoring stage

for(e in 1:nrow(FirstWinterMaster)) {
  DarvicCode <- FirstWinterMaster$Darvic[e]
  if(isTRUE(FirstWinterMaster$Date[e] < PracDF$Date[which(PracDF$Darvic == DarvicCode)])) { # if 'Master' date is earlier than first Peterhead date
    RowsToDrop <- append(RowsToDrop, e) # the observation in this row was before the first relevant Peterhead observation 
    
  }
}

FirstWinterMaster <- FirstWinterMaster[-RowsToDrop, ] # dropping the observations before first one in Peterhead

##2nd winter

PracDF <- subset(SecondWinterMaster, Date >= as.Date("01/09/2018",format="%d/%m/%Y") & RelevantSite == 1)
PracDF <- PracDF[order(PracDF$Date),]
PracDF <- PracDF[!duplicated(PracDF["Darvic"]),] # This dataframe now has only sighting per bird, the Second one in Peterhead


###shag dropping
ShagsToDrop <- c()

for(p in 1:nrow(PracDF)) {
  DarvicCode <- PracDF$Darvic[p]
  if(isTRUE(PracDF$Date[p] >= toolate2ndw)) {
    ShagsToDrop <- append(ShagsToDrop, DarvicCode)
    
  }
} 

SecondWinterMaster <- subset(SecondWinterMaster, !(SecondWinterMaster$Darvic %in% ShagsToDrop)) # drop the shags where their first winter Peterhead date was in December or late

###row dropping
RowsToDrop <- c() #indeces of rows that should not be used for the scoring stage

for(e in 1:nrow(SecondWinterMaster)) {
  DarvicCode <- SecondWinterMaster$Darvic[e]
  if(isTRUE(SecondWinterMaster$Date[e] < PracDF$Date[which(PracDF$Darvic == DarvicCode)])) { # if 'Master' date is earlier than Second Peterhead date
    RowsToDrop <- append(RowsToDrop, e) # the observation in this row was before the Second relevant Peterhead observation 
    
    
  }
}

SecondWinterMaster <- SecondWinterMaster[-RowsToDrop, ] # dropping the observations before first one in Peterhead

#-#-#-#

#set observation periods 

##for 1st winter
FirstWinterObsPeriodsDF <- data.frame("IntervalNumber" = 1:19) ## interval 1 and 19 are non-winter times
FirstWinterObsPeriodsDF$IntervalStart <- c(as.Date("01/01/2017",format="%d/%m/%Y"), seq.Date(from=as.Date("01/09/2017",format="%d/%m/%Y"), by=10, length.out= 17), as.Date("18/02/2018",format="%d/%m/%Y"))
FirstWinterObsPeriodsDF$IntervalEnd <- c(as.Date("31/08/2017",format="%d/%m/%Y"), seq.Date(from=as.Date("10/09/2017",format="%d/%m/%Y"),by=10, length.out= 17), as.Date("31/12/2018",format="%d/%m/%Y"))

#for 2nd winter
SecondWinterObsPeriodsDF <- data.frame("IntervalNumber" = 1:19) ## interval 1 and 19 are non-winter times
SecondWinterObsPeriodsDF$IntervalStart <- c(as.Date("01/01/2018",format="%d/%m/%Y"), seq.Date(from=as.Date("01/09/2018",format="%d/%m/%Y"), by=10, length.out= 17), as.Date("18/02/2019",format="%d/%m/%Y"))
SecondWinterObsPeriodsDF$IntervalEnd <- c(as.Date("31/08/2018",format="%d/%m/%Y"), seq.Date(from=as.Date("10/09/2018",format="%d/%m/%Y"),by=10, length.out= 17), as.Date("31/12/2019",format="%d/%m/%Y"))

#-#-#-#

#Sighting period assignments and final score-sheet setup
##2 sightings 8 days apart in the same time interval are more important than 2 sightings 3 days apart in 2 different intervals?
##Varying the time interval start dates (maintaining time interval lengths) can make the process more robust. 


##score summary creation
FirstWinterSummary <- data.frame("Darvic" = unique(FirstWinterMaster$Darvic)) ## column of unique darvic codes
SecondWinterSummary <- data.frame("Darvic" = unique(SecondWinterMaster$Darvic))

FirstWinterWorkingList <- data.frame("Darvic" = unique(FirstWinterMaster$Darvic)) ## column of unique darvic codes
SecondWinterWorkingList <- data.frame("Darvic" = unique(SecondWinterMaster$Darvic))


##Preparing for main loop
ObsIntervalAdjustments <- c(0:9) #+9 adjusts to 10 day which eclipses next sighting period, but the adjustment is at the end, so the 'wrong' one is not executed

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

#Beginning main loop

for(OIA in ObsIntervalAdjustments) { 
  
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

##assign sighting events to sighting periods - 0s on output are errors, 1s are too early, 19s are too late
  
###1st winter
  
  FirstWinterMaster$ObsPeriod <- 0
  
  for(d in 1:nrow(FirstWinterMaster)) {
    
    for(i in 1:19) {
      
      if(FirstWinterMaster$Date[d] >= FirstWinterObsPeriodsDF$IntervalStart[i] & FirstWinterMaster$Date[d] <= FirstWinterObsPeriodsDF$IntervalEnd[i]) { 
        FirstWinterMaster$ObsPeriod[d] <- FirstWinterObsPeriodsDF$IntervalNumber[i]
      }
    }
  }
  
###2nd winter
  
  SecondWinterMaster$ObsPeriod <- 0
  
  for(d in 1:nrow(SecondWinterMaster)) {
    
    for(i in 1:19) {
      
      if(SecondWinterMaster$Date[d] >= SecondWinterObsPeriodsDF$IntervalStart[i] & SecondWinterMaster$Date[d] <= SecondWinterObsPeriodsDF$IntervalEnd[i]) { 
        SecondWinterMaster$ObsPeriod[d] <- SecondWinterObsPeriodsDF$IntervalNumber[i]
      }
    }
  }
  
#-#-#-# 

##Separating data into whether the sightings were in Peterhead or not 
##observation period criteria currently should include all except errors
  
###1st Winter
  
  FirstWinterRelevant <- subset(FirstWinterMaster, ObsPeriod > 0 & ObsPeriod < 20 & RelevantSite == 1)
  FirstWinterIrrelevant <- subset(FirstWinterMaster, ObsPeriod > 0 & ObsPeriod < 20 & RelevantSite == 0)
  
###2nd Winter
  
  SecondWinterRelevant <- subset(SecondWinterMaster, ObsPeriod > 0 & ObsPeriod < 20 & RelevantSite == 1)
  SecondWinterIrrelevant <- subset(SecondWinterMaster, ObsPeriod > 0 & ObsPeriod < 20 & RelevantSite == 0)
  
#-#-#-#

##Rearranging data 
##Instead of ifelse to process it all at once on one dataframe, I do in Peterhead and out of Peterhead ones separately, because sometimes both happen in one time interval. Then adding the two dataframes together
 
###1st winter v2 IN PETERHEAD
  
  FirstWinterByObsIn <- data.frame("Darvic" = unique(FirstWinterMaster$Darvic)) ## column of unique darvic codes
  FirstWinterByObsIn[,sprintf("ObsPeriod %s", FirstWinterObsPeriodsDF$IntervalNumber)] <- 0 ## creating observation period columns and populating with 0
  FirstWinterByObsOut <- data.frame("Darvic" = unique(FirstWinterMaster$Darvic)) ## column of unique darvic codes
  FirstWinterByObsOut[,sprintf("ObsPeriod %s", FirstWinterObsPeriodsDF$IntervalNumber)] <- 0 ## creating observation period columns and populating with 0
  FirstWinterMasterIn <- subset(FirstWinterMaster, RelevantSite == 1) ##split Master into Peterhead and non-Peterhead observations
  FirstWinterMasterOut <- subset(FirstWinterMaster, RelevantSite == 0) ##split Master into Peterhead and non-Peterhead observations
  
###dataframe for Peterhead scoring
  
  for(n in 1:nrow(FirstWinterMasterIn) ) {
    
    darvic <- FirstWinterMasterIn$Darvic[n]
    rowM <- which(grepl(darvic, FirstWinterMasterIn$Darvic, fixed=TRUE)) ##rows where the Darvic code in question is in the main DF
    rowBO <- which(grepl(darvic, FirstWinterByObsIn$Darvic, fixed=TRUE)) ##the row where the Darvic code in questions is in the new DF
    
    obsvector <- FirstWinterMasterIn$ObsPeriod[rowM] ##this to create a vector of values of ObsPeriod for the particular shag
    obsvector <- obsvector + 1 ## adjust for extra column in ByObs dataframe. the scores will go into these columns
    
    for(i in obsvector) {
      
###to weight 'depth of winter' sightings higher (observation periods 10 to 16, 7 total) 
      ifelse(i >= DoW1 & i <= DoW2, FirstWinterByObsIn[rowBO,i] <- 1*WinterWeight, FirstWinterByObsIn[rowBO,i] <- 1)
      
    }
  }
  
###dataframe non-Peterhead scoring
  
  for(n in 1:nrow(FirstWinterMasterOut) ) {
    
    darvic <- FirstWinterMasterOut$Darvic[n]
    rowM <- which(grepl(darvic, FirstWinterMasterOut$Darvic, fixed=TRUE)) ##rows where the Darvic code in question is in the main DF
    rowBO <- which(grepl(darvic, FirstWinterByObsOut$Darvic, fixed=TRUE)) ##the row where the Darvic code in questions is in the new DF
    
    
    obsvector <- FirstWinterMasterOut$ObsPeriod[rowM] ##this to create a vector of values of ObsPeriod for the particular shag
    obsvector <- obsvector + 1 ## adjust for extra column in ByObs dataframe. the scores will go into these columns
    
    
    for(i in obsvector) {
      
### to weight 'depth of winter' sightings higher (observation periods 10 to 16, 7 total) 
      ifelse(i >= DoW1 & i <= DoW2, FirstWinterByObsOut[rowBO,i] <- 1*WinterWeight, FirstWinterByObsOut[rowBO,i] <- 1)
    }
  }
  
###Adding up the two data frames to get score
  
  FirstWinterByObsProduct <- merge(FirstWinterByObsIn,FirstWinterByObsOut,by="Darvic")
  
  S <- FirstWinterByObsProduct[,grepl("*\\.x$",names(FirstWinterByObsProduct))] - FirstWinterByObsProduct[,grepl("*\\.y$",names(FirstWinterByObsProduct))]
  
  FirstWinterByObsProduct <- cbind(FirstWinterByObsProduct[,1,drop=FALSE],S)
  FirstWinterByObsProduct$Score <- rowSums(FirstWinterByObsProduct[, 3:19])
  #FirstWinterByObsProduct$TimesInPeterhead <- rowSums(FirstWinterByObsProduct[, 3:19] > 0)
  FirstWinterByObsProduct$Qualified <- ifelse(FirstWinterByObsProduct$Score > 2, 1, 0) #1= yes, a Peterhead winterer, 0= no
  names(FirstWinterByObsProduct) <- gsub(".x", "", names(FirstWinterByObsProduct))
  FirstWinterSummary = merge(FirstWinterSummary, FirstWinterByObsProduct[, c("Darvic", "Qualified")], by="Darvic")
  names(FirstWinterSummary)[OIA+2] <- paste("Qualified for run ", OIA, sep="") 
  if(OIA == 0) { 
    FirstWinterWorkingList <- FirstWinterByObsProduct #preserving the list with the primary observation interval dates
    }
  
  
###2nd winter v2 IN PETERHEAD
  
  SecondWinterByObsIn <- data.frame("Darvic" = unique(SecondWinterMaster$Darvic)) ## column of unique darvic codes
  SecondWinterByObsIn[,sprintf("ObsPeriod %s", SecondWinterObsPeriodsDF$IntervalNumber)] <- 0 ## creating observation period columns and populating with 0
  SecondWinterByObsOut <- data.frame("Darvic" = unique(SecondWinterMaster$Darvic)) ## column of unique darvic codes
  SecondWinterByObsOut[,sprintf("ObsPeriod %s", SecondWinterObsPeriodsDF$IntervalNumber)] <- 0 ## creating observation period columns and populating with 0
  SecondWinterMasterIn <- subset(SecondWinterMaster, RelevantSite == 1) ##split Master into Peterhead and non-Peterhead observations
  SecondWinterMasterOut <- subset(SecondWinterMaster, RelevantSite == 0) ##split Master into Peterhead and non-Peterhead observations
  
###dataframe for Peterhead scoring
  
  for(n in 1:nrow(SecondWinterMasterIn) ) {
    
    darvic <- SecondWinterMasterIn$Darvic[n]
    rowM <- which(grepl(darvic, SecondWinterMasterIn$Darvic, fixed=TRUE)) ##rows where the Darvic code in question is in the main DF
    rowBO <- which(grepl(darvic, SecondWinterByObsIn$Darvic, fixed=TRUE)) ##the row where the Darvic code in questions is in the new DF
    
    obsvector <- SecondWinterMasterIn$ObsPeriod[rowM] ##this to create a vector of values of ObsPeriod for the particular shag
    obsvector <- obsvector + 1 ## adjust for extra column in ByObs dataframe. the scores will go into these columns
    
    for(i in obsvector) {
      
###to weight 'depth of winter' sightings higher (observation periods 10 to 16, 7 total) 
      
      ifelse(i >= DoW1 & i <= DoW2, SecondWinterByObsIn[rowBO,i] <- 1*WinterWeight, SecondWinterByObsIn[rowBO,i] <- 1)
      
    }
  }
  
###dataframe non-Peterhead scoring
  
  for(n in 1:nrow(SecondWinterMasterOut) ) {
    
    darvic <- SecondWinterMasterOut$Darvic[n]
    rowM <- which(grepl(darvic, SecondWinterMasterOut$Darvic, fixed=TRUE)) ##rows where the Darvic code in question is in the main DF
    rowBO <- which(grepl(darvic, SecondWinterByObsOut$Darvic, fixed=TRUE)) ##the row where the Darvic code in questions is in the new DF
    
    
    obsvector <- SecondWinterMasterOut$ObsPeriod[rowM] ##this to create a vector of values of ObsPeriod for the particular shag
    obsvector <- obsvector + 1 ## adjust for extra column in ByObs dataframe. the scores will go into these columns
    
    
    for(i in obsvector) {
      
###to weight 'depth of winter' sightings higher (observation periods 10 to 16, 7 total) 
      ifelse(i >= DoW1 & i <= DoW2, SecondWinterByObsOut[rowBO,i] <- 1*WinterWeight, SecondWinterByObsOut[rowBO,i] <- 1)
    }
  }
  
###Adding up the two data frames to get score
  
  SecondWinterByObsProduct <- merge(SecondWinterByObsIn,SecondWinterByObsOut,by="Darvic")
  
  S <- SecondWinterByObsProduct[,grepl("*\\.x$",names(SecondWinterByObsProduct))] - SecondWinterByObsProduct[,grepl("*\\.y$",names(SecondWinterByObsProduct))]
  
  SecondWinterByObsProduct <- cbind(SecondWinterByObsProduct[,1,drop=FALSE],S)
  SecondWinterByObsProduct$Score <- rowSums(SecondWinterByObsProduct[, 3:19])
  #SecondWinterByObsProduct$TimesInPeterhead <- rowSums(SecondWinterByObsProduct[, 3:19] > 0)
  SecondWinterByObsProduct$Qualified <- ifelse(SecondWinterByObsProduct$Score > 2, 1, 0) #1= yes, a Peterhead winterer, 0= no
  names(SecondWinterByObsProduct) <- gsub(".x", "", names(SecondWinterByObsProduct))
  SecondWinterSummary = merge(SecondWinterSummary, SecondWinterByObsProduct[, c("Darvic", "Qualified")], by="Darvic")
  names(SecondWinterSummary)[OIA+2] <- paste("Qualified for run ", OIA, sep="")
  if(OIA == 0) { 
    SecondWinterWorkingList <- SecondWinterByObsProduct #preserving the list with the primary observation interval dates
   }
  
#-#-#-# 
  
##shifting the interval brackets ready for next iteration of the loop
  
  FirstWinterObsPeriodsDF$IntervalStart <- FirstWinterObsPeriodsDF$IntervalStart + 1
  FirstWinterObsPeriodsDF$IntervalEnd <- FirstWinterObsPeriodsDF$IntervalEnd + 1
  
  SecondWinterObsPeriodsDF$IntervalStart <- SecondWinterObsPeriodsDF$IntervalStart + 1
  SecondWinterObsPeriodsDF$IntervalEnd <- SecondWinterObsPeriodsDF$IntervalEnd + 1

  print(OIA) #just a physical reassurance in the console that the script is running
  
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!  

##Main loop end  
  
}

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

##Finishing touches

FirstWinterSummary$Tally <- rowSums(FirstWinterSummary[, 2:10]) # tallying up which run qualified and how many times total
SecondWinterSummary$Tally <- rowSums(SecondWinterSummary[, 2:10])

FirstWinterWorkingList = merge(FirstWinterWorkingList, FirstWinterSummary[, c("Darvic", "Tally")], by="Darvic")
names(FirstWinterWorkingList) <- sub("^Tally$", "times_qualified", names(FirstWinterWorkingList))
FirstWinterWorkingList$Qualified <- NULL

SecondWinterWorkingList = merge(SecondWinterWorkingList, SecondWinterSummary[, c("Darvic", "Tally")], by="Darvic")
names(SecondWinterWorkingList) <- sub("^Tally$", "times_qualified", names(SecondWinterWorkingList))
SecondWinterWorkingList$Qualified <- NULL

FirstWinterQualified <- subset(FirstWinterWorkingList, times_qualified >= QualificationRequirement) #subsetting the birds that qualified at least as many times as specified at start
SecondWinterQualified <- subset(SecondWinterWorkingList, times_qualified >= QualificationRequirement)

#-#-#-# 

##Exporting dataframes to excel

write_xlsx(list("Winter_1_ByObs" = FirstWinterWorkingList,
                "Winter_2_ByObs" = SecondWinterWorkingList,
                "Winter_1_Qualified" = FirstWinterQualified,
                "Winter_2_Qualified" = SecondWinterQualified,
                "Winter_1_Summary" = FirstWinterSummary,
                "Winter_2_Summary" = SecondWinterSummary)
           , paste(FilePath_Master, FileName_Outpout_OBS, sep = ""))

#-#-#-# 

#Processing reproductive performance data

##Importing and quick formatting dataset of reproductive records
RSData <- read_excel(paste(FilePath_Master, FileName_RS, sep = ""), sheet = FileSheetNumber, col_names = TRUE)
str(RSData)
RSData$CRCode <- as.factor(RSData$CRCode)
RSData$colony <- as.factor(RSData$colony)
RSData$Year <- as.factor(RSData$Year)
RSData$sex <- as.factor(RSData$sex)
RSData$Cohort <- ""
RSData$Cohort[which(RSData$Year == 2018)] <- "2017-2018"
RSData$Cohort[which(RSData$Year == 2019)] <- "2018-2019"

#-#-#-#

#make sure we only use data of those that qualified from winter observations

##1st winter
RSData1 <- subset(RSData, Year == 2018)
nrow(RSData1)
RSData1 <- subset(RSData1, (RSData1$CRCode %in% FirstWinterQualified$Darvic))
nrow(RSData1)

##2nd winter
RSData2 <- subset(RSData, Year == 2019)
nrow(RSData2)
RSData2 <- subset(RSData2, (RSData2$CRCode %in% SecondWinterQualified$Darvic))
nrow(RSData2)

##putting the list back together
nrow(RSData)
RSData <- rbind(RSData1, RSData2)
nrow(RSData)

#-#-#-#

#adding columns

##Column to show success (fledged any or failed)
RSData$Success <- "error?"
RSData$Success <- ifelse(RSData$RS >= 1, "success","fail")

##Dropping shags with NAs in 'RS' as instructed
RSData <- RSData[complete.cases(RSData[,'RS']),]
#RSData$RS[RSData$RS == "NA"] <- "0"# in case we treat them as 0 outcome instead

#-#-#-#

#adding new data frames for information not present for all shags

##subset dataframe so only ones with age remain
RSData.with.age <- subset(RSData, !is.na(age))

##subset dataframe so only ones with sex remain
RSData.with.sex <- subset(RSData, sex != "NA")

##subset dataframe so only ones with sex AND age remain
RSData.with.sexandage <- subset(RSData, sex != "NA" & !is.na(age))

#-#-#-#

#Model #1 - year, colony

model1a <- glmer(RS ~ Year + colony + (1|CRCode) + (1|AttemptID), data = RSData, family = "poisson")
model1b <- glmer(RS ~ colony + (1|CRCode) + (1|AttemptID), data = RSData, family = "poisson")
model1c <- glmer(RS ~ Year + (1|CRCode) + (1|AttemptID), data = RSData, family = "poisson")
model1d <- glmer(RS ~ Year*colony + (1|CRCode) + (1|AttemptID), data = RSData, family = "poisson")
model1e <- glmer(RS ~ (1|CRCode) + (1|AttemptID), data = RSData, family = "poisson")

anova(model1a, model1b, model1c, model1d, model1e)

overdisp_fun(model1a)
overdisp_fun(model1b)
overdisp_fun(model1c)
overdisp_fun(model1d)
overdisp_fun(model1e)

summary(model1a) ##within 2 AIC
summary(model1b) ##within 2 AIC but lower

overdisp_fun(model1a)

summ(model1a, confint=TRUE, digits=3)
summ(model1b, confint=TRUE, digits=3)

#cc <- confint(model1a, parm="beta_", level = 0.95)
#cc

#-#-#-#

#model #2 - year, colony, age

model2a <- glmer(RS ~ Year*colony*age + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2b <- glmer(RS ~ colony*age + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2c <- glmer(RS ~ Year*age + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2d <- glmer(RS ~ Year*colony + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2e <- glmer(RS ~ colony*age + Year + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2f <- glmer(RS ~ Year*age + colony + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2g <- glmer(RS ~ Year*colony + age + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2h <- glmer(RS ~ Year + colony + age + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2i <- glmer(RS ~ Year + age + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2j <- glmer(RS ~ colony + age + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")
model2k <- glmer(RS ~ colony + Year + (1|CRCode) + (1|AttemptID), data = RSData.with.age, family = "poisson")

anova(model2a, model2b, model2c, model2d, model2e, model2f, model2g, model2h, model2i, model2j, model2k)

summary(model2a)
summary(model2b) ##best
summary(model2e)

summ(model2a, confint=TRUE, digits=3)
summ(model2b, confint=TRUE, digits=3) ##best
summ(model2e, confint=TRUE, digits=3)

#!!! test for overdispersion here too

###attempts at visualisation

effect_plot(model2a, pred = colony, interval=TRUE, plot.points=TRUE)
plot_summs(model2a, scale=TRUE, inner_ci_level = 0.95)
plot_summs(model2a, model2b, model2e, scale=TRUE)


#cc <- confint(model2b, parm="beta_", level = 0.95)
#cc

#-#-#-#

#model #3 - year, colony, sex

model3a <- glmer(RS ~ Year*colony*sex + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3b <- glmer(RS ~ colony*sex + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3c <- glmer(RS ~ Year*sex + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3d <- glmer(RS ~ Year*colony + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3e <- glmer(RS ~ colony*sex + Year + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3f <- glmer(RS ~ Year*sex + colony + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3g <- glmer(RS ~ Year*colony + sex + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3h <- glmer(RS ~ Year + colony + sex + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3i <- glmer(RS ~ Year + sex + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3j <- glmer(RS ~ colony + sex + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")
model3k <- glmer(RS ~ colony + Year + (1|CRCode) + (1|AttemptID), data = RSData.with.sex, family = "poisson")

anova(model3a, model3b, model3c, model3d, model3e, model3f, model3g, model3h, model3i, model3j, model3k)

summary(model3k) ##best
summary(model3d) 
summary(model3j)
summary(model3h)

#summ(model3a, confint=TRUE, digits=3)
#summ(model3b, confint=TRUE, digits=3) ##best
#summ(model3e, confint=TRUE, digits=3)

#effect_plot(model3a, pred = colony, interval=TRUE, plot.points=TRUE)
#plot_summs(model3a, scale=TRUE, inner_ci_level = 0.95)
#plot_summs(model3a, model3b, model3e, scale=TRUE)


#cc <- confint(model3b, parm="beta_", level = 0.95)
#cc

#-#-#-#

#model #4 - year, colony, age, sex

model4a <- glmer(RS ~ Year*colony*age*sex + (1|CRCode) + (1|AttemptID), data = RSData.with.sexandage, family = "poisson")
#!!! do only relevant ones, there would be too many

anova(model4a)

#modelx <- glmer(RS ~ Year*colony*age + (1|CRCode) + (1|AttemptID), data = RSData.with.sexandage, family = "poisson")
#summ(modelx, confint=TRUE, digits=3)

#summary(model4a)
#summary(model4b) ##best
#summary(model4e)


#summ(model4a, confint=TRUE, digits=3)
#summ(model4b, confint=TRUE, digits=3) ##best
#summ(model4e, confint=TRUE, digits=3)

#effect_plot(model4a, pred = colony, interval=TRUE, plot.points=TRUE)
#plot_summs(model4a, scale=TRUE, inner_ci_level = 0.95)
#plot_summs(model4a, model4b, model4e, scale=TRUE)


#cc <- confint(model4b, parm="beta_", level = 0.95)
#cc

#-#-#-#

#descriptive stats

##counting only relevant observations
date1718start <- as.Date("01/09/2017",format="%d/%m/%Y")
date1718end   <- as.Date("18/02/2018",format="%d/%m/%Y")
FirstWinterMaster.Counts <- subset(FirstWinterMaster, FirstWinterMaster$Date %in% date1718start:date1718end)

date1819start <- as.Date("01/09/2018",format="%d/%m/%Y")
date1819end   <- as.Date("18/02/2019",format="%d/%m/%Y")
SecondWinterMaster.Counts <- subset(SecondWinterMaster, SecondWinterMaster$Date %in% date1819start:date1819end)

###2017-18
ShagCount1718.AllSightings <- nrow(FirstWinterMaster.Counts) # all observation in the relevant time frame
ShagCount1718.PeterheadSightings <- length(which(FirstWinterMaster.Counts$RelevantSite == 1)) # peterhead observation in the relevant time frame
ShagCount1718.AllSurveyDays <- length(unique(FirstWinterMaster.Counts$Date)) # number of total survey days
ShagCount1718.PeterheadSurveyDays <- unique(FirstWinterMasterIn$Date) # see below
length(which(ShagCount1718.PeterheadSurveyDays %in% date1718start:date1718end)) # number of Peterhead survey days
ShagCount1718.AllBirds <- length(unique(FirstWinterMaster.Counts$Darvic)) # all birds
ShagCount1718.PeterheadBirds <- tapply(FirstWinterMaster.Counts$Darvic, FirstWinterMaster.Counts$RelevantSite, FUN = function(x) length(unique(x))) # all Peterhead birds (1 = in Peterhead)
ShagCount1718.QualifiedBirds <- length(FirstWinterQualified$Darvic) # qualified birds

###2018-19
ShagCount1819.AllSightings <- nrow(SecondWinterMaster.Counts) # all observation in the relevant time frame
ShagCount1819.PeterheadSightings <- length(which(SecondWinterMaster.Counts$RelevantSite == 1)) # peterhead observation in the relevant time frame
ShagCount1819.AllSurveyDays <- length(unique(SecondWinterMaster.Counts$Date)) # number of total survey days
ShagCount1819.PeterheadSurveyDays <- unique(SecondWinterMasterIn$Date) # see below
length(which(ShagCount1819.PeterheadSurveyDays %in% date1819start:date1819end)) # number of Peterhead survey days
ShagCount1819.AllBirds <- length(unique(SecondWinterMaster.Counts$Darvic)) # all birds
ShagCount1819.PeterheadBirds <- tapply(SecondWinterMaster.Counts$Darvic, SecondWinterMaster.Counts$RelevantSite, FUN = function(x) length(unique(x))) # all Peterhead birds (1 = in Peterhead)
ShagCount1819.QualifiedBirds <- length(SecondWinterQualified$Darvic) # qualified birds

###both winters
BothWinterMaster.Counts <- subset(FirstWinterMaster.Counts, FirstWinterMaster.Counts$Darvic %in% SecondWinterMaster.Counts$Darvic) #all observations of birds present in both winters
ShagCountBoth.PeterheadBirds <- length(unique(BothWinterMaster.Counts$Darvic))

#-#-#-#

##average first and last sighting intervals / dates
##.Intervals is all the relevant sightings of relevant shags

##1st winter
FirstWinterMaster.Intervals <- subset(FirstWinterMaster.Counts, FirstWinterMaster.Counts$Darvic %in% FirstWinterQualified$Darvic & FirstWinterMaster.Counts$RelevantSite == 1)
FirstWinterMaster.Intervals <- FirstWinterMaster.Intervals[order(FirstWinterMaster.Intervals$Date),]
FirstWinterMaster.Intervals.First <- FirstWinterMaster.Intervals[!duplicated(FirstWinterMaster.Intervals$Darvic),]
FirstWinterMaster.Intervals <- FirstWinterMaster.Intervals[order(FirstWinterMaster.Intervals$Date, decreasing = TRUE),]
FirstWinterMaster.Intervals.Last <- FirstWinterMaster.Intervals[!duplicated(FirstWinterMaster.Intervals$Darvic),]
FirstWinterMaster.Intervals.First$Winter <- "2017-2018" 

###first date
mean(FirstWinterMaster.Intervals.First$Date)
median(FirstWinterMaster.Intervals.First$Date)
sd(FirstWinterMaster.Intervals.First$Date)
mad(FirstWinterMaster.Intervals.First$Date)

###last date
mean(FirstWinterMaster.Intervals.Last$Date)
median(FirstWinterMaster.Intervals.Last$Date)
sd(FirstWinterMaster.Intervals.Last$Date)
mad(FirstWinterMaster.Intervals.Last$Date)

##2nd winter
SecondWinterMaster.Intervals <- subset(SecondWinterMaster.Counts, SecondWinterMaster.Counts$Darvic %in% SecondWinterQualified$Darvic & SecondWinterMaster.Counts$RelevantSite == 1)
SecondWinterMaster.Intervals <- SecondWinterMaster.Intervals[order(SecondWinterMaster.Intervals$Date),]
SecondWinterMaster.Intervals.First <- SecondWinterMaster.Intervals[!duplicated(SecondWinterMaster.Intervals$Darvic),]
SecondWinterMaster.Intervals <- SecondWinterMaster.Intervals[order(SecondWinterMaster.Intervals$Date, decreasing = TRUE),]
SecondWinterMaster.Intervals.Last <- SecondWinterMaster.Intervals[!duplicated(SecondWinterMaster.Intervals$Darvic),]
SecondWinterMaster.Intervals.First$Winter <- "2018-2019" 

###first date
mean(SecondWinterMaster.Intervals.First$Date)
median(SecondWinterMaster.Intervals.First$Date)
sd(SecondWinterMaster.Intervals.First$Date)
mad(SecondWinterMaster.Intervals.First$Date)

###last date
mean(SecondWinterMaster.Intervals.Last$Date)
median(SecondWinterMaster.Intervals.Last$Date)
sd(SecondWinterMaster.Intervals.Last$Date)
mad(SecondWinterMaster.Intervals.Last$Date)

##graph for 1st date spotted for both winters
TDF <- FirstWinterMaster.Intervals.First[, c(5, 9, 26)]
TDF$Date <- TDF$Date + 365  #this is cosmetic, just so it shows up better on histogram
TDF <- rbind(TDF, SecondWinterMaster.Intervals.First[, c(4, 8, 18)])

p8 <- ggplot(TDF, aes(x = Date)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  labs(x = "First time sighted in Peterhead", y = "Frequency") +
  facet_grid(Winter ~ .)

p8

#-#-#-#

##How many times seen in Peterhead histogram

###1st winter 
nrow(FirstWinterMaster.Intervals)
intervalDF1 <- as.data.frame(table(FirstWinterMaster.Intervals$Darvic))
intervalDF1$Year <- "2017-2018"

###2nd winter
nrow(SecondWinterMaster.Intervals)
intervalDF2 <- as.data.frame(table(SecondWinterMaster.Intervals$Darvic))
intervalDF2$Year <- "2018-2019"

###together then visualise
intervalDF <- rbind(intervalDF1, intervalDF2)

p2 <- ggplot(intervalDF, aes(x = Freq)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  labs(x = "Number of times recorded seen", y = "Frequency") +
  facet_grid(Year ~ .)

p2


#-#-#-#

##WIP survey effort histogram
p6 <- ggplot(FirstWinterMaster, aes(x = Location)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  labs(x = "x", y = "y") +
  aes(y = stat(count)/sum(stat(count)))# +
#facet_grid(Year ~ .)

p6

#-#-#-#

#statistical tests

##Sex ratios, counts, and chi^2 testing

###Bullers
ShagsFemaleBoB2018  <- length(which(RSData$sex == "F" & RSData$Year == "2018" & RSData$colony == "BoB"))  # number of females in 2018
ShagsMaleBoB2018    <- length(which(RSData$sex == "M" & RSData$Year == "2018" & RSData$colony == "BoB"))  # number of males in 2018
ShagsUnknownBoB2018 <- length(which(RSData$sex == "NA" & RSData$Year == "2018" & RSData$colony == "BoB")) # number of unknown sexes in 2018
ShagsMaleBoB2018/ShagsFemaleBoB2018 # male to female ratio in 2018

ShagsFemaleBoB2019  <- length(which(RSData$sex == "F" & RSData$Year == "2019" & RSData$colony == "BoB"))  # number of females in 2019
ShagsMaleBoB2019    <- length(which(RSData$sex == "M" & RSData$Year == "2019" & RSData$colony == "BoB"))  # number of males in 2019
ShagsUnknownBoB2019 <- length(which(RSData$sex == "NA" & RSData$Year == "2019" & RSData$colony == "BoB")) # number of unknown sexes in 2019
ShagsMaleBoB2019/ShagsFemaleBoB2019 # male to female ratio in 2019

chisq.test(x=c(ShagsFemaleBoB2018, ShagsMaleBoB2018), p=c(0.5, 0.5)) # BoB 2018
chisq.test(x=c(ShagsFemaleBoB2019, ShagsMaleBoB2019), p=c(0.5, 0.5)) # BoB 2019

###IoM
ShagsFemaleIoM2018  <- length(which(RSData$sex == "F" & RSData$Year == "2018" & RSData$colony == "IoM"))  # number of females in 2018
ShagsMaleIoM2018    <- length(which(RSData$sex == "M" & RSData$Year == "2018" & RSData$colony == "IoM"))  # number of males in 2018
ShagsUnknownIoM2018 <- length(which(RSData$sex == "NA" & RSData$Year == "2018" & RSData$colony == "IoM")) # number of unknown sexes in 2018
ShagsMaleIoM2018/ShagsFemaleIoM2018 # male to female ratio in 2018

ShagsFemaleIoM2019  <- length(which(RSData$sex == "F" & RSData$Year == "2019" & RSData$colony == "IoM"))  # number of females in 2019
ShagsMaleIoM2019    <- length(which(RSData$sex == "M" & RSData$Year == "2019" & RSData$colony == "IoM"))  # number of males in 2019
ShagsUnknownIoM2019 <- length(which(RSData$sex == "NA" & RSData$Year == "2019" & RSData$colony == "IoM")) # number of unknown sexes in 2019
ShagsMaleIoM2019/ShagsFemaleIoM2019 # male to female ratio in 2019


chisq.test(x=c(ShagsFemaleIoM2018, ShagsMaleIoM2018), p=c(0.5, 0.5)) # IoM 2018
chisq.test(x=c(ShagsFemaleIoM2019, ShagsMaleIoM2019), p=c(0.5, 0.5)) # IoM 2019

#-#-#-#

##age ranges + median

#max
max(RSData.with.age$age[which(RSData.with.age$colony == "IoM" & RSData.with.age$Year == 2018)]) # oldest IoM migrant in 2018
max(RSData.with.age$age[which(RSData.with.age$colony == "BoB" & RSData.with.age$Year == 2018)]) # oldest BoB migrant in 2018
max(RSData.with.age$age[which(RSData.with.age$colony == "IoM" & RSData.with.age$Year == 2019)]) # oldest IoM migrant in 2019
max(RSData.with.age$age[which(RSData.with.age$colony == "BoB" & RSData.with.age$Year == 2019)]) # oldest BoB migrant in 2019

#median
median(RSData.with.age$age[which(RSData.with.age$colony == "IoM" & RSData.with.age$Year == 2018)]) # median age of IoM migrants in 2018
median(RSData.with.age$age[which(RSData.with.age$colony == "BoB" & RSData.with.age$Year == 2018)]) # median age of BoB migrants in 2018
median(RSData.with.age$age[which(RSData.with.age$colony == "IoM" & RSData.with.age$Year == 2019)]) # median age of IoM migrants in 2019
median(RSData.with.age$age[which(RSData.with.age$colony == "BoB" & RSData.with.age$Year == 2019)]) # median age of BoB migrants in 2019

median(RSData.with.age$age[which(RSData.with.age$colony == "IoM")]) # median age of IoM migrants
median(RSData.with.age$age[which(RSData.with.age$colony == "BoB")]) # median age of BoB migrants 

#median absolute deviation
mad(RSData.with.age$age[which(RSData.with.age$colony == "IoM" & RSData.with.age$Year == 2018)]) # median absolute deviation of IoM migrants in 2018
mad(RSData.with.age$age[which(RSData.with.age$colony == "BoB" & RSData.with.age$Year == 2018)]) # median absolute deviation of age of BoB migrants in 2018
mad(RSData.with.age$age[which(RSData.with.age$colony == "IoM" & RSData.with.age$Year == 2019)]) # median absolute deviation of age of IoM migrants in 2019
mad(RSData.with.age$age[which(RSData.with.age$colony == "BoB" & RSData.with.age$Year == 2019)]) # median absolute deviation of age of BoB migrants in 2019

mad(RSData.with.age$age[which(RSData.with.age$colony == "IoM")]) # median absolute deviation of age of IoM migrants 
mad(RSData.with.age$age[which(RSData.with.age$colony == "BoB")]) # median absolute deviation of age of BoB migrants 

## few more numbers
length(unique(RSData$AttID)) # number of breeding events
length(unique(RSData$CRCode)) # number of individuals considered

#-#-#-#

##Histogram of age distribution in the two years
p1 <- ggplot(RSData.with.age, aes(x = age)) +
  aes(y = stat(count)/sum(stat(count))) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  labs(x = "Age", y = "Frequency") +
  facet_grid(Year ~ colony)

p1

#-#-#-#

##Histogram of age distribution across sexes and colonies

### BoB

TDF <- subset(RSData, colony == "BoB")

p3.1 <- ggplot(TDF, aes(x = age)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  xlim(c(3,20)) +
  # ylim(c(0,10)) +
  scale_y_continuous(breaks=c(0,5,10), limits=c(0,10)) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  labs(x = "Age", y = "Frequency") +
  facet_grid(Cohort ~ sex)

p3.1

### IoM

TDF <- subset(RSData, colony == "IoM")

p3.2 <- ggplot(TDF, aes(x = age)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  xlim(c(3,20)) +
  # ylim(c(0,10)) +
  scale_y_continuous(breaks=c(0,5,10), limits=c(0,10)) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  labs(x = "Age", y = "Frequency") +
  facet_grid(Cohort ~ sex)

p3.2

### years and colonies combined

p3 <- ggarrange(p3.1, p3.2, labels = c("BoB", "IoM"), label.x = 0.04, label.y = 0.87, ncol = 1, nrow = 2)
p3 <- annotate_figure(p3, left = text_grob("Frequency", color="black", rot=90, face="bold"), bottom = text_grob("Age", face="bold"))

p3

#-#-#-#

##Histograms of chicks fledged

###2018 IoM
p4v <- data.frame(RS = RSData$RS[which(RSData$Year == 2018 & RSData$colony == "IoM")])
p4.1 <- ggplot(p4v, aes(x = RS)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  aes(y = stat(count)/sum(stat(count))) +
  labs(x = "Chicks Fledged", y = "Proportion") 

p4.1

###2019 IoM
p4v <- data.frame(RS = RSData$RS[which(RSData$Year == 2019 & RSData$colony == "IoM")])
p4.2 <- ggplot(p4v, aes(x = RS)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  aes(y = stat(count)/sum(stat(count))) +
  labs(x = "Chicks Fledged", y = "Proportion") 

p4.2

###2018 BoB
p4v <- data.frame(RS = RSData$RS[which(RSData$Year == 2018 & RSData$colony == "BoB")])
p4.3 <- ggplot(p4v, aes(x = RS)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  aes(y = stat(count)/sum(stat(count))) +
  labs(x = "Chicks Fledged", y = "Proportion") 

p4.3

###2019 BoB
p4v <- data.frame(RS = RSData$RS[which(RSData$Year == 2019 & RSData$colony == "BoB")])
p4.4 <- ggplot(p4v, aes(x = RS)) +
  geom_histogram(binwidth = 1, fill="grey", color="black", alpha=0.5, position="identity") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  aes(y = stat(count)/sum(stat(count))) +
  labs(x = "Chicks fledged", y = "Proportion") 

p4.4

### years and colonies combined

p4 <- ggarrange(p4.1, p4.2, p4.3, p4.4, labels = c("A", "B", "C","D"), label.x = 0.85, label.y = 0.95, ncol = 2, nrow = 2)
p4 <-annotate_figure(p4, left = text_grob("Proportion", color="black", rot=90, face="bold"), bottom = text_grob("Chicks fledged", face="bold"))

p4

#-#-#-#

##boxplot for reproductive success

RSDataP <- RSData
RSDataP$Cohort <- "both years"
RSDataP <- rbind(RSDataP, RSData)

p5 <- ggplot(RSDataP, aes(x = colony, y = RS, width = 0.5)) 

min.mean.sd.max <- function(x) {
  r <- c(mean(x) - sd(x), l95ci(x), mean(x), h95ci(x), mean(x) + sd(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

p5 <- p5 + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
  ylim(c(-0.5, 4)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  labs(x = "Breeding colony", y = "Chicks fledged") +
  #coord_flip() +
  facet_wrap(. ~ Cohort)

p5

#-#-#-#

## ages and reproductive output

RSDataP7 <- aggregate(RSData.with.age[, 'RS'], list(RSData.with.age$age), FUN = "mean")
RSDataP7 <- merge(RSDataP7, aggregate(RSData.with.age[, 'RS'], list(RSData.with.age$age), FUN = "sd"), by="Group.1")
RSDataP7 <- merge(RSDataP7, aggregate(RSData.with.age[, 'RS'], list(RSData.with.age$age), FUN = "length"), by="Group.1")
names(RSDataP7) <- c("Year.RS", "Mean.RS", "SD.RS", "N.RS")
RSDataP7$SE.RS <- RSDataP7$SD.RS / sqrt(RSDataP7$N.RS - 1)

p7 <- ggplot(RSDataP7, aes(x = Year.RS, y = Mean.RS)) +
  geom_bar(stat="identity") +
  ylim(c(0, 3)) +
  geom_errorbar(aes(ymin = Mean.RS - SE.RS, ymax = Mean.RS + SE.RS), width = 0.2, position=position_dodge(0.9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color="black", size = 0.5)) +
  labs(x = "Age group", y = "Mean reproductive output") 

p7

#-#-#-#



























