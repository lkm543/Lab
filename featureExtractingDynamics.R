#Q: Transfer from Hospital day?
#Q: ICU in 30 days? or exceed 30?
#Remove all variables
cat("\014")  
rm(list=ls(all=TRUE))
#options(warn=-1)
#It is the Input table
#安裝package 
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")

#載入library 
library("rpart")
library("rpart.plot")
library("rattle")


labelOrigin<-read.table("EventData.csv",sep=",",header=TRUE)
transferHospitalOrigin<-read.table("RowData/05轉院註記.csv",sep=",",header=TRUE)
outPatientClinicOrigin<-read.table("RowData/02門診史.csv",sep=",",header=TRUE)
hospitalizationOrigin<-read.table("RowData/03住院史.csv",sep=",",header=TRUE)
emergencyOrigin<-read.table("RowData/04急診史.csv",sep=",",header=TRUE)
ICUStayOrigin<-read.table("RowData/07轉床紀錄.csv",sep=",",header=TRUE)
labelProcessed <- data.frame(chartNO=character(),
                             case=logical(),
                             control=logical(),
                             hospitalizationDay=integer(),
                             emergencyDay=integer(),
                             ICUStayDay=integer(),
                             outPatientClinicDay=integer())
counts <- 0



for (c in 1:nrow(labelOrigin)){
  chartNO <- as.character(labelOrigin[c,1])
  case <- as.character(labelOrigin[c,2])
  control <- as.character(labelOrigin[c,3])
  history <- as.character(labelOrigin[c,8])
  historyArray <- strsplit(history[1],";")
  diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
  #print(chartNO)
  #print(diseaseDate)
  ind_hospitalization_array <- which(!is.na(match(hospitalizationOrigin$CHARTNO2, chartNO)))
  ind_emergency_array <- which(!is.na(match(emergencyOrigin$CHARTNO2, chartNO)))
  ind_ICUStay_array <- which(!is.na(match(ICUStayOrigin$CHARTNO2, chartNO)))
  ind_outPatientClinic_array <- which(!is.na(match(outPatientClinicOrigin$CHARTNO2, chartNO)))
  ind_transferHospital_array <- which(!is.na(match(transferHospitalOrigin$CHARTNO2, chartNO)))
  
  #print(ind_array)
  MIN_hospitalization <- 9999
  emergencyDay <- 9999
  ICUStayDay <- 9999
  outPatientClinicDay <- 9999
  #MIN_hospitalization
  if(length(ind_hospitalization_array)>0){
    for (j in seq(1, length(ind_hospitalization_array))){
      index <- ind_hospitalization_array[j]
      inDate <- as.Date(hospitalizationOrigin$INDATE[index],format='%m/%d/%Y')
      outDate <- as.Date(hospitalizationOrigin$OUTDATE[index],format='%m/%d/%Y')
      
      diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
      diffOut <- as.numeric(difftime(diseaseDate, outDate, tz, units = c("days")))
      
      if(is.na(diffOut)){
        if(diffIn>0){
          MIN_hospitalization <- 0
        }
      }
      else{
        if(diffOut > 0 && diffOut<MIN_hospitalization){
          MIN_hospitalization <- diffOut
        }
        if(diffIn*diffOut<=0){
          MIN_hospitalization <- 0
        }
      }
    }
  }

  
  #emergencyDay
  if(length(ind_emergency_array)>0){
    for (j in seq(1, length(ind_emergency_array))){
      index <- ind_emergency_array[j]
      inDate <- as.Date(emergencyOrigin$COMECLINICDATE[index],format='%m/%d/%Y')
      
      diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
      
      if(diffIn>0&&diffIn<emergencyDay){
          emergencyDay <- diffIn
      }
    }
  }

  # ICU
  if(length(ind_ICUStay_array)>0){
    for (j in seq(1, length(ind_ICUStay_array))){
      index <- ind_ICUStay_array[j]
      inDate <- as.Date(ICUStayOrigin$TRANSFERINDATE[index],format='%m/%d/%Y')
      outDate <- as.Date(ICUStayOrigin$TRANSFEROUTDATE[index],format='%m/%d/%Y')
      
      diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
      diffOut <- as.numeric(difftime(diseaseDate, outDate, tz, units = c("days")))
      
      if(is.na(diffOut)){
        if(diffIn>0){
          ICUStayDay <- 0
        }
      }
      else{
        if(diffOut > 0 && diffOut<ICUStayDay){
          ICUStayDay <- diffOut
        }
        if(diffIn*diffOut<=0){
          ICUStayDay <- 0
        }
      }
    }
  }
  
  #outPatientClinicDay COMECLINICDATE
  if(length(ind_outPatientClinic_array)>0){
    for (j in seq(1, length(ind_outPatientClinic_array))){
      index <- ind_outPatientClinic_array[j]
      inDate <- as.Date(outPatientClinicOrigin$COMECLINICDATE[index],format='%m/%d/%Y')
      
      diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
      
      if(diffIn>0&&diffIn<outPatientClinicDay){
        outPatientClinicDay <- diffIn
      }
    }
  }
  
  
  newRow<-data.frame(chartNO,case,control,MIN_hospitalization,emergencyDay,ICUStayDay,outPatientClinicDay,stringsAsFactors=FALSE)
  names(newRow)<-c("chartNO","case","control","hospitalization","emergencyDay","ICUStayDay","outPatientClinicDay")
  labelProcessed <- rbind(labelProcessed, newRow)
  #print(c)
  
}
write.table(labelProcessed, file = "FeatureDynamics.csv", sep = ",",row.names = FALSE)

hospitalizationData <- subset(labelProcessed, select=c("case","hospitalization"))
emergencyDayData <- subset(labelProcessed, select=c("case","emergencyDay"))
ICUStayDayData <- subset(labelProcessed, select=c("case","ICUStayDay"))
outPatientClinicDayData <- subset(labelProcessed, select=c("case","outPatientClinicDay"))


hospitalizationData <- hospitalizationData[ order(-hospitalizationData[,2]),]
emergencyDayData <- emergencyDayData[ order(-emergencyDayData[,2]),]
ICUStayDayData <- ICUStayDayData[ order(-ICUStayDayData[,2]),]
outPatientClinicDayData <- outPatientClinicDayData[ order(-outPatientClinicDayData[,2]),]

set.seed(22)
#for (c in 1:nrow(hospitalizationData)){
  #Pivot <- (hospitalizationData[c,2]+hospitalizationData[c+1,2])/2


dtreeM<- rpart(formula = case ~ ., data = hospitalizationData, method = "class", control = rpart.control(cp = 0.01))
fancyRpartPlot(dtreeM)
#fancyRpartPlot(dtreeM,cex=0.9)
#}

