#Remove all variables
cat("\014")  
rm(list=ls(all=TRUE))
#options(warn=-1)
#It is the Input table
#安裝 package 
#install.packages("mclust")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")
#install.packages('pROC')
#install.packages("DMwR")
#source("https://bioconductor.org/biocLite.R")

#載入library 
#library("rpart")
#library("rpart.plot")
#library("rattle")
source('loadData.R',encoding = "UTF-8")
library(mlbench)
library(pROC)
library(vioplot)
library(beanplot)
library(e1071)
library(caret)
library(Matrix)
library(data.table)
library(mlr)
library(magrittr)
library(mclust)
library(tree)
library(DMwR)
library(class)
library(randomForest)
#####Parameter

if(TRUE){
  #Number of Histogram
  SeedRange <- 50
  histDraw <- FALSE
  
  #Gini limitation for general infomation (ICU,Transfer....)
  ThresholdGiniGeneral <- 30
  ThresholdGini <- 9999
  showGini <- TRUE
  showRatio <- TRUE
  accumulateGini <- TRUE
  accumulateRatio <- TRUE
  featurAna <- FALSE
  featureExtract <- FALSE
}
#Record limiatiom
if(TRUE){
  #Original Threshold
  threshold <- 9999999
  outPatientThresholdOrigin <- 7
  hospitalizationThresholdOrigin <- 7
  emergencyThresholdOrigin <- 7
  transferHospitalThresholdOrigin <- 7
  ICUStayThresholdOrigin <- 28
  ColonizationThresholdOrigin <- 30
  
  CCIthresholdOrigin <- 365
  HealthCareThresholdOrigin <- 365
  DiagnosisThresholdOrigin <- 365
  DrugThresholdOrigin <- 365
  
  #Optimization Threshold initialize
  outPatientThresholdOptimization <- 0
  hospitalizationThresholdOptimization <- 0
  emergencyThresholdOptimization <- 0
  transferHospitalThresholdOptimization <- 0
  ICUStayThresholdOptimization <- 0
  ColonizationThresholdOptimization <- 0
  
  #Optimization Ratio initialize
  outPatientRatioOptimization <- 0
  hospitalizationRatioOptimization <- 0
  emergencyRatioOptimization <- 0
  transferHospitalRatioOptimization <- 0
  ICUStayRatioOptimization <- 0
  ColonizationRatioOptimization <- 0
  
  #CCIThresholdOptimization <- 0
  DrugThresholdOptimization <- vector(mode = "numeric", length = 8)
  DiagnosisNTUThresholdOptimization <- vector(mode = "numeric", length = 14)
  
  outPatientThresholdOptimizationVec <- vector()
  hospitalizationThresholdOptimizationVec <- vector()
  emergencyThresholdOptimizationVec <- vector()
  transferHospitalThresholdOptimizationVec <- vector()
  ICUStayThresholdOptimizationVec <- vector()
  ColonizationThresholdOptimizationVec <- vector()
  DrugThresholdOptimizationVec <- vector()
  DiagnosisNTUThresholdOptimizationVec <- vector()
  
  outPatientRatioOptimizationVec <- vector()
  hospitalizationRatioOptimizationVec <- vector()
  emergencyRatioOptimizationVec <- vector()
  transferHospitalRatioOptimizationVec <- vector()
  ICUStayRatioOptimizationVec <- vector()
  ColonizationRatioOptimizationVec <- vector()
  DrugRatioOptimizationVec <- vector()
  DiagnosisNTURatioOptimizationVec <- vector()
  
  #CCIRatioOptimization <- 0
  DrugRatioOptimization <- vector(mode = "numeric", length = 8)
  DiagnosisNTURatioOptimization <- vector(mode = "numeric", length = 14)
}

#Load Raw Data or load RData
#loadData()
load("Lab.RData")

if(FALSE){
  x <- vector(mode="numeric", length=1000)
  y <- vector(mode="numeric", length=1000)
  for(i in seq(1,1000)){
    print(i)
    ratio = 0.01*(i)
    x[i] = ratio
    y[i] = costGradient(emergencyVec,emergencyThresholdOptimization,ratio)
  }
  plot(x,y,xlab='Ratio',ylab='costGradient');
}

#Vectors -> remember to iniitialize before
if(TRUE){
  AgeVec <- vector()
  GenderVec <- vector()
  outPatientClinicVec <- vector()
  hospitalizationVec <- vector()
  emergencyVec <- vector()
  transferHospitalVec <- vector()
  ICUStayVec <- vector()
  ColonizationVec <- vector()
  
  hospitalizationDayVec <- vector()
  hospitalizationDaythisTimeVec <- vector()
  ICUStayDayVec <- vector()
  
  CCI1Vec <- vector()
  CCI2Vec <- vector()
  DrugVec <- vector()
  DrugDOT1Vec <- vector()
  DrugDOT2Vec <- vector()
  DiagnosisVec <- vector()
}
#Process All data to accelerate
for (cou in 1:nrow(labelOrigin)){
  print(cou)
  chartNO <- as.character(labelOrigin[[1]][cou])
  case <- as.logical(labelOrigin[[2]][cou])
  control <- as.logical(labelOrigin[[3]][cou])
  if(case&&control)
    next
  history <- as.character(labelOrigin[[8]][cou])
  historyArray <- strsplit(history[1],";")
  diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
  inDateLabel <- as.POSIXct(labelOrigin[[12]][cou],format='%Y/%m/%d')
  
  if(TRUE){
    ind_hospitalization_array <- which(!is.na(match(hospitalizationOrigin$CHARTNO2, chartNO)))
    ind_emergency_array <- which(!is.na(match(emergencyOrigin$CHARTNO2, chartNO)))
    ind_ICUStay_array <- which(!is.na(match(ICUStayOrigin$CHARTNO2, chartNO)))
    ind_outPatientClinic_array <- which(!is.na(match(outPatientClinicOrigin$CHARTNO2, chartNO)))
    ind_transferHospital_array <- which(!is.na(match(transferHospitalOrigin$CHARTNO2, chartNO)))
    ind_array_Colonization <- which(!is.na(match(ColonizationOrigin$CHARTNO2, chartNO)))
    
    MIN_hospitalization <- Inf
    emergencyDay <- Inf
    ICUStayDay <- Inf
    outPatientClinicDay <- Inf
    transferDay <- Inf
    colonizationDay <- Inf
    #Gender and Age
    ind <- match(chartNO, basicInformationOrigin$CHARTNO2)
    if(!is.na(ind)){
      Gender <- basicInformationOrigin$ADMINISTRATIVESEXCODE[ind]
      if(Gender=='M')
        Gender = 1
      if(Gender=='F')
        Gender = 0
      birth <- as.Date(paste(as.character(basicInformationOrigin$BIRTHDAY[ind]),'01'),format="%Y%m%d")
      diseaseTime <- as.POSIXct(as.character(historyArray[[1]][2]),format='%Y%m%d %H:%M')
      Age <- difftime(diseaseTime, birth, tz,units = c("days"))
      Age <- as.integer(Age/365.25)
    }
    else{
      Gender <- NULL
      Age <-NULL
    }
    AgeVec <- rbind(AgeVec,c(as.numeric(case),Age));
    GenderVec <- rbind(GenderVec,c(as.numeric(case),Gender));
    #MIN_hospitalization
    if(length(ind_hospitalization_array)>0){
      for (j in seq(1, length(ind_hospitalization_array))){
        index <- ind_hospitalization_array[j]
        #inDate <- as.Date(hospitalizationOrigin$INDATE[index],format='%m/%d/%Y')
        outDate <- as.Date(hospitalizationOrigin$OUTDATE[index],format='%m/%d/%Y')
        
        #diffIn <- as.numeric(difftime(inDateLabel, inDate, tz, units = c("days")))
        if(is.na(outDate))
          break;
        diffOut <- as.numeric(difftime(inDateLabel, outDate, tz, units = c("days")))
        if(diffOut > 0 && diffOut<MIN_hospitalization){
          MIN_hospitalization <- diffOut
        }
      }
    }
    hospitalizationVec <- rbind(hospitalizationVec,c(as.numeric(case),MIN_hospitalization));
    #hospitalizationDay
    hospitalizationDay <- 0
    #hospitalizationDaythisTime <- 0
    #ind_array <- which(!is.na(match(hospitalizationOrigin$CHARTNO2, chartNO)))
    
    hospitalizationDay <- as.numeric(difftime(diseaseDate, inDateLabel, tz, units = c("days")))
    hospitalizationDayVec <- rbind(hospitalizationDayVec,c(as.numeric(case),hospitalizationDay));
    #hospitalizationDaythisTimeVec <- rbind(hospitalizationDaythisTimeVec,c(as.numeric(case),hospitalizationDaythisTime));
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
    emergencyVec <- rbind(emergencyVec,c(as.numeric(case),emergencyDay));
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
    ICUStayVec <- rbind(ICUStayVec,c(as.numeric(case),ICUStayDay));
    #ICU StayDays
    ICUStayDays <- 0
    ICUStayData <- ""
    ind_array <- which(!is.na(match(ICUStayOrigin$CHARTNO2, chartNO)))
    if(length(ind_array)>0){
      for (j in seq(1, length(ind_array))){
        index <- ind_array[j]
        #print(index)
        inDate <- as.Date(ICUStayOrigin$TRANSFERINDATE[index],format='%m/%d/%Y')
        outDate <- as.Date(ICUStayOrigin$TRANSFEROUTDATE[index],format='%m/%d/%Y')
        
        diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
        diffOut <- as.numeric(difftime(diseaseDate, outDate, tz, units = c("days")))
        
        ICUStayDataTest <- strsplit(as.character(ICUStayData),";")
        ICUStayDataTest <- do.call(rbind, ICUStayDataTest)
        
        #Repeat or not??
        repeatFlag = FALSE
        if(length(ICUStayDataTest)!=0){
          for(j in seq(1, ncol(ICUStayDataTest), by = 4)){
            inputdate <- as.Date(as.character(ICUStayDataTest[1,j+1]),format='%Y-%m-%d')
            outputdate <- as.Date(as.character(ICUStayDataTest[1,j+3]),format='%Y-%m-%d')
            if(inDate == inputdate && outDate == outputdate){
              repeatFlag <- TRUE
            }
          }
        }
        
        if(!repeatFlag){
          if(is.na(diffOut)){
            ICUStayDays <- ICUStayDays + diffIn
          }
          else{
            if(diffIn*diffOut<=0 && diffIn>2)
              if(diffOut<0){
                ICUStayDays <- ICUStayDays + diffIn
              }
            else {
              if(diffOut <= 28 && diffOut >= 0 && diffIn-diffOut >= 2){
                ICUStayDays <- ICUStayDays +diffIn - diffOut
              }
            }
          }
        }
      }
    }
    ICUStayDayVec <- rbind(ICUStayDayVec,c(as.numeric(case),ICUStayDays));
    #outPatientClinicDay COMECLINICDATE
    if(length(ind_outPatientClinic_array)>0){
      for (j in seq(1, length(ind_outPatientClinic_array))){
        index <- ind_outPatientClinic_array[j]
        inDate <- as.Date(outPatientClinicOrigin$COMECLINICDATE[index],format='%Y/%m/%d')
        
        diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
        
        if(diffIn>0&&diffIn<outPatientClinicDay){
          outPatientClinicDay <- diffIn
        }
      }
    }
    outPatientClinicVec <- rbind(outPatientClinicVec,c(as.numeric(case),outPatientClinicDay));
    #Transfer From other Hospital
    if(length(ind_transferHospital_array)>0){
      for (j in seq(1, length(ind_transferHospital_array))){
        index <- ind_transferHospital_array[j]
        inDate <- as.Date(transferHospitalOrigin$INDATE[index],format='%m/%d/%Y')
        
        diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
        
        if(diffIn>0 && diffIn<transferDay){
          transferDay <- diffIn
        }
      }
    }
    transferHospitalVec <- rbind(transferHospitalVec,c(as.numeric(case),transferDay));
    #Colonization
    if(length(ind_array_Colonization)>0){
      for (j in seq(1, length(ind_array_Colonization))){
        index <- ind_array_Colonization[j]
        inDate <- as.Date(ColonizationOrigin$SAMPLINGDATECHAR[index],format='%Y%m%d')
        
        diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
        
        if(diffIn>0 && diffIn<colonizationDay){
          colonizationDay <- diffIn
        }
      }
    }
    ColonizationVec <- rbind(ColonizationVec,c(as.numeric(case),colonizationDay));
  }
  #CCI
  if(TRUE){
    CCI <- 0
    ind_array_1 <- which(!is.na(match(CCI_1_Origin$CHARTNO2, chartNO)))
    ind_array_2 <- which(!is.na(match(CCI_2_Origin$CHARTNO2, chartNO)))
    #ind_array_3 <- which(!is.na(match(CCI_3_Origin$CHARTNO2, chartNO)))
    ind_array_4 <- which(!is.na(match(CCI_4_Origin$CHARTNO2, chartNO)))
    
    tempCCI_1<- data.frame(CHARTNO2=character(),
                           DIAGNOSISCODE=character(), 
                           DIAGNOSISENGNAME=character(), 
                           CREATEDATETIME=character(), 
                           stringsAsFactors=FALSE) 
    tempCCI_2 <- tempCCI_1
    tempCCI_3 <- tempCCI_1
    tempCCI_4 <- tempCCI_1
    if(length(ind_array_1)>0){
      tempCCI_1 <- subset(CCI_1_Origin[as.vector(ind_array_1),], select=c("CHARTNO2","DIAGNOSISCODE", "DIAGNOSISENGNAME","CREATEDATETIME"),stringsAsFactors=FALSE)
      tempCCI_1[,4] <- sapply(tempCCI_1[,4],as.character) 
      #tempCCI_1 <- as.character()
      for (j in seq(1, length(ind_array_1))){
        stringTime <-as.character(tempCCI_1[j,4])
        stringTimeArray <- strsplit(stringTime," ")
        stringTimeTransformed <- stringTimeArray[[1]][1]
        date <- as.POSIXct(as.character(stringTimeArray[[1]][3]),format= "%H:%M:%S")
        #if(!is.na(stringTimeArray[[1]][2]))
        if(stringTimeArray[[1]][2]=="下午")
          date <- date + 12*60*60 # add 3 hours
        stringTimeTransformed <-paste(stringTimeTransformed,format(date, format="%H:%M:%S"))
        tempCCI_1[j,4] <- stringTimeTransformed
      }
    }
    if(length(ind_array_2)>0){
      for (j in seq(1, length(ind_array_2))){
        counts <- 5
        #print(j)
        while(!is.na(as.numeric(CCI_2_Origin[ind_array_2[j],counts])[CCI_2_Origin[ind_array_2[j],counts]]) && counts<25){
          rm(newRowCCI2)
          stringTime <-as.character(CCI_2_Origin[ind_array_2[j],25])
          stringTime <-strftime(strptime(stringTime,"%m/%d/%Y"),"%Y/%m/%d")
          stringTime <- paste(stringTime,"00:00:00")
          newRowCCI2<-data.frame(chartNO,as.character(CCI_2_Origin[ind_array_2[j],counts]),as.character(CCI_2_Origin[ind_array_2[j],counts+21]),stringTime,stringsAsFactors=FALSE)
          names(newRowCCI2)<-c("CHARTNO2","DIAGNOSISCODE", "DIAGNOSISENGNAME","CREATEDATETIME")
          tempCCI_2 <- rbind(tempCCI_2, newRowCCI2)
          counts <- counts + 1 
        }
        
      }
    }
    if(length(ind_array_4)>0){
      tempCCI_4 <- subset(CCI_4_Origin[as.vector(ind_array_4),], select=c("CHARTNO2","DIAGNOSISCODE", "DIAGNOSISENGNAME","CREATEDATETIME"))
      tempCCI_4[,4] <- sapply(tempCCI_4[,4],as.character) 
      for (j in seq(1, length(ind_array_4))){
        tempCCI_4[j,4] <- paste(format(as.Date(tempCCI_4[j,4],format = "%Y/%m/%d"), "%Y/%m/%d"),"00:00:00")
      }
    }
    
    tempCCIResult_1 <- rbind(tempCCI_2,tempCCI_4)
    tempCCIResult_2 <- rbind(tempCCI_1, tempCCI_2,tempCCI_4)
    
    #It is the transfer table
    CCI1 <- 0
    CCI2 <- 0
    Catagory <- matrix(FALSE, nrow = 1, ncol = 17)
    if(nrow(tempCCIResult_1)>0){
      for(CCIindex in 1:nrow(tempCCIResult_1)){
        CCIDate <- as.POSIXct(tempCCIResult_1[CCIindex,4],format = "%Y/%m/%d %H:%M:%S")
        diffCCIDisease <- as.numeric(difftime(diseaseDate, CCIDate, tz, units = c("days")))
        
        if(diffCCIDisease>=0 && diffCCIDisease<=CCIthresholdOrigin){
          OriginICD <- toString(tempCCIResult_1[CCIindex,2])
          OriginICD <- gsub('\\.','',OriginICD)
          for(CCI_Index in 1:nrow(CCIStandard)){
            if(grepl(CCIStandard[CCI_Index,3], OriginICD, ignore.case = TRUE)||grepl(CCIStandard[CCI_Index,2], OriginICD, ignore.case = TRUE)){
              if(!Catagory[CCIStandard[CCI_Index,5]]){
                CCI1 <- CCI1 + CCIStandard[CCI_Index,1]
                Catagory[CCIStandard[CCI_Index,5]] <- TRUE
              }
            }
          }
        }
      }
    }
    Catagory <- matrix(FALSE, nrow = 1, ncol = 17)
    if(nrow(tempCCIResult_2)>0){
      for(CCIindex in 1:nrow(tempCCIResult_2)){
        CCIDate <- as.POSIXct(tempCCIResult_2[CCIindex,4],format = "%Y/%m/%d %H:%M:%S")
        diffCCIDisease <- as.numeric(difftime(diseaseDate, CCIDate, tz, units = c("days")))
        if(diffCCIDisease>=0 && diffCCIDisease<=CCIthresholdOrigin){
          OriginICD <- toString(tempCCIResult_2[CCIindex,2])
          OriginICD <- gsub('\\.','',OriginICD)
          for(CCI_Index in 1:nrow(CCIStandard)){
            if(grepl(CCIStandard[CCI_Index,3], OriginICD, ignore.case = TRUE)||grepl(CCIStandard[CCI_Index,2], OriginICD, ignore.case = TRUE)){
              if(!Catagory[CCIStandard[CCI_Index,5]]){
                CCI2 <- CCI2 + CCIStandard[CCI_Index,1]
                Catagory[CCIStandard[CCI_Index,5]] <- TRUE
              }
            }
          }
        }
      }
    }
    CCI1Vec <- rbind(CCI1Vec,c(as.numeric(case),CCI1));
    CCI2Vec <- rbind(CCI2Vec,c(as.numeric(case),CCI2));
  }
  #Drugs
  if(TRUE){
    Drug1_Antibacterial <- Inf
    Drug2_Antifungal <- Inf
    Drug3_Chemotherapy <- Inf
    Drug4_Gastric <- Inf
    Drug5_Immunosuppressive <- Inf
    Drug6_Monoclonal <- Inf
    Drug7_Steroid  <- Inf
    Drug8_TPN <- Inf
    
    DrugDT <- rep(Inf, 8)
    DOT1 <- vector(mode = "numeric", length = 8)
    DOT2 <- matrix(c(FALSE), nrow = 8, ncol = 90)
    
    which(!is.na(match(Diagnosis_Procedure$CHARTNO2, chartNO)))
    ind_Diagnosis <- which(!is.na(match(Diagnosis$CHARTNO2,chartNO)))
    ind_Diagnosis_TPN <- which(!is.na(match(Diagnosis_TPN$CHARTNO2,chartNO)))
    ind_Diagnosis_DOT_1 <- which(!is.na(match(Diagnosis_DOT_1$CHARTNO2,chartNO)))
    ind_Diagnosis_DOT_1_TPN <- which(!is.na(match(Diagnosis_DOT_1_TPN$CHARTNO2,chartNO)))
    ind_Diagnosis_DOT_2 <- which(!is.na(match(Diagnosis_DOT_2$CHARTNO2,chartNO)))
    ind_Diagnosis_DOT_2_TPN <- which(!is.na(match(Diagnosis_DOT_2_TPN$CHARTNO2,chartNO)))
    
    #antibacterial, antifungal, PPI 30
    #others 90
    if(length(ind_Diagnosis)>0){
      for(Diagnosisindex in 1:length(ind_Diagnosis)){
        index <- ind_Diagnosis[Diagnosisindex]
        Drug <- Diagnosis[[2]][index]
        DrugDate <- Diagnosis[[3]][index]
        ind_Drug <- which(!is.na(match(Drug_Origin$DRUGCODE, Drug)))
        if(!is.na(DrugDate)){
          diffDRUG <- as.numeric(difftime(diseaseDate, as.Date(DrugDate), tz, units = c("days")))
          #Bug!! ind_Drug==na
          if(length(ind_Drug)>0&&diffDRUG>0){
            for(Drugindex in 1:length(ind_Drug)){
              DrugCatalogIndex <- ind_Drug[Drugindex]
              DrugCatalog <- Drug_Origin[[2]][DrugCatalogIndex]
              if(DrugCatalog==1&&DrugDT[1]>diffDRUG){
                DrugDT[1] <- diffDRUG
              }
              if(DrugCatalog==2&&DrugDT[2]>diffDRUG){
                DrugDT[2] <- diffDRUG
              }
              if(DrugCatalog==3&&DrugDT[3]>diffDRUG){
                DrugDT[3] <- diffDRUG
              }
              if(DrugCatalog==4&&DrugDT[4]>diffDRUG){
                DrugDT[4] <- diffDRUG
              }
              if(DrugCatalog==5&&DrugDT[5]>diffDRUG){
                DrugDT[5] <- diffDRUG
              }
              if(DrugCatalog==6&&DrugDT[6]>diffDRUG){
                DrugDT[6] <- diffDRUG
              }
              if(DrugCatalog==7&&DrugDT[7]>diffDRUG){
                DrugDT[7] <- diffDRUG
              }
            }
          }
        }
      }
    }
    if(length(ind_Diagnosis_TPN)>0){
      for(Diagnosisindex in 1:length(ind_Diagnosis_TPN)){
        index <- ind_Diagnosis_TPN[Diagnosisindex]
        Drug <- Diagnosis_TPN[[2]][index]
        DrugDate <- Diagnosis_TPN[[3]][index]
        ind_Drug <- which(!is.na(match(Drug_8_Origin$DRUGCODE, Drug)))
        if(!is.na(DrugDate)){
          diffDRUG <- as.numeric(difftime(diseaseDate, as.Date(DrugDate), tz, units = c("days")))
          if(diffDRUG <= threshold && diffDRUG > 0 && DrugDT[8]>diffDRUG &&diffDRUG>0){
            DrugDT[8] <- diffDRUG
          }
        }
      }
    }
    if(length(ind_Diagnosis_DOT_1)>0){
      for(Diagnosisindex in 1:length(ind_Diagnosis_DOT_1)){
        index <- ind_Diagnosis_DOT_1[Diagnosisindex]
        
        Flag90 <- FALSE
        Flag30 <- FALSE
        Drug <- Diagnosis_DOT_1[[2]][index]
        DrugStartDate <- as.POSIXct(as.character(Diagnosis_DOT_1[[3]][index]),format='%Y/%m/%d')
        DrugEndDate <- as.POSIXct(as.character(Diagnosis_DOT_1[[4]][index]),format='%Y/%m/%d')
        DRUGDOT <- as.numeric(difftime(DrugEndDate, as.Date(DrugStartDate), tz, units = c("days")))
        
        ind_Drug <- which(!is.na(match(Drug_Origin$DRUGCODE, Drug)))
        if(length(ind_Drug)>0){
          diffDRUG <- as.numeric(difftime(diseaseDate, as.Date(DrugEndDate), tz, units = c("days")))
          
          if(diffDRUG <= 90 && diffDRUG > 0){
            Flag90 <- TRUE
          }
          
          if(diffDRUG <= 30 && diffDRUG > 0){
            Flag30 <- TRUE
          }
          
          #Bug!! ind_Drug==na
          if(length(ind_Drug)>0&&diffDRUG>0){
            for(Drugindex in 1:length(ind_Drug)){
              DrugCatalogIndex <- ind_Drug[Drugindex]
              DrugCatalog <- Drug_Origin[[2]][DrugCatalogIndex]
              if(DrugCatalog==1 && Flag30 && DOT1[1] < DRUGDOT){
                DOT1[1] <- DRUGDOT
              }
              if(DrugCatalog==2 && Flag30 && DOT1[2] < DRUGDOT){
                DOT1[2] <- DRUGDOT
              }
              if(DrugCatalog==3 && Flag90 && DOT1[3] < DRUGDOT){
                DOT1[3] <- DRUGDOT
              }
              if(DrugCatalog==4 && Flag30 && DOT1[4] < DRUGDOT){
                DOT1[4] <- DRUGDOT
              }
              if(DrugCatalog==5 && Flag90 && DOT1[5] < DRUGDOT){
                DOT1[5] <- DRUGDOT
              }
              if(DrugCatalog==6 && Flag90 && DOT1[6] < DRUGDOT){
                DOT1[6] <- DRUGDOT
              }
              if(DrugCatalog==7 && Flag90 && DOT1[7] < DRUGDOT){
                DOT1[7] <- DRUGDOT
              }
            }
          }
        }
      }
    }
    if(length(ind_Diagnosis_DOT_1_TPN)>0){
      for(Diagnosisindex in 1:length(ind_Diagnosis_DOT_1_TPN)){
        index <- ind_Diagnosis_DOT_1_TPN[Diagnosisindex]
        
        Flag90 <- FALSE
        Flag30 <- FALSE
        Drug <- Diagnosis_DOT_1_TPN[[2]][index]
        DrugStartDate <- as.POSIXct(as.character(Diagnosis_DOT_1_TPN[[3]][index]),format='%m/%d/%Y')
        DrugEndDate <- as.POSIXct(as.character(Diagnosis_DOT_1_TPN[[4]][index]),format='%m/%d/%Y')
        DRUGDOT <- as.numeric(difftime(DrugEndDate, as.Date(DrugStartDate), tz, units = c("days")))
        
        ind_Drug <- which(!is.na(match(Drug_8_Origin$DRUGCODE, Drug)))
        if(length(ind_Drug)>0){
          diffDRUG <- as.numeric(difftime(diseaseDate, as.Date(DrugEndDate), tz, units = c("days")))
          
          if(diffDRUG <= 90 && diffDRUG > 0){
            Flag90 <- TRUE
          }
          
          if(diffDRUG <= 30 && diffDRUG > 0){
            Flag30 <- TRUE
            if(DRUGDOT+diffDRUG<=30)
              DOT1[8] <- DRUGDOT
            else
              DOT1[8] <- 30 - diffDRUG
            
          }
        }
      }
    }
    if(length(ind_Diagnosis_DOT_2)>0){
      for(Diagnosisindex in 1:(length(ind_Diagnosis_DOT_2))){
        index <- ind_Diagnosis_DOT_2[Diagnosisindex]
        
        Flag90 <- FALSE
        Flag30 <- FALSE
        Drug <- Diagnosis_DOT_2[[2]][index]
        DrugDate <- as.POSIXct(as.character(Diagnosis_DOT_2[[3]][index]),format='%Y-%m-%d')
        
        ind_Drug <- which(!is.na(match(Drug_Origin$DRUGCODE, Drug)))
        if(length(ind_Drug)>0){
          diffDRUG <- as.numeric(difftime(diseaseDate, as.Date(DrugDate), tz, units = c("days")))
          
          if(diffDRUG <= 90 && diffDRUG > 0){
            Flag90 <- TRUE
          }
          
          if(diffDRUG <= 30 && diffDRUG > 0){
            Flag30 <- TRUE
          }
          
          #Bug!! ind_Drug==na
          if(length(ind_Drug)>0&&diffDRUG>0){
            for(Drugindex in 1:(length(ind_Drug))){
              DrugCatalogIndex <- ind_Drug[Drugindex]
              DrugCatalog <- Drug_Origin[[2]][DrugCatalogIndex]
              diffDRUG <- as.integer(diffDRUG)
              if(DrugCatalog==1 && Flag30){
                DOT2[DrugCatalog,diffDRUG] <- TRUE
              }
              if(DrugCatalog==2 && Flag30){
                DOT2[DrugCatalog,diffDRUG] <- TRUE
              }
              if(DrugCatalog==3 && Flag90){
                DOT2[DrugCatalog,diffDRUG] <- TRUE
              }
              if(DrugCatalog==4 && Flag30){
                DOT2[DrugCatalog,diffDRUG] <- TRUE
              }
              if(DrugCatalog==5 && Flag90){
                DOT2[DrugCatalog,diffDRUG] <- TRUE
              }
              if(DrugCatalog==6 && Flag90){
                DOT2[DrugCatalog,diffDRUG] <- TRUE
              }
              if(DrugCatalog==7 && Flag90){
                DOT2[DrugCatalog,diffDRUG] <- TRUE
              }
            }
          }
        }
      }
    }
    DOT2 <- DOT2*1
    DOT2 <- rowSums(DOT2)
    DOT2_TPN <- matrix(c(FALSE), nrow = 1, ncol = 90)
    if(length(ind_Diagnosis_DOT_2_TPN)>0){
      for(Diagnosisindex in 1:(length(ind_Diagnosis_DOT_2_TPN))){
        index <- ind_Diagnosis_DOT_2_TPN[Diagnosisindex]
        
        Flag90 <- FALSE
        Flag30 <- FALSE
        Drug <- Diagnosis_DOT_2_TPN[[2]][index]
        DrugDate <- as.POSIXct(as.character(Diagnosis_DOT_2_TPN[[3]][index]),format='%Y-%m-%d')
        
        ind_Drug <- which(!is.na(match(Drug_8_Origin$DRUGCODE, Drug)))
        if(length(ind_Drug)>0){
          #cat("hahahahahahahaha")
          diffDRUG <- as.numeric(difftime(diseaseDate, as.Date(DrugDate), tz, units = c("days")))
          
          if(diffDRUG <= 90 && diffDRUG > 0){
            Flag90 <- TRUE
            
            DOT2_TPN[1,as.integer(diffDRUG)] <- TRUE
          }
          
          if(diffDRUG <= 30 && diffDRUG > 0){
            Flag30 <- TRUE
          }
          
          
        }
      }
    }
    DOT2_TPN <- DOT2_TPN*1
    DOT2_TPN <- rowSums(DOT2_TPN)
    DOT2[8] <- DOT2_TPN[1]
    Drug1_Antibacterial <- Inf
    Drug2_Antifungal <- Inf
    Drug3_Chemotherapy <- Inf
    Drug4_Gastric <- Inf
    Drug5_Immunosuppressive <- Inf
    Drug6_Monoclonal <- Inf
    Drug7_Steroid  <- Inf
    Drug8_TPN <- Inf
    
    DrugVec <- rbind(DrugVec,c(as.numeric(case),DrugDT));
    DrugDOT1Vec <- rbind(DrugDOT1Vec,c(as.numeric(case),DOT1));
    DrugDOT2Vec <- rbind(DrugDOT2Vec,c(as.numeric(case),DOT2));
  }
  #Diagnosis
  if(TRUE){
    DiagnosisDT <- rep(Inf, 14)
    ind_Procedure <- which(!is.na(match(Diagnosis_Procedure_NTU$CHARTNO2, chartNO)))
    if(length(ind_Procedure)>0){
      for(Diagnosisindex in 1:(length(ind_Procedure))){
        index <- ind_Procedure[Diagnosisindex]
        Procedure <- Diagnosis_Procedure_NTU[index,2]
        ProcedureDate <- Diagnosis_Procedure_NTU[index,3]
        
        diffProcedure <- as.numeric(difftime(diseaseDate, as.Date(ProcedureDate), tz, units = c("days")))
        
        if(diffProcedure <= threshold && diffProcedure > 0){
          ind_EndotrachealIntubation <- which(!is.na(match(CCI_EndotrachealIntubation_NTU$ordercode, Procedure)))
          ind_ExtracorporealMembraneOxygenation <- which(!is.na(match(CCI_ExtracorporealMembraneOxygenation_NTU$ordercode, Procedure)))
          ind_Hemodialysis <- which(!is.na(match(CCI_Hemodialysis_NTU$ordercode, Procedure)))
          ind_IndwellingCentralBenousCatheters <- which(!is.na(match(CCI_IndwellingCentralBenousCatheters_NTU$ordercode, Procedure)))
          ind_IndwellingUrinaryCatheter<- which(!is.na(match(CCI_IndwellingUrinaryCatheter_NTU$ordercode, Procedure)))
          ind_IntravascularCatheter <- which(!is.na(match(CCI_IntravascularCatheter_NTU$ordercode, Procedure)))
          ind_MechanicValve <- which(!is.na(match(CCI_MechanicValve_NTU$ordercode, Procedure)))
          ind_MechanicalVentilation <- which(!is.na(match(CCI_MechanicalVentilation_NTU$ordercode, Procedure)))
          ind_Pacemaker <- which(!is.na(match(CCI_Pacemaker_NTU$ordercode, Procedure)))
          ind_PleuralDrainage <- which(!is.na(match(CCI_PleuralDrainage_NTU$ordercode, Procedure)))
          ind_ProsthesisJoint <- which(!is.na(match(CCI_ProsthesisJoint_NTU$ordercode, Procedure)))
          ind_SolidOrganTransplantation <- which(!is.na(match(CCI_SolidOrganTransplantation_NTU$ordercode, Procedure)))
          ind_TotalParenteralNutrition <- which(!is.na(match(CCI_TotalParenteralNutrition_NTU$ORDERCODE, Procedure)))
          ind_Surgery_NTU <- which(!is.na(match(CCI_Surgery_NTU$ordercode, Procedure)))
          
          if(length(ind_Surgery_NTU)>0&&DiagnosisDT[1]>diffProcedure)
            DiagnosisDT[1] <- diffProcedure
          if(length(ind_EndotrachealIntubation)>0&&DiagnosisDT[2]>diffProcedure)
            DiagnosisDT[2] <- diffProcedure
          if(length(ind_ExtracorporealMembraneOxygenation)>0&&DiagnosisDT[3]>diffProcedure)
            DiagnosisDT[3] <- diffProcedure
          if(length(ind_Hemodialysis)>0&&DiagnosisDT[4]>diffProcedure)
            DiagnosisDT[4] <- diffProcedure
          if(length(ind_IndwellingCentralBenousCatheters)>0&&DiagnosisDT[5]>diffProcedure)
            DiagnosisDT[5] <- diffProcedure
          if(length(ind_IndwellingUrinaryCatheter)>0&&DiagnosisDT[6]>diffProcedure)
            DiagnosisDT[6] <- diffProcedure
          if(length(ind_IntravascularCatheter)>0&&DiagnosisDT[7]>diffProcedure)
            DiagnosisDT[7] <- diffProcedure
          if(length(ind_MechanicValve)>0&&DiagnosisDT[8]>diffProcedure)
            DiagnosisDT[8] <- diffProcedure
          if(length(ind_MechanicalVentilation)>0&&DiagnosisDT[9]>diffProcedure)
            DiagnosisDT[9] <- diffProcedure
          if(length(ind_Pacemaker)>0&&DiagnosisDT[10]>diffProcedure)
            DiagnosisDT[10] <- diffProcedure
          if(length(ind_PleuralDrainage)>0&&DiagnosisDT[11]>diffProcedure)
            DiagnosisDT[11] <- diffProcedure
          if(length(ind_ProsthesisJoint)>0&&DiagnosisDT[12]>diffProcedure)
            DiagnosisDT[12] <- diffProcedure
          if(length(ind_SolidOrganTransplantation)>0&&DiagnosisDT[13]>diffProcedure)
            DiagnosisDT[13] <- diffProcedure
          if(length(ind_TotalParenteralNutrition)>0&&DiagnosisDT[14]>diffProcedure)
            DiagnosisDT[14] <- diffProcedure
          
        }
        
      }
    }
    DiagnosisVec <- rbind(DiagnosisVec,c(as.numeric(case),DiagnosisDT));
  }
}

DataPrcoessed <- cbind(AgeVec,GenderVec[,2],outPatientClinicVec[,2],hospitalizationVec[,2],emergencyVec[,2],
                       transferHospitalVec[,2],ICUStayVec[,2],ColonizationVec[,2],hospitalizationDayVec[,2],
                       ICUStayDayVec[,2],CCI1Vec[,2],CCI2Vec[,2],DrugVec[,2:ncol(DrugVec)],
                       DrugDOT1Vec[,2:ncol(DrugDOT1Vec)],DrugDOT2Vec[,2:ncol(DrugDOT2Vec)],
                       DiagnosisVec[,2:ncol(DiagnosisVec)])

#save.image("Lab2.RData")
load("Lab2.RData")

#filter duplicated data
DataPrcoessed <- DataPrcoessed[!duplicated(labelOrigin[1]), ]

# Ratio Functions
Sigmoid <- function(x,Threshold,ratio){
  if (is.na(x)||is.infinite(x))
    return (0)
  return(1/(1+exp(ratio*(x-Threshold))))
}
optimizeRatio <- function(input,Threshold,ratio=1){
  #input <- input[which(is.finite(input[,2])),]
  #input <- input[which((as.matrix(input[,2])<30)),]
  optRatio <- ratio
  learningRate <- 1
  costGradientValue <- 100
  decay <- 0.997
  decayNow <- 1
  step <- 100
  while(abs(step)>0.0001){
    if(optRatio != 0.01){
      #cost <- as.numeric(costFunctionoptimizeRatio(input,Threshold,optRatio))
      costGradientValue <- as.numeric(costGradient(input,Threshold,optRatio))
      cat("costGradient",as.numeric(costGradientValue),"\t")
      #cat("cost",cost,"\n")
    }
    if(optRatio - costGradientValue * learningRate * decayNow<0)
      optRatio <- 0.01
    else{
      step <- costGradientValue * learningRate * decayNow
      optRatio <- optRatio - step
    }
    decayNow <- decayNow*decay
    cat("optRatio",as.numeric(optRatio),"\t")
    cat("decayNow",decayNow,"\n")
  }
  #temp <- sapply(input[,2],function(x){Sigmoid(x,Threshold,optRatio)})
  #temp2 <- shapiro.test(model$residual)
  cat("************OptRatio:",optRatio,"\n")
  return(optRatio)
}
optimizeRatioGoldenSection <- function(input,Threshold,ratio=1){
  Times <- 20 #0.618^20~6.6*10^-5
  GoldenRatio <- (3-sqrt(5))/2
  
  Left <- 0
  Right <- ratio
  
  for (x in c(1:Times)){
    LeftPoint <- (Right-Left)*GoldenRatio+Left
    RightPoint <- Right-(Right-Left)*GoldenRatio
    #if(costLeft>=costRight)
    #cat("LeftPoint: ",LeftPoint , "\t")
    #cat("RightPoint: ",RightPoint , "\t")
    costLeft <- as.numeric(costFunctionoptimizeRatio(input,Threshold,LeftPoint))
    #cat("costLeft: ",costLeft , "\t")
    #if(costLeft<=costRight)
    costRight <- as.numeric(costFunctionoptimizeRatio(input,Threshold,RightPoint))
    #cat("costRight:",costRight, "\n")
    
    if(costLeft>costRight)
      Left <- LeftPoint
    else
      Right <- RightPoint
  }
  optRatioLeft <- (Right+Left)/2
  costLeftFinal <- (costLeft+costRight)/2
  
  Left <- 1
  Right <- 10
  
  for (x in c(1:Times)){
    LeftPoint <- (Right-Left)*GoldenRatio+Left
    RightPoint <- Right-(Right-Left)*GoldenRatio
    #if(costLeft>=costRight)
    #cat("LeftPoint: ",LeftPoint , "\t")
    #cat("RightPoint: ",RightPoint , "\t")
    costLeft <- as.numeric(costFunctionoptimizeRatio(input,Threshold,LeftPoint))
    #cat("costLeft: ",costLeft , "\t")
    #if(costLeft<=costRight)
    costRight <- as.numeric(costFunctionoptimizeRatio(input,Threshold,RightPoint))
    #cat("costRight:",costRight, "\n")
    
    if(costLeft>costRight)
      Left <- LeftPoint
    else
      Right <- RightPoint
  }
  optRatioRight <- (Right+Left)/2
  costRightFinal <- (costLeft+costRight)/2
  
  if(costLeftFinal > costRightFinal)
    optRatio <- optRatioRight
  else
    optRatio <- optRatioLeft
  
  cat("***OptRatio:",optRatio,"\n")
  return(optRatio)
}
costGradient <- function(input,Threshold,ratio=1){  
  if(is.infinite(Threshold))
    return(0)
  #input <- input[which(is.finite(input[,2])),]
  #input <- input[which(!is.na(input[,2])),]
  T_Amounts <- sum(input[,1])
  N_Amounts <- sum(1-input[,1])
  costMatrix <- matrix(0, nrow = nrow(input), ncol = 4)
  costMatrix[,1:2] <- input[,1:2]
  costFunction_1 <-function(x,y)
  {
    if(y==1)
      return ((1-Sigmoid(x,Threshold,ratio)**2+(0.5-dnorm(log10(ratio)))**2)/T_Amounts)
    else
      return (((Sigmoid(x,Threshold,ratio))**2+(0.5-dnorm(log10(ratio)))**2)/N_Amounts)
  }
  costFunction_2 <-function(x,y)
  {
    if(y==1)
      return ((Sigmoid(x,Threshold,ratio)**2+(0.5-dnorm(log10(ratio)))**2)/T_Amounts)
    else
      return (((1-Sigmoid(x,Threshold,ratio))**2+(0.5-dnorm(log10(ratio)))**2)/N_Amounts)
  }
  
  costMatrix[,3] <- mapply(costFunction_1,x=costMatrix[,2],y=costMatrix[,1])
  costMatrix[,4] <- mapply(costFunction_2,x=costMatrix[,2],y=costMatrix[,1])
  
  c1 <- sum(costMatrix[,3])
  c2 <- sum(costMatrix[,4])
  
  if(c1<c2)
    index <- 2
  else
    index <- 1
  
  Gradient <- 0.0
  temp1 <- 0
  temp2 <- 0
  for(i in seq(1,nrow(input))){
    y <- input[i,1]
    x <- input[i,2]
    s <- Sigmoid(x,Threshold,ratio)
    f <- dnorm(log10(ratio))
    if(is.infinite(x))
      temp = 0
    else{
      if(index==1){
        if(y==1){
          temp <- (2*s*s*(s-1)*(x-Threshold)+2*(0.5-f)*f*log10(ratio)/ratio/log(10))*y/T_Amounts
          #temp1 <- 2*s*s*(s-1)*(x-Threshold)*y/T_Amounts+temp1
          #temp2 <- 2*(0.5-f)*f*log10(ratio)/ratio/log(10)*y/T_Amounts+temp2
          #print(2*s*s*(s-1)*(x-Threshold)*y/T_Amounts)
        }
        else{
          temp <- (2*(s-1)*(s-1)*s*(x-Threshold)+2*(0.5-f)*f*log10(ratio)/ratio/log(10))*(1-y)/N_Amounts
          #temp1 <- 2*(s-1)*(s-1)*s*(x-Threshold)*(1-y)/N_Amounts+temp1
          #temp2 <- 2*(0.5-f)*f*log10(ratio)/ratio/log(10)*(1-y)/N_Amounts+temp2
          #print(2*(s-1)*(s-1)*s*(x-Threshold)*(1-y))
        }
      }
      else{
        if(y==0)
          temp <- (2*s*s*(s-1)*(x-Threshold)+2*(0.5-f)*f*log10(ratio)/ratio/log(10))*(1-y)/N_Amounts
        else
          temp <- (2*(s-1)*(s-1)*s*(x-Threshold)+2*(0.5-f)*f*log10(ratio)/ratio/log(10))*y/T_Amounts
      }
    }
    
    if(is.na(y)||is.na(x)||is.na(s)||is.na(f)||is.na(temp)){
      print(Threshold)
      print(ratio)
      print(y)
      print(x)
      print(s)
      print(f)
    }
    Gradient <- Gradient+temp
  }
  return(Gradient)
}
costFunctionoptimizeRatio <- function(input,Threshold,ratio){
  
  #input <- input[which(is.finite(as.matrix(input[,2]))),]
  costMatrix <- matrix(0, nrow = nrow(input), ncol = 4)
  costMatrix[,1:2] <- as.matrix(input[,1:2])
  
  T_Amounts <- sum(costMatrix[,1])
  N_Amounts <- sum(1-costMatrix[,1])
  ratio <- as.numeric(ratio)
  # 1-> infinite
  # 2,RMSE-> 0.03
  # 3
  costType <- 2
  
  # 1 -> +1/0
  # 0 -> 0/+1
  costFunction_1 <-function(x,y)
  {
    if(costType==1){
      if(y==1)
        return (-1*log(Sigmoid(x,Threshold,ratio))/T_Amounts)
      else
        return (0)
    }
    if(costType==2){
      if(y==1)
        return ((1-Sigmoid(x,Threshold,ratio))**2/T_Amounts)
      else
        return (Sigmoid(x,Threshold,ratio)**2/N_Amounts)
    }
    if(costType==3){
      if(y==1)
        return (y*y*(1-Sigmoid(x,Threshold,ratio))/T_Amounts)
      else
        return (((1-y)*(1-y)*Sigmoid(x,Threshold,ratio))/N_Amounts)
    }
    if(costType==4){
      if(y==1)
        return ((1-Sigmoid(x,Threshold,ratio)**2+(0.5-dnorm(log10(ratio)))**2)/T_Amounts)
      else
        return (((Sigmoid(x,Threshold,ratio))**2+(0.5-dnorm(log10(ratio)))**2)/N_Amounts)
    }
  }
  
  # 1 -> 0/+1
  # 0 -> +1/0
  costFunction_2 <-function(x,y)
  {
    if(costType==1){
      if(y==0)
        return (-1*log(1-Sigmoid(x,Threshold,ratio))/N_Amounts)
      else
        return (0)
    }    
    if(costType==2){
      if(y==1)
        return (Sigmoid(x,Threshold,ratio)**2/T_Amounts)
      else
        return ((1-Sigmoid(x,Threshold,ratio))**2/N_Amounts)
    }
    if(costType==3){
      if(y==1)
        return (y*y*Sigmoid(x,Threshold,ratio)/T_Amounts)
      else
        return ((1-y)*(1-y)*(1-Sigmoid(x,Threshold,ratio))/N_Amounts)
    }
    if(costType==4){
      if(y==1)
        return ((Sigmoid(x,Threshold,ratio)**2+(0.5-dnorm(log10(ratio)))**2)/T_Amounts)
      else
        return (((1-Sigmoid(x,Threshold,ratio))**2+(0.5-dnorm(log10(ratio)))**2)/N_Amounts)
    }
    #return(y*y*Sigmoid(x,Threshold,ratio)+
    #      (1-y)*(1-y)*(1-Sigmoid(x,Threshold,ratio)))
  }
  
  costMatrix[,3] <- mapply(costFunction_1,x=costMatrix[,2],y=costMatrix[,1])
  costMatrix[,4] <- mapply(costFunction_2,x=costMatrix[,2],y=costMatrix[,1])
  c1 <- sum(costMatrix[,3])
  c2 <- sum(costMatrix[,4])
  cost = min(c1,c2)
  return(cost)
}
plotCostFunction <- function(data,t){
  expon <- FALSE
  if(!expon){
    x <- vector(mode="numeric", length=200)
    y <- vector(mode="numeric", length=200)
    for(i in seq(200)){
      #print(i)
      ratio = 0.01*(i)
      x[i] = ratio
      y[i] = costFunctionoptimizeRatio(data,t,ratio)
    }
    plot(x,y,xlab='Ratio',ylab='Cost');
  }
  else{
    x <- vector(mode="numeric", length=200)
    y <- vector(mode="numeric", length=200)
    for(i in seq(1,200)){
      #print(i)
      ratio = 10**((i-50)/5)
      x[i] = ratio
      y[i] = costFunctionoptimizeRatio(data,t,ratio)
    }
    plot((seq(1,200)-50)/5,y,xlab='Ratio(10^)',ylab='Cost');
  }
  
}
if(FALSE){
  attach(mtcars)
  par(mfrow=c(3,2))
  plotCostFunction(outPatientClinicVec,outPatientThresholdOptimization);
  plotCostFunction(hospitalizationDayVec,hospitalizationThresholdOptimization);
  plotCostFunction(emergencyVec,emergencyThresholdOptimization);
  plotCostFunction(transferHospitalVec,transferHospitalThresholdOptimization);
  plotCostFunction(ICUStayDayVec,ICUStayThresholdOptimization);
  plotCostFunction(ColonizationVec,ColonizationThresholdOptimization);
}
if(FALSE){
  optimizeRatioGoldenSection(outPatientClinicVec,outPatientThresholdOptimization);
  optimizeRatioGoldenSection(hospitalizationDayVec,hospitalizationThresholdOptimization);
  optimizeRatioGoldenSection(emergencyVec,emergencyThresholdOptimization);
  optimizeRatioGoldenSection(transferHospitalVec,transferHospitalThresholdOptimization);
  optimizeRatioGoldenSection(ICUStayDayVec,ICUStayThresholdOptimization);
  optimizeRatioGoldenSection(ColonizationVec,ColonizationThresholdOptimization);
}
plotNormalDistributionFunction <- function(){
  x <- vector(mode="numeric", length=10000)
  y <- vector(mode="numeric", length=10000)
  for(i in seq(1,10000)){
    #print(i)
    ratio = 0.001*i
    
    x[i] = ratio
    #y[i] = costFunctionoptimizeRatio(data,t,ratio)
    y[i] = (0.5-dnorm(log10(ratio)))**2
  }
  plot(x,y,xlab='Ratio(10^)',ylab='Cost');
}
# Threshold Functions
GiniPivot <- function(input,ThresholdGiniPivot){
  
  NCase <- sum(input[,1]==1)
  NControl <- sum(input[,1]==0)
  
  input <- input[order(input[,2]),c(1,2)]
  pivot <- Inf
  index <- 0
  pVector <- c()
  GiniVector <- c()
  tempp <- sum((input[,1]==1)*(input[,2]<ThresholdGiniPivot))
  tempn <- sum((input[,1]==2)*(input[,2]<ThresholdGiniPivot))
  for (j in seq(1, (nrow(input)-1))){
    #print(j)
    if(input[j,2] > ThresholdGiniPivot || is.infinite(input[j,2]))
      break;
    dataSize <- nrow(input)
    jtemp <- j
    if(input[j,2]==input[j+1,2])
      j = min(which((input[,2]!=as.numeric(input[j,2])) == TRUE))
    pivot_temp <- (input[j,2]+input[j+1,2])/2
    temp1 <- input[1:j,1]
    temp2 <- input[(j+1):nrow(input),1]
    temp1p <- length(which(temp1==1))
    temp1n <- length(which(temp1==0))
    temp2p <- length(which(temp2==1))
    temp2n <- length(which(temp2==0))
    
    #candidaTemp <- c(Positive=temp1p,Negative=NCase-temp1p)
    #BacteriaTemp <- c(Positive=temp1n,Negative=NControl-temp1n)
    #ChiTable <- rbind(candidaTemp,BacteriaTemp)
    
    #fiTest <- fisher.test(ChiTable)
    #propTest <- prop.test(ChiTable)
    #pValue <- fiTest$p.value
    
    #Gini Function
    
    Gini_temp <- (1-(temp1p*temp1p+temp1n*temp1n)/j/j)*j/(NControl+NCase)+
      (1-((NCase-temp1p)*(NCase-temp1p)+(NControl-temp1n)*(NControl-temp1n))/(NControl+NCase-j)/(NControl+NCase-j))*(NControl+NCase-j)/(NControl+NCase)
    pValue <- Gini_temp
    
    if(pValue<pivot){
      index <- j
      pivot <- pValue
    }
    #pVector <- c(pVector,pValue)
    GiniVector <- c(GiniVector,Gini_temp)
    #pVector <- c(pVector,pValue)
    j <- jtemp
  }
  #plot(GiniVector,pVector, xlab = "Gini Index",ylab = "pValue")
  #abline(h = 0.025, col="red")
  #cat("p-Value",pivot,"\n")
  
  #plot(pVector,xlab = "Index",ylab = "Gini Index")
  #abline(h = 0.025, col="red")
  #abline(v = (index+0.5), col="blue")
  if(is.infinite(input[index+1,2]))
    PIVOT <- input[index,2]
  else
    PIVOT <- (input[index,2]+input[index+1,2])/2
  cat("***OptThreshold: ",PIVOT,"\n")
  return (PIVOT)
}
#Feature Extracting
featureExtractionCrisp <- function(Data,threshold){
  Data[Data<=threshold] <- 1
  Data[Data>threshold] <- 0
  return (Data)
}
featureExtractionSoft <- function(Data,THRESHOLD,RATIO){
  Data <- mapply(Sigmoid,x=Data,Threshold=THRESHOLD,ratio=RATIO)
  return (Data)
}

#Calculate Threshold and Ratio
for (seed in 1:SeedRange){
  cat("seed:",seed, "\n")
  set.seed(seed)
  ind <- sample(1:nrow(DataPrcoessed), size = 0.75*nrow(DataPrcoessed))
  
  trainDataOrigin <- DataPrcoessed[ind,]
  testDataOrigin <- DataPrcoessed[-ind,]
  
  #trainDataOrigin <- labelOrigin
  if(showGini){
    outPatientThresholdOptimization <- GiniPivot(trainDataOrigin[,c(1,4)],ThresholdGiniGeneral)
    hospitalizationThresholdOptimization <- GiniPivot(trainDataOrigin[,c(1,5)],ThresholdGiniGeneral)
    emergencyThresholdOptimization <- GiniPivot(trainDataOrigin[,c(1,6)],ThresholdGiniGeneral)
    transferHospitalThresholdOptimization <- GiniPivot(trainDataOrigin[,c(1,7)],ThresholdGiniGeneral)
    ICUStayThresholdOptimization <- GiniPivot(trainDataOrigin[,c(1,8)],ThresholdGiniGeneral)
    ColonizationThresholdOptimization <- GiniPivot(trainDataOrigin[,c(1,9)],ThresholdGiniGeneral)
    print("Drug:")
    for(i in 1:8){
      DrugThresholdOptimization[i] <- GiniPivot(trainDataOrigin[,c(1,(i+13))],ThresholdGini)
    }
    print("Diagnosis:")
    for(i in 1:13){
      DiagnosisNTUThresholdOptimization[i] <- GiniPivot(trainDataOrigin[,c(1,i+37)],ThresholdGini)
    }
  }
  if(accumulateGini){
    outPatientThresholdOptimizationVec <- c(outPatientThresholdOptimizationVec,outPatientThresholdOptimization)
    hospitalizationThresholdOptimizationVec <- c(hospitalizationThresholdOptimizationVec,hospitalizationThresholdOptimization)
    emergencyThresholdOptimizationVec <- c(emergencyThresholdOptimizationVec,emergencyThresholdOptimization)
    transferHospitalThresholdOptimizationVec <- c(transferHospitalThresholdOptimizationVec,transferHospitalThresholdOptimization)
    ICUStayThresholdOptimizationVec <- c(ICUStayThresholdOptimizationVec,ICUStayThresholdOptimization)
    ColonizationThresholdOptimizationVec <- c(ColonizationThresholdOptimizationVec,ColonizationThresholdOptimization)
    DrugThresholdOptimizationVec <- rbind(DrugThresholdOptimizationVec,DrugThresholdOptimization)
    DiagnosisNTUThresholdOptimizationVec <- rbind(DiagnosisNTUThresholdOptimizationVec,DiagnosisNTUThresholdOptimization)
  }
  if(showRatio){
    outPatientRatioOptimization <- optimizeRatioGoldenSection(trainDataOrigin[,c(1,4)],outPatientThresholdOptimization)
    hospitalizationRatioOptimization <- optimizeRatioGoldenSection(trainDataOrigin[,c(1,5)],hospitalizationThresholdOptimization)
    emergencyRatioOptimization <- optimizeRatioGoldenSection(trainDataOrigin[,c(1,6)],emergencyThresholdOptimization)
    transferHospitalRatioOptimization <- optimizeRatioGoldenSection(trainDataOrigin[,c(1,7)],transferHospitalThresholdOptimization)
    ICUStayRatioOptimization <- optimizeRatioGoldenSection(trainDataOrigin[,c(1,8)],ICUStayThresholdOptimization)
    ColonizationRatioOptimization <- optimizeRatioGoldenSection(trainDataOrigin[,c(1,9)],ColonizationThresholdOptimization)
    print("Drug:")
    for(j in 1:8){
      DrugRatioOptimization[j] <- optimizeRatioGoldenSection(trainDataOrigin[,c(1,j+13)],DrugThresholdOptimization[j])
    }
    print("Diagnosis:")
    for(j in 1:13){
      DiagnosisNTURatioOptimization[j] <- optimizeRatioGoldenSection(trainDataOrigin[,c(1,j+37)],DiagnosisNTUThresholdOptimization[j])
    }
  }
  if(accumulateRatio){
    outPatientRatioOptimizationVec <- c(outPatientRatioOptimizationVec,outPatientRatioOptimization)
    hospitalizationRatioOptimizationVec <- c(hospitalizationRatioOptimizationVec,hospitalizationRatioOptimization)
    emergencyRatioOptimizationVec <- c(emergencyRatioOptimizationVec,emergencyRatioOptimization)
    transferHospitalRatioOptimizationVec <- c(transferHospitalRatioOptimizationVec,transferHospitalRatioOptimization)
    ICUStayRatioOptimizationVec <- c(ICUStayRatioOptimizationVec,ICUStayRatioOptimization)
    ColonizationRatioOptimizationVec <- c(ColonizationRatioOptimizationVec,ColonizationRatioOptimization)
    DrugRatioOptimizationVec <- rbind(DrugRatioOptimizationVec,DrugRatioOptimization)
    DiagnosisNTURatioOptimizationVec <- rbind(DiagnosisNTURatioOptimizationVec,DiagnosisNTURatioOptimization)
  }
}

#save.image("Lab3.RData")
load("Lab3.RData")

Drug1_Antibacterial <- Inf
Drug2_Antifungal <- Inf
Drug3_Chemotherapy <- Inf
Drug4_Gastric <- Inf
Drug5_Immunosuppressive <- Inf
Drug6_Monoclonal <- Inf
Drug7_Steroid  <- Inf
Drug8_TPN <- Inf

colnames(DataPrcoessed) <- c("Case", "Age", "Gender", "門診", "住院", "急診", "轉院", "ICU", "念珠菌移生"
                             , "住院天數", "ICU天數", "CCI1", "CCI2", "Antibacterial", "Antifungal", "Chemotherapy"
                             , "Gastric", "Immunosuppressive", "Monoclonal", "Steroid","TPN"
                             ,"Antibacterial_DOT1", "Antifungal_DOT1", "Chemotherapy_DOT1", "Gastric_DOT1"
                             , "Immunosuppressive_DOT1", "Monoclonal_DOT1", "Steroid_DOT1","TPN_DOT1"
                             ,"Antibacterial_DOT2", "Antifungal_DOT2", "Chemotherapy_DOT2", "Gastric_DOT2"
                             , "Immunosuppressive_DOT2", "Monoclonal_DOT2", "Steroid_DOT2","TPN_DOT2"
                             ,"Surgery","EndotrachealIntubation","ExtracorporealMembraneOxygenation","Hemodialysis"
                             ,"IndwellingCentralBenousCatheters","IndwellingUrinaryCatheter","IntravascularCatheter","MechanicValve"
                             ,"MechanicalVentilation","Pacemaker","PleuralDrainage","ProsthesisJoint","SolidOrganTransplantation"
                             ,"TotalParenteralNutrition")

AUCCrisp <- vector()
AUCCrispOpt <- vector()
AUCSoft <- vector()
AUCCrispTest <- vector()
AUCCrispTestOpt <- vector()
AUCSoftTest <- vector()
for (seed in 1:SeedRange){
  cat("seed:",seed,"\n")
  Ratio <- Inf
  set.seed(seed)
  
  ind <- sample(1:nrow(DataPrcoessed), size = 0.75*nrow(DataPrcoessed))
  
  trainDataOrigin <- DataPrcoessed[ind,]
  testDataOrigin <- DataPrcoessed[-ind,]
  
  NCase <- sum(trainDataOrigin[,1]==1)
  NControl <- sum(trainDataOrigin[,1]==0)
  
  #trainDataOrigin <- data.frame(trainDataOrigin)
  #trainDataOrigin$Case <- as.factor(trainDataOrigin$Case)
  #trainDataOrigin <- SMOTE(Case ~ ., trainDataOrigin, perc.over = as.integer(NControl/NCase*100),perc.under=100)
  #trainDataOrigin <- do.call(cbind, trainDataOrigin)
  #trainDataOrigin[,1] <- trainDataOrigin[,1]-1
  
  #Oversampling
  train_case <- trainDataOrigin[trainDataOrigin[,1] == 1,]
  train_control <- trainDataOrigin[trainDataOrigin[,1] == 0,]
  
  data_random_index <- sample(1:nrow(train_case), nrow(train_control),replace = TRUE)
  data_random_index2 <- sample(1:nrow(train_control), nrow(train_control),replace = TRUE)
  training_set_random <- rbind(train_case[data_random_index,], train_control[data_random_index2,])
  
  trainDataOrigin <- training_set_random
  
  CrispData <- trainDataOrigin
  CrispDataOpt <- trainDataOrigin
  FuzzyData <- trainDataOrigin
  CrisptestDataOrigin <- as.matrix(testDataOrigin)
  CrisptestDataOriginOpt <- as.matrix(testDataOrigin)
  FuzzytestDataOrigin <- as.matrix(testDataOrigin)
  if(TRUE){
    CrispData[,4] <- featureExtractionCrisp(CrispData[,4],outPatientThresholdOrigin)
    CrispData[,5] <- featureExtractionCrisp(CrispData[,5],hospitalizationThresholdOrigin)
    CrispData[,6] <- featureExtractionCrisp(CrispData[,6],emergencyThresholdOrigin)
    CrispData[,7] <- featureExtractionCrisp(CrispData[,7],transferHospitalThresholdOrigin)
    CrispData[,8] <- featureExtractionCrisp(CrispData[,8],ICUStayThresholdOrigin)
    CrispData[,9] <- featureExtractionCrisp(CrispData[,9],ColonizationThresholdOrigin)
    for(i in 1:8){
      CrispData[,i+13] <- featureExtractionCrisp(CrispData[,i+13],DrugThresholdOrigin)
    }
    for(i in 1:14){
      CrispData[,i+37] <- featureExtractionCrisp(CrispData[,i+37],DiagnosisThresholdOrigin)
    }
    CrisptestDataOrigin[,4] <- featureExtractionCrisp(CrisptestDataOrigin[,4],outPatientThresholdOrigin)
    CrisptestDataOrigin[,5] <- featureExtractionCrisp(CrisptestDataOrigin[,5],hospitalizationThresholdOrigin)
    CrisptestDataOrigin[,6] <- featureExtractionCrisp(CrisptestDataOrigin[,6],emergencyThresholdOrigin)
    CrisptestDataOrigin[,7] <- featureExtractionCrisp(CrisptestDataOrigin[,7],transferHospitalThresholdOrigin)
    CrisptestDataOrigin[,8] <- featureExtractionCrisp(CrisptestDataOrigin[,8],ICUStayThresholdOrigin)
    CrisptestDataOrigin[,9] <- featureExtractionCrisp(CrisptestDataOrigin[,9],ColonizationThresholdOrigin)
    for(i in 1:8){
      CrisptestDataOrigin[,i+13] <- featureExtractionCrisp(CrisptestDataOrigin[,i+13],DrugThresholdOrigin)
    }
    for(i in 1:14){
      CrisptestDataOrigin[,i+37] <- featureExtractionCrisp(CrisptestDataOrigin[,i+37],DiagnosisThresholdOrigin)
    }
  }
  if(TRUE){
    CrispDataOpt[,4] <- featureExtractionCrisp(CrispDataOpt[,4],outPatientThresholdOptimizationVec[seed])
    CrispDataOpt[,5] <- featureExtractionCrisp(CrispDataOpt[,5],hospitalizationThresholdOptimizationVec[seed])
    CrispDataOpt[,6] <- featureExtractionCrisp(CrispDataOpt[,6],emergencyThresholdOptimizationVec[seed])
    CrispDataOpt[,7] <- featureExtractionCrisp(CrispDataOpt[,7],transferHospitalThresholdOptimizationVec[seed])
    CrispDataOpt[,8] <- featureExtractionCrisp(CrispDataOpt[,8],ICUStayThresholdOptimizationVec[seed])
    CrispDataOpt[,9] <- featureExtractionCrisp(CrispDataOpt[,9],ColonizationThresholdOptimizationVec[seed])
    for(i in 1:8){
      CrispDataOpt[,i+13] <- featureExtractionCrisp(CrispDataOpt[,i+13],DrugThresholdOptimizationVec[seed,i])
    }
    for(i in 1:14){
      CrispDataOpt[,i+37] <- featureExtractionCrisp(CrispDataOpt[,i+37],DiagnosisNTUThresholdOptimizationVec[seed,i])
    }
    CrisptestDataOriginOpt[,4] <- featureExtractionCrisp(CrisptestDataOriginOpt[,4],outPatientThresholdOptimizationVec[seed])
    CrisptestDataOriginOpt[,5] <- featureExtractionCrisp(CrisptestDataOriginOpt[,5],hospitalizationThresholdOptimizationVec[seed])
    CrisptestDataOriginOpt[,6] <- featureExtractionCrisp(CrisptestDataOriginOpt[,6],emergencyThresholdOptimizationVec[seed])
    CrisptestDataOriginOpt[,7] <- featureExtractionCrisp(CrisptestDataOriginOpt[,7],transferHospitalThresholdOptimizationVec[seed])
    CrisptestDataOriginOpt[,8] <- featureExtractionCrisp(CrisptestDataOriginOpt[,8],ICUStayThresholdOptimizationVec[seed])
    CrisptestDataOriginOpt[,9] <- featureExtractionCrisp(CrisptestDataOriginOpt[,9],ColonizationThresholdOptimizationVec[seed])
    for(i in 1:8){
      CrisptestDataOriginOpt[,i+13] <- featureExtractionCrisp(CrisptestDataOriginOpt[,i+13],DrugThresholdOptimizationVec[seed,i])
    }
    for(i in 1:14){
      CrisptestDataOriginOpt[,i+37] <- featureExtractionCrisp(CrisptestDataOriginOpt[,i+37],DiagnosisNTUThresholdOptimizationVec[seed,i])
    }
  }
  if(TRUE){
    FuzzyData[,4] <- featureExtractionSoft(FuzzyData[,4],outPatientThresholdOptimizationVec[seed],outPatientRatioOptimizationVec[seed])
    FuzzyData[,5] <- featureExtractionSoft(FuzzyData[,5],hospitalizationThresholdOptimizationVec[seed],hospitalizationRatioOptimizationVec[seed])
    FuzzyData[,6] <- featureExtractionSoft(FuzzyData[,6],emergencyThresholdOptimizationVec[seed],emergencyRatioOptimizationVec[seed])
    FuzzyData[,7] <- featureExtractionSoft(FuzzyData[,7],transferHospitalThresholdOptimizationVec[seed],transferHospitalRatioOptimizationVec[seed])
    FuzzyData[,8] <- featureExtractionSoft(FuzzyData[,8],ICUStayThresholdOptimizationVec[seed],ICUStayRatioOptimizationVec[seed])
    FuzzyData[,9] <- featureExtractionSoft(FuzzyData[,9],ColonizationThresholdOptimizationVec[seed],ColonizationRatioOptimizationVec[seed])
    for(i in 1:8){
      FuzzyData[,i+13] <- featureExtractionSoft(FuzzyData[,i+13],DrugThresholdOptimizationVec[seed,i],DrugRatioOptimizationVec[seed,i])
    }
    for(i in 1:14){
      FuzzyData[,i+37] <- featureExtractionSoft(FuzzyData[,i+37],DiagnosisNTUThresholdOptimizationVec[seed,i],DiagnosisNTURatioOptimizationVec[seed,i])
    }
    FuzzytestDataOrigin[,4] <- featureExtractionSoft(FuzzytestDataOrigin[,4],outPatientThresholdOptimizationVec[seed],outPatientRatioOptimizationVec[seed])
    FuzzytestDataOrigin[,5] <- featureExtractionSoft(FuzzytestDataOrigin[,5],hospitalizationThresholdOptimizationVec[seed],hospitalizationRatioOptimizationVec[seed])
    FuzzytestDataOrigin[,6] <- featureExtractionSoft(FuzzytestDataOrigin[,6],emergencyThresholdOptimizationVec[seed],emergencyRatioOptimizationVec[seed])
    FuzzytestDataOrigin[,7] <- featureExtractionSoft(FuzzytestDataOrigin[,7],transferHospitalThresholdOptimizationVec[seed],transferHospitalRatioOptimizationVec[seed])
    FuzzytestDataOrigin[,8] <- featureExtractionSoft(FuzzytestDataOrigin[,8],ICUStayThresholdOptimizationVec[seed],ICUStayRatioOptimizationVec[seed])
    FuzzytestDataOrigin[,9] <- featureExtractionSoft(FuzzytestDataOrigin[,9],ColonizationThresholdOptimizationVec[seed],ColonizationRatioOptimizationVec[seed])
    for(i in 1:8){
      FuzzytestDataOrigin[,i+13] <- featureExtractionSoft(FuzzytestDataOrigin[,i+13],DrugThresholdOptimizationVec[seed,i],DiagnosisNTURatioOptimizationVec[seed,i])
      #FuzzytestDataOrigin[,i+14] <- featureExtractionSoft(FuzzytestDataOrigin[,i+14],30,DiagnosisNTURatioOptimizationVec[seed,i])
    }
    for(i in 1:14){
      FuzzytestDataOrigin[,i+37] <- featureExtractionSoft(FuzzytestDataOrigin[,i+37],DiagnosisNTUThresholdOptimizationVec[seed,i],DiagnosisNTURatioOptimizationVec[seed,i])
      #FuzzytestDataOrigin[,i+38] <- featureExtractionSoft(FuzzytestDataOrigin[,i+38],365,DiagnosisNTURatioOptimizationVec[seed,i])
    }
  }
  if(FALSE){
    FuzzyData[,4] <- featureExtractionSoft(FuzzyData[,4],outPatientThresholdOptimizationVec[seed],Ratio)
    FuzzyData[,5] <- featureExtractionSoft(FuzzyData[,5],hospitalizationThresholdOptimizationVec[seed],Ratio)
    FuzzyData[,6] <- featureExtractionSoft(FuzzyData[,6],emergencyThresholdOptimizationVec[seed],Ratio)
    FuzzyData[,7] <- featureExtractionSoft(FuzzyData[,7],transferHospitalThresholdOptimizationVec[seed],Ratio)
    FuzzyData[,8] <- featureExtractionSoft(FuzzyData[,8],ICUStayThresholdOptimizationVec[seed],Ratio)
    FuzzyData[,9] <- featureExtractionSoft(FuzzyData[,9],ColonizationThresholdOptimizationVec[seed],Ratio)
    for(i in 1:8){
      FuzzyData[,i+13] <- featureExtractionSoft(FuzzyData[,i+13],DrugThresholdOptimizationVec[seed,i],Ratio)
    }
    for(i in 1:14){
      FuzzyData[,i+37] <- featureExtractionSoft(FuzzyData[,i+37],DiagnosisNTUThresholdOptimizationVec[seed,i],Ratio)
    }
    FuzzytestDataOrigin[,4] <- featureExtractionSoft(FuzzytestDataOrigin[,4],outPatientThresholdOptimizationVec[seed],Ratio)
    FuzzytestDataOrigin[,5] <- featureExtractionSoft(FuzzytestDataOrigin[,5],hospitalizationThresholdOptimizationVec[seed],Ratio)
    FuzzytestDataOrigin[,6] <- featureExtractionSoft(FuzzytestDataOrigin[,6],emergencyThresholdOptimizationVec[seed],Ratio)
    FuzzytestDataOrigin[,7] <- featureExtractionSoft(FuzzytestDataOrigin[,7],transferHospitalThresholdOptimizationVec[seed],Ratio)
    FuzzytestDataOrigin[,8] <- featureExtractionSoft(FuzzytestDataOrigin[,8],ICUStayThresholdOptimizationVec[seed],Ratio)
    FuzzytestDataOrigin[,9] <- featureExtractionSoft(FuzzytestDataOrigin[,9],ColonizationThresholdOptimizationVec[seed],Ratio)
    for(i in 1:8){
      FuzzytestDataOrigin[,i+13] <- featureExtractionSoft(FuzzytestDataOrigin[,i+13],DrugThresholdOptimizationVec[seed,i],Ratio)
      #FuzzytestDataOrigin[,i+14] <- featureExtractionSoft(FuzzytestDataOrigin[,i+14],30,DiagnosisNTURatioOptimizationVec[seed,i])
    }
    for(i in 1:14){
      FuzzytestDataOrigin[,i+37] <- featureExtractionSoft(FuzzytestDataOrigin[,i+37],DiagnosisNTUThresholdOptimizationVec[seed,i],Ratio)
      #FuzzytestDataOrigin[,i+38] <- featureExtractionSoft(FuzzytestDataOrigin[,i+38],365,DiagnosisNTURatioOptimizationVec[seed,i])
    }
  }
  CrispData[is.infinite(CrispData)] <- 0
  CrispDataOpt[is.infinite(CrispDataOpt)] <- 0
  FuzzyData[is.infinite(FuzzyData)] <- 0
  CrisptestDataOrigin[is.infinite(CrisptestDataOrigin)] <- 0
  CrisptestDataOriginOpt[is.infinite(CrisptestDataOriginOpt)] <- 0
  FuzzytestDataOrigin[is.infinite(FuzzytestDataOrigin)] <- 0
  
  CrispData <- as.data.frame(CrispData)
  CrispDataOpt <- as.data.frame(CrispDataOpt)
  FuzzyData <- as.data.frame(FuzzyData)
  CrisptestDataOrigin <- as.data.frame(CrisptestDataOrigin)
  CrisptestDataOriginOpt <- as.data.frame(CrisptestDataOriginOpt)
  FuzzytestDataOrigin <- as.data.frame(FuzzytestDataOrigin)
  #V2+V3+V4+V8+V9+V10+V11+V13+V14+V16+V18+V19+V20+V23+V24+V26+V27+V30+V33+V35+V37+V38+V40+V45+V47+V49
  #V4+V9+V13+V14+V39+V40+V45+V47
  #V15+V2+V13+V10+V14+V5+V4+V38+V43+V6
  GLM <- TRUE
  PLOT <- TRUE
  if(GLM){
    
    if(PLOT){
      attach(mtcars)
      par(mfrow=c(1,1))
    }
    print("GLM")
    #V15+V2+V13+V10+V14+V5+V4+V38+V43+
    #colnames(DataPrcoessed)[15]+colnames(DataPrcoessed)[2]+colnames(DataPrcoessed)[13]+colnames(DataPrcoessed)[10]
    #+colnames(DataPrcoessed)[14]+colnames(DataPrcoessed)[5]+colnames(DataPrcoessed)[4]+colnames(DataPrcoessed)[38]
    #+colnames(DataPrcoessed)[43]
    #classifier = glm(formula = Case ~ Antifungal+Age+CCI2+住院天數+Antibacterial+住院+門診+Surgery+IndwellingUrinaryCatheter,family = binomial,data = CrispData[1:51])
    classifier = glm(formula = Case ~ .,family = binomial,data = CrispData[1:51])
    px <- predict(classifier, type = 'response')
    up <- roc(CrispData$Case ~ px)
    AUCCrisp <- cbind(AUCCrisp,round(up$auc, 4))
    
    if(PLOT){
      plot.roc(up, legacy.axes = TRUE,main="AUC Comparison",col = "green")
      text(0.2, 0.25, paste('Crisp AUC(Train) = ', round(up$auc, 4), sep = ''),col = "green")
    }
    
    px <- predict(classifier, CrisptestDataOrigin)
    up <- roc(CrisptestDataOrigin$Case ~ px)
    AUCCrispTest <- cbind(AUCCrispTest,round(up$auc, 4))
    
    if(PLOT){
      plot.roc(up, legacy.axes = TRUE,main="Crisp Test", add = TRUE,col = "blue")
      text(0.2, 0.2, paste('Crisp AUC(Test) = ', round(up$auc, 4), sep = ''),col = "blue")
    }
    
    #classifier = glm(formula = Case ~ Antifungal+Age+CCI2+住院天數+Antibacterial+住院+門診+Surgery+IndwellingUrinaryCatheter,family = binomial,data = CrispDataOpt[1:51])
    classifier = glm(formula = Case ~ .,family = binomial,data = CrispDataOpt[1:51])
    px <- predict(classifier, type = 'response')
    up <- roc(CrispDataOpt$Case ~ px)
    AUCCrispOpt <- cbind(AUCCrispOpt,round(up$auc, 4))
    
    if(PLOT){
      plot.roc(up, legacy.axes = TRUE,main="", add = TRUE,col = "red")
      text(0.2, 0.15, paste('Crisp Opt AUC(Train) = ', round(up$auc, 4), sep = ''),col = "red")
    }
    
    px <- predict(classifier, CrisptestDataOriginOpt)
    up <- roc(CrisptestDataOrigin$Case ~ px)
    AUCCrispTestOpt <- cbind(AUCCrispTestOpt,round(up$auc, 4))
    
    if(PLOT){
      plot.roc(up, legacy.axes = TRUE,main="Crisp Opt Test", add = TRUE,col = "purple")
      text(0.2, 0.10, paste('Crisp Opt AUC(Test) = ', round(up$auc, 4), sep = ''),col = "purple")
    }
    
    #classifier = glm(formula = Case ~ Antifungal+Age+CCI2+住院天數+Antibacterial+住院+門診+Surgery+IndwellingUrinaryCatheter,family = binomial,data = FuzzyData[1:51])
    classifier = glm(formula = Case ~ .,family = binomial,data = FuzzyData[1:51])
    px <- predict(classifier, type = 'response')
    up <- roc(FuzzyData$Case ~ px)
    AUCSoft <- cbind(AUCSoft,round(up$auc, 4))
    
    if(PLOT){
      plot.roc(up, legacy.axes = TRUE,main="Fuzzy Train", add = TRUE,col = "orange")
      text(0.2, 0.05, paste('Fuzzy AUC(Train) = ', round(up$auc, 4), sep = ''),col = "orange")
    }
    
    px <- predict(classifier, FuzzytestDataOrigin)
    up <- roc(FuzzytestDataOrigin$Case ~ px)
    AUCSoftTest <- cbind(AUCSoftTest,round(up$auc, 4))
    
    if(PLOT){
      plot.roc(up, legacy.axes = TRUE,main="Fuzzy Test", add = TRUE,col = "black")
      text(0.2, 0.00, paste('Fuzzy AUC(Test) = ', round(up$auc, 4), sep = ''),col = "black")
    }
  }
  
  DT <- FALSE
  if(DT){
    print("Decision Tree")
    classifier=tree(formula = Case ~ Antifungal+Age+CCI2+住院天數+Antibacterial+住院+門診+Surgery+IndwellingUrinaryCatheter, data=CrispData)
    plot(classifier)
    text(classifier)
    y_pred = predict(classifier, newdata = CrisptestDataOrigin[,2:51])
    y_pred = ifelse(y_pred > 0.5 , 1, 0)
    cm = table(CrisptestDataOrigin[, 1], y_pred)
    tp = cm[2,2]
    tn = cm[1,1]
    fp = cm[1,2]
    fn = cm[2,1]
    sen = tp /(tp + fn)
    spe = tn /(tn + fp)
    ppv = tp /(tp + fp)
    npv = tn /(tn + fn)
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    f1 = (2*tp) / (2*tp + fp + fn)
    LR_pos = tp / fp
    LR_neg = fn / tn
    DOR = (tp*tn) / (fp*fn)
    para_test <- array()
    para_test = cbind(sen, spe, ppv, npv, accuracy, f1, LR_pos, LR_neg, DOR)
    para_test
    
    classifier=tree(formula = Case ~ Antifungal+Age+CCI2+住院天數+Antibacterial+住院+門診+Surgery+IndwellingUrinaryCatheter, data=FuzzyData)
    plot(classifier)
    text(classifier)
    y_pred = predict(classifier, newdata = FuzzytestDataOrigin[,2:51])
    y_pred = ifelse(y_pred > 0.5 , 1, 0)
    cm = table(FuzzytestDataOrigin[, 1], y_pred)
    tp = cm[2,2]
    tn = cm[1,1]
    fp = cm[1,2]
    fn = cm[2,1]
    sen = tp /(tp + fn)
    spe = tn /(tn + fp)
    ppv = tp /(tp + fp)
    npv = tn /(tn + fn)
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    f1 = (2*tp) / (2*tp + fp + fn)
    LR_pos = tp / fp
    LR_neg = fn / tn
    DOR = (tp*tn) / (fp*fn)
    para_test <- array()
    para_test = cbind(sen, spe, ppv, npv, accuracy, f1, LR_pos, LR_neg, DOR)
    para_test
    
  }
  
  SVM <- FALSE
  if(SVM){
    print("SVM")
    classifier = svm(form = V1 ~ ., data = CrispData, method = 'linear')
    y_pred = predict(classifier, newdata = CrisptestDataOrigin[,2:51])
    y_pred = ifelse(y_pred > 0.5 , 1, 0)
    cm = table(CrisptestDataOrigin[, 1], y_pred)
    tp = cm[2,2]
    tn = cm[1,1]
    fp = cm[1,2]
    fn = cm[2,1]
    sen = tp /(tp + fn)
    spe = tn /(tn + fp)
    ppv = tp /(tp + fp)
    npv = tn /(tn + fn)
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    f1 = (2*tp) / (2*tp + fp + fn)
    LR_pos = tp / fp
    LR_neg = fn / tn
    DOR = (tp*tn) / (fp*fn)
    para_train <- array()
    para_train = cbind(sen, spe, ppv, npv, accuracy, f1, LR_pos, LR_neg, DOR)
    para_train
    
    
    classifier = svm(form = V1 ~ ., data = FuzzyData, method = 'linear')
    y_pred = predict(classifier, newdata = FuzzytestDataOrigin[,2:51])
    y_pred = ifelse(y_pred > 0.5 , 1, 0)
    cm = table(FuzzytestDataOrigin[, 1], y_pred)
    tp = cm[2,2]
    tn = cm[1,1]
    fp = cm[1,2]
    fn = cm[2,1]
    sen = tp /(tp + fn)
    spe = tn /(tn + fp)
    ppv = tp /(tp + fp)
    npv = tn /(tn + fn)
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    f1 = (2*tp) / (2*tp + fp + fn)
    LR_pos = tp / fp
    LR_neg = fn / tn
    DOR = (tp*tn) / (fp*fn)
    para_train <- array()
    para_train = cbind(sen, spe, ppv, npv, accuracy, f1, LR_pos, LR_neg, DOR)
    para_train
  }
  
  KNN <- FALSE
  if(KNN){
    y_pred = knn(train = CrispData[,2:51],
                 test = CrisptestDataOrigin[, 2:51],
                 cl = CrispData[, 1],
                 k = 3)
    cm = table(CrisptestDataOrigin[, 1], y_pred)
    tp = cm[2,2]
    tn = cm[1,1]
    fp = cm[1,2]
    fn = cm[2,1]
    sen = tp /(tp + fn)
    spe = tn /(tn + fp)
    ppv = tp /(tp + fp)
    npv = tn /(tn + fn)
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    f1 = (2*tp) / (2*tp + fp + fn)
    LR_pos = tp / fp
    LR_neg = fn / tn
    DOR = (tp*tn) / (fp*fn)
    para_test <- array()
    para_test = cbind(sen, spe, ppv, npv, accuracy, f1, LR_pos, LR_neg, DOR)
    para_test
    
    
    y_pred = knn(train = FuzzyData[,2:51],
                 test = FuzzytestDataOrigin[, 2:51],
                 cl = FuzzyData[, 1],
                 k = 3)
    cm = table(FuzzytestDataOrigin[, 1], y_pred)
    tp = cm[2,2]
    tn = cm[1,1]
    fp = cm[1,2]
    fn = cm[2,1]
    sen = tp /(tp + fn)
    spe = tn /(tn + fp)
    ppv = tp /(tp + fp)
    npv = tn /(tn + fn)
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    f1 = (2*tp) / (2*tp + fp + fn)
    LR_pos = tp / fp
    LR_neg = fn / tn
    DOR = (tp*tn) / (fp*fn)
    para_test <- array()
    para_test = cbind(sen, spe, ppv, npv, accuracy, f1, LR_pos, LR_neg, DOR)
    para_test
  }
  
  naiveBaye <- FALSE
  if(naiveBaye){
    classifier = classifier = randomForest(V1~.,data = CrispData,ntree = 1000)
    y_pred = predict(classifier, newdata = CrisptestDataOrigin[,2:51])
    cm = table(CrisptestDataOrigin[, 1], y_pred)
    tp = cm[2,2]
    tn = cm[1,1]
    fp = cm[1,2]
    fn = cm[2,1]
    sen = tp /(tp + fn)
    spe = tn /(tn + fp)
    ppv = tp /(tp + fp)
    npv = tn /(tn + fn)
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    f1 = (2*tp) / (2*tp + fp + fn)
    LR_pos = tp / fp
    LR_neg = fn / tn
    DOR = (tp*tn) / (fp*fn)
    para_train <- array()
    para_train = cbind(sen, spe, ppv, npv, accuracy, f1, LR_pos, LR_neg, DOR)
    para_train
    
    
    classifier = naiveBayes(V1 ~ ., data=FuzzyData)
    y_pred = predict(classifier, newdata = FuzzytestDataOrigin[,2:51])
    cm = table(FuzzytestDataOrigin[, 1], y_pred)
    tp = cm[2,2]
    tn = cm[1,1]
    fp = cm[1,2]
    fn = cm[2,1]
    sen = tp /(tp + fn)
    spe = tn /(tn + fp)
    ppv = tp /(tp + fp)
    npv = tn /(tn + fn)
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    f1 = (2*tp) / (2*tp + fp + fn)
    LR_pos = tp / fp
    LR_neg = fn / tn
    DOR = (tp*tn) / (fp*fn)
    para_train <- array()
    para_train = cbind(sen, spe, ppv, npv, accuracy, f1, LR_pos, LR_neg, DOR)
    para_train
  }
}


#V15+V2+V13+V10+V14+V5+V4+V38+V43+V6
attach(mtcars)
par(mfrow=c(3,2))

hist(AUCCrisp)
hist(AUCCrispTest)
hist(AUCCrispOpt)
hist(AUCCrispTestOpt)
hist(AUCSoft)
hist(AUCSoftTest)
median(AUCCrisp)
median(AUCCrispOpt)
median(AUCSoft)
median(AUCCrispTest)
median(AUCCrispTestOpt)
median(AUCSoftTest)
sum(AUCCrisp)/50
sum(AUCCrispOpt)/50
sum(AUCSoft)/50
sum(AUCCrispTest)/50
sum(AUCCrispTestOpt)/50
sum(AUCSoftTest)/50

median(hospitalizationThresholdOptimizationVec)
median(hospitalizationRatioOptimizationVec)
sum(hospitalizationThresholdOptimizationVec)/50
sum(hospitalizationRatioOptimizationVec)/50
sd(hospitalizationThresholdOptimizationVec)
sd(hospitalizationRatioOptimizationVec)

#Anti bacteria
median(DrugThresholdOptimizationVec[,1])
median(DrugRatioOptimizationVec[,1])
sum(DrugThresholdOptimizationVec[,1])/50
sum(DrugRatioOptimizationVec[,1])/50
sd(DrugThresholdOptimizationVec[,1])
sd(DrugRatioOptimizationVec[,1])


#Anti fungal
median(DrugThresholdOptimizationVec[,2])
median(DrugRatioOptimizationVec[,2])
sum(DrugThresholdOptimizationVec[,2])/50
sum(DrugRatioOptimizationVec[,2])/50
sd(DrugThresholdOptimizationVec[,2])
sd(DrugRatioOptimizationVec[,2])


control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(FuzzyData[,2:51], FuzzyData[,1], sizes=c(1:20), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))
#https://read01.com/zh-tw/KBx4d0.html#.W1jiedIzaUk



DrugTitle <- c("Antibacterial","Antifungal","Chemotherapy","Gastric Acid-Suppressive Medications","Immunosuppressive agents",
               "Monoclonal agents","Systemic steroid","TPN_DRUGCODE")
DiagnosisTitle <- c("Surgery","EndotrachealIntubation","ExtracorporealMembraneOxygenation","Hemodialysis",
                    "IndwellingCentralBenousCatheters","IndwellingUrinaryCatheter","IntravascularCatheter",
                    "MechanicValve","MechanicalVentilation","Pacemaker","PleuralDrainage",
                    "ProsthesisJoint","SolidOrganTransplantation","TotalParenteralNutrition")

histDraw<-TRUE
if(histDraw){
  attach(mtcars)
  par(mfrow=c(3,2))
  
  hist(outPatientThresholdOptimizationVec, main="門診",breaks = 30)
  hist(hospitalizationThresholdOptimizationVec, main="住院",breaks = 30)
  hist(emergencyThresholdOptimizationVec, main="急診",breaks = 30)
  hist(transferHospitalThresholdOptimizationVec, main="轉院",breaks = 30)
  hist(ICUStayThresholdOptimizationVec, main="ICU",breaks = 30)
  hist(ColonizationThresholdOptimizationVec, main="念珠菌移生",breaks = 30)
  
  par(mfrow=c(3,2))
  hist(outPatientRatioOptimizationVec, main="門診",breaks = 30)
  hist(hospitalizationRatioOptimizationVec, main="住院",breaks = 30)
  hist(emergencyRatioOptimizationVec, main="急診",breaks = 30)
  hist(transferHospitalRatioOptimizationVec, main="轉院",breaks = 30)
  hist(ICUStayRatioOptimizationVec, main="ICU",breaks = 30)
  hist(ColonizationRatioOptimizationVec, main="念珠菌移生",breaks = 30)
  
  attach(mtcars)
  par(mfrow=c(2,2))
  for(i in 1:8)
    hist(DrugThresholdOptimizationVec[,i],main=DrugTitle[i],breaks = 100)
  for(i in 1:8)
    hist(DrugRatioOptimizationVec[,i],main=DrugTitle[i],breaks = 100)
  
  attach(mtcars)
  par(mfrow=c(3,2))
  for(i in 1:11)
    hist(DiagnosisNTUThresholdOptimizationVec[,i],main=DiagnosisTitle[i],breaks = 100)
  for(i in 1:11)
    hist(DiagnosisNTURatioOptimizationVec[,i],main=DiagnosisTitle[i],breaks = 100)
}

if(featureExtract){
  # featureExtractionCrisp
  # featureExtractionSoft
  # Train Feature Extracting
  emergencyTrainOri<- emergencyExtracting(trainDataOrigin,emergencyThresholdOrigin)
  emergencyTrainOpt<- emergencyExtracting(trainDataOrigin,emergencyThresholdOptimization)
  outPatientTrainOri<- outPatientExtracting(trainDataOrigin,outPatientThresholdOrigin)
  outPatientTrainOpt<- outPatientExtracting(trainDataOrigin,outPatientThresholdOptimization)
  hospitalizationTrainOri<- hospitalizationExtracting(trainDataOrigin,hospitalizationThresholdOrigin)
  hospitalizationTrainOpt<- hospitalizationExtracting(trainDataOrigin,hospitalizationThresholdOptimization)
  transferTrainOri<- transferHospitalExtracting(trainDataOrigin,transferHospitalThresholdOrigin)
  transferTrainOpt<- trantestsferHospitalExtracting(trainDataOrigin,transferHospitalThresholdOptimization)
  ICUStayTrainOri<- ICUStayExtracting(trainDataOrigin,ICUStayThresholdOrigin)
  ICUStayTrainOpt<- ICUStayExtracting(trainDataOrigin,ICUStayThresholdOptimization)
  ColonizationTrainOri<- ColonizationExtracting(trainDataOrigin,ColonizationThresholdOrigin)
  ColonizationTrainOpt<- ColonizationExtracting(trainDataOrigin,ColonizationThresholdOptimization)
}
if(featurAna){
  FeaturAnalysis(trainDataOrigin,emergencyTrainOri)
  FeaturAnalysis(trainDataOrigin,emergencyTrainOpt)
  FeaturAnalysis(trainDataOrigin,outPatientTrainOri)
  FeaturAnalysis(trainDataOrigin,outPatientTrainOpt)
  FeaturAnalysis(trainDataOrigin,hospitalizationTrainOri)
  FeaturAnalysis(trainDataOrigin,hospitalizationTrainOpt)
  FeaturAnalysis(trainDataOrigin,transferTrainOri)
  FeaturAnalysis(trainDataOrigin,transferTrainOpt)
  FeaturAnalysis(trainDataOrigin,ICUStayTrainOri)
  FeaturAnalysis(trainDataOrigin,ICUStayTrainOpt)
  FeaturAnalysis(trainDataOrigin,ColonizationTrainOri)
  FeaturAnalysis(trainDataOrigin,ColonizationTrainOpt)
}
if(featureExtract){
  # Test Feature Extracting
  emergencyTestOri<- emergencyExtracting(testDataOrigin,emergencyThresholdOrigin)
  emergencyTestOpt<- emergencyExtracting(testDataOrigin,emergencyThresholdOptimization)
  outPatientTestOri<- outPatientExtracting(testDataOrigin,outPatientThresholdOrigin)
  outPatientTestOpt<- outPatientExtracting(testDataOrigin,outPatientThresholdOptimization)
  hospitalizationTestOri<- hospitalizationExtracting(testDataOrigin,hospitalizationThresholdOrigin)
  hospitalizationTestOpt<- hospitalizationExtracting(testDataOrigin,hospitalizationThresholdOptimization)
  transferTestOri<- transferHospitalExtracting(testDataOrigin,transferHospitalThresholdOrigin)
  transferTestOpt<- transferHospitalExtracting(testDataOrigin,transferHospitalThresholdOptimization)
  ICUStayTestOri<- ICUStayExtracting(testDataOrigin,ICUStayThresholdOrigin)
  ICUStayTestOpt<- ICUStayExtracting(testDataOrigin,ICUStayThresholdOptimization)
  ColonizationTestOri<- ColonizationExtracting(testDataOrigin,ColonizationThresholdOrigin)
  ColonizationTestOpt<- ColonizationExtracting(testDataOrigin,ColonizationThresholdOptimization)
}
if(featurAna){
  FeaturAnalysis(testDataOrigin,emergencyTestOri)
  FeaturAnalysis(testDataOrigin,emergencyTestOpt)
  FeaturAnalysis(testDataOrigin,outPatientTestOri)
  FeaturAnalysis(testDataOrigin,outPatientTestOpt)
  FeaturAnalysis(testDataOrigin,hospitalizationTestOri)
  FeaturAnalysis(testDataOrigin,hospitalizationTestOpt)
  FeaturAnalysis(testDataOrigin,transferTestOri)
  FeaturAnalysis(testDataOrigin,transferTestOpt)
  FeaturAnalysis(testDataOrigin,ICUStayTestOri)
  FeaturAnalysis(testDataOrigin,ICUStayTestOpt)
  FeaturAnalysis(testDataOrigin,ColonizationTestOri)
  FeaturAnalysis(testDataOrigin,ColonizationTestOpt)
}=]
,LLTGGGGGGGYVFGFGFG3333333333333333333333333333333333333333RTFGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG1Qｖｙvｖｙyvyｖｙvyｖｙvyｖｙｖｙvyｖｙvyｖｙvyｖｙvyｖｙvyｖｙvyvyｖｙvyｖｙvyｖｙｖｙvyｖｙvyvyｖｙvyｖｙｖｙvyｖｙvyvyｖｙｖｙvyｖｙvyvyｖｙvyｖｙvyｖｙｖｙvyｖｙvyｖｙvyvyｖｙvyｖｙvyｖｙvyｖｙvyｖｙvyｖｙvyｖｙｖｙvyｖｙvyｖｙvyvyｖｙvyｖｙvyｖｙｖｙvyｖｙvyvyｖｙｖｙvyｖｙvyｖｙvyvyｖｙvyｖｙｖｙvyｖｙvyｖｙvyvyｖｙvyｖｙvyｖｙｖｙvyｖｙvyｖｙvyvyｖｙvyｖｙvyｖｙｖｙvyｖｙvyvyｖｙｖｙvyｖｙvyvyｖｙvyｖｙｖｙvyｖｙvyvyｖｙvyｖｙｖｙvyｖｙvyvyｖｙｖｙvyｖｙvyｖｙvyvyｖｙｖｙvyｖｙvyvyｖｙｖｙvyvy/