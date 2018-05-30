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
#source("https://bioconductor.org/biocLite.R")

#載入library 
#library("rpart")
#library("rpart.plot")
#library("rattle")
library(vioplot)
library(beanplot)
library(e1071)
library(caret)
library(Matrix)
library(data.table)
library(mlr)
library(magrittr)

#Parameter
SeedRange <- 50
counts <- 0
threshold <- 30
CCIthreshold <- 365
Healththreshold <- 365
Procedurethreshold <- 365
histDraw <- FALSE

outPatientThresholdOrigin <- 7
hospitalizationThresholdOrigin <- 7
emergencyThresholdOrigin <- 7
transferHospitalThresholdOrigin <- 7
ICUStayThresholdOrigin <- 28
ColonizationThresholdOrigin <- 30
CCIthresholdOrigin <- 365
HealthCareThresholdOrigin <- 
ProcedureThresholdOrigin <- 
  Medications


outPatientThresholdOptimization <- 0
hospitalizationThresholdOptimization <- 0
emergencyThresholdOptimization <- 0
transferHospitalThresholdOptimization <- 0
ICUStayThresholdOptimization <- 0
ColonizationThresholdOptimization <- 0

HealthCareThresholdOptimization <- vector(mode = "numeric", length = 8)
DrugsThresholdOptimization <- vector(mode = "numeric", length = 8)



labelOrigin<-read.table("EventData.csv",sep=",",header=TRUE)
ind <- which(with( labelOrigin, case==TRUE & control==TRUE ))
labelOrigin <- labelOrigin[ -ind, ]
outPatientClinicOrigin<-read.table("RowData/02門診史.txt",sep="\t",header=TRUE)
hospitalizationOrigin<-read.table("RowData/03住院史.csv",sep=",",header=TRUE)
emergencyOrigin<-read.table("RowData/04急診史.csv",sep=",",header=TRUE)
transferHospitalOrigin<-read.table("RowData/05轉院註記.csv",sep=",",header=TRUE)
ICUStayOrigin<-read.table("RowData/07轉床紀錄.csv",sep=",",header=TRUE)
ColonizationOrigin<-read.csv("RowData/06-1candida colonization.csv",sep=",", header=TRUE)



outPatientThresholdOptimizationVec <- c()
hospitalizationThresholdOptimizationVec <- c()
emergencyThresholdOptimizationVec <- c()
transferHospitalThresholdOptimizationVec <- c()
ICUStayThresholdOptimizationVec <- c()
ColonizationThresholdOptimizationVec <- c()


labelProcessed <- data.frame(chartNO=character(),
                             case=logical(),
                             control=logical(),
                             hospitalizationDay=integer(),
                             emergencyDay=integer(),
                             ICUStayDay=integer(),
                             outPatientClinicDay=integer())



#Split to train and test
for (seed in 1:SeedRange){
  cat("seed:",seed)
  set.seed(seed)
  a <- createDataPartition(labelOrigin[[2]], p = 0.75, list=FALSE)
  trainDataOrigin <- setDT(labelOrigin[a,])
  testDataOrigin <- setDT(labelOrigin[-a,])
  
  #existingDF = rbind(existingDF,newrow)
  NCase <- 0
  NControl <- 0
  NCase_Control <- 0
  for (ind in 1:nrow(labelOrigin)){
    Data <- as.matrix(labelOrigin[ind,])
    if(Data[2]==TRUE && Data[3] == FALSE)
      NCase <- NCase + 1
    if(Data[3]==TRUE && Data[2] == FALSE)
      NControl <- NControl + 1
    if(Data[2]==TRUE && Data[3]==TRUE)
      NCase_Control <- NCase_Control + 1
  }
  
  
  #trainDataOrigin <- labelOrigin
  
  
  outPatientClinicVec <- vector()
  hospitalizationVec <- vector()
  emergencyVec <- vector()
  transferHospitalVec <- vector()
  ICUStayVec <- vector()
  ColonizationVec <- vector()
    
  for (cou in 1:nrow(trainDataOrigin)){
    
    chartNO <- as.character(trainDataOrigin[[1]][cou])
    case <- as.logical(trainDataOrigin[[2]][cou])
    control <- as.logical(trainDataOrigin[[3]][cou])
    if(case&&control)
      next
    history <- as.character(trainDataOrigin[[8]][cou])
    historyArray <- strsplit(history[1],";")
    diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
    
    ind_hospitalization_array <- which(!is.na(match(hospitalizationOrigin$CHARTNO2, chartNO)))
    ind_emergency_array <- which(!is.na(match(emergencyOrigin$CHARTNO2, chartNO)))
    ind_ICUStay_array <- which(!is.na(match(ICUStayOrigin$CHARTNO2, chartNO)))
    ind_outPatientClinic_array <- which(!is.na(match(outPatientClinicOrigin$CHARTNO2, chartNO)))
    ind_transferHospital_array <- which(!is.na(match(transferHospitalOrigin$CHARTNO2, chartNO)))
    ind_array_Colonization <- which(!is.na(match(ColonizationOrigin$CHARTNO2, chartNO)))
    
    MIN_hospitalization <- 9999999
    emergencyDay <- 9999
    ICUStayDay <- 9999
    outPatientClinicDay <- 9999
    transferDay <- 9999
    colonizationDay <- 9999
    #MIN_hospitalization
    if(length(ind_hospitalization_array)>0){
      for (j in seq(1, length(ind_hospitalization_array))){
        index <- ind_hospitalization_array[j]
        inDate <- as.Date(hospitalizationOrigin$INDATE[index],format='%m/%d/%Y')
        outDate <- as.Date(hospitalizationOrigin$OUTDATE[index],format='%m/%d/%Y')
        
        diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
        diffOut <- as.numeric(difftime(diseaseDate, outDate, tz, units = c("days")))
        #print(diffIn)
        #print(diffOut)
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
    
    #print(c)
    if(MIN_hospitalization<threshold){
      hospitalizationVec <- rbind(hospitalizationVec,c(as.numeric(case),MIN_hospitalization));
    }
    #print(c(as.numeric(case),MIN_hospitalization))
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
    if(emergencyDay<threshold)
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
    if(ICUStayDay<threshold)
      ICUStayVec <- rbind(ICUStayVec,c(as.numeric(case),ICUStayDay));
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
    if(outPatientClinicDay<threshold)
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
    if(transferDay<threshold)
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
    if(colonizationDay<threshold)
      ColonizationVec <- rbind(ColonizationVec,c(as.numeric(case),colonizationDay));
  
  }
  
  #nrow(outPatientClinicVec)
  #nrow(hospitalizationVec)
  #nrow(emergencyVec)
  #nrow(transferHospitalVec)
  #nrow(ICUStayVec)
  #nrow(ColonizationVec)
  
  
  GiniPivot <- function(input){
    input <- input[order(input[,2]),c(1,2)]
    pivot <- 100
    index <- 0
    pVector <- c()
    GiniVector <- c()
    for (j in seq(1, (nrow(input)-1))){
      dataSize <- nrow(input)
      jtemp <- j
      while(input[j,2]==input[j+1,2])
        j <- j+1
      pivot_temp <- (input[index,2]+input[index+1,2])/2
      temp1 <- input[1:j,1]
      temp2 <- input[(j+1):nrow(input),1]
      tempp <- length(which(input[,1]==1))
      tempn <- length(which(input[,1]==0))
      temp1p <- length(which(temp1==1))
      temp1n <- length(which(temp1==0))
      temp2p <- length(which(temp2==1))
      temp2n <- length(which(temp2==0))
      
      candidaTemp <- c(Positive=temp1p,Negative=NCase-temp1p)
      BacteriaTemp <- c(Positive=temp1n,Negative=NControl-temp1n)
      ChiTable <- rbind(candidaTemp,BacteriaTemp)
      
      fiTest <- fisher.test(ChiTable)
      #propTest <- prop.test(ChiTable)
      pValue <- fiTest$p.value
      
      #Gini Function
      
      Gini_temp <- (1-(temp1p*temp1p+temp1n*temp1n)/j/j)*j/(NControl+NCase)+
        (1-((NCase-temp1p)*(NCase-temp1p)+(NControl-temp1n)*(NControl-temp1n))/(NControl+NCase-j)/(NControl+NCase-j))*(NControl+NCase-j)/(NControl+NCase)
      pValue <- Gini_temp
  
      if(pValue<pivot){
        index <- j
        pivot <- pValue
      }
      #pVector <- c(pVector,pValue)
      Gini_Index <- Gini_temp
      GiniVector <- c(GiniVector,Gini_Index)
      pVector <- c(pVector,pValue)
      j <- jtemp
    }
    #plot(GiniVector,pVector, xlab = "Gini Index",ylab = "pValue")
    #abline(h = 0.025, col="red")
    #cat("p-Value",pivot,"\n")
    plot(pVector,xlab = "Index",ylab = "Gini Index")
    abline(h = 0.025, col="red")
    abline(v = (index+0.5), col="blue")
    print((input[index,2]+input[index+1,2])/2)
    return ((input[index,2]+input[index+1,2])/2)
  }
  emergencyExtracting <- function(label,threshold){
    output <- vector()
    for (i in seq(1, nrow(label))){
      
      chartNO <- as.character(label[[1]][i])
      case <- as.character(label[[2]][i])
      control <- as.character(label[[3]][i])
      history <- as.character(label[[8]][i])
      historyArray <- strsplit(history[1],";")
      diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
      
      emergencyNow <- FALSE
      #emergencyDataNow <- ''
      ind_array <- which(!is.na(match(emergencyOrigin$CHARTNO2, chartNO)))
      
      if(length(ind_array)>0){
        for (j in seq(1, length(ind_array))){
          index <- ind_array[j]
          emergencyDate <- as.Date(emergencyOrigin$COMECLINICDATE[index],format='%m/%d/%Y')
          diffemg <- as.numeric(difftime(diseaseDate, emergencyDate, tz, units = c("days")))
          if(diffemg <= threshold && diffemg > 0){
            emergencyNow <- TRUE
            #emergencyDataNow <- paste(emergencyDataNow,as.character(emergencyDate),';')
          }
        }
      }
      output <- rbind(output,emergencyNow)
    }
    return(output)
  }
  outPatientExtracting <- function(label,threshold){
    
    output <- vector()
    for (i in seq(1, nrow(label))){
      
      chartNO <- as.character(label[[1]][i])
      case <- as.character(label[[2]][i])
      control <- as.character(label[[3]][i])
      history <- as.character(label[[8]][i])
      historyArray <- strsplit(history[1],";")
      diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
      
      outPatientClinicNow <- FALSE
      #emergencyDataNow <- ''
      ind_array <- which(!is.na(match(outPatientClinicOrigin$CHARTNO2, chartNO)))
      
      if(length(ind_array)>0){
        for (j in seq(1, length(ind_array))){
          index <- ind_array[j]
          #print(index)
          outPatientClinicDate <- as.Date(outPatientClinicOrigin$COMECLINICDATE[index],format='%Y/%m/%d')
          diffemg <- as.numeric(difftime(diseaseDate, outPatientClinicDate, tz, units = c("days")))
          if(diffemg < threshold && diffemg > 0){
            outPatientClinicNow <- TRUE
            #outPatientClinicDataNow <- paste(outPatientClinicDataNow,as.character(outPatientClinicDate),';')
          }
      }
      }
      output <- rbind(output,outPatientClinicNow)
    }
    return(output)
  }
  hospitalizationExtracting <- function(label,threshold){
    
    output <- vector()
    for (i in seq(1, nrow(label))){
      
      chartNO <- as.character(label[[1]][i])
      case <- as.character(label[[2]][i])
      control <- as.character(label[[3]][i])
      history <- as.character(label[[8]][i])
      historyArray <- strsplit(history[1],";")
      diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
      
      hospitalizationNow <- FALSE
      #emergencyDataNow <- ''
      ind_array <- which(!is.na(match(hospitalizationOrigin$CHARTNO2, chartNO)))
      
      if(length(ind_array)>0){
        for (j in seq(1, length(ind_array))){
          index <- ind_array[j]
          
          inDate <- as.Date(hospitalizationOrigin$INDATE[index],format='%m/%d/%Y')
          outDate <- as.Date(hospitalizationOrigin$OUTDATE[index],format='%m/%d/%Y')
          
          diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
          diffOut <- as.numeric(difftime(diseaseDate, outDate, tz, units = c("days")))
          #print(diffIn)
          #print(diffOut)
          if(is.na(diffOut)){
            if(diffIn>0){
              hospitalizationNow <- TRUE
             }
          }
          else{
            if(diffOut <= threshold && diffOut > 0){
              hospitalizationNow <- TRUE
              }
            else{
              if (diffIn*diffOut<=0){
                hospitalizationNow <- TRUE
              }
            }
          }
        }
      }
      #print(hospitalizationNow)
      output <- rbind(output,hospitalizationNow)
    }
    return(output)
  }
  transferHospitalExtracting <- function(label,threshold){
    
    output <- vector()
    for (i in seq(1, nrow(label))){
      
      chartNO <- as.character(label[[1]][i])
      case <- as.character(label[[2]][i])
      control <- as.character(label[[3]][i])
      history <- as.character(label[[8]][i])
      historyArray <- strsplit(history[1],";")
      diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
      
      transferHospitalNow <- FALSE
      #emergencyDataNow <- ''
      ind_array <- which(!is.na(match(transferHospitalOrigin$CHARTNO2, chartNO)))
      
      if(length(ind_array)>0){
        for (j in seq(1, length(ind_array))){
          index <- ind_array[j]
          inDate <- as.Date(transferHospitalOrigin$INDATE[index],format='%m/%d/%Y')
          
          diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
          
          if(diffIn <= threshold && diffIn >= 0){
            transferHospitalNow <- TRUE
          }
        }
      }
      output <- rbind(output,transferHospitalNow)
    }
    return(output)
  }
  ICUStayExtracting <- function(label,threshold){
    output <- vector()
    for (i in seq(1, nrow(label))){
      
      chartNO <- as.character(label[[1]][i])
      case <- as.character(label[[2]][i])
      control <- as.character(label[[3]][i])
      history <- as.character(label[[8]][i])
      historyArray <- strsplit(history[1],";")
      diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
      
      ICUStayNow <- FALSE
      ind_array <- which(!is.na(match(ICUStayOrigin$CHARTNO2, chartNO)))
      
      if(length(ind_array)>0){
        for (j in seq(1, length(ind_array))){
          index <- ind_array[j]
          
          inDate <- as.Date(ICUStayOrigin$TRANSFERINDATE[index],format='%m/%d/%Y')
          outDate <- as.Date(ICUStayOrigin$TRANSFEROUTDATE[index],format='%m/%d/%Y')
          
          diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
          diffOut <- as.numeric(difftime(diseaseDate, outDate, tz, units = c("days")))
          #print(index)
          if(is.na(diffOut)){
            if(diffIn>0){
              ICUStayNow <- TRUE
            }
          }
          else{
            if(diffOut <= threshold && diffOut > 0){
              ICUStayNow <- TRUE
            }
            else{
              if (diffIn*diffOut<=0){
                ICUStayNow <- TRUE
              }
            }
          }
        }
      }
      output <- rbind(output,ICUStayNow)
    }
    return(output)
  }
  ColonizationExtracting <- function(label,threshold){
    
    output <- vector()
    for (i in seq(1, nrow(label))){
      
      chartNO <- as.character(label[[1]][i])
      case <- as.character(label[[2]][i])
      control <- as.character(label[[3]][i])
      history <- as.character(label[[8]][i])
      historyArray <- strsplit(history[1],";")
      diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
      
      ColonizationNow <- FALSE
      #emergencyDataNow <- ''
      ind_array <- which(!is.na(match(ColonizationOrigin$CHARTNO2, chartNO)))
      
      if(length(ind_array)>0){
        for (j in seq(1, length(ind_array))){
          index <- ind_array[j]
          inDate <- as.Date(ColonizationOrigin$SAMPLINGDATECHAR[index],format='%Y%m%d')
          
          diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
          
          if(diffIn <= threshold && diffIn >= 0){
            ColonizationNow <- TRUE
          }
        }
      }
      output <- rbind(output,ColonizationNow)
    }
    return(output)
  }
  
  FeaturAnalysis <- function(label,feature){
    CaseTrue <- 0
    CaseFalse <- 0
    ControlTrue <-0
    ControlFalse <-0
    
    if(nrow(label)==nrow(feature)){
      for (i in seq(1, nrow(label))){
        chartNO <- as.character(label[[1]][i])
        case <- as.logical(label[[2]][i])
        control <- as.logical(label[[3]][i])
        if(case&&control)
          next
        featureNow <- as.logical(feature[i,1])
        if(case){
          if(featureNow){
            CaseTrue <- CaseTrue + 1
          }
          else{
            CaseFalse <- CaseFalse + 1
          }
        }
        else{
          if(featureNow){
            ControlTrue <- ControlTrue + 1
          }
          else{
            ControlFalse <- ControlFalse + 1
          }
        }
      }
    }
    else
      print("Data Length Error")
    print("case")
    print(CaseTrue/(CaseTrue+CaseFalse)*100)
    print("control")
    print(ControlTrue/(ControlTrue+ControlFalse)*100)
  }
  
  
  outPatientThresholdOptimization <- GiniPivot(outPatientClinicVec)
  hospitalizationThresholdOptimization <- GiniPivot(hospitalizationVec)
  emergencyThresholdOptimization <- GiniPivot(emergencyVec)
  transferHospitalThresholdOptimization <- GiniPivot(transferHospitalVec)
  ICUStayThresholdOptimization <- GiniPivot(ICUStayVec)
  ColonizationThresholdOptimization <- GiniPivot(ColonizationVec)
  
  outPatientThresholdOptimizationVec <- c(outPatientThresholdOptimizationVec,outPatientThresholdOptimization)
  hospitalizationThresholdOptimizationVec <- c(hospitalizationThresholdOptimizationVec,hospitalizationThresholdOptimization)
  emergencyThresholdOptimizationVec <- c(emergencyThresholdOptimizationVec,emergencyThresholdOptimization)
  transferHospitalThresholdOptimizationVec <- c(transferHospitalThresholdOptimizationVec,transferHospitalThresholdOptimization)
  ICUStayThresholdOptimizationVec <- c(ICUStayThresholdOptimizationVec,ICUStayThresholdOptimization)
  ColonizationThresholdOptimizationVec <- c(ColonizationThresholdOptimizationVec,ColonizationThresholdOptimization)
  
}
  if(histDraw){
    hist(outPatientThresholdOptimizationVec)
    hist(hospitalizationThresholdOptimizationVec)
    hist(emergencyThresholdOptimizationVec)
    hist(transferHospitalThresholdOptimizationVec)
    hist(ICUStayThresholdOptimizationVec)
    hist(ColonizationThresholdOptimizationVec)
  }
  # Train Feature Extracting
  emergencyTrainOri<- emergencyExtracting(trainDataOrigin,emergencyThresholdOrigin)
  emergencyTrainOpt<- emergencyExtracting(trainDataOrigin,emergencyThresholdOptimization)
  outPatientTrainOri<- outPatientExtracting(trainDataOrigin,outPatientThresholdOrigin)
  outPatientTrainOpt<- outPatientExtracting(trainDataOrigin,outPatientThresholdOptimization)
  hospitalizationTrainOri<- hospitalizationExtracting(trainDataOrigin,hospitalizationThresholdOrigin)
  hospitalizationTrainOpt<- hospitalizationExtracting(trainDataOrigin,hospitalizationThresholdOptimization)
  transferTrainOri<- transferHospitalExtracting(trainDataOrigin,transferHospitalThresholdOrigin)
  transferTrainOpt<- transferHospitalExtracting(trainDataOrigin,transferHospitalThresholdOptimization)
  ICUStayTrainOri<- ICUStayExtracting(trainDataOrigin,ICUStayThresholdOrigin)
  ICUStayTrainOpt<- ICUStayExtracting(trainDataOrigin,ICUStayThresholdOptimization)
  ColonizationTrainOri<- ColonizationExtracting(trainDataOrigin,ColonizationThresholdOrigin)
  ColonizationTrainOpt<- ColonizationExtracting(trainDataOrigin,ColonizationThresholdOptimization)



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



#Draw Pictures

hospitalizationVec_Case <- hospitalizationVec[hospitalizationVec[,1]==1,]
hospitalizationVec_Control<- hospitalizationVec[hospitalizationVec[,1]==0,]

outPatientClinicVec_Case <- outPatientClinicVec[outPatientClinicVec[,1]==1,]
outPatientClinicVec_Control<- outPatientClinicVec[outPatientClinicVec[,1]==0,]

emergencyVec_Case <- emergencyVec[emergencyVec[,1]==1,]
emergencyVec_Control<- emergencyVec[emergencyVec[,1]==0,]

ICUStayVec_Case <- ICUStayVec[ICUStayVec[,1]==1,]
ICUStayVec_Control<- ICUStayVec[ICUStayVec[,1]==0,]

transferHospital_Case <- transferHospitalVec[transferHospitalVec[,1]==1,]
transferHospital_Control<- transferHospitalVec[transferHospitalVec[,1]==0,]

Colonization_Case <- ColonizationVec[ColonizationVec[,1]==1,]
Colonization_Control<- ColonizationVec[ColonizationVec[,1]==0,]

Feature = c(as.numeric(hospitalizationVec_Case[,2]),as.numeric(hospitalizationVec_Control[,2]),
            as.numeric(outPatientClinicVec_Case[,2]),as.numeric(outPatientClinicVec_Control[,2]),
            as.numeric(emergencyVec_Case[,2]),as.numeric(emergencyVec_Control[,2]),
            as.numeric(ICUStayVec_Case[,2]),as.numeric(ICUStayVec_Control[,2]),
            as.numeric(transferHospital_Case[,2]),as.numeric(transferHospital_Control[,2]),
            as.numeric(Colonization_Case[,2]),as.numeric(Colonization_Control[,2]))

group <- rep(c(1 , 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2), c(nrow(hospitalizationVec_Case),nrow(hospitalizationVec_Control),
                                              nrow(outPatientClinicVec_Case),nrow(outPatientClinicVec_Control),
                                              nrow(emergencyVec_Case),nrow(emergencyVec_Control),
                                              nrow(ICUStayVec_Case),nrow(ICUStayVec_Control),
                                              nrow(transferHospital_Case),nrow(transferHospital_Control),
                                              nrow(Colonization_Case),nrow(Colonization_Control)
                                              ))

treatment <- rep(c("住院", "門診","急診", "ICU","轉院","Colonization"), 
                 c(nrow(hospitalizationVec),nrow(outPatientClinicVec),nrow(emergencyVec),
                   nrow(ICUStayVec),nrow(transferHospitalVec),nrow(ColonizationVec)))

beanplot(Feature ~ group*treatment, ll = 0.04,ylim=c(-10,90), xlim=c(0.7,7.5),
         main = "Feature Distribution", side = "both",  col = list("purple", c("lightblue", "black")),bw="nrd0",
         wd = 12.5 , axes=F , beanlines = "median")

axis(1,at=c(1, 2, 3 , 4 ,5 ,6),  labels=c("Colonization", "ICU","住院", "門診","急診","轉院"))
axis(2)

legend("bottomright", fill = c("purple", "lightblue"), legend = c("Case", "Control"), box.lty=0)