#install Packages if needed
#install.packages("xgboost")
#install.packages("caret")
#install.packages("data.table")
#install.packages("mlr")
#install.packages("e1071")
require(xgboost)
library(caret)
library(Matrix)
library(data.table)
library(mlr)

#Remove all variables
options(warn=-1)
cat("\014")  
rm(list=ls(all=TRUE))
#It is the Input table
Data<-read.table("Feature.csv",sep=",",header=TRUE)
ind <- which(with( Data, case==TRUE & control==TRUE ))
Data <- Data[ -ind, ]
NCase <- sum(Data[[2]], na.rm=TRUE)
NControl <- sum(Data[[3]], na.rm=TRUE)
#Split to train and test
Ratio = 0.75
set.seed(22)
a <- createDataPartition(Data[[2]], p = Ratio, list=FALSE)
trainDataOrigin <- setDT(Data[a,])
ind <- which(with( trainDataOrigin, case==TRUE & control==FALSE ))
TrainCase <- trainDataOrigin[ ind, ]
TrainControl <- trainDataOrigin[ -ind, ]

Ntrain <- nrow(trainDataOrigin)
NTrainCase <- round(Ntrain/2)
NTraincontrol <- Ntrain-NTrainCase

TrainCaseSample <- TrainCase[sample(nrow(TrainCase), size=NTrainCase, replace=T), ]
TrainControlSample <- TrainControl[sample(nrow(TrainControl), size=NTraincontrol, replace=T), ]
TrainSample <- rbind(TrainCaseSample,TrainControlSample)

TrainSample <- TrainSample[sample(nrow(TrainSample)),]

testDataOrigin <- setDT(Data[-a,])

#Start to predict
trainData <- subset(TrainSample, select=c("case","gender", "age","emergency","hospitalization","hospitalizationDay","outPatientClinic","transferHospital","ICUStay","ICUStayDays","priCandida","priBacteria","CCI1","CCI2","Colonization","Drug1_Antibacterial","Drug2_Antifungal","Drug3_Chemotherapy","Drug4_Gastric",
                                              "Drug5_Immunosuppressive","Drug6_Monoclonal","Drug7_Steroid",
                                              "AcutePancreatitis","GastrointestinalPerforation","Malnutrition_CCI","Stroke","EndotrachealIntubation",
                                              "ExtracorporealMembraneOxygenation","Hemodialysis","IndwellingCentralBenousCatheters",
                                              "IndwellingUrinaryCatheter","IntravascularCatheter","MechanicValve","MechanicalVentilation",
                                              "Pacemaker","PleuralDrainage","ProsthesisJoint","SolidOrganTransplantation","TotalParenteralNutrition",
                                              "Malnutrition_Albumin","EndotrachealIntubation_NTU",
                                              "ExtracorporealMembraneOxygenation_NTU","Hemodialysis_NTU","IndwellingCentralBenousCatheters_NTU",
                                              "IndwellingUrinaryCatheter_NTU","IntravascularCatheter_NTU","MechanicValve_NTU","MechanicalVentilation_NTU",
                                              "Pacemaker_NTU","PleuralDrainage_NTU","ProsthesisJoint_NTU","SolidOrganTransplantation_NTU","TotalParenteralNutrition_NTU",
                                              "Surgery_NTU"))
trainLabel <- subset(TrainSample, select="case")
testData <- subset(testDataOrigin, select=c("case","gender", "age","emergency","hospitalization","hospitalizationDay","outPatientClinic","transferHospital","ICUStay","ICUStayDays","priCandida","priBacteria","CCI1","CCI2","Colonization","Drug1_Antibacterial","Drug2_Antifungal","Drug3_Chemotherapy","Drug4_Gastric",
                                            "Drug5_Immunosuppressive","Drug6_Monoclonal","Drug7_Steroid",
                                            "AcutePancreatitis","GastrointestinalPerforation","Malnutrition_CCI","Stroke","EndotrachealIntubation",
                                            "ExtracorporealMembraneOxygenation","Hemodialysis","IndwellingCentralBenousCatheters",
                                            "IndwellingUrinaryCatheter","IntravascularCatheter","MechanicValve","MechanicalVentilation",
                                            "Pacemaker","PleuralDrainage","ProsthesisJoint","SolidOrganTransplantation","TotalParenteralNutrition",
                                            "Malnutrition_Albumin","EndotrachealIntubation_NTU",
                                            "ExtracorporealMembraneOxygenation_NTU","Hemodialysis_NTU","IndwellingCentralBenousCatheters_NTU",
                                            "IndwellingUrinaryCatheter_NTU","IntravascularCatheter_NTU","MechanicValve_NTU","MechanicalVentilation_NTU",
                                            "Pacemaker_NTU","PleuralDrainage_NTU","ProsthesisJoint_NTU","SolidOrganTransplantation_NTU","TotalParenteralNutrition_NTU",
                                            "Surgery_NTU"))

testLabel <- subset(testDataOrigin, select="case")
trainData[is.na(trainData)] <- "Missing" 
testData[is.na(testData)] <- "Missing"

#trainData <- as.factor(unlist(trainData))
#preparing matrix 
labels <- TrainSample$case 
ts_label <- testDataOrigin$case

new_tr <- model.matrix(~.+0,data = trainData[,-c("case"),with=F])
new_ts <- model.matrix(~.+0,data = testData[,-c("case"),with=F])

labels <- as.numeric(labels)
ts_label <- as.numeric(ts_label)

dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1, gamma=0.5, max_depth=20, min_child_weight=1, subsample=0.3, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 500, maximize = F)

min(xgbcv$test.error.mean)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 1000, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

confusionMatrix (xgbpred, ts_label)
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:10], cex=1.5, left_margin = 13) 


#Basic Statastic, case-control-case+control
result <- matrix(0, nrow=3, ncol=7)

for(i in 1:nrow(Data)){
  case <- Data[i,3]
  control <- Data[i,4]
  gender <- Data[i,10]
  age <- Data[i,11]
  emergency <- Data[i,12]
  hospitalization <- Data[i,14]
  outPatientClinic <- Data[i,16]
  
  if(case && control){
    index <- 3
  }
  else{
    if(case)
      index <- 1
    else
      index <- 2
  }
  
  result[index,2] <- result[index,2] + 1
  if(as.character(gender)=="M"){
    result[index,3] <- result[index,3] + 1
  }
  
  result[index,4] <- result[index,4] + age
  
  if(hospitalization)
    result[index,5] <- result[index,5] + 1
  if(emergency)
    result[index,6] <- result[index,6] + 1
  if(outPatientClinic)
    result[index,7] <- result[index,7] + 1
  #print(case)
}
for(i in 3:7){
  result[1,i] <- result[1,i]/result[1,2]
  result[2,i] <- result[2,i]/result[2,2]
  result[3,i] <- result[3,i]/result[3,2]
}
#newRow<-data.frame(,stringsAsFactors=FALSE)
#names(newRow)<-c("disease","amount","gender(M/F)","ageAverage","hospitalization(%)","emergency(%)","outPatientClinic(%)")
