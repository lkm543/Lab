#install Packages if needed
#install.packages("neuralnet")
#install.packages("nnet")
#install.packages("magrittr")
require(neuralnet)
require(nnet)
library(caret)
library(Matrix)
library(data.table)
library(mlr)
library(magrittr)
#Remove all variables
options(warn=-1)
cat("\014")  
rm(list=ls(all=TRUE))

#It is the Input table
Data<-read.table("Feature.csv",sep=",",header=TRUE)
#Split to train and test
set.seed(22)
a <- createDataPartition(Data[[3]], p = 0.8, list=FALSE)
trainDataOrigin <- setDT(Data[a,])
testDataOrigin <- setDT(Data[-a,])

trainData <- subset(trainDataOrigin, select=c("case","gender", "age","emergency","hospitalization","hospitalizationDay","outPatientClinic","transferHospital","ICUStay","ICUStayDays","priCandida","priBacteria","CCI1","CCI2","Colonization","Drug1_Antibacterial","Drug2_Antifungal","Drug3_Chemotherapy","Drug4_Gastric",
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

testData <- subset(testDataOrigin, select=c("gender", "age","emergency","hospitalization","hospitalizationDay","outPatientClinic","transferHospital","ICUStay","ICUStayDays","priCandida","priBacteria","CCI1","CCI2","Colonization","Drug1_Antibacterial","Drug2_Antifungal","Drug3_Chemotherapy","Drug4_Gastric",
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

# Converting
(to.replace <- names(which(sapply(trainData, is.logical))));
for (var in to.replace) trainData[, var:= (as.numeric(get(var)-0.5)*2), with=FALSE];
trainData$gender <- unclass(trainData$gender) %>% as.numeric *2 - 3
trainData$case <- (trainData$case + 1) / 2

(to.replace <- names(which(sapply(testData, is.logical))));
for (var in to.replace) testData[, var:= (as.numeric(get(var)-0.5)*2), with=FALSE];
testData$gender <- unclass(testData$gender) %>% as.numeric *2 - 3

formula.bpn <- case ~ gender + age + emergency + hospitalization + hospitalizationDay + outPatientClinic + transferHospital + ICUStay + ICUStayDays + priCandida + priBacteria + CCI1 + CCI2 + Colonization + Drug1_Antibacterial + Drug2_Antifungal + Drug3_Chemotherapy + Drug4_Gastric + Drug5_Immunosuppressive + Drug6_Monoclonal + Drug7_Steroid

# tune parameters
#model <- caret::train(form=formula.bpn,     # formula
#               data=trainData,           # 資料
#               method="neuralnet",   # 類神經網路(bpn)
#               
#               # 最重要的步驟：觀察不同排列組合(第一層1~4個nodes ; 第二層0~4個nodes)
#               # 看何種排列組合(多少隱藏層、每層多少個node)，會有最小的RMSE
#               tuneGrid = expand.grid(.layer1=c(16:32), .layer2=c(16:32), .layer3=c(0)),               
#               
#               # 以下的參數設定，和上面的neuralnet內一樣
#               learningrate = 0.01,  # learning rate
#               threshold = 0.2,     # partial derivatives of the error function, a stopping criteria
#               stepmax = 500,         # 最大的ieration數 = 500000(5*10^5)
#                lifesign="full",
#                lifesign.step= 10 
#)

#model
#f1 <- as.formula('case ~ gender + age + emergency + hospitalization + hospitalizationDay + outPatientClinic + transferHospital + ICUStay + ICUStayDays + priCandida + priBacteria + CCI1 + CCI2 + Colonization + Drug1_Antibacterial + Drug2_Antifungal + Drug3_Chemotherapy + Drug4_Gastric + Drug5_Immunosuppressive + Drug6_Monoclonal + Drug7_Steroid')
#model <- train(data.frame(trainData[,2:ncol(trainData)]), trainData$case , method = nnet, tuneGrid = expand.grid(.layer1 = c(1:32), .layer2 = c(1:32), .layer3 = c(0)), learningrate = 0.01)
#model

bpn <- neuralnet(formula = formula.bpn, 
                 data = trainData,
                 hidden = c(16,16),       # 一個隱藏層：2個node
                 learningrate = 0.2, # learning rate
                 threshold = 1.2,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 30000,        # 最大的ieration數 = 500000(5*10^5)
                 lifesign="full",
                 lifesign.step= 10 
                 #linear.output=T
                 )

plot(bpn)

# 使用bpn模型，輸入test set後進行預測
# 需要注意的是，輸入的test資料只能包含input node的值
# 所以取前四個欄位，丟入模型進行預測
pred <- compute(bpn, testData);

# 預測結果
#pred$net.result
pred$net.result[ pred$net.result <0.5 ] <- 0
pred$net.result[ pred$net.result >0.5 ] <- 1

# Converting
Label <- subset(testDataOrigin, select=c("case"));
(to.replace <- names(which(sapply(Label, is.logical))));
for (var in to.replace) Label[, var:= (as.numeric(get(var)-0.5)*2), with=FALSE];
Label <- (Label + 1) / 2;

# 混淆矩陣 (預測率有96.67%)
table(real    = unlist(Label),  predict = unlist(pred$net.result))