#Q: Transfer from Hospital day?
#Q: ICU in 30 days? or exceed 30?
#Remove all variables
cat("\014")  
rm(list=ls(all=TRUE))
#options(warn=-1)
Sys.setenv(TZ="Asia/Taipei")
#It is the Input table
rawLabel<-read.table("RowData/06念珠菌菌血症史_菌血症史.txt",sep="\t",header=TRUE);
labelOrigin<-read.table("EventData.csv",sep=",",header=TRUE)

basicInformationOrigin<-read.table("RowData/01性別出生日期.csv",sep=",",header=TRUE)
outPatientClinicOrigin<-read.table("RowData/02門診史.txt",sep="\t",header=TRUE)
hospitalizationOrigin<-read.table("RowData/03住院史.csv",sep=",",header=TRUE)
emergencyOrigin<-read.table("RowData/04急診史.csv",sep=",",header=TRUE)
transferHospitalOrigin<-read.table("RowData/05轉院註記.csv",sep=",",header=TRUE)
ICUStayOrigin<-read.table("RowData/07轉床紀錄.csv",sep=",",header=TRUE)
CCI_1_Origin<-read.table("RowData/10-1門診臨床診斷.txt",sep="\t",header=TRUE,quote = "", fill=TRUE)
#CCI_3_Origin<-read.csv("RowData/10-2住院臨床診斷.txt",sep="\t", header=TRUE)
CCI_2_Origin<-read.csv("RowData/10-3住院病歷室疾分診斷.csv",sep=",", header=TRUE)
#CCI_3_Origin<-read.csv("RowData/10-4急診診斷.csv",sep=",", header=TRUE)
#Only for hospitalization
CCI_4_Origin<-read.csv("RowData/10-2住院臨床診斷.txt",sep="\t", header=TRUE)
ColonizationOrigin<-read.csv("RowData/06-1candida colonization.csv",sep=",", header=TRUE)
CCIStandard <- read.table("RowData/ICD_Standard.csv",sep=",",header=TRUE);
#Diagnosis for drugs
Diagnosis_1 <- read.table("RowData/09-1門診處方醫令(clear).txt",quote = "",sep="\t",header=TRUE, fill = TRUE);
Diagnosis_4 <- read.table("RowData/09-4住院護理給藥紀錄.txt",sep="\t",header=TRUE, fill = TRUE,quote = "");
Diagnosis_7 <- read.table("RowData/09-7急診護理給藥紀錄.txt",sep="\t",header=TRUE, fill = TRUE,quote = "");

Diagnosis_1_TPN <- read.table("RowData/09-1門診處方醫令_TPN(clear).csv",sep=",",header=TRUE)
Diagnosis_4_TPN <- read.table("RowData/09-4住院護理給藥紀錄_TPN.txt",sep="\t",header=TRUE, fill = TRUE,quote = "");
Diagnosis_7_TPN <- read.table("RowData/09-7急診護理給藥紀錄_TPN.csv",sep=",",header=TRUE)

Diagnosis_DOT_1 <- subset(Diagnosis_1, select=c("CHARTNO2","DRUGCODE", "START_DATE", "END_DATE"),stringsAsFactors=FALSE)
Diagnosis_1 <- subset(Diagnosis_1, select=c("CHARTNO2","DRUGCODE", "END_DATE"),stringsAsFactors=FALSE)
Diagnosis_4 <- subset(Diagnosis_4, select=c("CHARTNO2","DRUGCODE", "GIVEDRUGDATETIME"),stringsAsFactors=FALSE)
Diagnosis_7 <- subset(Diagnosis_7, select=c("CHARTNO2","DRUGCODE", "GIVEDRUGDATETIME"),stringsAsFactors=FALSE)

colnames(Diagnosis_1) <- c("CHARTNO2", "DRUGCODE", "DATE")
Diagnosis_1[3] <- lapply(Diagnosis_1[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
colnames(Diagnosis_4) <- c("CHARTNO2", "DRUGCODE", "DATE")
Diagnosis_4[3] <- lapply(Diagnosis_4[3],function(x){strsplit(as.character(x)," ")[[1]][1]})
Diagnosis_4[3] <- lapply(Diagnosis_4[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
colnames(Diagnosis_7) <- c("CHARTNO2", "DRUGCODE", "DATE")
Diagnosis_7[3] <- lapply(Diagnosis_7[3],function(x){strsplit(as.character(x)," ")[[1]][1]})
Diagnosis_7[3] <- lapply(Diagnosis_7[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})

Diagnosis <- rbind(Diagnosis_1,Diagnosis_4,Diagnosis_7);
Diagnosis_DOT_2 <- rbind(Diagnosis_4,Diagnosis_7);

Diagnosis_DOT_1_TPN <- subset(Diagnosis_1_TPN, select=c("CHARTNO2","DRUGCODE", "START_DATE", "END_DATE"),stringsAsFactors=FALSE)
Diagnosis_1_TPN <- subset(Diagnosis_1_TPN, select=c("CHARTNO2","DRUGCODE", "END_DATE"),stringsAsFactors=FALSE)
Diagnosis_4_TPN <- subset(Diagnosis_4_TPN, select=c("CHARTNO2","DRUGCODE", "GIVEDRUGDATETIME"),stringsAsFactors=FALSE)
Diagnosis_7_TPN <- subset(Diagnosis_7_TPN, select=c("CHARTNO2","DRUGCODE", "GIVEDRUGDATETIME"),stringsAsFactors=FALSE)

colnames(Diagnosis_1_TPN) <- c("CHARTNO2", "DRUGCODE", "DATE")
Diagnosis_1_TPN[3] <- lapply(Diagnosis_1_TPN[3],function(x){as.POSIXct(as.character(x),format='%m/%d/%Y')})
colnames(Diagnosis_4_TPN) <- c("CHARTNO2", "DRUGCODE", "DATE")
Diagnosis_4_TPN[3] <- lapply(Diagnosis_4_TPN[3],function(x){strsplit(as.character(x)," ")[[1]][1]})
Diagnosis_4_TPN[3] <- lapply(Diagnosis_4_TPN[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
colnames(Diagnosis_7_TPN) <- c("CHARTNO2", "DRUGCODE", "DATE")
Diagnosis_7_TPN[3] <- lapply(Diagnosis_7_TPN[3],function(x){strsplit(as.character(x)," ")[[1]][1]})
Diagnosis_7_TPN[3] <- lapply(Diagnosis_7_TPN[3],function(x){as.POSIXct(as.character(x),format='%m/%d/%Y')})

Diagnosis_TPN <- rbind(Diagnosis_1_TPN,Diagnosis_4_TPN,Diagnosis_7_TPN);
Diagnosis_DOT_2_TPN <- rbind(Diagnosis_4_TPN,Diagnosis_7_TPN);

#DRUGs Catalogs
Drug_1_Origin<-read.csv("RowData/DRUG2.0/Antibacterial agents.csv",sep=",", header=TRUE)
Drug_1_Origin <- subset(Drug_1_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
Drug_1_Origin <- cbind(Drug_1_Origin,index = 1)
Drug_2_Origin<-read.csv("RowData/DRUG2.0/Antifungal agents.csv",sep=",", header=TRUE)
Drug_2_Origin <- subset(Drug_2_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
Drug_2_Origin <- cbind(Drug_2_Origin,index = 2)
Drug_3_Origin<-read.csv("RowData/DRUG2.0/Chemotherapy.csv",sep=",", header=TRUE)
Drug_3_Origin <- subset(Drug_3_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
Drug_3_Origin <- cbind(Drug_3_Origin,index = 3)
Drug_4_Origin<-read.csv("RowData/DRUG2.0/Gastric Acid-Suppressive Medications.csv",sep=",", header=TRUE)
Drug_4_Origin <- subset(Drug_4_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
Drug_4_Origin <- cbind(Drug_4_Origin,index = 4)
Drug_5_Origin<-read.csv("RowData/DRUG2.0/Immunosuppressive agents.csv",sep=",", header=TRUE)
Drug_5_Origin <- subset(Drug_5_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
Drug_5_Origin <- cbind(Drug_5_Origin,index = 5)
Drug_6_Origin<-read.csv("RowData/DRUG2.0/Monoclonal agents.csv",sep=",", header=TRUE)
Drug_6_Origin <- subset(Drug_6_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
Drug_6_Origin <- cbind(Drug_6_Origin,index = 6)
Drug_7_Origin<-read.csv("RowData/DRUG2.0/Systemic steroid.csv",sep=",", header=TRUE)
Drug_7_Origin <- subset(Drug_7_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
Drug_7_Origin <- cbind(Drug_7_Origin,index = 7)
Drug_8_Origin<-read.csv("RowData/DRUG2.0/TPN_DRUGCODE.csv",sep=",", header=TRUE)
Drug_8_Origin <- subset(Drug_8_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
Drug_8_Origin <- cbind(Drug_8_Origin,index = 8)


Drug_Origin <- rbind(Drug_1_Origin,Drug_2_Origin,Drug_3_Origin,Drug_4_Origin,Drug_5_Origin,Drug_6_Origin,Drug_7_Origin,Drug_8_Origin);

#Diagnosis for Health Care
Diagnosis_HealthCare_1 <- read.table("RowData/10-1門診臨床診斷.txt",sep="\t",header=TRUE, fill = TRUE,stringsAsFactors=FALSE,quote = "");
Diagnosis_HealthCare_2 <-read.csv("RowData/10-2住院臨床診斷.txt",sep="\t", header=TRUE,stringsAsFactors=FALSE);
Diagnosis_HealthCare_3 <-read.csv("RowData/10-3住院病歷室疾分診斷.csv",sep=",", header=TRUE,stringsAsFactors=FALSE);

Diagnosis_HealthCare_1 <- subset(Diagnosis_HealthCare_1, select=c("CHARTNO2","DIAGNOSISCODE", "CREATEDATETIME"),stringsAsFactors=FALSE)
Diagnosis_HealthCare_2 <- subset(Diagnosis_HealthCare_2, select=c("CHARTNO2","DIAGNOSISCODE", "CREATEDATETIME"),stringsAsFactors=FALSE)


Diagnosis_HealthCare_1[3] <- lapply(Diagnosis_HealthCare_1[3],function(x){as.Date(as.POSIXct(x,format='%Y/%m/%d'))})
#for (i in 1:nrow(Diagnosis_HealthCare_1)){
#  stringTime <-as.character(Diagnosis_HealthCare_1[i,3])
#  stringTimeArray <- strsplit(stringTime," ")
#  stringTimeTransformed <- stringTimeArray[[1]][1]
#date <- as.POSIXct(as.character(stringTimeArray[[1]][3]),format= "%H:%M:%S")
#if(!is.na(stringTimeArray[[1]][2]))
#if(stringTimeArray[[1]][2]=="下午"){
#  date <- date + 12*60*60 # add 3 hours
#}
#stringTimeTransformed <- gsub("/",replacement="-",stringTimeTransformed) 
#stringTimeTransformed <-paste(stringTimeTransformed,format(date, format="%H:%M:%S"))
#Diagnosis_HealthCare_1[i,3] <- as.character(stringTimeTransformed)
#Diagnosis_HealthCare_1[i,3] <- ''
#  Diagnosis_HealthCare_1[i,3] <- as.Date(as.POSIXct(stringTimeTransformed,format='%Y/%m/%d'))

#}

Diagnosis_HealthCare_2[3] <- lapply(Diagnosis_HealthCare_2[3],function(x){as.POSIXct(as.character(x),format="%Y/%m/%d")})

#Many!!
Diagnosis_HealthCare_3_Temp <- data.frame(CHARTNO2 = c(), DIAGNOSISCODE = c(),CREATEDATETIME= c())
for (i in 1:nrow(Diagnosis_HealthCare_3)){
  chart <- Diagnosis_HealthCare_3[i,1]
  time <- Diagnosis_HealthCare_3[i,25]
  time <- as.POSIXct(as.character(time),format='%m/%d/%Y')
  #print(i)
  index = 0
  for (j in 1:20){
    if (Diagnosis_HealthCare_3[i,j+4]=='NULL'){
      index <- j
      break
    }
  }
  Diagnosis_HealthCare_3_Temp2 <- data.frame(CHARTNO2 = chart, DIAGNOSISCODE = t(Diagnosis_HealthCare_3[i,5:(4+index-1)]),CREATEDATETIME= time)
  names(Diagnosis_HealthCare_3_Temp2)<-c("CHARTNO2","DIAGNOSISCODE", "CREATEDATETIME")
  #print(Diagnosis_HealthCare_3_Temp2)
  Diagnosis_HealthCare_3_Temp <- rbind(Diagnosis_HealthCare_3_Temp,Diagnosis_HealthCare_3_Temp2)
  
}
#names(Diagnosis_HealthCare_3_Temp)<-c("CHARTNO2","DIAGNOSISCODE", "CREATEDATETIME")
rownames(Diagnosis_HealthCare_3_Temp) <- NULL
Diagnosis_HealthCare <- rbind(Diagnosis_HealthCare_1,Diagnosis_HealthCare_2,Diagnosis_HealthCare_3_Temp)

#Diagnosis for Procedure
Diagnosis_Procedure_1 <-read.csv("RowData/8-3-1門診醫療處置_醫師建檔.csv",sep=",", header=TRUE,stringsAsFactors=FALSE);
Diagnosis_Procedure_2 <-read.csv("RowData/8-3-2門診醫療處置_疾分人員建檔.csv",sep=",", header=TRUE,stringsAsFactors=FALSE);
Diagnosis_Procedure_3 <-read.csv("RowData/8-4-1住院醫療處置_醫師建檔.csv",sep=",", header=TRUE);
Diagnosis_Procedure_4 <-read.csv("RowData/8-4-2住院醫療處置_疾分人員建檔.csv",sep=",", header=TRUE,stringsAsFactors=FALSE);

Diagnosis_Procedure_NTU_1 <-read.csv("RowData/8-1門診醫療處置_台大醫令碼.txt",sep="\t", header=TRUE,stringsAsFactors=FALSE,quote = "");
Diagnosis_Procedure_NTU_2 <-read.csv("RowData/8-2 住院醫療處置_台大醫令碼.txt",sep="\t", header=TRUE,stringsAsFactors=FALSE,quote = "");

Diagnosis_Procedure_NTU_1 <- subset(Diagnosis_Procedure_NTU_1, select=c("CHARTNO2","ORDERCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
Diagnosis_Procedure_NTU_2 <- subset(Diagnosis_Procedure_NTU_2, select=c("CHARTNO2","ORDERCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)

Diagnosis_Procedure_NTU_1[3] <- lapply(Diagnosis_Procedure_NTU_1[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
Diagnosis_Procedure_NTU_2[3] <- lapply(Diagnosis_Procedure_NTU_2[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
Diagnosis_Procedure_NTU <- rbind(Diagnosis_Procedure_NTU_1,Diagnosis_Procedure_NTU_2)

Diagnosis_Procedure_1 <- subset(Diagnosis_Procedure_1, select=c("CHARTNO2","OPERATIONCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
Diagnosis_Procedure_1[3] <- lapply(Diagnosis_Procedure_1[3],function(x){as.POSIXct(as.character(x),format='%m/%d/%Y')})
names(Diagnosis_Procedure_1) <- c("CHARTNO2","OPERATIONCODE", "CREATEDATETIME")

#Diagnosis_Procedure_2 Many!! 
#data.frame(CHARTNO2 = c(), DIAGNOSISCODE = c(),CREATEDATETIME= c())
Diagnosis_Procedure_2_Temp <- data.frame(CHARTNO2 = c(), OPERATIONCODE = c(),CREATEDATETIME= c())
for (i in 1:nrow(Diagnosis_Procedure_2)){
  chart <- Diagnosis_Procedure_2[i,1]
  time <- Diagnosis_Procedure_2[i,12]
  time <- as.POSIXct(as.character(time),format='%m/%d/%Y')
  #print(i)
  index = 0
  for (j in 1:3){
    if (Diagnosis_Procedure_2[i,j+7]=='NULL'){
      index <- j
      break
    }
  }
  Diagnosis_Procedure_2_Temp2 <- data.frame(CHARTNO2 = chart, DIAGNOSISCODE = t(Diagnosis_Procedure_2[i,8:(7+index-1)]),CREATEDATETIME= time)
  names(Diagnosis_Procedure_2_Temp2)<-c("CHARTNO2","OPERATIONCODE", "CREATEDATETIME")
  rownames(Diagnosis_Procedure_2_Temp2) <- c()
  Diagnosis_Procedure_2_Temp <- rbind(Diagnosis_Procedure_2_Temp,Diagnosis_Procedure_2_Temp2)
  
}


Diagnosis_Procedure_3 <- subset(Diagnosis_Procedure_3, select=c("CHARTNO2","OPERATIONCODE", "COMPLETEDATETIME"),stringsAsFactors=FALSE)
Diagnosis_Procedure_3[3] <- lapply(Diagnosis_Procedure_3[3],function(x){as.POSIXct(as.character(x),format='%m/%d/%Y')})
names(Diagnosis_Procedure_3) <- c("CHARTNO2","OPERATIONCODE", "CREATEDATETIME")

#Diagnosis_Procedure_4 Many!!
Diagnosis_Procedure_4_Temp <- data.frame(CHARTNO2 = c(), OPERATIONCODE = c(),CREATEDATETIME= c())
for (i in 1:nrow(Diagnosis_Procedure_4)){
  chart <- Diagnosis_Procedure_4[i,1]
  time <- Diagnosis_Procedure_4[i,25]
  time <- as.POSIXct(as.character(time),format='%m/%d/%Y')
  print(i)
  index = 0
  for (j in 1:20){
    if (Diagnosis_Procedure_4[i,j+4]=='NULL'){
      index <- j
      break
    }
  }
  Diagnosis_Procedure_4_Temp2 <- data.frame(CHARTNO2 = chart, DIAGNOSISCODE = t(Diagnosis_Procedure_4[i,5:(4+index-1)]),CREATEDATETIME= time)
  names(Diagnosis_Procedure_4_Temp2)<-c("CHARTNO2","OPERATIONCODE", "CREATEDATETIME")
  Diagnosis_Procedure_4_Temp <- rbind(Diagnosis_Procedure_4_Temp,Diagnosis_Procedure_4_Temp2)
}
rownames(Diagnosis_Procedure_4_Temp) <- NULL
Diagnosis_Procedure <- rbind(Diagnosis_Procedure_1,Diagnosis_Procedure_2_Temp,Diagnosis_Procedure_3,Diagnosis_Procedure_4_Temp)
#Diagnosis_Procedure_2 <- subset(Diagnosis_Procedure_2, select=c("CHARTNO2","OPERATIONCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
#Diagnosis_Procedure_3 <- subset(Diagnosis_Procedure_3, select=c("CHARTNO2","OPERATIONCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
#Diagnosis_Procedure_4 <- subset(Diagnosis_Procedure_4, select=c("CHARTNO2","OPERATIONCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)

CCI_Albumin<-read.csv("RowData/11ALB檢驗報告.csv",sep=",", header=TRUE)
CCI_Albumin<-subset(CCI_Albumin, select=c("CHARTNO2","CONFIRMRESULT", "REPORTDATETIME"),stringsAsFactors=FALSE)


#Diagnosis for Healthcare
CCI_AcutePancreatitis<-read.csv("RowData/Diagnosis/Acute pancreatitis.csv",sep=",", header=TRUE)
CCI_GastrointestinalPerforation<-read.csv("RowData/Diagnosis/Gastrointestinal perforation.csv",sep=",", header=TRUE)
CCI_Malnutrition<-read.csv("RowData/Diagnosis/Malnutrition.csv",sep=",", header=TRUE)
CCI_Stroke<-read.csv("RowData/Diagnosis/stroke.csv",sep=",", header=TRUE)
#Diagnosis for Healthcare
CCI_EndotrachealIntubation<-read.csv("RowData/Diagnosis/Endotracheal intubation.csv",sep=",", header=TRUE)
CCI_ExtracorporealMembraneOxygenation<-read.csv("RowData/Diagnosis/Extracorporeal Membrane Oxygenation.csv",sep=",", header=TRUE)
CCI_Hemodialysis<-read.csv("RowData/Diagnosis/Hemodialysis.csv",sep=",", header=TRUE)
CCI_IndwellingCentralBenousCatheters<-read.csv("RowData/Diagnosis/Indwelling central venous catheters.csv",sep=",", header=TRUE)
CCI_IndwellingUrinaryCatheter<-read.csv("RowData/Diagnosis/Indwelling urinary catheter.csv",sep=",", header=TRUE)
CCI_IntravascularCatheter<-read.csv("RowData/Diagnosis/Intravascular catheter.csv",sep=",", header=TRUE)
CCI_MechanicValve<-read.csv("RowData/Diagnosis/Mechanic valve.csv",sep=",", header=TRUE)
CCI_MechanicalVentilation<-read.csv("RowData/Diagnosis/mechanical ventilation.csv",sep=",", header=TRUE)
CCI_Pacemaker<-read.csv("RowData/Diagnosis/Pacemaker.csv",sep=",", header=TRUE)
CCI_PleuralDrainage<-read.csv("RowData/Diagnosis/Pleural drainage.csv",sep=",", header=TRUE)
CCI_ProsthesisJoint<-read.csv("RowData/Diagnosis/Prosthesis joint.csv",sep=",", header=TRUE)
CCI_SolidOrganTransplantation<-read.csv("RowData/Diagnosis/Solid-organ transplantation.csv",sep=",", header=TRUE)
CCI_TotalParenteralNutrition<-read.csv("RowData/Diagnosis/Total parenteral nutrition.csv",sep=",", header=TRUE)


#Diagnosis for Healthcare NTU
CCI_EndotrachealIntubation_NTU<-read.csv("RowData/台大醫令碼/Endotracheal intubation.csv",sep=",", header=TRUE)
CCI_ExtracorporealMembraneOxygenation_NTU<-read.csv("RowData/台大醫令碼/Extracorporeal Membrane Oxygenation.csv",sep=",", header=TRUE)
CCI_Hemodialysis_NTU<-read.csv("RowData/台大醫令碼/Hemodialysis.csv",sep=",", header=TRUE)
CCI_IndwellingCentralBenousCatheters_NTU<-read.csv("RowData/台大醫令碼/Indwelling central venous catheter.csv",sep=",", header=TRUE)
CCI_IndwellingUrinaryCatheter_NTU<-read.csv("RowData/台大醫令碼/Indwelling urinary catheter.csv",sep=",", header=TRUE)
CCI_IntravascularCatheter_NTU<-read.csv("RowData/台大醫令碼/Intravascular catheter.csv",sep=",", header=TRUE)
CCI_MechanicValve_NTU<-read.csv("RowData/台大醫令碼/Mechanic valve.csv",sep=",", header=TRUE)
CCI_MechanicalVentilation_NTU<-read.csv("RowData/台大醫令碼/Mechanical ventilation.csv",sep=",", header=TRUE)
CCI_Pacemaker_NTU<-read.csv("RowData/台大醫令碼/Pacemaker.csv",sep=",", header=TRUE)
CCI_PleuralDrainage_NTU<-read.csv("RowData/台大醫令碼/Pleural drainage.csv",sep=",", header=TRUE)
CCI_ProsthesisJoint_NTU<-read.csv("RowData/台大醫令碼/Prosthesis joint.csv",sep=",", header=TRUE)
CCI_SolidOrganTransplantation_NTU<-read.csv("RowData/台大醫令碼/Solid-organ transplantation.csv",sep=",", header=TRUE)
CCI_TotalParenteralNutrition_NTU<-read.csv("RowData/台大醫令碼/Total parenteral nutrition.csv",sep=",", header=TRUE)
CCI_Surgery_NTU<-read.csv("RowData/台大醫令碼/Surgery.csv",sep=",", header=TRUE)
save.image("Lab.RData")
load("Lab.RData")
#It is the Output table
labelProcessed <- data.frame(chartNO=character(),
                             case=logical(),
                             control=logical(),
                             candidaPersistent=logical(),
                             candidaPolymicrobial=logical(),
                             bactPersistent=logical(),
                             bactPolymicrobial=logical(),
                             history=character(),
                             gender=character(),
                             age=integer(),
                             emergency=logical(),
                             emergencyData=character(),
                             hospitalization=logical(),
                             hospitalizationDay=integer(),
                             hospitalizationDaythisTime=integer(),
                             hospitalizationData=character(),
                             outPatientClinic=logical(),
                             outPatientClinicData=character(),
                             transferHospital=logical(),
                             ICUStay=logical(),
                             ICUStayDays=integer(),
                             ICUStayData=character(),
                             priCandida=logical(),
                             priBacteria=logical(),
                             CCI1=integer(),
                             CCI2=integer(),
                             CCIData1=character(),
                             CCIData2=character(),
                             Drug=logical(),
                             Colonization=logical(),
                             Drug1_Antibacterial=logical(),
                             Drug2_Antifungal=logical(),
                             Drug3_Chemotherapy=logical(),
                             Drug4_Gastric=logical(),
                             Drug5_Immunosuppressive=logical(),
                             Drug6_Monoclonal=logical(),
                             Drug7_Steroid=logical(),
                             Drug8_TPN=logical(),
                             
                             Drug1_Antibacterial_DOT1=integer(),
                             Drug2_Antifungal_DOT1=integer(),
                             Drug3_Chemotherapy_DOT1=integer(),
                             Drug4_Gastric_DOT1=integer(),
                             Drug5_Immunosuppressive_DOT1=integer(),
                             Drug6_Monoclonal_DOT1=integer(),
                             Drug7_Steroid_DOT1=integer(),
                             Drug8_TPN_DOT1=integer(),
                             
                             
                             Drug1_Antibacterial_DOT2=integer(),
                             Drug2_Antifungal_DOT2=integer(),
                             Drug3_Chemotherapy_DOT2=integer(),
                             Drug4_Gastric_DOT2=integer(),
                             Drug5_Immunosuppressive_DOT2=integer(),
                             Drug6_Monoclonal_DOT2=integer(),
                             Drug7_Steroid_DOT2=integer(),
                             Drug8_TPN_DOT2=integer(),
                             
                             AcutePancreatitis=logical(),
                             GastrointestinalPerforation=logical(),
                             Malnutrition_CCI=logical(),
                             Stroke=logical(),
                             EndotrachealIntubation=logical(),
                             ExtracorporealMembraneOxygenation=logical(),
                             Hemodialysis=logical(),
                             IndwellingCentralBenousCatheters=logical(),
                             IndwellingUrinaryCatheter=logical(),
                             IntravascularCatheter=logical(),
                             MechanicValve=logical(),
                             MechanicalVentilation=logical(),
                             Pacemaker=logical(),
                             PleuralDrainage=logical(),
                             ProsthesisJoint=logical(),
                             SolidOrganTransplantation=logical(),
                             TotalParenteralNutrition=logical(),
                             Malnutrition_Albumin=logical(),
                             EndotrachealIntubation_NTU=logical(),
                             ExtracorporealMembraneOxygenation_NTU=logical(),
                             Hemodialysis_NTU=logical(),
                             IndwellingCentralBenousCatheters_NTU=logical(),
                             IndwellingUrinaryCatheter_NTU=logical(),
                             IntravascularCatheter_NTU=logical(),
                             MechanicValve_NTU=logical(),
                             MechanicalVentilation_NTU=logical(),
                             Pacemaker_NTU=logical(),
                             PleuralDrainage_NTU=logical(),
                             ProsthesisJoint_NTU=logical(),
                             SolidOrganTransplantation_NTU=logical(),
                             TotalParenteralNutrition_NTU=logical(),
                             Surgery_NTU=logical(),
                             stringsAsFactors=FALSE)


for (i in 1:nrow(labelOrigin)){
  print(i)
  chartNO <- as.character(labelOrigin[i,1])
  case <- as.character(labelOrigin[i,2])
  control <- as.character(labelOrigin[i,3])
  candidaPersistent <- as.character(labelOrigin[i,4])
  candidaPolymicrobial <- as.character(labelOrigin[i,5])
  bactPersistent <- as.character(labelOrigin[i,6])
  bactPolymicrobial <- as.character(labelOrigin[i,7])
  history <- as.character(labelOrigin[i,8])
  historyArray <- strsplit(history[1],";")
  diseaseDate <- as.POSIXct(historyArray[[1]][2],format='%Y%m%d %H:%M')
  
  #Gender and Age
  ind <- match(chartNO, basicInformationOrigin$CHARTNO2)
  
  if(!is.na(ind)){
    Gender <- basicInformationOrigin$ADMINISTRATIVESEXCODE[ind]
    birth <- as.Date(paste(as.character(basicInformationOrigin$BIRTHDAY[ind]),'01'),format="%Y%m%d")
    diseaseTime <- as.POSIXct(as.character(historyArray[[1]][2]),format='%Y%m%d %H:%M')
    Age <- difftime(diseaseTime, birth, tz,units = c("days"))
    Age <- as.integer(Age/365.25)
    #print(Age)
  }
  else{
    Gender <- NULL
    Age <-NULL
  }
  
  #Hospitalization
  hospitalizationNow <- FALSE
  hospitalizationDay <- 0
  hospitalizationDaythisTime <- 0
  hospitalizationDataNow <- ""
  ind_array <- which(!is.na(match(hospitalizationOrigin$CHARTNO2, chartNO)))
  if(length(ind_array)>0){
    for (j in seq(1, length(ind_array))){
      index <- ind_array[j]
      inDate <- as.Date(hospitalizationOrigin$INDATE[index],format='%m/%d/%Y')
      outDate <- as.Date(hospitalizationOrigin$OUTDATE[index],format='%m/%d/%Y')
      
      diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
      diffOut <- as.numeric(difftime(diseaseDate, outDate, tz, units = c("days")))
      if(is.na(diffOut)){
        if(diffIn>0){
          hospitalizationNow <- TRUE
          hospitalizationDay <- diffIn
          hospitalizationDataNow <- paste(hospitalizationDataNow,'inDate;',as.character(inDate),';outDate;',as.character(outDate),';')
        }
      }
      else{
        if(diffOut <= 7 && diffOut > 0){
          hospitalizationNow <- TRUE
          hospitalizationDay <- diffIn - diffOut
          hospitalizationDataNow <- paste(hospitalizationDataNow,'inDate;',as.character(inDate),';outDate;',as.character(outDate),';')
        }
        else{
          if (diffIn*diffOut<=0){
            hospitalizationNow <- TRUE
            hospitalizationDay <- diffIn
            hospitalizationDaythisTime <- diffIn
            hospitalizationDataNow <- paste(hospitalizationDataNow,'inDate;',as.character(inDate),';outDate;',as.character(outDate),';')
          }
        }
      }
    }
  }
  #print(hospitalizationDay)
  
  #Emergency
  emergencyNow <- FALSE
  emergencyDataNow <- ""
  
  ind_array <- which(!is.na(match(emergencyOrigin$CHARTNO2, chartNO)))
  
  if(length(ind_array)>0){
    for (j in seq(1, length(ind_array))){
      index <- ind_array[j]
      #print(index)
      emergencyDate <- as.Date(emergencyOrigin$COMECLINICDATE[index],format='%m/%d/%Y')
      diffemg <- as.numeric(difftime(diseaseDate, emergencyDate, tz, units = c("days")))
      if(diffemg <= 7 && diffemg > 0){
        emergencyNow <- TRUE
        emergencyDataNow <- paste(emergencyDataNow,as.character(emergencyDate),';')
      }
    }
  }
  
  #ICU Stay
  ICUStayDays <- 0
  ICUStay <- FALSE
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
          ICUStay <- TRUE
          ICUStayDays <- ICUStayDays + diffIn
        }
        else{
          if(diffIn*diffOut<=0 && diffIn>2)
            if(diffOut<0){
              ICUStay <- TRUE
              ICUStayDays <- ICUStayDays + diffIn
              ICUStayData <- paste(ICUStayData,'inDate;',as.character(inDate),';outDate;',as.character(outDate),';')
            }
          else {
            if(diffOut <= 28 && diffOut >= 0 && diffIn-diffOut >= 2){
              ICUStay <- TRUE
              ICUStayDays <- ICUStayDays +diffIn - diffOut
              ICUStayData <- paste(ICUStayData,'inDate;',as.character(inDate),';outDate;',as.character(outDate),';')
            }
          }
        }
      }
    }
  }
  
  
  #Transfer From other Hospital
  transferHospital <- FALSE
  ind_array <- which(!is.na(match(transferHospitalOrigin$CHARTNO2, chartNO)))
  if(length(ind_array)>0){
    for (j in seq(1, length(ind_array))){
      index <- ind_array[j]
      inDate <- as.Date(transferHospitalOrigin$INDATE[index],format='%m/%d/%Y')
      
      diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
      
      if(diffIn <= 7 && diffIn >= 0){
        transferHospital <- TRUE
      }
    }
  }
  
  #Pri Candia/Bacteria
  priCandida <- FALSE
  priBacteria <- FALSE
  
  ind_array <- which(!is.na(match(rawLabel$CHARTNO2, chartNO)))
  if(length(ind_array)>0){
    for (j in seq(1, length(ind_array))){
      index <- ind_array[j]
      rawDisease <- as.character(rawLabel[index,12])
      
      #Candida
      if (grepl("candida", rawDisease, ignore.case = TRUE)){
        Bact <- FALSE
        Candida <- TRUE
      }
      #Bacteria
      else{
        Bact <- TRUE
        Candida <- FALSE
      }
      
      time <- as.POSIXct(as.character(rawLabel$SAMPLINGDATECHAR[index]),format='%Y%m%d %H:%M')
      timediff <- as.numeric(difftime(diseaseDate, time , tz,units = c("days")))
      #print(timediff)
      if(Candida == TRUE && timediff<=30 &&  timediff>0){
        priCandida = TRUE
      }
      if(Bact == TRUE && timediff<=30 &&  timediff>0){
        priBacteria = TRUE
      }
      
    }
  }
  
  #outPatientClinic
  outPatientClinicNow <- FALSE
  outPatientClinicDataNow <- ""
  
  ind_array <- which(!is.na(match(outPatientClinicOrigin$CHARTNO2, chartNO)))
  
  if(length(ind_array)>0){
    for (j in seq(1, length(ind_array))){
      index <- ind_array[j]
      #print(index)
      outPatientClinicDate <- as.Date(outPatientClinicOrigin$COMECLINICDATE[index],format='%Y/%m/%d')
      diffemg <- as.numeric(difftime(diseaseDate, outPatientClinicDate, tz, units = c("days")))
      if(diffemg < 7 && diffemg > 0){
        outPatientClinicNow <- TRUE
        outPatientClinicDataNow <- paste(outPatientClinicDataNow,as.character(outPatientClinicDate),';')
      }
    }
  }
  
  
  #CCI
  CCI <- 0
  CCIData <- ""
  #ICD_Converter <- ICD10ToCCI();
  #ICD_Converter <- LoadDataset(ICD_Converter,"ICD_Standard.csv");
  #ICD_Converter <- LoadTestData(ICD_Converter,"data.csv");
  
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
  
  #if(length(ind_array_3)>0){
  #  tempCCI_3 <- subset(CCI_3_Origin[as.vector(ind_array_3),], select=c("CHARTNO2","DIAGNOSISCODE", "DIAGNOSISENGNAME","CREATEDATETIME"))
  #  tempCCI_3[,4] <- sapply(tempCCI_3[,4],as.character) 
  #  for (j in seq(1, length(ind_array_3))){
  #    tempCCI_3[j,4] <- paste(format(as.Date(tempCCI_3[j,4],format = "%m/%d/%Y"), "%Y/%m/%d"),"00:00:00")
  #  }
  #}
  
  if(length(ind_array_4)>0){
    tempCCI_4 <- subset(CCI_4_Origin[as.vector(ind_array_4),], select=c("CHARTNO2","DIAGNOSISCODE", "DIAGNOSISENGNAME","CREATEDATETIME"))
    tempCCI_4[,4] <- sapply(tempCCI_4[,4],as.character) 
    for (j in seq(1, length(ind_array_4))){
      tempCCI_4[j,4] <- paste(format(as.Date(tempCCI_4[j,4],format = "%Y/%m/%d"), "%Y/%m/%d"),"00:00:00")
    }
  }
  
  #tempCCI_1to4 <- rbind(tempCCI_1, tempCCI_2,tempCCI_3,tempCCI_4)
  tempCCIResult_1 <- rbind(tempCCI_2,tempCCI_4)
  tempCCIResult_2 <- rbind(tempCCI_1, tempCCI_2,tempCCI_4)
  
  #It is the transfer table
  #diseaseDate
  CCI1 <- 0
  CCI2 <- 0
  CCIData1 <- ""
  Catagory <- matrix(FALSE, nrow = 1, ncol = 17)
  
  if(nrow(tempCCIResult_1)>0){
    for(CCIindex in 1:nrow(tempCCIResult_1)){
      CCIDate <- as.POSIXct(tempCCIResult_1[CCIindex,4],format = "%Y/%m/%d %H:%M:%S")
      diffCCIDisease <- as.numeric(difftime(diseaseDate, CCIDate, tz, units = c("days")))
      
      if(diffCCIDisease>=0 && diffCCIDisease<=365){
        OriginICD <- toString(tempCCIResult_1[CCIindex,2])
        OriginICD <- gsub('\\.','',OriginICD)
        test_result <- is.na(pmatch(t(CCIStandard[2]),OriginICD))
        if(!all(test_result)){
          # Calculate CCI,for it is in the transfer table
          Index <- match(FALSE,test_result)
          if(!Catagory[CCIStandard[Index,5]]){
            CCI1 <- CCI1 + CCIStandard[Index,1]
            Catagory[CCIStandard[Index,5]] <- TRUE
            CCIData1 <- paste(CCIData1,CCIStandard[Index,4],"(",CCIStandard[Index,1],")",";")
          }
        }
      }
    }
  }
  
  
  Catagory <- matrix(FALSE, nrow = 1, ncol = 17)
  CCIData2 <- ""
  if(nrow(tempCCIResult_2)>0){
    for(CCIindex in 1:nrow(tempCCIResult_2)){
      CCIDate <- as.POSIXct(tempCCIResult_2[CCIindex,4],format = "%Y/%m/%d %H:%M:%S")
      diffCCIDisease <- as.numeric(difftime(diseaseDate, CCIDate, tz, units = c("days")))
      
      if(diffCCIDisease>=0 && diffCCIDisease<=365){
        OriginICD <- toString(tempCCIResult_2[CCIindex,2])
        OriginICD <- gsub('\\.','',OriginICD)
        test_result <- is.na(pmatch(t(CCIStandard[2]),OriginICD))
        if(!all(test_result)){
          # Calculate CCI,for it is in the transfer table
          Index <- match(FALSE,test_result)
          if(!Catagory[CCIStandard[Index,5]]){
            CCI2 <- CCI2 + CCIStandard[Index,1]
            Catagory[CCIStandard[Index,5]] <- TRUE
            CCIData2 <- paste(CCIData2,CCIStandard[Index,4],"(",CCIStandard[Index,1],")",";")
          }
        }
      }
    }
  }
  
  print(CCI1)
  print(CCI2)
  
  #Colonization
  Colonization<-FALSE
  ind_array_Colonization <- which(!is.na(match(ColonizationOrigin$CHARTNO2, chartNO)))
  
  
  if(length(ind_array_Colonization)>0){
    for (j in seq(1, length(ind_array_Colonization))){
      index <- ind_array_Colonization[j]
      inDate <- as.Date(ColonizationOrigin$SAMPLINGDATECHAR[index],format='%Y%m%d')
      
      diffIn <- as.numeric(difftime(diseaseDate, inDate, tz, units = c("days")))
      
      if(diffIn <= 30 && diffIn >= 0){
        Colonization <- TRUE
      }
    }
  }
  
  #Drugs
  
  Drug1_Antibacterial <- FALSE
  Drug2_Antifungal <- FALSE
  Drug3_Chemotherapy <- FALSE
  Drug4_Gastric <- FALSE
  Drug5_Immunosuppressive <- FALSE
  Drug6_Monoclonal <- FALSE
  Drug7_Steroid  <- FALSE
  Drug8_TPN <- FALSE
  
  Drug1_Antibacterial_DOT1 <- 0
  Drug2_Antifungal_DOT1 <- 0
  Drug3_Chemotherapy_DOT1 <- 0
  Drug4_Gastric_DOT1 <- 0
  Drug5_Immunosuppressive_DOT1 <- 0
  Drug6_Monoclonal_DOT1 <- 0
  Drug7_Steroid_DOT1 <- 0
  Drug8_TPN_DOT1 <- 0
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
      
      Flag90 <- FALSE
      Flag30 <- FALSE
      Drug <- Diagnosis[[2]][index]
      DrugDate <- Diagnosis[[3]][index]
      
      ind_Drug <- which(!is.na(match(Drug_Origin$DRUGCODE, Drug)))
      if(!is.na(DrugDate)){
        diffDRUG <- as.numeric(difftime(diseaseDate, as.Date(DrugDate), tz, units = c("days")))
        
        if(diffDRUG <= 90 && diffDRUG > 0){
          Flag90 <- TRUE
        }
        
        if(diffDRUG <= 30 && diffDRUG > 0){
          Flag30 <- TRUE
        }
        
        #Bug!! ind_Drug==na
        if(length(ind_Drug)>0){
          for(Drugindex in 1:length(ind_Drug)){
            DrugCatalogIndex <- ind_Drug[Drugindex]
            DrugCatalog <- Drug_Origin[[2]][DrugCatalogIndex]
            if(DrugCatalog==1 && Flag30){
              Drug1_Antibacterial <- TRUE
            }
            if(DrugCatalog==2 && Flag30){
              Drug2_Antifungal <- TRUE
            }
            if(DrugCatalog==3 && Flag90){
              Drug3_Chemotherapy <- TRUE
            }
            if(DrugCatalog==4 && Flag30){
              Drug4_Gastric <- TRUE
            }
            if(DrugCatalog==5 && Flag90){
              Drug5_Immunosuppressive <- TRUE
            }
            if(DrugCatalog==6 && Flag90){
              Drug6_Monoclonal <- TRUE
            }
            if(DrugCatalog==7 && Flag90){
              Drug7_Steroid <- TRUE
            }
          }
        }
      }
    }
  }
  
  if(length(ind_Diagnosis_TPN)>0){
    for(Diagnosisindex in 1:length(ind_Diagnosis_TPN)){
      index <- ind_Diagnosis_TPN[Diagnosisindex]
      
      Flag90 <- FALSE
      Flag30 <- FALSE
      Drug <- Diagnosis_TPN[[2]][index]
      DrugDate <- Diagnosis_TPN[[3]][index]
      
      ind_Drug <- which(!is.na(match(Drug_8_Origin$DRUGCODE, Drug)))
      if(!is.na(DrugDate)){
        diffDRUG <- as.numeric(difftime(diseaseDate, as.Date(DrugDate), tz, units = c("days")))
        
        if(diffDRUG <= 90 && diffDRUG > 0){
          Flag90 <- TRUE
        }
        
        if(diffDRUG <= 30 && diffDRUG > 0){
          Flag30 <- TRUE
          Drug8_TPN <- TRUE
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
        if(length(ind_Drug)>0){
          for(Drugindex in 1:length(ind_Drug)){
            DrugCatalogIndex <- ind_Drug[Drugindex]
            DrugCatalog <- Drug_Origin[[2]][DrugCatalogIndex]
            if(DrugCatalog==1 && Flag30){
              Drug1_Antibacterial_DOT1 <- DRUGDOT
            }
            if(DrugCatalog==2 && Flag30){
              Drug2_Antifungal_DOT1 <- DRUGDOT
            }
            if(DrugCatalog==3 && Flag90){
              Drug3_Chemotherapy_DOT1 <- DRUGDOT
            }
            if(DrugCatalog==4 && Flag30){
              Drug4_Gastric_DOT1 <- DRUGDOT
            }
            if(DrugCatalog==5 && Flag90){
              Drug5_Immunosuppressive_DOT1 <- DRUGDOT
            }
            if(DrugCatalog==6 && Flag90){
              Drug6_Monoclonal_DOT1 <- DRUGDOT
            }
            if(DrugCatalog==7 && Flag90){
              Drug7_Steroid_DOT1 <- DRUGDOT
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
            Drug8_TPN_DOT1 <- DRUGDOT
          else
            Drug8_TPN_DOT1 <- 30 - diffDRUG
            
        }
      }
    }
  }
  
  DOT2 <- matrix(c(FALSE), nrow = 8, ncol = 90)
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
        if(length(ind_Drug)>0){
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
  
  
  Drug1_Antibacterial_DOT2 <- DOT2[1]
  Drug2_Antifungal_DOT2 <- DOT2[2]
  Drug3_Chemotherapy_DOT2 <- DOT2[3]
  Drug4_Gastric_DOT2 <- DOT2[4]
  Drug5_Immunosuppressive_DOT2<- DOT2[5]
  Drug6_Monoclonal_DOT2 <- DOT2[6]
  Drug7_Steroid_DOT2 <- DOT2[7]
  
  Drug8_TPN_DOT2 <- DOT2_TPN[1]
  #Diagnosis
  AcutePancreatitis = FALSE
  GastrointestinalPerforation = FALSE
  Malnutrition_Albumin = FALSE
  Malnutrition_CCI = FALSE
  Stroke = FALSE
  EndotrachealIntubation = FALSE
  ExtracorporealMembraneOxygenation = FALSE
  Hemodialysis = FALSE
  IndwellingCentralBenousCatheters = FALSE
  IndwellingUrinaryCatheter = FALSE
  IntravascularCatheter = FALSE
  MechanicValve = FALSE
  MechanicalVentilation = FALSE
  Pacemaker = FALSE
  PleuralDrainage = FALSE
  ProsthesisJoint = FALSE
  SolidOrganTransplantation = FALSE
  TotalParenteralNutrition = FALSE
  
  ind_HealthCare <- which(!is.na(match(Diagnosis_HealthCare$CHARTNO2, chartNO)))
  ind_Procedure <- which(!is.na(match(Diagnosis_Procedure$CHARTNO2, chartNO)))
  
  if(length(ind_HealthCare)>0){
    for(Diagnosisindex in 1:length(ind_HealthCare)){
      index <- ind_HealthCare[Diagnosisindex]
      HealthCare <- Diagnosis_HealthCare[index,2]
      HealthCareDate <- Diagnosis_HealthCare[index,3]
      
      
      diffHealthCare <- as.numeric(difftime(diseaseDate, as.Date(HealthCareDate), tz, units = c("days")))
      
      if(diffHealthCare <= 30 && diffHealthCare > 0){
        ind_AcutePancreatitis <- which(!is.na(match(CCI_AcutePancreatitis$DIAGNOSISCODE, HealthCare)))
        ind_GastrointestinalPerforation <- which(!is.na(match(CCI_GastrointestinalPerforation$DIAGNOSISCODE, HealthCare)))
        ind_Malnutrition <- which(!is.na(match(CCI_Malnutrition, HealthCare)))
        ind_Stroke <- which(!is.na(match(CCI_Stroke$DIAGNOSISCODE, HealthCare)))
        if(length(ind_AcutePancreatitis)>0)
          AcutePancreatitis <- TRUE
        if(length(ind_GastrointestinalPerforation)>0)
          GastrointestinalPerforation <- TRUE
        if(length(ind_Malnutrition)>0)
          Malnutrition__CCI <- TRUE
        if(length(ind_Stroke)>0)
          Stroke <- TRUE
        
      }
      
    }
  }
  
  if(length(ind_Procedure)>0){
    for(Diagnosisindex in 1:length(ind_Procedure)){
      index <- ind_Procedure[Diagnosisindex]
      Procedure <- Diagnosis_Procedure[index,2]
      ProcedureDate <- Diagnosis_Procedure[index,3]
      
      diffProcedure <- as.numeric(difftime(diseaseDate, as.Date(ProcedureDate), tz, units = c("days")))
      
      if(diffProcedure <= 30 && diffProcedure > 0){
        ind_EndotrachealIntubation <- which(!is.na(match(CCI_EndotrachealIntubation$OPERATIONCODE, Procedure)))
        ind_ExtracorporealMembraneOxygenation <- which(!is.na(match(CCI_ExtracorporealMembraneOxygenation$OPERATIONCODE, Procedure)))
        ind_Hemodialysis <- which(!is.na(match(CCI_Hemodialysis$OPERATIONCODE, Procedure)))
        ind_IndwellingCentralBenousCatheters <- which(!is.na(match(CCI_IndwellingCentralBenousCatheters$OPERATIONCODE, Procedure)))
        ind_IndwellingUrinaryCatheter<- which(!is.na(match(CCI_IndwellingUrinaryCatheter$OPERATIONCODE, Procedure)))
        ind_IntravascularCatheter <- which(!is.na(match(CCI_IntravascularCatheter$OPERATIONCODE, Procedure)))
        ind_MechanicValve <- which(!is.na(match(CCI_MechanicValve$OPERATIONCODE, Procedure)))
        ind_MechanicalVentilation <- which(!is.na(match(CCI_MechanicalVentilation$OPERATIONCODE, Procedure)))
        ind_Pacemaker <- which(!is.na(match(CCI_Pacemaker$OPERATIONCODE, Procedure)))
        ind_PleuralDrainage <- which(!is.na(match(CCI_PleuralDrainage$OPERATIONCODE, Procedure)))
        ind_ProsthesisJoint <- which(!is.na(match(CCI_ProsthesisJoint$OPERATIONCODE, Procedure)))
        ind_SolidOrganTransplantation <- which(!is.na(match(CCI_SolidOrganTransplantation$OPERATIONCODE, Procedure)))
        ind_TotalParenteralNutrition <- which(!is.na(match(CCI_TotalParenteralNutrition$OPERATIONCODE, Procedure)))
        
        if(length(ind_EndotrachealIntubation)>0)
          EndotrachealIntubation <- TRUE
        if(length(ind_ExtracorporealMembraneOxygenation)>0)
          ExtracorporealMembraneOxygenation <- TRUE
        if(length(ind_Hemodialysis)>0)
          Hemodialysis <- TRUE
        if(length(ind_IndwellingCentralBenousCatheters)>0)
          IndwellingCentralBenousCatheter <- TRUE
        if(length(ind_IndwellingUrinaryCatheter)>0)
          IndwellingUrinaryCatheter <- TRUE
        if(length(ind_IntravascularCatheter)>0)
          IntravascularCatheter <- TRUE
        if(length(ind_MechanicValve)>0)
          MechanicValve <- TRUE
        if(length(ind_MechanicalVentilation)>0)
          MechanicalVentilation <- TRUE
        if(length(ind_Pacemaker)>0)
          Pacemaker <- TRUE
        if(length(ind_PleuralDrainage)>0)
          PleuralDrainage <- TRUE
        if(length(ind_ProsthesisJoint)>0)
          ProsthesisJoint <- TRUE
        if(length(ind_SolidOrganTransplantation)>0)
          SolidOrganTransplantation <- TRUE
        if(length(ind_TotalParenteralNutrition)>0)
          TotalParenteralNutrition <- TRUE
        
      }
      
    }
  }
  
  #Malnutrition
  ind_Malnutrition <- which(!is.na(match(CCI_Albumin$CHARTNO2, chartNO)))
  if(length(ind_Malnutrition)>0){
    for(Diagnosisindex in 1:length(ind_Malnutrition)){
      index <- ind_Malnutrition[Diagnosisindex]
      Albumin <- CCI_Albumin[index,2]
      AlbuminDate <- CCI_Albumin[index,3]
      AlbuminDate <- as.POSIXct(as.character(AlbuminDate),format='%m/%d/%Y')
      diffAlbumin <- as.numeric(difftime(diseaseDate, as.Date(AlbuminDate), tz, units = c("days")))
      if(Albumin=='<1.5')
      {
        Malnutrition_Albumin <- TRUE
      }
      else{
        if(diffAlbumin <= 30 && diffAlbumin > 0 && as.integer(Albumin) < 2){
          Malnutrition_Albumin <- TRUE
        }
      }
    }
  }
  
  
  #Diagnosis NTU
  EndotrachealIntubation_NTU = FALSE
  ExtracorporealMembraneOxygenation_NTU = FALSE
  Hemodialysis_NTU = FALSE
  IndwellingCentralBenousCatheters_NTU = FALSE
  IndwellingUrinaryCatheter_NTU = FALSE
  IntravascularCatheter_NTU = FALSE
  MechanicValve_NTU = FALSE
  MechanicalVentilation_NTU = FALSE
  Pacemaker_NTU = FALSE
  PleuralDrainage_NTU = FALSE
  ProsthesisJoint_NTU = FALSE
  SolidOrganTransplantation_NTU = FALSE
  TotalParenteralNutrition_NTU = FALSE
  Surgery_NTU <- FALSE
  
  ind_Procedure <- which(!is.na(match(Diagnosis_Procedure_NTU$CHARTNO2, chartNO)))
  
  
  if(length(ind_Procedure)>0){
    for(Diagnosisindex in 1:(length(ind_Procedure))){
      index <- ind_Procedure[Diagnosisindex]
      Procedure <- Diagnosis_Procedure_NTU[index,2]
      ProcedureDate <- Diagnosis_Procedure_NTU[index,3]
      
      diffProcedure <- as.numeric(difftime(diseaseDate, as.Date(ProcedureDate), tz, units = c("days")))
      
      if(diffProcedure <= 30 && diffProcedure > 0){
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
        
        if(length(ind_Surgery_NTU)>0)
          Surgery_NTU <- TRUE
        if(length(ind_EndotrachealIntubation)>0)
          EndotrachealIntubation_NTU <- TRUE
        if(length(ind_ExtracorporealMembraneOxygenation)>0)
          ExtracorporealMembraneOxygenation_NTU <- TRUE
        if(length(ind_Hemodialysis)>0)
          Hemodialysis_NTU <- TRUE
        if(length(ind_IndwellingCentralBenousCatheters)>0)
          IndwellingCentralBenousCatheter_NTU <- TRUE
        if(length(ind_IndwellingUrinaryCatheter)>0)
          IndwellingUrinaryCatheter_NTU <- TRUE
        if(length(ind_IntravascularCatheter)>0)
          IntravascularCatheter_NTU <- TRUE
        if(length(ind_MechanicValve)>0)
          MechanicValve_NTU <- TRUE
        if(length(ind_MechanicalVentilation)>0)
          MechanicalVentilation_NTU <- TRUE
        if(length(ind_Pacemaker)>0)
          Pacemaker_NTU <- TRUE
        if(length(ind_PleuralDrainage)>0)
          PleuralDrainage_NTU <- TRUE
        if(length(ind_ProsthesisJoint)>0)
          ProsthesisJoint_NTU <- TRUE
        if(length(ind_SolidOrganTransplantation)>0)
          SolidOrganTransplantation_NTU <- TRUE
        if(length(ind_TotalParenteralNutrition)>0)
          TotalParenteralNutrition_NTU <- TRUE
        
      }
      
    }
  }
  
  
  newRow<-data.frame(chartNO,case,control,candidaPersistent,candidaPolymicrobial,bactPersistent,bactPolymicrobial,history,
                     Gender,Age,emergencyNow,emergencyDataNow,hospitalizationNow,hospitalizationDay,hospitalizationDaythisTime,hospitalizationDataNow,
                     outPatientClinicNow,outPatientClinicDataNow,transferHospital,ICUStay,ICUStayDays,ICUStayData,priCandida,
                     priBacteria,CCI1,CCI2,CCIData1,CCIData2,Colonization,Drug1_Antibacterial,Drug2_Antifungal,
                     Drug3_Chemotherapy,Drug4_Gastric,Drug5_Immunosuppressive,Drug6_Monoclonal,Drug7_Steroid,Drug8_TPN,
                     Drug1_Antibacterial_DOT1,Drug2_Antifungal_DOT1,Drug3_Chemotherapy_DOT1,Drug4_Gastric_DOT1,
                     Drug5_Immunosuppressive_DOT1,Drug6_Monoclonal_DOT1,Drug7_Steroid_DOT1,Drug8_TPN_DOT1,
                     Drug1_Antibacterial_DOT2,Drug2_Antifungal_DOT2,Drug3_Chemotherapy_DOT2,Drug4_Gastric_DOT2,
                     Drug5_Immunosuppressive_DOT2,Drug6_Monoclonal_DOT2,Drug7_Steroid_DOT2,Drug8_TPN_DOT2,
                     AcutePancreatitis,GastrointestinalPerforation,Malnutrition_CCI,Stroke,EndotrachealIntubation,
                     ExtracorporealMembraneOxygenation,Hemodialysis,IndwellingCentralBenousCatheters,
                     IndwellingUrinaryCatheter,IntravascularCatheter,MechanicValve,MechanicalVentilation,
                     Pacemaker,PleuralDrainage,ProsthesisJoint,SolidOrganTransplantation,TotalParenteralNutrition,
                     Malnutrition_Albumin,EndotrachealIntubation_NTU,
                     ExtracorporealMembraneOxygenation_NTU,Hemodialysis_NTU,IndwellingCentralBenousCatheters_NTU,
                     IndwellingUrinaryCatheter_NTU,IntravascularCatheter_NTU,MechanicValve_NTU,MechanicalVentilation_NTU,
                     Pacemaker_NTU,PleuralDrainage_NTU,ProsthesisJoint_NTU,SolidOrganTransplantation_NTU,TotalParenteralNutrition_NTU,
                     Surgery_NTU,stringsAsFactors=FALSE)
  
  names(newRow)<-c("chartNO","case","control","candidaPersistent","candidaPolymicrobial","bactPersistent","bactPolymicrobial"
                   ,"history","gender","age","emergency","emergencyData","hospitalization","hospitalizationDay","hospitalizationDaythisTime",
                   "hospitalizationData","outPatientClinic","outPatientClinicData","transferHospital","ICUStay",
                   "ICUStayDays","ICUStayData","priCandida","priBacteria","CCI1","CCI2","CCIData1","CCIData2",
                   "Colonization","Drug1_Antibacterial","Drug2_Antifungal","Drug3_Chemotherapy","Drug4_Gastric",
                   "Drug5_Immunosuppressive","Drug6_Monoclonal","Drug7_Steroid","Drug8_TPN",
                   "Drug1_Antibacterial_DOT1","Drug2_Antifungal_DOT1","Drug3_Chemotherapy_DOT1","Drug4_Gastric_DOT1",
                   "Drug5_Immunosuppressive_DOT1","Drug6_Monoclonal_DOT1","Drug7_Steroid_DOT1","Drug8_TPN_DOT1",
                   "Drug1_Antibacterial_DOT2","Drug2_Antifungal_DOT2","Drug3_Chemotherapy_DOT2","Drug4_Gastric_DOT2",
                   "Drug5_Immunosuppressive_DOT2","Drug6_Monoclonal_DOT2","Drug7_Steroid_DOT2","Drug8_TPN_DOT2",
                   "AcutePancreatitis","GastrointestinalPerforation","Malnutrition_CCI","Stroke","EndotrachealIntubation",
                   "ExtracorporealMembraneOxygenation","Hemodialysis","IndwellingCentralBenousCatheters",
                   "IndwellingUrinaryCatheter","IntravascularCatheter","MechanicValve","MechanicalVentilation",
                   "Pacemaker","PleuralDrainage","ProsthesisJoint","SolidOrganTransplantation","TotalParenteralNutrition",
                   "Malnutrition_Albumin","EndotrachealIntubation_NTU",
                   "ExtracorporealMembraneOxygenation_NTU","Hemodialysis_NTU","IndwellingCentralBenousCatheters_NTU",
                   "IndwellingUrinaryCatheter_NTU","IntravascularCatheter_NTU","MechanicValve_NTU","MechanicalVentilation_NTU",
                   "Pacemaker_NTU","PleuralDrainage_NTU","ProsthesisJoint_NTU","SolidOrganTransplantation_NTU","TotalParenteralNutrition_NTU",
                   "Surgery_NTU")
  
  labelProcessed <- rbind(labelProcessed, newRow)
}
write.table(labelProcessed, file = "Feature.csv", sep = ",",row.names = FALSE)

