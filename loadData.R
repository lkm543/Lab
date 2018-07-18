loadData <- function(x) {
  
  #####Load Raw Data####
  labelOrigin<<-read.table("EventData.csv",sep=",",header=TRUE)
  ind <- which(with( labelOrigin, case==TRUE & control==TRUE ))
  labelOrigin <<- labelOrigin[ -ind, ]
  
  basicInformationOrigin<<-read.table("RowData/01性別出生日期.csv",sep=",",header=TRUE)
  outPatientClinicOrigin<<-read.table("RowData/02門診史.txt",sep="\t",header=TRUE)
  hospitalizationOrigin<<-read.table("RowData/03住院史.csv",sep=",",header=TRUE)
  emergencyOrigin<<-read.table("RowData/04急診史.csv",sep=",",header=TRUE)
  transferHospitalOrigin<<-read.table("RowData/05轉院註記.csv",sep=",",header=TRUE)
  ICUStayOrigin<<-read.table("RowData/07轉床紀錄.csv",sep=",",header=TRUE)
  ColonizationOrigin<<-read.csv("RowData/06-1candida colonization.csv",sep=",", header=TRUE)
  
  print("Load basicinfo,outPatientClinic,hospitalization,emergency,ICUStay,transferHospital,Colonization successfully!!!")
  
  CCI_1_Origin<<-read.table("RowData/10-1門診臨床診斷.txt",sep="\t",header=TRUE,quote = "", fill=TRUE)
  #CCI_3_Origin<<-read.csv("RowData/10-2住院臨床診斷.txt",sep="\t", header=TRUE)
  CCI_2_Origin<<-read.csv("RowData/10-3住院病歷室疾分診斷.csv",sep=",", header=TRUE)
  #CCI_3_Origin<<-read.csv("RowData/10-4急診診斷.csv",sep=",", header=TRUE)
  #Only for hospitalization
  CCI_4_Origin<<-read.csv("RowData/10-2住院臨床診斷.txt",sep="\t", header=TRUE)
  ColonizationOrigin<<-read.csv("RowData/06-1candida colonization.csv",sep=",", header=TRUE)
  CCIStandard <<- read.table("RowData/ICD_Standard.csv",sep=",",header=TRUE);
  #Diagnosis for drugs
  Diagnosis_1 <<- read.table("RowData/09-1門診處方醫令(clear).txt",quote = "",sep="\t",header=TRUE, fill = TRUE);
  Diagnosis_4 <<- read.table("RowData/09-4住院護理給藥紀錄.txt",sep="\t",header=TRUE, fill = TRUE,quote = "");
  Diagnosis_7 <<- read.table("RowData/09-7急診護理給藥紀錄.txt",sep="\t",header=TRUE, fill = TRUE,quote = "");
  
  Diagnosis_1_TPN <<- read.table("RowData/09-1門診處方醫令_TPN(clear).csv",sep=",",header=TRUE)
  Diagnosis_4_TPN <<- read.table("RowData/09-4住院護理給藥紀錄_TPN.txt",sep="\t",header=TRUE, fill = TRUE,quote = "");
  Diagnosis_7_TPN <<- read.table("RowData/09-7急診護理給藥紀錄_TPN.csv",sep=",",header=TRUE)
  
  Diagnosis_DOT_1 <<- subset(Diagnosis_1, select=c("CHARTNO2","DRUGCODE", "START_DATE", "END_DATE"),stringsAsFactors=FALSE)
  Diagnosis_1 <<- subset(Diagnosis_1, select=c("CHARTNO2","DRUGCODE", "END_DATE"),stringsAsFactors=FALSE)
  Diagnosis_4 <<- subset(Diagnosis_4, select=c("CHARTNO2","DRUGCODE", "GIVEDRUGDATETIME"),stringsAsFactors=FALSE)
  Diagnosis_7 <<- subset(Diagnosis_7, select=c("CHARTNO2","DRUGCODE", "GIVEDRUGDATETIME"),stringsAsFactors=FALSE)
  
  colnames(Diagnosis_1) <<- c("CHARTNO2", "DRUGCODE", "DATE")
  Diagnosis_1[3] <<- lapply(Diagnosis_1[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
  colnames(Diagnosis_4) <<- c("CHARTNO2", "DRUGCODE", "DATE")
  Diagnosis_4[3] <<- lapply(Diagnosis_4[3],function(x){strsplit(as.character(x)," ")[[1]][1]})
  Diagnosis_4[3] <<- lapply(Diagnosis_4[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
  colnames(Diagnosis_7) <<- c("CHARTNO2", "DRUGCODE", "DATE")
  Diagnosis_7[3] <<- lapply(Diagnosis_7[3],function(x){strsplit(as.character(x)," ")[[1]][1]})
  Diagnosis_7[3] <<- lapply(Diagnosis_7[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
  
  Diagnosis <<- rbind(Diagnosis_1,Diagnosis_4,Diagnosis_7);
  Diagnosis_DOT_2 <<- rbind(Diagnosis_4,Diagnosis_7);
  
  Diagnosis_DOT_1_TPN <<- subset(Diagnosis_1_TPN, select=c("CHARTNO2","DRUGCODE", "START_DATE", "END_DATE"),stringsAsFactors=FALSE)
  Diagnosis_1_TPN <<- subset(Diagnosis_1_TPN, select=c("CHARTNO2","DRUGCODE", "END_DATE"),stringsAsFactors=FALSE)
  Diagnosis_4_TPN <<- subset(Diagnosis_4_TPN, select=c("CHARTNO2","DRUGCODE", "GIVEDRUGDATETIME"),stringsAsFactors=FALSE)
  Diagnosis_7_TPN <<- subset(Diagnosis_7_TPN, select=c("CHARTNO2","DRUGCODE", "GIVEDRUGDATETIME"),stringsAsFactors=FALSE)
  
  colnames(Diagnosis_1_TPN) <<- c("CHARTNO2", "DRUGCODE", "DATE")
  Diagnosis_1_TPN[3] <<- lapply(Diagnosis_1_TPN[3],function(x){as.POSIXct(as.character(x),format='%m/%d/%Y')})
  colnames(Diagnosis_4_TPN) <<- c("CHARTNO2", "DRUGCODE", "DATE")
  Diagnosis_4_TPN[3] <<- lapply(Diagnosis_4_TPN[3],function(x){strsplit(as.character(x)," ")[[1]][1]})
  Diagnosis_4_TPN[3] <<- lapply(Diagnosis_4_TPN[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
  colnames(Diagnosis_7_TPN) <<- c("CHARTNO2", "DRUGCODE", "DATE")
  Diagnosis_7_TPN[3] <<- lapply(Diagnosis_7_TPN[3],function(x){strsplit(as.character(x)," ")[[1]][1]})
  Diagnosis_7_TPN[3] <<- lapply(Diagnosis_7_TPN[3],function(x){as.POSIXct(as.character(x),format='%m/%d/%Y')})
  
  Diagnosis_TPN <<- rbind(Diagnosis_1_TPN,Diagnosis_4_TPN,Diagnosis_7_TPN);
  Diagnosis_DOT_2_TPN <<- rbind(Diagnosis_4_TPN,Diagnosis_7_TPN);
  
  print("Load CCI&Diagnosis successfully!!!")
  
  #DRUGs Catalogs
  Drug_1_Origin<<-read.csv("RowData/DRUG2.0/Antibacterial agents.csv",sep=",", header=TRUE)
  Drug_1_Origin <<- subset(Drug_1_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
  Drug_1_Origin <<- cbind(Drug_1_Origin,index = 1)
  Drug_2_Origin<<-read.csv("RowData/DRUG2.0/Antifungal agents.csv",sep=",", header=TRUE)
  Drug_2_Origin <<- subset(Drug_2_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
  Drug_2_Origin <<- cbind(Drug_2_Origin,index = 2)
  Drug_3_Origin<<-read.csv("RowData/DRUG2.0/Chemotherapy.csv",sep=",", header=TRUE)
  Drug_3_Origin <<- subset(Drug_3_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
  Drug_3_Origin <<- cbind(Drug_3_Origin,index = 3)
  Drug_4_Origin<<-read.csv("RowData/DRUG2.0/Gastric Acid-Suppressive Medications.csv",sep=",", header=TRUE)
  Drug_4_Origin <<- subset(Drug_4_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
  Drug_4_Origin <<- cbind(Drug_4_Origin,index = 4)
  Drug_5_Origin<<-read.csv("RowData/DRUG2.0/Immunosuppressive agents.csv",sep=",", header=TRUE)
  Drug_5_Origin <<- subset(Drug_5_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
  Drug_5_Origin <<- cbind(Drug_5_Origin,index = 5)
  Drug_6_Origin<<-read.csv("RowData/DRUG2.0/Monoclonal agents.csv",sep=",", header=TRUE)
  Drug_6_Origin <<- subset(Drug_6_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
  Drug_6_Origin <<- cbind(Drug_6_Origin,index = 6)
  Drug_7_Origin<<-read.csv("RowData/DRUG2.0/Systemic steroid.csv",sep=",", header=TRUE)
  Drug_7_Origin <<- subset(Drug_7_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
  Drug_7_Origin <<- cbind(Drug_7_Origin,index = 7)
  Drug_8_Origin<<-read.csv("RowData/DRUG2.0/TPN_DRUGCODE.csv",sep=",", header=TRUE)
  Drug_8_Origin <<- subset(Drug_8_Origin, select=c("DRUGCODE"),stringsAsFactors=FALSE)
  Drug_8_Origin <<- cbind(Drug_8_Origin,index = 8)
  
  
  Drug_Origin <<- rbind(Drug_1_Origin,Drug_2_Origin,Drug_3_Origin,Drug_4_Origin,Drug_5_Origin,Drug_6_Origin,Drug_7_Origin,Drug_8_Origin);
  
  print("Load Drug successfully!!!")
  
  #Diagnosis for Health Care
  Diagnosis_HealthCare_1 <<- read.table("RowData/10-1門診臨床診斷.txt",sep="\t",header=TRUE, fill = TRUE,stringsAsFactors=FALSE,quote = "");
  Diagnosis_HealthCare_2 <<-read.csv("RowData/10-2住院臨床診斷.txt",sep="\t", header=TRUE,stringsAsFactors=FALSE);
  Diagnosis_HealthCare_3 <<-read.csv("RowData/10-3住院病歷室疾分診斷.csv",sep=",", header=TRUE,stringsAsFactors=FALSE);
  
  Diagnosis_HealthCare_1 <<- subset(Diagnosis_HealthCare_1, select=c("CHARTNO2","DIAGNOSISCODE", "CREATEDATETIME"),stringsAsFactors=FALSE)
  Diagnosis_HealthCare_2 <<- subset(Diagnosis_HealthCare_2, select=c("CHARTNO2","DIAGNOSISCODE", "CREATEDATETIME"),stringsAsFactors=FALSE)
  
  Diagnosis_HealthCare_1[3] <<- lapply(Diagnosis_HealthCare_1[3],function(x){as.Date(as.POSIXct(x,format='%Y/%m/%d'))})
  Diagnosis_HealthCare_2[3] <<- lapply(Diagnosis_HealthCare_2[3],function(x){as.POSIXct(as.character(x),format="%Y/%m/%d")})
  
  #Many!!
  Diagnosis_HealthCare_3_Temp <<- data.frame(CHARTNO2 = c(), DIAGNOSISCODE = c(),CREATEDATETIME= c())
  for (i in 1:nrow(Diagnosis_HealthCare_3)){
    #print(i)
    chart <- Diagnosis_HealthCare_3[i,1]
    time <- Diagnosis_HealthCare_3[i,25]
    time <- as.POSIXct(as.character(time),format='%m/%d/%Y')
    #print(i)
    index <- 0
    for (j in 1:20){
      if (Diagnosis_HealthCare_3[i,j+4]=='NULL'){
        index <- j
        break
      }
    }
    Diagnosis_HealthCare_3_Temp2 <<- data.frame(CHARTNO2 = chart, DIAGNOSISCODE = t(Diagnosis_HealthCare_3[i,5:(4+index-1)]),CREATEDATETIME= time)
    #print(Diagnosis_HealthCare_3_Temp2)
    names(Diagnosis_HealthCare_3_Temp2)<<-c("CHARTNO2","DIAGNOSISCODE", "CREATEDATETIME")
    #print(Diagnosis_HealthCare_3_Temp2)
    Diagnosis_HealthCare_3_Temp <<- rbind(Diagnosis_HealthCare_3_Temp,Diagnosis_HealthCare_3_Temp2)
    
  }
  #names(Diagnosis_HealthCare_3_Temp)<<-c("CHARTNO2","DIAGNOSISCODE", "CREATEDATETIME")
  rownames(Diagnosis_HealthCare_3_Temp) <<- NULL
  Diagnosis_HealthCare <<- rbind(Diagnosis_HealthCare_1,Diagnosis_HealthCare_2,Diagnosis_HealthCare_3_Temp)
  
  print("Load HealthCare successfully!!!")
  
  #Diagnosis for Procedure
  Diagnosis_Procedure_1 <<-read.csv("RowData/8-3-1門診醫療處置_醫師建檔.csv",sep=",", header=TRUE,stringsAsFactors=FALSE);
  Diagnosis_Procedure_2 <<-read.csv("RowData/8-3-2門診醫療處置_疾分人員建檔.csv",sep=",", header=TRUE,stringsAsFactors=FALSE);
  Diagnosis_Procedure_3 <<-read.csv("RowData/8-4-1住院醫療處置_醫師建檔.csv",sep=",", header=TRUE);
  Diagnosis_Procedure_4 <<-read.csv("RowData/8-4-2住院醫療處置_疾分人員建檔.csv",sep=",", header=TRUE,stringsAsFactors=FALSE);
  
  Diagnosis_Procedure_NTU_1 <<-read.csv("RowData/8-1門診醫療處置_台大醫令碼.txt",sep="\t", header=TRUE,stringsAsFactors=FALSE,quote = "");
  Diagnosis_Procedure_NTU_2 <<-read.csv("RowData/8-2 住院醫療處置_台大醫令碼.txt",sep="\t", header=TRUE,stringsAsFactors=FALSE,quote = "");
  
  Diagnosis_Procedure_NTU_1 <<- subset(Diagnosis_Procedure_NTU_1, select=c("CHARTNO2","ORDERCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
  Diagnosis_Procedure_NTU_2 <<- subset(Diagnosis_Procedure_NTU_2, select=c("CHARTNO2","ORDERCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
  
  Diagnosis_Procedure_NTU_1[3] <<- lapply(Diagnosis_Procedure_NTU_1[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
  Diagnosis_Procedure_NTU_2[3] <<- lapply(Diagnosis_Procedure_NTU_2[3],function(x){as.POSIXct(as.character(x),format='%Y/%m/%d')})
  Diagnosis_Procedure_NTU <<- rbind(Diagnosis_Procedure_NTU_1,Diagnosis_Procedure_NTU_2)
  
  Diagnosis_Procedure_1 <<- subset(Diagnosis_Procedure_1, select=c("CHARTNO2","OPERATIONCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
  Diagnosis_Procedure_1[3] <<- lapply(Diagnosis_Procedure_1[3],function(x){as.POSIXct(as.character(x),format='%m/%d/%Y')})
  names(Diagnosis_Procedure_1) <<- c("CHARTNO2","OPERATIONCODE", "CREATEDATETIME")
  
  #Diagnosis_Procedure_2 Many!! 
  #data.frame(CHARTNO2 = c(), DIAGNOSISCODE = c(),CREATEDATETIME= c())
  Diagnosis_Procedure_2_Temp <<- data.frame(CHARTNO2 = c(), OPERATIONCODE = c(),CREATEDATETIME= c())
  for (i in 1:nrow(Diagnosis_Procedure_2)){
    chart <- Diagnosis_Procedure_2[i,1]
    time <- Diagnosis_Procedure_2[i,12]
    time <- as.POSIXct(as.character(time),format='%m/%d/%Y')
    #print(i)
    index <- 0
    for (j in 1:3){
      if (Diagnosis_Procedure_2[i,j+7]=='NULL'){
        index <- j
        break
      }
    }
    Diagnosis_Procedure_2_Temp2 <<- data.frame(CHARTNO2 = chart, DIAGNOSISCODE = t(Diagnosis_Procedure_2[i,8:(7+index-1)]),CREATEDATETIME= time)
    names(Diagnosis_Procedure_2_Temp2)<<-c("CHARTNO2","OPERATIONCODE", "CREATEDATETIME")
    rownames(Diagnosis_Procedure_2_Temp2) <<- c()
    Diagnosis_Procedure_2_Temp <<- rbind(Diagnosis_Procedure_2_Temp,Diagnosis_Procedure_2_Temp2)
    
  }
  
  
  Diagnosis_Procedure_3 <<- subset(Diagnosis_Procedure_3, select=c("CHARTNO2","OPERATIONCODE", "COMPLETEDATETIME"),stringsAsFactors=FALSE)
  Diagnosis_Procedure_3[3] <<- lapply(Diagnosis_Procedure_3[3],function(x){as.POSIXct(as.character(x),format='%m/%d/%Y')})
  names(Diagnosis_Procedure_3) <<- c("CHARTNO2","OPERATIONCODE", "CREATEDATETIME")
  
  #Diagnosis_Procedure_4 Many!!
  Diagnosis_Procedure_4_Temp <<- data.frame(CHARTNO2 = c(), OPERATIONCODE = c(),CREATEDATETIME= c())
  for (i in 1:nrow(Diagnosis_Procedure_4)){
    chart <- Diagnosis_Procedure_4[i,1]
    time <- Diagnosis_Procedure_4[i,25]
    time <- as.POSIXct(as.character(time),format='%m/%d/%Y')
    #print(i)
    index <- 0
    for (j in 1:20){
      if (Diagnosis_Procedure_4[i,j+4]=='NULL'){
        index <- j
        break
      }
    }
    Diagnosis_Procedure_4_Temp2 <<- data.frame(CHARTNO2 = chart, DIAGNOSISCODE = t(Diagnosis_Procedure_4[i,5:(4+index-1)]),CREATEDATETIME= time)
    names(Diagnosis_Procedure_4_Temp2)<<-c("CHARTNO2","OPERATIONCODE", "CREATEDATETIME")
    Diagnosis_Procedure_4_Temp <<- rbind(Diagnosis_Procedure_4_Temp,Diagnosis_Procedure_4_Temp2)
  }
  rownames(Diagnosis_Procedure_4_Temp) <<- NULL
  Diagnosis_Procedure <<- rbind(Diagnosis_Procedure_1,Diagnosis_Procedure_2_Temp,Diagnosis_Procedure_3,Diagnosis_Procedure_4_Temp)
  
  print("Load Procedure successfully!!!")
  
  #Diagnosis_Procedure_2 <<- subset(Diagnosis_Procedure_2, select=c("CHARTNO2","OPERATIONCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
  #Diagnosis_Procedure_3 <<- subset(Diagnosis_Procedure_3, select=c("CHARTNO2","OPERATIONCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
  #Diagnosis_Procedure_4 <<- subset(Diagnosis_Procedure_4, select=c("CHARTNO2","OPERATIONCODE", "EXECUTECOMPLETEDATETIME"),stringsAsFactors=FALSE)
  
  CCI_Albumin<<-read.csv("RowData/11ALB檢驗報告.csv",sep=",", header=TRUE)
  CCI_Albumin<<-subset(CCI_Albumin, select=c("CHARTNO2","CONFIRMRESULT", "REPORTDATETIME"),stringsAsFactors=FALSE)
  
  
  #Diagnosis for Healthcare
  CCI_AcutePancreatitis<<-read.csv("RowData/Diagnosis/Acute pancreatitis.csv",sep=",", header=TRUE)
  CCI_GastrointestinalPerforation<<-read.csv("RowData/Diagnosis/Gastrointestinal perforation.csv",sep=",", header=TRUE)
  CCI_Malnutrition<<-read.csv("RowData/Diagnosis/Malnutrition.csv",sep=",", header=TRUE)
  CCI_Stroke<<-read.csv("RowData/Diagnosis/stroke.csv",sep=",", header=TRUE)
  #Diagnosis for Healthcare
  CCI_EndotrachealIntubation<<-read.csv("RowData/Diagnosis/Endotracheal intubation.csv",sep=",", header=TRUE)
  CCI_ExtracorporealMembraneOxygenation<<-read.csv("RowData/Diagnosis/Extracorporeal Membrane Oxygenation.csv",sep=",", header=TRUE)
  CCI_Hemodialysis<<-read.csv("RowData/Diagnosis/Hemodialysis.csv",sep=",", header=TRUE)
  CCI_IndwellingCentralBenousCatheters<<-read.csv("RowData/Diagnosis/Indwelling central venous catheters.csv",sep=",", header=TRUE)
  CCI_IndwellingUrinaryCatheter<<-read.csv("RowData/Diagnosis/Indwelling urinary catheter.csv",sep=",", header=TRUE)
  CCI_IntravascularCatheter<<-read.csv("RowData/Diagnosis/Intravascular catheter.csv",sep=",", header=TRUE)
  CCI_MechanicValve<<-read.csv("RowData/Diagnosis/Mechanic valve.csv",sep=",", header=TRUE)
  CCI_MechanicalVentilation<<-read.csv("RowData/Diagnosis/mechanical ventilation.csv",sep=",", header=TRUE)
  CCI_Pacemaker<<-read.csv("RowData/Diagnosis/Pacemaker.csv",sep=",", header=TRUE)
  CCI_PleuralDrainage<<-read.csv("RowData/Diagnosis/Pleural drainage.csv",sep=",", header=TRUE)
  CCI_ProsthesisJoint<<-read.csv("RowData/Diagnosis/Prosthesis joint.csv",sep=",", header=TRUE)
  CCI_SolidOrganTransplantation<<-read.csv("RowData/Diagnosis/Solid-organ transplantation.csv",sep=",", header=TRUE)
  CCI_TotalParenteralNutrition<<-read.csv("RowData/Diagnosis/Total parenteral nutrition.csv",sep=",", header=TRUE)
  
  
  #Diagnosis for Healthcare NTU
  CCI_EndotrachealIntubation_NTU<<-read.csv("RowData/台大醫令碼/Endotracheal intubation.csv",sep=",", header=TRUE)
  CCI_ExtracorporealMembraneOxygenation_NTU<<-read.csv("RowData/台大醫令碼/Extracorporeal Membrane Oxygenation.csv",sep=",", header=TRUE)
  CCI_Hemodialysis_NTU<<-read.csv("RowData/台大醫令碼/Hemodialysis.csv",sep=",", header=TRUE)
  CCI_IndwellingCentralBenousCatheters_NTU<<-read.csv("RowData/台大醫令碼/Indwelling central venous catheter.csv",sep=",", header=TRUE)
  CCI_IndwellingUrinaryCatheter_NTU<<-read.csv("RowData/台大醫令碼/Indwelling urinary catheter.csv",sep=",", header=TRUE)
  CCI_IntravascularCatheter_NTU<<-read.csv("RowData/台大醫令碼/Intravascular catheter.csv",sep=",", header=TRUE)
  CCI_MechanicValve_NTU<<-read.csv("RowData/台大醫令碼/Mechanic valve.csv",sep=",", header=TRUE)
  CCI_MechanicalVentilation_NTU<<-read.csv("RowData/台大醫令碼/Mechanical ventilation.csv",sep=",", header=TRUE)
  CCI_Pacemaker_NTU<<-read.csv("RowData/台大醫令碼/Pacemaker.csv",sep=",", header=TRUE)
  CCI_PleuralDrainage_NTU<<-read.csv("RowData/台大醫令碼/Pleural drainage.csv",sep=",", header=TRUE)
  CCI_ProsthesisJoint_NTU<<-read.csv("RowData/台大醫令碼/Prosthesis joint.csv",sep=",", header=TRUE)
  CCI_SolidOrganTransplantation_NTU<<-read.csv("RowData/台大醫令碼/Solid-organ transplantation.csv",sep=",", header=TRUE)
  CCI_TotalParenteralNutrition_NTU<<-read.csv("RowData/台大醫令碼/Total parenteral nutrition.csv",sep=",", header=TRUE)
  CCI_Surgery_NTU<<-read.csv("RowData/台大醫令碼/Surgery.csv",sep=",", header=TRUE)
  
  print("Load CCI successfully!!!")
  save.image("Lab.RData")
  #load("Lab.RData")
}