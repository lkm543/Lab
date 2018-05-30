ICD10ToCCI <- setClass(
  "ICD10ToCCI",
  slots = c(
    Dataset = list(ICD = "character" ,CCI = "numeric",disease = "character", Catagory = "numeric"),
    TestData_Origin = list(Name = "character",ICD = "character"),
    TestData_Final = list(Name = "character", ICDTF = "logical" , CataTF = "logical" , CCI = "numeric" )
    TestData1 = list(CHARTNO2 = "character",DIAGNOSISCODE = "character",DIAGNOSISENGNAME = "character",CREATEDATETIME = "character"),
    TestData2 = list(CHARTNO2 = "character",DIAGNOSISCODE = "character",DIAGNOSISENGNAME = "character",CREATEDATETIME = "character")
  )
)


setGeneric(name="DataProcess",
           def=function(theObject)
           {
             standardGeneric("DataProcess")
           }
)

setMethod(f="DataProcess",
          signature="ICD10ToCCI",
          definition=function(theObject)
          {
            ##########function declaration##########
            ConvertToCCI <- function(theObject2,ICD10){
              OriginICD = toString(ICD10);

              ICD = OriginICD;
              test_result = is.na(pmatch(theObject2@Dataset.ICD,ICD));
              test_result2 = is.na(pmatch(ICD,theObject2@Dataset.ICD));
              if(!all(test_result)){
                # Calculate CCI,for it is in the transfer table
                Index = match(FALSE,test_result);
                CCI_Code = theObject2@Dataset.CCI[Index];
              }
              else if (!test_result2){
                Index = pmatch(ICD,theObject@Dataset.ICD);
                CCI_Code = theObject2@Dataset.CCI[Index];
              }
              else{
                # No need to calculate CCI,for it is not in the transfer table
                CCI_Code = 0 ;
              }
              return (CCI_Code);
            }

            AddPatient <- function(theObject3,AName,AICD10){

              pos = regexpr(pattern='\\.',AICD10);
              NewICD = AICD10;
              PatientCCI = ConvertToCCI(theObject3,NewICD);

              theObject3@TestData_Final.Name <- c(theObject3@TestData_Final.Name,AName);
              theObject3@TestData_Final.CCI <- c(theObject3@TestData_Final.CCI,PatientCCI);

              newrow = c(logical(length = length(theObject3@Dataset.ICD)));


              test_result = is.na(pmatch(theObject3@Dataset.ICD,NewICD));
              test_result2 = is.na(pmatch(NewICD,theObject3@Dataset.ICD));

              Catanewrow = c(logical(length = theObject3@Dataset.Catagory[length(theObject3@Dataset.ICD)]));

              if(!all(test_result)){
                # Calculate CCI,for it is in the transfer table
                Index = match(FALSE,test_result);
                CCI_Code = theObject3@Dataset.CCI[Index];
                CataIndex = theObject3@Dataset.Catagory[Index];
                newrow[Index] <- TRUE;
                Catanewrow[CataIndex] <- TRUE;
              }
              else if (!test_result2){
                Index = pmatch(NewICD,theObject3@Dataset.ICD);
                CCI_Code = theObject3@Dataset.CCI[Index];
                CataIndex = theObject3@Dataset.Catagory[Index];
                newrow[Index] <- TRUE;
                Catanewrow[CataIndex] <- TRUE;
              }
              else{
                CCI_Code = 0;
                # print(NewICD);
              }



              #1ST row
              if(length(theObject3@TestData_Final.Name)==0){
                theObject3@TestData_Final.ICDTF <- newrow;
                theObject3@TestData_Final.CataTF <- Catanewrow;
              }
              else{
                theObject3@TestData_Final.ICDTF <- c(theObject3@TestData_Final.ICDTF, newrow);
                theObject3@TestData_Final.CataTF <- c(theObject3@TestData_Final.CataTF, Catanewrow);
              }
              validObject(theObject3);
              return(theObject3);
            }

            UpdatePatient <- function(theObject4,AName,AICD10_2){
                 NewICD = AICD10_2;

                 test_result = is.na(pmatch(theObject4@Dataset.ICD,NewICD));
                 test_result2 = is.na(pmatch(NewICD,theObject4@Dataset.ICD));

                 Index_Name = match(AName,theObject4@TestData_Final.Name)-1;
                 if(!all(test_result)){
                   # Calculate CCI,for it is in the transfer table
                   Index = match(FALSE,test_result);
                   CCI_Code = theObject4@Dataset.CCI[Index];
                   CataIndex = theObject4@Dataset.Catagory[Index];
                   if(!theObject4@TestData_Final.CataTF[Index_Name*theObject4@Dataset.Catagory[length(theObject4@Dataset.ICD)]+CataIndex]){
                     theObject4@TestData_Final.CataTF[Index_Name*theObject4@Dataset.Catagory[length(theObject4@Dataset.ICD)]+CataIndex] <- TRUE;
                     theObject4@TestData_Final.CCI[Index_Name+1] <- as.numeric(theObject4@TestData_Final.CCI[Index_Name+1] + ConvertToCCI(theObject4,NewICD));
                   }
                 }
                 else if (!test_result2){
                   Index = pmatch(NewICD,theObject4@Dataset.ICD);
                   CCI_Code = theObject4@Dataset.CCI[Index];
                   CataIndex = theObject4@Dataset.Catagory[Index];
                   if(!theObject4@TestData_Final.CataTF[Index_Name*theObject4@Dataset.Catagory[length(theObject4@Dataset.ICD)]+CataIndex]){
                     theObject4@TestData_Final.CataTF[Index_Name*theObject4@Dataset.Catagory[length(theObject4@Dataset.ICD)]+CataIndex] <- TRUE;
                    theObject4@TestData_Final.CCI[Index_Name+1] <- as.numeric(theObject4@TestData_Final.CCI[Index_Name+1] + ConvertToCCI(theObject4,NewICD));
                   }
                  }
              validObject(theObject4);
              return(theObject4);
            }

            ##########function declaration##########

            for(i in 1:length(theObject@TestData_Origin.Name))
            {
              PatientName = theObject@TestData_Origin.Name[i];
              PatientICD10 = theObject@TestData_Origin.ICD[i];
              #Add new Patient
              if(is.na(match(PatientName,theObject@TestData_Final.Name))){
                theObject <- AddPatient(theObject,PatientName,PatientICD10);
              }
              #Patient Exists
              else
                theObject <- UpdatePatient(theObject,PatientName,PatientICD10);
            }
            validObject(theObject);
            return(theObject);
          }
)


setGeneric(name="LoadDataset",
           def=function(theObject,filename)
           {
             standardGeneric("LoadDataset")
           }
)

setMethod(f="LoadDataset",
          signature="ICD10ToCCI",
          definition=function(theObject,filename)
          {
            temp <- read.table(filename,sep=",",header=TRUE);
            temp = t(temp);
            #It is the transfer table
            theObject@Dataset.CCI <- as.numeric(temp[1,]);
            theObject@Dataset.ICD <- temp[2,];
            theObject@Dataset.disease <- temp[3,];
            theObject@Dataset.Catagory <- as.numeric(temp[4,]);
            validObject(theObject);
            return(theObject);
          }
)

setGeneric(name="LoadTestData1",
           def=function(theObject,filename)
           {
             standardGeneric("LoadTestData1")
           }
)

setMethod(f="LoadTestData1",
          signature="ICD10ToCCI",
          definition=function(theObject,TestData)
          {
            theObject@TestData1.CHARTNO2 <- as.character(TestData[1,]);
            theObject@TestData1.DIAGNOSISCODE <- as.character(TestData[2,]);
            theObject@TestData1.DIAGNOSISENGNAME <- as.character(TestData[3,]);
            theObject@TestData1.CREATEDATETIME <- as.character(TestData[4,]);
            validObject(theObject);
            return(theObject);
          }
)



setGeneric(name="WriteToFile",
           def=function(theObject,filename)
           {
             standardGeneric("WriteToFile")
           }
)

setMethod(f="WriteToFile",
          signature="ICD10ToCCI",
          definition=function(theObject,filename)
          {
            name = theObject@TestData_Final.Name;
            CCI = theObject@TestData_Final.CCI;
            cata = theObject@TestData_Final.CataTF;
            cata_len = theObject@Dataset.Catagory[length(theObject@Dataset.ICD)];
            dim(cata) = c(cata_len,length(name));
            cata = t(cata);
            output_cata <- vector(mode="character", length=length(name))
            dim(output_cata)=c(length(name),1);
            # print(cata);
            for (i in 1:length(name)){
              for(j in 1:cata_len){
                if(cata[i,j]==TRUE)
                {
                  # print(j);
                  Index = match(j,theObject@Dataset.Catagory);
                  # print(Index);
                  output_cata[i] = paste(output_cata[i],theObject@Dataset.disease[Index],'(',theObject@Dataset.CCI[Index],')',';');
                }
              }
            }

            # print(cata)
            O = cbind(name,CCI,output_cata);
            # print(O);
            write.table(O, file = filename , sep = ",",row.names=FALSE);
          }
)

setClass(Class="Result",
         representation(
           CCI="integer",
           CCIData="character"
         )
)

setGeneric(name="getCCI",
           def=function(theObject)
           {
             standardGeneric("getCCI")
           }
)

setMethod(f="getCCI",
          signature="ICD10ToCCI",
          definition=function(theObject)
          {
            #DataProcess(theObject);
            
            
            return(new("Result",
                       CCI=CCI,
                       CCIData=CCIData))
          }
)


