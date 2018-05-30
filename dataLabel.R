#Remove all variables
rm(list=ls(all=TRUE))
options(warn=-1)
cat("\014")  
#It is the Input table
labelOrigin<-read.table("RowData/06©À¯]µßµß¦å¯g¥v_µß¦å¯g¥v.txt",sep="\t",header=TRUE);
#It is the Processed table
#ACCOUNTIDSE2, BACTCODE, SAMPLINGDATECHAR, INDATE, EMGACCOUNTIDSE2
labelProcessed <- data.frame(chartNO=integer(),
                             case=logical(),
                             control=logical(),
                             candidaPersistent=logical(),
                             candidaPolymicrobial=logical(),
                             bactPersistent=logical(),
                             bactPolymicrobial=logical(),
                             history=character(),
                             ACCOUNTIDSE2=character(),
                             BACTCODE=character(),
                             SAMPLINGDATECHAR=character(),
                             INDATE=character(),
                             EMGACCOUNTIDSE2=character(),
                             stringsAsFactors=FALSE)

#Read all data in
for (i in 1:nrow(labelOrigin)) {
  if(i%%500==0)
    print(i)
  chartNO <- as.character(labelOrigin[i,1])
  bactCode <- as.character(labelOrigin[i,12])
  stringDate <- as.character(labelOrigin[i,15])
  ACCOUNTIDSE2 <- as.character(labelOrigin[i,22])
  BACTCODE <- as.character(labelOrigin[i,12])
  SAMPLINGDATECHAR <- as.character(labelOrigin[i,15])
  INDATE <- as.character(labelOrigin[i,23])
  EMGACCOUNTIDSE2 <- as.character(labelOrigin[i,26])
  samplingTime <- as.POSIXct(as.character(labelOrigin[i,15]),format='%Y%m%d %H:%M')
  #print(yearThreshold)
  
  if(substr(stringDate, 1, 4) == "2016" || substr(stringDate, 1, 4) == "2017" ){
  newEvent = FALSE
    
  #Candida
  if (grepl("candida", bactCode, ignore.case = TRUE)){
    Bact <- FALSE
    Candida <- TRUE
  }
  #Bacteria
  else{
    Bact <- TRUE
    Candida <- FALSE
  }
  

  #Check if it is already in the dataframe or not?
  #Add First Patient!
  if(nrow (labelProcessed)==0){
    
    newRow<-data.frame(chartNO,Candida,Bact,Candida,FALSE,Bact,FALSE,paste(bactCode,stringDate, sep = ";"),
                       ACCOUNTIDSE2,BACTCODE,SAMPLINGDATECHAR,INDATE,EMGACCOUNTIDSE2,stringsAsFactors=FALSE)
    names(newRow)<-c("chartNO","case","control","candidaPersistent","candidaPolymicrobial","bactPersistent","bactPolymicrobial",
                     "history","ACCOUNTIDSE2","BACTCODE","SAMPLINGDATECHAR","INDATE","EMGACCOUNTIDSE2")
    
    labelProcessed <- rbind(labelProcessed, newRow)
    
  }
  else {
    #Update Patient
    if (chartNO %in% labelProcessed$chartNO){
      #Get old data
      #ind <- max(match(personNO, labelProcessed$personNO))
      ind_array <- which(!is.na(match(labelProcessed$chartNO, chartNO)))
      ind <- max(ind_array)
      
      oldPatientData <- as.matrix(labelProcessed[ind,])
      
      oldCID <- as.character(oldPatientData[1])
      oldCase <- as.logical(oldPatientData[2])
      oldControl <- as.logical(oldPatientData[3])
      oldCandidaPersistent <- as.logical(oldPatientData[4])
      oldCandidaPolymicrobial <- as.logical(oldPatientData[5])
      oldBactPersistent <- as.logical(oldPatientData[6])
      oldBactPolymicrobial <- as.logical(oldPatientData[7])
      oldHistory <- as.character(oldPatientData[8])
      #print(oldHistory)
      
      newCID <- oldCID
      newCase <- oldCase || Candida
      newControl <- oldControl || Bact
      newCandidaPersistent <- oldCandidaPersistent
      newCandidaPolymicrobial <- oldCandidaPolymicrobial
      newBactPersistent <- oldBactPersistent
      newBactPolymicrobial <- oldBactPolymicrobial
      newHistory <- oldHistory
      
      #print(oldHistory)
      
      
      #Retrieve histroy from data 
      oldHistoryArray <- strsplit(as.character(oldHistory),";")
      oldHistoryArray <- do.call(rbind, oldHistoryArray)
      
      
      #Polymicrobial or not?
      if(Bact){
        for(j in seq(1, ncol(oldHistoryArray), by = 2)){
          disease <- oldHistoryArray[1,j]
          time <- as.POSIXct(as.character(oldHistoryArray[1,j+1]),format='%Y%m%d %H:%M')
          timediff <- abs(difftime(time, samplingTime, tz,units = c("days")))
          if(disease != bactCode && timediff<2 && !grepl("candida", disease, ignore.case = TRUE)){
            newBactPolymicrobial = TRUE
          }
        }
      }
      if(Candida){
        for(j in seq(1, ncol(oldHistoryArray), by = 2)){
          disease <- oldHistoryArray[1,j]
          time <- as.POSIXct(as.character(oldHistoryArray[1,j+1]),format='%Y%m%d %H:%M')
          timediff <- abs(difftime(time, samplingTime, tz,units = c("days")))
          if(disease != bactCode && timediff<2 && grepl("candida", disease, ignore.case = TRUE)){
            newCandidaPolymicrobial = TRUE
          }
        }
      }
      
      #Persistent or not?
      if(Bact){
        for(j in seq(1, ncol(oldHistoryArray), by = 2)){
          disease <- oldHistoryArray[1,j]
          #print(oldHistoryArray)
          time <- as.POSIXct(as.character(oldHistoryArray[1,j+1]),format='%Y%m%d %H:%M')
          timediff <- abs(difftime(time, samplingTime, tz,units = c("days")))
          if(disease == bactCode && timediff>30 ){
            newBactPersistent = FALSE
          }
        }
      }
      if(Candida){
        for(j in seq(1, ncol(oldHistoryArray), by = 2)){
          disease <- oldHistoryArray[1,j]
          #print(oldHistoryArray)
          time <- as.POSIXct(as.character(oldHistoryArray[1,j+1]),format='%Y%m%d %H:%M')
          timediff <- abs(difftime(time, samplingTime, tz,units = c("days")))
          if(disease == bactCode && timediff>30){
            newCandidaPersistent = FALSE
          }
        }
      }
      
      #newEvent or not
      for(j in seq(1, ncol(oldHistoryArray), by = 2)){
        disease <- oldHistoryArray[1,j]
        time <- as.POSIXct(as.character(oldHistoryArray[1,j+1]),format='%Y%m%d %H:%M')
        timediff <- abs(difftime(time, samplingTime, tz,units = c("days")))
        if(disease == bactCode && timediff>30){
          newEvent = TRUE
        }
        if(disease != bactCode && timediff>2){
          newEvent = TRUE
        }
      }
      
      #Check previous IF <30 day, not recorded
      flag <- TRUE
      for(index in ind_array){
        oldPatientData <- as.matrix(labelProcessed[index,])
        oldHistory <- as.character(oldPatientData[8])
        oldHistoryArray <- strsplit(as.character(oldHistory),";")
        oldHistoryArray <- do.call(rbind, oldHistoryArray)
        for(j in seq(1, ncol(oldHistoryArray), by = 2)){
          disease <- oldHistoryArray[1,j]
          time <- as.POSIXct(as.character(oldHistoryArray[1,j+1]),format='%Y%m%d %H:%M')
          timediff <- abs(difftime(time, samplingTime, tz,units = c("days")))
          if(disease == bactCode && timediff<30){
            newEvent <- FALSE
            flag <- FALSE
          }
          if(disease != bactCode && timediff<2){
            newEvent = FALSE
          }
        }
      }
      
      
      #Add to History
      #flag <- TRUE
      #for(j in seq(1, ncol(oldHistoryArray), by = 2)){
      #  disease <- oldHistoryArray[1,j]
      #  time <- as.POSIXct(as.character(oldHistoryArray[1,j+1]),format='%Y%m%d %H:%M')
      #  timediff <- abs(difftime(time, samplingTime, tz,units = c("days")))
      #  if(bactCode==disease && timediff<30){
      #    flag <- FALSE
      #  }
      #}
      if(flag & !newEvent){
        newHistory <- paste(newHistory,bactCode,stringDate, sep = ";")
      }
      
      if (!newEvent){
        #Update Final Result
        newPatientData = data.frame(newCID,newCase,newControl,newCandidaPersistent,newCandidaPolymicrobial,newBactPersistent,newBactPolymicrobial,newHistory,
                                    ACCOUNTIDSE2,BACTCODE,SAMPLINGDATECHAR,INDATE,EMGACCOUNTIDSE2,stringsAsFactors=FALSE)
        names(newPatientData)<-c("chartNO","case","control","candidaPersistent","candidaPolymicrobial","bactPersistent","bactPolymicrobial",
                                 "history","ACCOUNTIDSE2","BACTCODE","SAMPLINGDATECHAR","INDATE","EMGACCOUNTIDSE2")
        labelProcessed[ind,] <- newPatientData
      }
      else{
        #Update Final Result
        newPatientData = data.frame(newCID,oldCase,oldControl,newCandidaPersistent,oldCandidaPolymicrobial,newBactPersistent,oldBactPolymicrobial,newHistory,
                                    ACCOUNTIDSE2,BACTCODE,SAMPLINGDATECHAR,INDATE,EMGACCOUNTIDSE2,stringsAsFactors=FALSE)
        names(newPatientData)<-c("chartNO","case","control","candidaPersistent","candidaPolymicrobial","bactPersistent","bactPolymicrobial",
                                 "history","ACCOUNTIDSE2","BACTCODE","SAMPLINGDATECHAR","INDATE","EMGACCOUNTIDSE2")
        labelProcessed[ind,] <- newPatientData
        
        #new Event!!!
        newCID <- oldCID
        newCase <- Candida
        newControl <- Bact
        newCandidaPersistent <- Candida
        newCandidaPolymicrobial <- FALSE
        newBactPersistent <- Bact
        newBactPolymicrobial <- FALSE
        newHistory <- paste(bactCode,stringDate, sep = ";")
        
        newPatientData <- data.frame(newCID,newCase,newControl,newCandidaPersistent,newCandidaPolymicrobial,newBactPersistent,newBactPolymicrobial,newHistory,
                                     ACCOUNTIDSE2,BACTCODE,SAMPLINGDATECHAR,INDATE,EMGACCOUNTIDSE2,stringsAsFactors=FALSE)
        names(newPatientData) <- c("chartNO","case","control","candidaPersistent","candidaPolymicrobial","bactPersistent","bactPolymicrobial",
                                   "history","ACCOUNTIDSE2","BACTCODE","SAMPLINGDATECHAR","INDATE","EMGACCOUNTIDSE2")
        labelProcessed <- rbind(labelProcessed, newPatientData)
      }
    }
    #Add New Patient
    else{
      if((Candida==TRUE && Bact==FALSE)||(Candida==FALSE && Bact==TRUE)){
      newRow<-data.frame(chartNO,Candida,Bact,Candida,FALSE,Bact,FALSE,paste(bactCode,stringDate, sep = ";"),
                         ACCOUNTIDSE2,BACTCODE,SAMPLINGDATECHAR,INDATE,EMGACCOUNTIDSE2,stringsAsFactors=FALSE)
      names(newRow)<-c("chartNO","case","control","candidaPersistent","candidaPolymicrobial","bactPersistent","bactPolymicrobial",
                       "history","ACCOUNTIDSE2","BACTCODE","SAMPLINGDATECHAR","INDATE","EMGACCOUNTIDSE2")
      labelProcessed <- rbind(labelProcessed, newRow)
      }
    }
  }
  
  #Insert into Dataset
  }
}
#existingDF = rbind(existingDF,newrow)
NCase <- 0
NControl <- 0
NCase_Control <- 0
for (ind in 1:nrow(labelProcessed)){
  Data <- as.matrix(labelProcessed[ind,])
  if(Data[2]==TRUE && Data[3] == FALSE)
    NCase <- NCase + 1
  if(Data[3]==TRUE && Data[2] == FALSE)
    NControl <- NControl + 1
  if(Data[2]==TRUE && Data[3]==TRUE)
    NCase_Control <- NCase_Control + 1
}
Data[0]
print(NCase)
print(NControl)
print(NCase_Control)
write.table(labelProcessed, file = "EventData.csv", sep = ",",row.names = FALSE)