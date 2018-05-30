#Remove all variables
cat("\014")  
rm(list=ls(all=TRUE))
#It is the Input table
Data<-read.table("Feature.csv",sep=",",header=TRUE)

#Candida True/False, Bacteria True/False , True Percentage*2, p
Analysis <- matrix(0, nrow = 6, ncol = ncol(Data))

for(i in 1:ncol(Data)){
  if(mode(Data[[i]])=='logical'&& !is.na(Data[1, i]))
  {
    for(j in 1:nrow(Data)){
      if(xor(Data[j,2], Data[j,3]))
      if(Data[j,2]){
        if(Data[j,i]){
          Analysis[1,i] <- Analysis[1,i] + 1;
        }
        else{
          Analysis[2,i] <- Analysis[2,i] + 1;
        }
      }
      else{
        if(Data[j,i]){
          Analysis[3,i] <- Analysis[3,i] + 1;
        }
        else{
          Analysis[4,i] <- Analysis[4,i] + 1;
        }
      }
        
    }
  }  
}


for(i in 1:ncol(Data)){
  if(sum(Analysis[,i])>0)
  {
    Analysis[5,i] <- Analysis[1,i]/(Analysis[1,i] + Analysis[2,i])*100;
    Analysis[6,i] <- Analysis[3,i]/(Analysis[3,i] + Analysis[4,i])*100;
  }  
}

write.table(Analysis, file = "Analysis.csv", sep = ",",row.names = FALSE)