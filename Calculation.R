#It is the Input table
Data<-read.table("Feature.csv",sep=",",header=TRUE)


ICU_Case_Positive <- 0
ICU_Case_Negative <- 0
ICU_Control_Positive <- 0
ICU_Control_Negative <- 0


for (i in 1:nrow(Data)){
  if(Data[i,2]){
    if(Data[i,19])
      ICU_Case_Positive <- ICU_Case_Positive+1
    else
      ICU_Case_Negative <- ICU_Case_Negative+1
  }
  else{
    if(Data[i,19])
      ICU_Control_Positive <- ICU_Control_Positive+1
    else
      ICU_Control_Negative <- ICU_Control_Negative+1
  }
}

print(ICU_Case_Positive/(ICU_Case_Positive+ICU_Case_Negative))
print(ICU_Control_Positive/(ICU_Control_Positive+ICU_Control_Negative))