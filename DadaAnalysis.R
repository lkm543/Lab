#It is the Input table
Data<-read.table("Feature.csv",sep=",",header=TRUE)

N_Control <- 0
N_Case <- 0
N_Control_Day <- 0
N_Case_Day <- 0

for (i in 1:nrow(Data)){
  #Hispitalization Day
  if(Data[i,]$case==TRUE && Data[i,]$control==FALSE){
    N_Case <- N_Case+1
    N_Case_Day <- N_Case_Day + Data[i,]$hospitalizationDay
  }
  if(Data[i,]$case==FALSE && Data[i,]$control==TRUE){
    N_Control <- N_Control+1
    N_Control_Day <- N_Control_Day + Data[i,]$hospitalizationDay
  }
}
N_Control_Day/N_Control
N_Case_Day/N_Case

mat<-matrix(0, nrow=4, ncol=17)
rownames(mat) = c('CasePositve','CaseNegative','ControlPositve','ControlNegative')
for (i in 1:nrow(Data)){
  #Hispitalization Day
  for(j in 36:52){
    if(Data[i,]$case){
      if(Data[i,j]){
        mat[1,j-35] <- mat[1,j-35] + 1
      }
      else{
        mat[2,j-35] <- mat[2,j-35] + 1
        
      }
    }
    if(Data[i,]$control){
      if(Data[i,j]){
        mat[3,j-35] <- mat[3,j-35] + 1
      }
      else{
        mat[4,j-35] <- mat[4,j-35] + 1
      }
    }
  }
}

mat2<-matrix(0, nrow=17, ncol=2)
for(k in 1:17){
  mat2[k,1] <- (mat[1,k]/(mat[1,k]+mat[2,k]))
  mat2[k,2] <- (mat[3,k]/(mat[3,k]+mat[4,k]))
}
