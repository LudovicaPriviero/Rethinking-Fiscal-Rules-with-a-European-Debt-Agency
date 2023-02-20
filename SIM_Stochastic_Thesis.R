# clear memory 
rm(list=ls())
#Check the working directory before importing else provide full path
#setwd(path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# packages used


listofpackages <- c("dygraphs", "dplyr","ellipse","reshape2","ggplot2","xts","readxl","xlsx","stats",
                    "plotly", "hrbrthemes", "zoo", "dynlm")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}


##### N.B. DEFICIT IN THE DATA  is (t-g)

##### DATABASE IN PANEL FORMAT: 

MSList <- read.table("data/MSList2.csv",header = T,sep = ";",dec = ".", stringsAsFactors = FALSE)
DB <- read_excel("data/DB.xlsx")
FwdDB <- read_excel("data/Forward_DB.xlsx")
RecYears <- read_excel("data/RecessionYears.xlsx")
SwapMonthlyData <- read.table("data/SwapMonthlyData.csv",header = T,sep = ";",dec = ".", stringsAsFactors = FALSE) 
TotalDataBYMS <- read.table("data/TotalDataByMS_2021.csv",header = T,sep = ";",dec = ".", stringsAsFactors = FALSE) 
TTC <- read.table("data/TTC SOV.csv",header = F,sep = ";",dec = ".")
MES <- read_excel("data/MES.xlsx")

## -----------------------------------------------------------------------------
## Get the data in .xts format for annual time series 
## -----------------------------------------------------------------------------

dates <-seq(as.Date("1995-12-15"),length=27, by="years")
dates2<-seq(as.Date("2000-12-15"),length=22, by="years")
dates3<-seq(as.Date("2021-12-15"),length=30, by="years")
dates4<- seq(as.Date("2019-12-15"),length=32, by="years")
dates5<- seq(as.Date("2001-01-15"),length=257, by="months")
dates6 <-seq(as.Date("2001-01-15"),length=252, by="months")
dates7 <-seq(as.Date("2021-12-15"),length=21, by="years")

## -----------------------------------------------------------------------------
## Databanking
## -----------------------------------------------------------------------------


TestFirst <- 0
for (MS in MSList[MSList$Exclude=="N",1])
{
  DB_ByMS <-DB[DB$MS==MS,]
  DB_ByMS<-as.data.frame(DB_ByMS[, -c(1,2)])
  tsdata <- as.xts(DB_ByMS, order.by = dates)
  
  IC_ByMS <- TotalDataBYMS[TotalDataBYMS$MS==MS,]
  IC_ByMS<-as.data.frame(IC_ByMS[,-c(1:7,9)])
  tsdata1 <- as.xts(IC_ByMS, order.by=dates6)
  tsdata.yearly1 = to.yearly(tsdata1[, 1])[,4]
  
  tsdata = cbind(tsdata,tsdata.yearly1)
  
  names = c("GDP", "Debt", "PrimaryDeficit", "Deficit", "SFA", "Growth","IC")
  NameByMS <-MSList[MSList$MS==MS,4]
  j=1
  for (i in names) {
    names[j] = paste(NameByMS,i, sep="_")
    j=j+1 
  }
  colnames(tsdata)=names
  if (TestFirst==0) 
  {
    MyData <- tsdata
    TestFirst <- 1
    
  } else {
    MyData <- cbind(MyData, tsdata)
  } 
}
rm(tsdata, DB_ByMS)


SwapMonthlyData<-as.data.frame(SwapMonthlyData[,-c(1:2)])
SwapMonthly <- as.xts(SwapMonthlyData, order.by=dates5)
tsdata.yearly_IR = to.yearly(SwapMonthly[, 1])[,4]
colnames(tsdata.yearly_IR) <- c("IR")
Swap_annual<- tsdata.yearly_IR[-22,]


TestFirst <- 0
for (MS in MSList[MSList$Simulation=="Y",1])
{
  DB_ByMS <-FwdDB[FwdDB$MS==MS,]
  DB_ByMS<-as.data.frame(DB_ByMS[, -c(1,2)])
  tsdata <- as.xts(DB_ByMS, order.by = dates4)
  
  names = c("GDP", "AvgCost", "Growth")
  NameByMS <-MSList[MSList$MS==MS,4]
  j=1
  for (i in names) {
    names[j] = paste(NameByMS,i, sep="_")
    j=j+1 
  }
  colnames(tsdata)=names
  if (TestFirst==0) 
  {
    FwdData <- tsdata
    TestFirst <- 1
    
  } else {
    FwdData <- cbind(FwdData, tsdata)
  } 
}

FwdData<- FwdData[c(-1,-2),]

FwdData$IR <- 0.01
FwdData[1,"IR"]<-Swap_annual[21,]

rm(tsdata, DB_ByMS, tsdata.yearly_IR, IC_ByMS, SwapMonthlyData,tsdata.yearly1, tsdata1)
## -----------------------------------------------------------------------------
## Calculate the Average Cost of Debt: 
## -----------------------------------------------------------------------------

TestFirst <- 0
for (Name in MSList[MSList$Exclude=="N",4])
{
  tsdata<- as.xts(array(data = NA, dim = 27), order.by=dates)
  t1<-nrow(tsdata)
  colnames(tsdata)<- c("CM")
  for (i in 2:t1) {
    tsdata[i, "CM"][[1]]= (- MyData[i,paste(Name, "Deficit", sep="_")][[1]] + 
                             + MyData[i,paste(Name, "PrimaryDeficit", sep="_")][[1]]) /MyData[i-1,paste(Name, "Debt", sep="_")][[1]]
  }
  names = c("CM")
  j=1
  for (i in names) {
    names[j] = paste(Name,i, sep="_")
    j=j+1 
  }
  colnames(tsdata)=names
  if (TestFirst==0) 
  {
    AvgCost <- tsdata
    TestFirst <- 1
    
  } else {
    AvgCost <- cbind(AvgCost, tsdata)
  } 
}
rm(tsdata)


MyData<- cbind(MyData, AvgCost)
MyData <- MyData[,sort(names(MyData))]

MyData <- cbind(MyData, Swap_annual)

#-----------------------------------
# generating SFA for Ireland

SFA<- as.xts(array(data = NA, dim = 27), order.by=dates)
t1<-nrow(SFA)
Name <- "IE"
for (i in 2:t1) {
  SFA[i,][[1]]= MyData[i,paste(Name, "Deficit", sep="_")][[1]] + 
    + (MyData[i,paste(Name, "Debt", sep="_")][[1]] - MyData[i-1,paste(Name, "Debt", sep="_")][[1]])
}

MyData$IE_SFA<- SFA

rm(SFA)


## -----------------------------------------------------------------------------
## Simulation - Within Sample
## -----------------------------------------------------------------------------


TestFirst <- 0
for (Name in MSList[MSList$Exclude=="N",4])
{
  tsdata<- as.xts(array(data = NA, dim = 22), order.by=dates2)
  t1<-nrow(tsdata)
  colnames(tsdata)<- c("B")
  tsdata[1, c("B")] <- MyData[6,paste(Name, "Debt", sep="_")][[1]]
  tsdata$B_Cov <- 0
  tsdata$B_Slow <- 0
  tsdata$B_Fast <- 0
  tsdata$bCov<- 0
  tsdata$bS<- 0
  tsdata$bF<- 0
  tsdata$b<- 0 
  dummy <- ifelse((MyData[,paste(Name, "PrimaryDeficit", sep="_")]+MyData[,paste(Name, "SFA", sep="_")]) < 0, 1, 0)
  
  for (i in 2:t1) {
    tsdata[i, "B"][[1]] = tsdata[i-1, "B"][[1]]*(1 + MyData[5+i,paste(Name, "CM", sep="_")][[1]])+
      - MyData[5+i,paste(Name, "PrimaryDeficit", sep="_")][[1]] + MyData[5+i,paste(Name, "SFA", sep="_")][[1]]
    
    tsdata[i, "B_Cov"][[1]] = tsdata[i-1, "B_Cov"][[1]]*(1 + MyData[5+i,paste(Name, "CM", sep="_")][[1]])+
      - dummy[5+i,]*(MyData[5+i,paste(Name, "PrimaryDeficit", sep="_")][[1]]+ MyData[5+i,paste(Name, "SFA", sep="_")][[1]])*RecYears[5+i,3][[1]]
    
    tsdata[i, "B_Slow"][[1]] = tsdata[i-1, "B_Slow"][[1]]*(1 + MyData[5+i,paste(Name, "CM", sep="_")][[1]])+
      - dummy[5+i,]*(MyData[5+i,paste(Name, "PrimaryDeficit", sep="_")][[1]] +
                       + MyData[5+i,paste(Name, "SFA", sep="_")][[1]] )*RecYears[5+i,2][[1]]
    
    tsdata[i, "B_Fast"][[1]] = tsdata[i, "B"][[1]]- tsdata[i, "B_Slow"][[1]] -tsdata[i, "B_Cov"][[1]]
    
    tsdata[i, "bCov"][[1]] = tsdata[i, "B_Cov"][[1]]/MyData[5+i,paste(Name, "GDP", sep="_")][[1]]
    
    tsdata[i, "bS"][[1]] = tsdata[i, "B_Slow"][[1]]/MyData[5+i,paste(Name, "GDP", sep="_")][[1]]
    
    tsdata[i, "bF"][[1]] = tsdata[i, "B_Fast"][[1]]/MyData[5+i,paste(Name, "GDP", sep="_")][[1]]
    
    tsdata[i, "b"][[1]] = tsdata[i, "B"][[1]]/MyData[5+i,paste(Name, "GDP", sep="_")][[1]]
    
  }
  names = c("B", "B_Cov","B_Slow", "B_Fast", "bCov", 
            "bS", "bF", "b")
  j=1
  for (i in names) {
    names[j] = paste(Name,i, sep="_")
    j=j+1 
  }
  colnames(tsdata)=names
  if (TestFirst==0) 
  {
    SimDebt1 <- tsdata
    TestFirst <- 1
    
  } else {
    SimDebt1 <- cbind(SimDebt1, tsdata)
  } 
}
rm(tsdata, dummy )

## -----------------------------------------------------------------------------
## Regression - coefficients' estimate
## -----------------------------------------------------------------------------

### Average Cost regression
Coeff_AvgC<-matrix(0,4,6)
Resid_AvgC<- matrix(0,20,6)
List_AvgC=list()
i=1
for (Name in MSList[MSList$Simulation=="Y",4])
{
  y1 <- stats::lag(MyData[,paste(Name, "CM", sep="_")])
  y3 <- stats::lag(MyData$IR)
  List_AvgC[[i]] = lm(MyData[,paste(Name, "CM", sep="_")] ~ y1 + MyData$IR + y3)
  
  Coeff_AvgC[,i]=as.vector(summary(List_AvgC[[i]])$coef[,1])
  Resid_AvgC[,i]= as.numeric(List_AvgC[[i]]$resid)
  i=1+i
}

name <- MSList[MSList$Simulation=="Y",4]
colnames(Coeff_AvgC)<- name
colnames(Resid_AvgC) <- name

Resid_AvgC <- as.data.frame(Resid_AvgC)
Coeff_AvgC <- as.data.frame(Coeff_AvgC)

rm(y1,y3,Swap_annual,SwapMonthly)

### Growth regression
Coeff_G<-matrix(0,2,6)
Resid_G<- matrix(0,25,6)
List_G=list()
i=1
for (Name in MSList[MSList$Simulation=="Y",4])
{
  y1 <- stats::lag(MyData[,paste(Name, "Growth", sep="_")])
  List_G[[i]] = lm(MyData[,paste(Name, "Growth", sep="_")] ~ y1)
  
  Coeff_G[,i]=as.vector(summary(List_G[[i]])$coef[,1])
  Resid_G[,i]= as.numeric(List_G[[i]]$resid)
  i=1+i
}

name <- MSList[MSList$Simulation=="Y",4]
colnames(Coeff_G)<- name
colnames(Resid_G) <- name

Resid_G <- as.data.frame(Resid_G)
Coeff_G <- as.data.frame(Coeff_G)

### Swap rate (EDA rate) regression

regr<- lm(MyData$IR ~ stats::lag(MyData$IR))
Resid_Swap <- as.data.frame(as.numeric(regr$resid))
Coeff_Swap <- as.data.frame(regr$coefficients)

rm(y1)
## -----------------------------------------------------------------------------
## Stochastic simulation - Out of Sample - Exogenous variables and debt cost 
## -----------------------------------------------------------------------------
#input to simulate swap rate (same for all countries)
swap_target <- 0.04 ### =(r*+inflation*) both set to be equal to 2%
#input to simulate growth rate (country specific, but long-run same here)
growth_target<- matrix(0.025,1,6)
colnames(growth_target)<- name

#hypotesis of a negative shock in the economy
shock_g <-as.xts(array(0, c(21, 6)), order.by=dates7)
colnames(shock_g) <- name
for (Name in MSList[MSList$Simulation=="Y",4]) {
  shock_g[c(5,6),Name]<- min(Resid_G[,Name])
}

#input to simulate idiomatic cost, Through the Cycle Transition matrix, TTC)
TTC <- as.matrix(TTC) #ncol=8, nrow=8)
D_TTC<-diag(eigen(TTC)$values)
S_TTC<-eigen(TTC)$vectors
S_TTC_Inv<-solve(S_TTC)

UnitMatrix <- diag(1,8,8)
unitVector<- as.vector(rep.int(1, 8))
v <- as.vector(c(0,0,0,0,0,0,0,1))
rr <- .30

Grade <- c("AAA","AA", "A","BBB", "BB","B","CCC", "CC", "C","" )
Position <- c(1,2,3,4,5,6,7,7,7,7)
Ratings <- data.frame(Grade, Position)
Rating <- matrix(c("AAA","A","AA","BB","BBB","BBB"),1,6)
colnames(Rating)<- name
#SET HERE THE NUMBER OF REPLICATIONS 
nrep <- 100

for (Name in MSList[MSList$Simulation=="Y",4])
{
  #Name <- "DE"
  a0 <- Coeff_AvgC[1, Name]
  a1<- Coeff_AvgC[2, Name]
  a2<- Coeff_AvgC[3, Name]
  a3<- Coeff_AvgC[4, Name]
  
  b1<- Coeff_Swap[2,]
  
  c1<- Coeff_G[2, Name]
  g_target<- growth_target[, Name][[1]]
 
  r_bt <-as.xts(array(NA, c(21, nrep)), order.by=dates7)
  r_bt[1,] <- MyData[27,paste(Name, "CM", sep="_")]
  IR_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  IR_bt[1,] <- MyData[27,"IR"]
  g_bt<- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  g_bt[1,] <- MyData[27,paste(Name, "Growth", sep="_")][[1]]
  IC <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  IC[1,] <- MyData[27,paste(Name, "IC", sep="_")][[1]]
  
  res1_bt <- array(0, c(21, nrep))
  res2_bt <- array(0, c(21, nrep))
  res3_bt <- array(0, c(21, nrep))
  
  
  
  for (i in 1:nrep){
    res1_bt[,i] <- sample(Resid_Swap[,1], size = 21, replace = T) 
    res2_bt[,i] <- sample(Resid_G[,Name], size = 21, replace = T) 
    res3_bt[,i] <- sample(Resid_AvgC[,Name], size = 21, replace = T)
    
    
    for (j in 2:21) {
      IR_bt[j,i][[1]] <- b1*IR_bt[j-1,i][[1]] + (1-b1)*swap_target + res1_bt[j,i]
      g_bt[j,i][[1]] <- c1*g_bt[j-1,i][[1]] + (1-c1)*g_target + res2_bt[j,i] #+ shock_g[j,Name]
      r_bt[j,i][[1]] <- a0 + a1*r_bt[j-1,i][[1]] +
        + a2*IR_bt[j,i][[1]] + a3*IR_bt[j-1,i][[1]]+ res3_bt[j,i]
      RF <- IR_bt[j,i][[1]]
      alpha <- 1/(1+RF)
      B <- D_TTC/(1+RF)
      Bfirst <- unitVector-diag(B)
      Bfirst <- B/Bfirst 
      GrossPerpetuity <- alpha/(1-alpha)*unitVector
      GrossPerpetuity <- GrossPerpetuity-S_TTC%*%Bfirst%*%S_TTC_Inv%*%v  ### eq. 8 
      
      D_TTCInv <- solve(D_TTC)
      temp <- (UnitMatrix-D_TTCInv)%*%Bfirst
      temp <- rr*S_TTC%*%temp%*%S_TTC_Inv%*%v ### eq. 9
      temp <- (unitVector- temp)
      NetPerpetuity <- GrossPerpetuity/temp ### eq. 10
      cf <- 1/NetPerpetuity ### eq. 13 annual fundamental pricing
      
      MSCurrentRating <- Rating[, Name][[1]]
      PriceRecord <- Ratings[Ratings$Grade==MSCurrentRating,2]
      IC[j,i][[1]] <- cf[PriceRecord]
   
      }
  }
     
  x = list(g_bt,r_bt,IC)
  
  names = c("g_bt","r_bt", "IC")
  # j=1
  # for (i in names) {
  #   names[j] = paste(Name,i, sep="_")
  #   j=j+1 
  # }
  names(x) <- names
  
  x = assign( paste("List",Name, sep="_") , x )
  
  rm(g_bt,r_bt,IC, x)
}
rm(B, Bfirst, cf, D_TTC, D_TTCInv, GrossPerpetuity, NetPerpetuity, Rating, Ratings,
   S_TTC, S_TTC_Inv, TTC, UnitMatrix, shock_g)

## -----------------------------------------------------------------------------
## Stochastic simulation - Out of Sample - SCENARIO 0 (No EDA)
## -----------------------------------------------------------------------------

###Set parameters: 
beta <- 0.05
gamma <- 0.02
delta <- 0.1

b <- 0.60

list_r_bt <- list( List_DE$r_bt, List_ES$r_bt, List_FR$r_bt, List_GR$r_bt, 
                   List_IT$r_bt,  List_PT$r_bt)
list_g_bt <- list( List_DE$g_bt, List_ES$g_bt, List_FR$g_bt, List_GR$g_bt, 
                   List_IT$g_bt,  List_PT$g_bt)
names(list_r_bt) <- name
names(list_g_bt) <- name
for (Name in MSList[MSList$Simulation=="Y",4])
{
  #Name <- "DE"
  
  
  r_bt <-list_r_bt[[Name]]
  g_bt <-list_g_bt[[Name]]
  b_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  b_bt[1,] <- SimDebt1[22,paste(Name, "B", sep="_")][[1]]/MyData[27,paste(Name, "GDP", sep="_")][[1]]
  bS_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  bS_bt[1,] <- SimDebt1[22,paste(Name, "bS", sep="_")][[1]] + SimDebt1[22,paste(Name, "bCov", sep="_")][[1]]
  bF_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  bF_bt[1,] <- SimDebt1[22,paste(Name, "bF", sep="_")][[1]]
  # bCov_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  # bCov_bt[1,] <- SimDebt1[22,paste(Name, "bCov", sep="_")][[1]]
  bT_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  bT_bt[1, ] <- b_bt[1,][[1]] - 10*beta*(bF_bt[1,][[1]]+
                                           - b*(bF_bt[1,][[1]]/(bF_bt[1, ][[1]]+bS_bt[1, i][[1]]))) +
    -10*gamma*(bS_bt[1,][[1]]- b*(1- (bF_bt[1, ][[1]]/(bF_bt[1, ][[1]]+bS_bt[1, ][[1]]))))
  PrDef_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  PrDef_bt[1,] <- MyData[27,paste(Name, "PrimaryDeficit", sep="_")][[1]]/MyData[27,paste(Name, "GDP", sep="_")][[1]]
  
  
  d_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  d_bt[1,] <- b_bt[1,] 
  
  for (i in 1:nrep){
    
    for (j in 2:21) {
        
        PrDef_bt[j, i][[1]] = b_bt[j-1, i][[1]]*((r_bt[j,i][[1]]+
                                                    - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) +
          + delta*(b_bt[j-1, i][[1]] -bT_bt[j-1, i][[1]])
        
        b_bt[j, i][[1]] = b_bt[j-1, i][[1]]* (1+ ((r_bt[j,i][[1]] +
                                                     - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))) +
          - PrDef_bt[j, i][[1]] 
        
        bS_bt[j, i][[1]] =  bS_bt[j-1, i][[1]]*((1+r_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) +
          - (1 - bF_bt[j-1, i][[1]]/b_bt[j-1, i][[1]])*(PrDef_bt[j, i][[1]])
        
        bF_bt[j, i][[1]] = b_bt[j, i][[1]] - bS_bt[j, i][[1]]
        
        bT_bt[j, i][[1]] <- b_bt[j, i][[1]] - 10*beta*(bF_bt[j, i][[1]]+
                                                         - b*(bF_bt[j, i][[1]]/(bF_bt[j, i][[1]]+bS_bt[j, i][[1]]))) +
          -10*gamma*(bS_bt[j, i][[1]]- b*(1- (bF_bt[j, i][[1]]/(bF_bt[j, i][[1]]+bS_bt[j, i][[1]]))))
        d_bt[j,i][[1]] = b_bt[j, i][[1]]
    
    }
  }
  x = list(d_bt,b_bt,bF_bt,bS_bt,bT_bt,g_bt,r_bt, PrDef_bt)
  
  names = c("d_bt","b_bt","bF_bt","bS_bt","bT_bt","g_bt","r_bt", "PrDef_bt")
  # j=1
  # for (i in names) {
  #   names[j] = paste(Name,i, sep="_")
  #   j=j+1 
  # }
  names(x) <- names
  
  x = assign( paste("List_S0",Name, sep="_") , x )
  
  rm(b_bt,bF_bt,bS_bt,bT_bt,g_bt,r_bt, PrDef_bt,x)
}

## -----------------------------------------------------------------------------
## Stochastic simulation - Out of Sample - SCENARIO 1 (Small EDA)
## -----------------------------------------------------------------------------


###Set parameters: 
beta <- 0.05
gamma <- 0.02
delta <- 0.1

b <- 0.60

list_r_bt <- list( List_DE$r_bt, List_ES$r_bt, List_FR$r_bt, List_GR$r_bt, 
                   List_IT$r_bt,  List_PT$r_bt)
list_g_bt <- list( List_DE$g_bt, List_ES$g_bt, List_FR$g_bt, List_GR$g_bt, 
                   List_IT$g_bt,  List_PT$g_bt)
names(list_r_bt) <- name
names(list_g_bt) <- name
for (Name in MSList[MSList$Simulation=="Y",4])
{
  #Name <- "DE"
  
  
  r_bt <-list_r_bt[[Name]]
  g_bt <-list_g_bt[[Name]]
  b_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  b_bt[1,] <- SimDebt1[22,paste(Name, "B", sep="_")][[1]]/MyData[27,paste(Name, "GDP", sep="_")][[1]]
  bS_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  bS_bt[1,] <- SimDebt1[22,paste(Name, "bS", sep="_")][[1]]
  bF_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  bF_bt[1,] <- SimDebt1[22,paste(Name, "bF", sep="_")][[1]]
  bCov_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  bCov_bt[1,] <- SimDebt1[22,paste(Name, "bCov", sep="_")][[1]]
  bT_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  bT_bt[1, ] <- b_bt[1,][[1]] - 10*beta*(bF_bt[1,][[1]]+
                                           - b*(bF_bt[1,][[1]]/(bF_bt[1, ][[1]]+bS_bt[1, i][[1]]))) +
    -10*gamma*(bS_bt[1,][[1]]- b*(1- (bF_bt[1, ][[1]]/(bF_bt[1, ][[1]]+bS_bt[1, ][[1]]))))
  PrDef_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  PrDef_bt[1,] <- MyData[27,paste(Name, "PrimaryDeficit", sep="_")][[1]]/MyData[27,paste(Name, "GDP", sep="_")][[1]]
  
  
  P_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  NL_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  L_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  b_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  Res_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  EL_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  TR_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  EN_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  
  P_EDA_bt[1,] <- 0
  NL_EDA_bt[1,] <- 0
  L_EDA_bt[1,] <- 0
  b_EDA_bt[1,] <- 0
  Res_EDA_bt[1,] <- 0
  EN_EDA_bt[1,] <- 0
  EL_EDA_bt[1,] <- 0
  TR_EDA_bt[1,] <- 0
  
  d_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  d_bt[1,] <- b_bt[1,] + L_EDA_bt[1,]
  
  dummy_rg<- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  
  
  for (i in 1:nrep){
    
    m = 4
    
    for (j in 2:21) {
      
      if (j==2) {
        P_EDA_bt[j,i][[1]]<-0
        NL_EDA_bt[j,i][[1]] <- 1/5*(1/(1+g_bt[j,i][[1]]))*bCov_bt[j-1,i][[1]] +
          + r_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*bCov_bt[j-1,i][[1]]
        L_EDA_bt[j,i][[1]] <- NL_EDA_bt[j,i][[1]]
        b_EDA_bt[j,i][[1]] <- NL_EDA_bt[j,i][[1]]
        Res_EDA_bt[j,i][[1]] <- 0 
        EL_EDA_bt[j,i][[1]] <- 0 
        EN_EDA_bt[j,i][[1]] <- 0 
        TR_EDA_bt[j,i][[1]] <-  Res_EDA_bt[j,i][[1]] + EN_EDA_bt[j,i][[1]]
        
        PrDef_bt[j, i][[1]] = b_bt[j-1, i][[1]]*((r_bt[j,i][[1]]+
                                                    - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) +
          + delta*(b_bt[j-1, i][[1]] -bT_bt[j-1, i][[1]])
        
        b_bt[j, i][[1]] = b_bt[j-1, i][[1]]* (1+ ((r_bt[j,i][[1]] +
                                                     - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))) +
          - PrDef_bt[j, i][[1]] +
          + P_EDA_bt[j,i][[1]] +
          - NL_EDA_bt[j,i][[1]]
        
        bCov_bt[j,i][[1]] = bCov_bt[j-1,i][[1]] - 1/5*1/(1+g_bt[j,i][[1]])*bCov_bt[j-1,i][[1]] +
          - g_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*bCov_bt[j-1,i][[1]]
        
        bS_bt[j, i][[1]] = bS_bt[j-1, i][[1]]* (1 + ((r_bt[j,i][[1]] +
                                                        - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))) +
          - (1 - bF_bt[j-1, i][[1]]/b_bt[j-1, i][[1]])*(PrDef_bt[j, i][[1]] + P_EDA_bt[j,i][[1]])
        
        bF_bt[j, i][[1]] = b_bt[j, i][[1]] - bS_bt[j, i][[1]] - bCov_bt[j, i][[1]]
        
        bT_bt[j, i][[1]] <- b_bt[j, i][[1]] - 10*beta*(bF_bt[j, i][[1]]+
                                                         - b*(bF_bt[j, i][[1]]/(bF_bt[j, i][[1]]+bS_bt[j, i][[1]]))) +
          -10*gamma*(bS_bt[j, i][[1]]- b*(1- (bF_bt[j, i][[1]]/(bF_bt[j, i][[1]]+bS_bt[j, i][[1]]))))
        d_bt[j,i][[1]] = b_bt[j, i][[1]] + L_EDA_bt[j, i][[1]]
        
      }
      
      else if ((j>2)&&(j<7)) {
        dummy_rg[j,i][[1]] <- ifelse(IR_bt[j,i][[1]] > g_bt[j,i][[1]], 1, 0)
        P_EDA_bt[j,i][[1]]<- dummy_rg[j,i][[1]]*(IR_bt[j,i][[1]] - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])*b_EDA_bt[j-1,i][[1]]
        NL_EDA_bt[j,i][[1]] <- 1/m * 1/(1+g_bt[j,i][[1]])*bCov_bt[j-1,i][[1]] +
          + r_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*bCov_bt[j-1,i][[1]]
        b_EDA_bt[j,i][[1]] <- b_EDA_bt[j-1,i][[1]]*(1+IR_bt[j,i][[1]])/(1+g_bt[j,i][[1]]) +
          + NL_EDA_bt[j,i][[1]]
        L_EDA_bt[j,i][[1]] <- L_EDA_bt[j-1,i][[1]]+
          - L_EDA_bt[j-1,i][[1]]* g_bt[j,i][[1]]/(1+g_bt[j,i][[1]])+
          + NL_EDA_bt[j,i][[1]]
        Res_EDA_bt[j,i][[1]] <- Res_EDA_bt[j-1,i][[1]]*(1+g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])+
          + P_EDA_bt[j,i][[1]]
        
        EL_EDA_bt[j,i][[1]] <- 0 
        EN_EDA_bt[j,i][[1]] <- 0 
        TR_EDA_bt[j,i][[1]] <-  Res_EDA_bt[j,i][[1]] + EN_EDA_bt[j,i][[1]]
        
        
        PrDef_bt[j, i][[1]] = b_bt[j-1, i][[1]]*((r_bt[j,i][[1]]+
                                                    - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) +
          + delta*(b_bt[j-1, i][[1]] -bT_bt[j-1, i][[1]])
        
        b_bt[j, i][[1]] = b_bt[j-1, i][[1]]* (1+ ((r_bt[j,i][[1]] +
                                                     - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))) +
          - PrDef_bt[j, i][[1]] +
          + P_EDA_bt[j,i][[1]] +
          - NL_EDA_bt[j,i][[1]]
        
        bCov_bt[j,i][[1]] = bCov_bt[j-1,i][[1]] - 1/m*1/(1+g_bt[j,i][[1]])*bCov_bt[j-1,i][[1]] +
          - g_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*bCov_bt[j-1,i][[1]]
        
        bS_bt[j, i][[1]] = bS_bt[j-1, i][[1]]* (1 + ((r_bt[j-1,i][[1]] +
                                                        - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))) +
          - (1 - bF_bt[j-1, i][[1]]/b_bt[j-1, i][[1]])*(PrDef_bt[j, i][[1]] + P_EDA_bt[j,i][[1]])
        
        bF_bt[j, i][[1]] = b_bt[j, i][[1]] - bS_bt[j, i][[1]] - bCov_bt[j, i][[1]]
        
        bT_bt[j, i][[1]] <- b_bt[j, i][[1]] - 10*beta*(bF_bt[j, i][[1]]+
                                                         - b*(bF_bt[j, i][[1]]/(bF_bt[j, i][[1]]+bS_bt[j, i][[1]]))) +
          -10*gamma*(bS_bt[j, i][[1]]- b*(1- (bF_bt[j, i][[1]]/(bF_bt[j, i][[1]]+bS_bt[j, i][[1]]))))
        d_bt[j,i][[1]] = b_bt[j, i][[1]] + L_EDA_bt[j, i][[1]]
        
        m= m+1
      }
      else {
        dummy_rg[j,i][[1]] <- ifelse( IR_bt[j,i][[1]] > g_bt[j,i][[1]], 1, 0)
        P_EDA_bt[j,i][[1]]<- dummy_rg[j,i][[1]]*(IR_bt[j,i][[1]] - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])*b_EDA_bt[j-1,i][[1]]
        NL_EDA_bt[j,i][[1]] <- 0
        b_EDA_bt[j,i][[1]] <- b_EDA_bt[j-1,i][[1]]*(1+IR_bt[j,i][[1]])/(1+g_bt[j,i][[1]])
        L_EDA_bt[j,i][[1]] <- L_EDA_bt[j-1,i][[1]]+
          - L_EDA_bt[j-1,i][[1]]* g_bt[j,i][[1]]/(1+g_bt[j,i][[1]])
        Res_EDA_bt[j,i][[1]] <- Res_EDA_bt[j-1,i][[1]]*(1+g_bt[j,i][[1]])/(1+g_bt[j,i][[1]]) +
          + P_EDA_bt[j,i][[1]]
        
        EL_EDA_bt[j,i][[1]] <- 0 
        EN_EDA_bt[j,i][[1]] <- 0 
        TR_EDA_bt[j,i][[1]] <-  Res_EDA_bt[j,i][[1]] + EN_EDA_bt[j,i][[1]]
        
        PrDef_bt[j, i][[1]] = b_bt[j-1, i][[1]]*((r_bt[j,i][[1]]+
                                                    - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) +
          + delta*(b_bt[j-1, i][[1]] -bT_bt[j-1, i][[1]])
        
        b_bt[j, i][[1]] = b_bt[j-1, i][[1]]* (1+ ((r_bt[j,i][[1]] +
                                                     - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))) +
          - PrDef_bt[j, i][[1]] +
          + P_EDA_bt[j,i][[1]]
        
        bCov_bt[j,i][[1]] = 0
        bS_bt[j, i][[1]] = bS_bt[j-1, i][[1]]* (1 + ((r_bt[j,i][[1]] +
                                                        - g_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))) +
          - (1 - bF_bt[j-1, i][[1]]/b_bt[j-1, i][[1]])*(PrDef_bt[j, i][[1]] + P_EDA_bt[j,i][[1]])
        
        bF_bt[j, i][[1]] = b_bt[j, i][[1]] - bS_bt[j, i][[1]] - bCov_bt[j, i][[1]]
        
        bT_bt[j, i][[1]] <- b_bt[j, i][[1]] - 10*beta*(bF_bt[j, i][[1]]+
                                                         - b*(bF_bt[j, i][[1]]/(bF_bt[j, i][[1]]+bS_bt[j, i][[1]]))) +
          -10*gamma*(bS_bt[j, i][[1]]- b*(1- (bF_bt[j, i][[1]]/(bF_bt[j, i][[1]]+bS_bt[j, i][[1]]))))
        d_bt[j,i][[1]] = b_bt[j, i][[1]] + L_EDA_bt[j, i][[1]]
      }
    }
  }
  x = list(d_bt,b_bt,bF_bt,bS_bt,bT_bt,bCov_bt,g_bt,r_bt, PrDef_bt,
           P_EDA_bt,NL_EDA_bt,L_EDA_bt,b_EDA_bt,Res_EDA_bt, EN_EDA_bt,
           EL_EDA_bt, TR_EDA_bt)
  
  names = c("d_bt","b_bt","bF_bt","bS_bt","bT_bt","bCov_bt","g_bt","r_bt", "PrDef_bt",
            "P_EDA_bt","NL_EDA_bt","L_EDA_bt","b_EDA_bt","Res_EDA_bt", "EN_EDA_bt",
            "EL_EDA_bt", "TR_EDA_bt")
  # j=1
  # for (i in names) {
  #   names[j] = paste(Name,i, sep="_")
  #   j=j+1 
  # }
  names(x) <- names
  
  x = assign( paste("List_S1",Name, sep="_") , x )
  
  rm(b_bt,bF_bt,bS_bt,bT_bt,bCov_bt,g_bt,r_bt, PrDef_bt,
     P_EDA_bt,NL_EDA_bt,L_EDA_bt,b_EDA_bt,Res_EDA_bt, EN_EDA_bt,
     EL_EDA_bt, TR_EDA_bt, x, dummy_rg)
}

## -----------------------------------------------------------------------------
## Stochastic simulation - Out of Sample - SCENARIO 2 (Large EDA)
## -----------------------------------------------------------------------------

###Set parameters: 
beta <- 0.05 
gamma <- 0.02
delta <- 0.1
b <- 0.60

###to replicate path 1:
# gamma_list <- array(c(0.042, 0.036, 0.042, 0.029, 0.044, 0.046), c(1,6))
# colnames(gamma_list)<- name

list_IC <- list( List_DE$IC, List_ES$IC, List_FR$IC, List_GR$IC, 
                   List_IT$IC,  List_PT$IC)
names(list_IC) <- name
for (Name in MSList[MSList$Simulation=="Y",4])
{
  # Name = "DE"
  r_bt <-list_r_bt[[Name]]
  g_bt <-list_g_bt[[Name]]
  IC <-list_IC[[Name]]
  
  # gamma<- gamma_list[[1, Name]]
  
  b_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  b_bt[1,] <- SimDebt1[22,paste(Name, "B", sep="_")][[1]]/MyData[27,paste(Name, "GDP", sep="_")][[1]]
  
  PrDef_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  PrDef_bt[1,] <- MyData[27,paste(Name, "PrimaryDeficit", sep="_")][[1]]/MyData[27,paste(Name, "GDP", sep="_")][[1]]
  #PrDef_bt <-list_temp[[Name]][["PrDef_bt"]]    ### Primary Def equal the one of Scenario 1 
  
  GDP_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  GDP_bt[1,] <- MyData[27,paste(Name, "GDP", sep="_")][[1]]
  
  # r_AV_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  # r_AV_bt[1,]<- r_bt[1,]
  
  ### Definire tasso ECB
  r_ECB_bt <- as.xts(array(0.00, c(21, nrep)), order.by=dates7) # r_ECB_bt <-IR_bt
  r_ECB_bt <- g_bt
  P_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  NL_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7) 
  L_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  b_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  EL_EDA_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  RY_EDA_bt<- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  EN_EDA_bt<- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  TR_EDA_bt<- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  LT_EDA_bt<- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  
  
  P_EDA_bt[1,] <- 0
  NL_EDA_bt[1,] <- 0
  L_EDA_bt[1,] <- 0
  b_EDA_bt[1,] <- 0
  EL_EDA_bt[1,] <- 0
  RY_EDA_bt[1,] <- 0
  EN_EDA_bt[1,] <- 0
  TR_EDA_bt[1,] <- 0
  LT_EDA_bt[1,] <- 0
  
  
  d_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  d_bt[1,] <- b_bt[1,] + L_EDA_bt[1,]
  bT_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  bT_bt[1,] <- b* b_bt[1,]/d_bt[1,]
  dT_bt <- as.xts(array(NA, c(21, nrep)), order.by=dates7)
  dT_bt[1,] = d_bt[1,][[1]] +
    - 10*beta*(b_bt[1,][[1]] - bT_bt[1,][[1]]) +
    - 10*gamma*(L_EDA_bt[1,][[1]] - LT_EDA_bt[1,][[1]] )
  
  
  K <- 704799*(1+0.02)^10*MES[MES$Name==Name,2][[1]]/100
  
  
  for (i in 1:nrep){
    
    m = 4
    
    for (j in 2:21) {
      
      
      GDP_bt[j,i][[1]] <- GDP_bt[j-1,i][[1]]*(1+g_bt[j,i][[1]])
      
      if (j==2) {
        P_EDA_bt[j,i][[1]]<-0
        # r_AV_bt [j,i][[1]] <- r_bt[j,i][[1]]*b_bt[j-1, i][[1]]/d_bt[j-1, i][[1]] +
        #   + IC[j,i][[1]]*L_EDA_bt[j-1,i][[1]]/d_bt[j-1, i][[1]]
        
        PrDef_bt[j, i][[1]] = P_EDA_bt[j,i][[1]] +
          + b_bt[j-1, i][[1]]*((r_bt[j,i][[1]]- g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) +
          - g_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*L_EDA_bt[j-1,i][[1]] +
          + delta*(d_bt[j-1, i][[1]] -dT_bt[j-1, i][[1]])  ###comment this expression to set Primary Def equal to S1
        
          
        NL_EDA_bt[j,i][[1]] <- 1/5*(1/(1+g_bt[j,i][[1]]))*b_bt[j-1,i][[1]] +
          + r_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*b_bt[j-1,i][[1]] +
          - PrDef_bt[j, i][[1]] 
        L_EDA_bt[j,i][[1]] <- NL_EDA_bt[j,i][[1]]
        b_EDA_bt[j,i][[1]] <- NL_EDA_bt[j,i][[1]]
        EL_EDA_bt[j,i][[1]] <- 0
        RY_EDA_bt[j,i][[1]] <- 0
        EN_EDA_bt[j,i][[1]] <- K/GDP_bt[j,i][[1]] 
        TR_EDA_bt[j,i][[1]] <- RY_EDA_bt[j,i][[1]]+EN_EDA_bt[j,i][[1]]
        b_bt[j, i][[1]] = b_bt[j-1, i][[1]]* (1/(1+g_bt[j,i][[1]])) +
          - 1/5*b_bt[j-1, i][[1]]*(1/(1+g_bt[j,i][[1]]))
        d_bt[j,i][[1]] = b_bt[j, i][[1]] + L_EDA_bt[j, i][[1]]
        
        bT_bt[j,i][[1]] = 0.6*b_bt[j, i][[1]]/d_bt[j, i][[1]]
        LT_EDA_bt[j,i][[1]] = 0.6*L_EDA_bt[j, i][[1]]/d_bt[j, i][[1]]
        dT_bt[j,i][[1]] = d_bt[j,i][[1]] +
          - 10*beta*(b_bt[j,i][[1]] - bT_bt[j,i][[1]]) +
          - 10*gamma*(L_EDA_bt[j,i][[1]] - LT_EDA_bt[j,i][[1]])
      }
      
      else if ((j>2)&&(j<7)) {
        P_EDA_bt[j,i][[1]]<- IC[j,i][[1]]*(b_EDA_bt[j-1,i][[1]] - RY_EDA_bt[j-1,i][[1]])/(1+g_bt[j,i][[1]])
        
        # r_AV_bt [j,i][[1]] <- r_bt[j,i][[1]]*b_bt[j-1, i][[1]]/d_bt[j-1, i][[1]] +
        #   + IC[j,i][[1]]*L_EDA_bt[j-1,i][[1]]/d_bt[j-1, i][[1]]
        
        PrDef_bt[j, i][[1]] = P_EDA_bt[j,i][[1]] +
          + b_bt[j-1, i][[1]]*((r_bt[j,i][[1]]- g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) +
          - g_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*L_EDA_bt[j-1,i][[1]] +
          + delta*(d_bt[j-1, i][[1]] -dT_bt[j-1, i][[1]]) 
        
        NL_EDA_bt[j,i][[1]] <- 1/m*(1/(1+g_bt[j,i][[1]]))*b_bt[j-1,i][[1]] +
          + r_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*b_bt[j-1,i][[1]] +
          - PrDef_bt[j,i][[1]] +
          + P_EDA_bt[j,i][[1]]
        
        L_EDA_bt[j,i][[1]] <- (1/(1+g_bt[j,i][[1]]))* L_EDA_bt[j-1,i][[1]] + NL_EDA_bt[j,i][[1]]
        b_EDA_bt[j,i][[1]] <- b_EDA_bt[j-1,i][[1]]*((1+IR_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) + NL_EDA_bt[j,i][[1]]
        RY_EDA_bt[j,i][[1]] <- ((1+r_ECB_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))*RY_EDA_bt[j-1,i][[1]]+
          + P_EDA_bt[j,i][[1]]
        EL_EDA_bt[j,i][[1]] <- L_EDA_bt[j,i][[1]] + RY_EDA_bt[j,i][[1]] - b_EDA_bt[j,i][[1]]
        EN_EDA_bt[j,i][[1]] <- EN_EDA_bt[j-1,i][[1]]
        TR_EDA_bt[j,i][[1]] <- RY_EDA_bt[j,i][[1]]+EN_EDA_bt[j,i][[1]]
        
        b_bt[j, i][[1]] = b_bt[j-1, i][[1]]* (1/(1+g_bt[j,i][[1]])) +
          - 1/m*b_bt[j-1, i][[1]]*(1/(1+g_bt[j,i][[1]]))
        d_bt[j,i][[1]] = b_bt[j, i][[1]] + L_EDA_bt[j, i][[1]]
        
        bT_bt[j,i][[1]] = 0.6*b_bt[j, i][[1]]/d_bt[j, i][[1]]
        LT_EDA_bt[j,i][[1]] = 0.6*L_EDA_bt[j, i][[1]]/d_bt[j, i][[1]]
        dT_bt[j,i][[1]] = d_bt[j,i][[1]] +
          - 10*beta*(b_bt[j,i][[1]] - bT_bt[j,i][[1]]) +
          - 10*gamma*(L_EDA_bt[j,i][[1]] - LT_EDA_bt[j,i][[1]] )
        
        m= m-1
      }
      else {
        P_EDA_bt[j,i][[1]]<- IC[j,i][[1]]*(b_EDA_bt[j-1,i][[1]] - RY_EDA_bt[j-1,i][[1]])/(1+g_bt[j,i][[1]])
        
        # r_AV_bt [j,i][[1]] <- r_bt[j,i][[1]]*b_bt[j-1, i][[1]]/d_bt[j-1, i][[1]] +
        #   + IC[j,i][[1]]*L_EDA_bt[j-1,i][[1]]/d_bt[j-1, i][[1]]
        
        PrDef_bt[j, i][[1]] = P_EDA_bt[j,i][[1]] +
          + b_bt[j-1, i][[1]]*((r_bt[j,i][[1]]- g_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) +
          - g_bt[j,i][[1]]/(1+g_bt[j,i][[1]])*L_EDA_bt[j-1,i][[1]] +
          + delta*(d_bt[j-1, i][[1]] -dT_bt[j-1, i][[1]]) 
        
        NL_EDA_bt[j,i][[1]] <- - PrDef_bt[j, i][[1]] + P_EDA_bt[j,i][[1]]
        
        L_EDA_bt[j,i][[1]] <- (1/(1+g_bt[j,i][[1]]))* L_EDA_bt[j-1,i][[1]] + NL_EDA_bt[j,i][[1]]
        b_EDA_bt[j,i][[1]] <- b_EDA_bt[j-1,i][[1]]*((1+IR_bt[j,i][[1]])/(1+g_bt[j,i][[1]])) + NL_EDA_bt[j,i][[1]]
        RY_EDA_bt[j,i][[1]] <- ((1+r_ECB_bt[j,i][[1]])/(1+g_bt[j,i][[1]]))*RY_EDA_bt[j-1,i][[1]]+
          + P_EDA_bt[j,i][[1]]
        EL_EDA_bt[j,i][[1]] <- L_EDA_bt[j,i][[1]] + RY_EDA_bt[j,i][[1]] - b_EDA_bt[j,i][[1]]
        EN_EDA_bt[j,i][[1]] <- EN_EDA_bt[j-1,i][[1]]
        TR_EDA_bt[j,i][[1]] <- RY_EDA_bt[j,i][[1]]+EN_EDA_bt[j,i][[1]]
        
        b_bt[j,i][[1]] = 0
        d_bt[j,i][[1]] = b_bt[j, i][[1]] + L_EDA_bt[j, i][[1]]
        
        bT_bt[j,i][[1]] = 0
        LT_EDA_bt[j,i][[1]] = 0.6
        dT_bt[j,i][[1]] = d_bt[j,i][[1]] +
          - 10*gamma*(L_EDA_bt[j,i][[1]] - LT_EDA_bt[j,i][[1]])
      }
    }
  }
  x = list(b_bt,bT_bt,g_bt,r_bt, PrDef_bt, d_bt,dT_bt, GDP_bt, IC,
           P_EDA_bt, NL_EDA_bt, L_EDA_bt,b_EDA_bt, 
           EL_EDA_bt,RY_EDA_bt,EN_EDA_bt,TR_EDA_bt,LT_EDA_bt)
  
  names = c("b_bt","bT_bt","g_bt","r_bt","PrDef_bt", "d_bt","dT_bt", "GDP_bt", "IC",
            "P_EDA_bt", "NL_EDA_bt", "L_EDA_bt","b_EDA_bt", 
            "EL_EDA_bt","RY_EDA_bt","EN_EDA_bt", "TR_EDA_bt", "LT_EDA_bt")
  # j=1
  # for (i in names) {
  #   names[j] = paste(Name,i, sep="_")
  #   j=j+1 
  # }
  names(x) <- names
  
  x = assign(paste("List_S2",Name,sep="_") , x )
  
  rm(b_bt,bT_bt,g_bt,r_bt, PrDef_bt, d_bt,dT_bt, GDP_bt, IC,
     P_EDA_bt, NL_EDA_bt, L_EDA_bt,b_EDA_bt, 
     EL_EDA_bt,RY_EDA_bt,EN_EDA_bt,TR_EDA_bt,LT_EDA_bt, x)
  
}


rm(list_g_bt, list_r_bt, list_IC)

#-------------------------------------------------------------------------------
### EDA   
#-------------------------------------------------------------------------------
list_temp0<- list(List_S0_DE,List_S0_ES,List_S0_FR,List_S0_GR,List_S0_IT,List_S0_PT)
list_temp1<- list(List_S1_DE,List_S1_ES,List_S1_FR,List_S1_GR,List_S1_IT,List_S1_PT)
list_temp2 <- list(List_S2_DE,List_S2_ES,List_S2_FR,List_S2_GR,List_S2_IT,List_S2_PT)
names(list_temp2)<- name
names(list_temp1)<- name
names(list_temp0)<- name

Total_GDP<- List_S2_DE$GDP_bt +List_S2_ES$GDP_bt+List_S2_FR$GDP_bt+
  +List_S2_GR$GDP_bt+List_S2_IT$GDP_bt+List_S2_PT$GDP_bt

###scenario 1 
R_EDA_1 <- as.xts(array(0, c(21, nrep)), order.by=dates7)
L_EDA_1 <- as.xts(array(0, c(21, nrep)), order.by=dates7)
B_EDA_1 <- as.xts(array(0, c(21, nrep)), order.by=dates7)
TR_EDA_1 <- as.xts(array(0, c(21, nrep)), order.by=dates7)
EL_EDA_1 <- as.xts(array(0, c(21, nrep)), order.by=dates7)


test <- 0 
for (Name in MSList[MSList$Simulation=="Y",4])
{
  temp_R<- list_temp1[[Name]]$Res_EDA_bt
  temp_GDP <- list_temp2[[Name]]$GDP_bt
  temp_B<- list_temp1[[Name]]$b_EDA_bt
  temp_L<- list_temp1[[Name]]$L_EDA_bt
  temp_EL<- list_temp1[[Name]]$EL_EDA_bt
  temp_TR<- list_temp1[[Name]]$TR_EDA_bt
  
  if (test==0) 
  {
    R_EDA_1 <- temp_R*temp_GDP/Total_GDP
    L_EDA_1 <- temp_L*temp_GDP/Total_GDP
    B_EDA_1 <- temp_B*temp_GDP/Total_GDP
    EL_EDA_1 <- temp_EL*temp_GDP/Total_GDP
    TR_EDA_1 <- temp_TR*temp_GDP/Total_GDP
    test <- 1
    
  } else {
    R_EDA_1 <- R_EDA_1 + temp_R*temp_GDP/Total_GDP
    L_EDA_1 <- L_EDA_1 + temp_L*temp_GDP/Total_GDP
    B_EDA_1 <- B_EDA_1 +temp_B*temp_GDP/Total_GDP
    EL_EDA_1 <-  EL_EDA_1 + temp_EL*temp_GDP/Total_GDP
    TR_EDA_1 <-  TR_EDA_1 + temp_TR*temp_GDP/Total_GDP
  } 
}


EDA_1 <- list(R_EDA_1, L_EDA_1, B_EDA_1,EL_EDA_1,TR_EDA_1)
names <- c("R_EDA_1", "L_EDA_1", "B_EDA_1","EL_EDA_1","TR_EDA_1")


temp_mean <- as.xts(array(NA, 21), order.by=dates7)
temp_q95 <- as.xts(array(NA, 21), order.by=dates7)

test<-0
for (k in 1:length(EDA_1))
  { temp<- EDA_1[[k]]
  temp_mean <- as.xts(array(NA, 21), order.by=dates7)
  temp_q95 <- as.xts(array(NA, 21), order.by=dates7)
for (i in 1:21){
  # obtaining the means and upper bound
    temp_mean[i] <- mean(temp[i, ])
    temp_q95[i] <- quantile(temp[i, ], 0.95)
  }
  if (test==0) 
  { tsdata<- cbind(temp_mean, temp_q95)
    test <- 1
  
  } else {
    tsdata<- cbind(tsdata, temp_mean, temp_q95)
  } 
}

colnames(tsdata) <- c("R_EDA_1_mean", "R_EDA_1_q95","L_EDA_1_mean","L_EDA_1_q95",
                      "B_EDA_1_mean","B_EDA_1_q95","EL_EDA_1_mean","EL_EDA_1_q95",
                      "TR_EDA_1_mean", "TR_EDA_1_q95")

Stochastic_EDA <- tsdata
  

rm(R_EDA_1, L_EDA_1, B_EDA_1, EL_EDA_1, TR_EDA_1, tsdata, temp_mean, temp, temp_q95)
rm(temp_B,temp_GDP,temp_EL, temp_L,temp_R,temp_TR)

###scenario 2
R_EDA_2 <- as.xts(array(0, c(21, nrep)), order.by=dates7)
L_EDA_2 <- as.xts(array(0, c(21, nrep)), order.by=dates7)
B_EDA_2 <- as.xts(array(0, c(21, nrep)), order.by=dates7)
TR_EDA_2 <- as.xts(array(0, c(21, nrep)), order.by=dates7)
EL_EDA_2 <- as.xts(array(0, c(21, nrep)), order.by=dates7)


test <- 0 
for (Name in MSList[MSList$Simulation=="Y",4])
{
  temp_R<- list_temp2[[Name]]$RY_EDA_bt
  temp_GDP <- list_temp2[[Name]]$GDP_bt
  temp_B<- list_temp2[[Name]]$b_EDA_bt
  temp_L<- list_temp2[[Name]]$L_EDA_bt
  temp_EL<- list_temp2[[Name]]$EL_EDA_bt
  temp_TR<- list_temp2[[Name]]$TR_EDA_bt
  
  if (test==0) 
  {
    R_EDA_2 <- temp_R*temp_GDP/Total_GDP
    L_EDA_2 <- temp_L*temp_GDP/Total_GDP
    B_EDA_2 <- temp_B*temp_GDP/Total_GDP
    EL_EDA_2 <- temp_EL*temp_GDP/Total_GDP
    TR_EDA_2 <- temp_TR*temp_GDP/Total_GDP
    test <- 1
    
  } else {
    R_EDA_2 <- R_EDA_2 + temp_R*temp_GDP/Total_GDP
    L_EDA_2 <- L_EDA_2 + temp_L*temp_GDP/Total_GDP
    B_EDA_2 <- B_EDA_2 +temp_B*temp_GDP/Total_GDP
    EL_EDA_2 <-  EL_EDA_2 + temp_EL*temp_GDP/Total_GDP
    TR_EDA_2 <-  TR_EDA_2 + temp_TR*temp_GDP/Total_GDP
  } 
}

EDA_2 <- list(R_EDA_2, L_EDA_2, B_EDA_2,EL_EDA_2,TR_EDA_2)
names <- c("R_EDA_2", "L_EDA_2", "B_EDA_2","EL_EDA_2","TR_EDA_2")

temp_mean <- as.xts(array(NA, 21), order.by=dates7)
temp_q95 <- as.xts(array(NA, 21), order.by=dates7)

test<-0
for (k in 1:length(EDA_2))
{ temp<- EDA_2[[k]]
temp_mean <- as.xts(array(NA, 21), order.by=dates7)
temp_q95 <- as.xts(array(NA, 21), order.by=dates7)
for (i in 1:21){
  # obtaining the means and upper bound
  temp_mean[i] <- mean(temp[i, ])
  temp_q95[i] <- quantile(temp[i, ], 0.95)
}
if (test==0) 
{ tsdata<- cbind(temp_mean, temp_q95)
test <- 1

} else {
  tsdata<- cbind(tsdata, temp_mean, temp_q95)
} 
}

colnames(tsdata) <- c("R_EDA_2_mean", "R_EDA_2_q95","L_EDA_2_mean","L_EDA_2_q95",
                      "B_EDA_2_mean","B_EDA_2_q95","EL_EDA_2_mean","EL_EDA_2_q95",
                      "TR_EDA_2_mean", "TR_EDA_2_q95")

Stochastic_EDA <- cbind(Stochastic_EDA, tsdata)

rm(temp_B,temp_GDP,temp_EL, temp_L,temp_R,temp_TR)
rm(R_EDA_2, L_EDA_2, B_EDA_2, EL_EDA_2, TR_EDA_2, tsdata, temp_mean, temp, temp_q95)

#-------------------------------------------------------------------------------
### DATA BACKUP  
#-------------------------------------------------------------------------------
IR_bt_mean<- as.xts(array(NA, 21), order.by=dates7)
IR_bt_q95<- as.xts(array(NA, 21), order.by=dates7)

for (i in 1:21){
  IR_bt_mean[i] <- mean(IR_bt[i, ])
  IR_bt_q95[i] <- quantile(IR_bt[i, ], 0.95)
}

Stochastic_EDA <- cbind(Stochastic_EDA, IR_bt_mean, IR_bt_q95)
write.table(Stochastic_EDA, file = "data/Stochastic_EDA_Thesis.csv", row.names = F, sep = ";", dec='.')

#-------### SCENARIO 0 ###-------

test =  0 
for (Name in MSList[MSList$Simulation=="Y",4])
{
  test1 <- 0 
  for (i in 1:8)
  { temp <- list_temp0[[Name]][[i]]
  temp_mean <- as.xts(array(NA, 21), order.by=dates7)
  temp_q95 <- as.xts(array(NA, 21), order.by=dates7)
  
  for (h in 1:21){
    temp_mean[h] <- mean(temp[h, ])
    temp_q95[h] <- quantile(temp[h, ], 0.95)
  }
  
  if (test1==0) 
  {   tsdata<- cbind(temp_mean, temp_q95)
  test1 <- 1
  
  } else {
    tsdata<- cbind(tsdata, temp_mean, temp_q95)
  } 
  }
  
  names = c("d_bt_mean","d_bt_q95","b_bt_mean","b_bt_q95","bF_bt_mean","bF_bt_q95","bS_bt_mean","bS_bt_q95",
            "bT_bt_mean","bT_bt_q95", "g_bt_mean","g_bt_q95","r_bt_mean","r_bt_q95", "PrDef_bt_mean","PrDef_bt_q95")
  j=1
  for (i in names) {
    names[j] = paste(Name,i, sep="_")
    j=j+1
  }
  colnames(tsdata) = names 
  
  if (test==0) 
  {
    Total_Data_S0<- tsdata
    test <- 1
    
  } else {
    Total_Data_S0<- cbind(Total_Data_S0, tsdata)
  } 
}



#-------### SCENARIO 1 ###-------
 
test =  0 
for (Name in MSList[MSList$Simulation=="Y",4])
{
  test1 <- 0 
  for (i in 1:17)
  { temp <- list_temp1[[Name]][[i]]
    temp_mean <- as.xts(array(NA, 21), order.by=dates7)
    temp_q95 <- as.xts(array(NA, 21), order.by=dates7)
  
  for (h in 1:21){
    temp_mean[h] <- mean(temp[h, ])
    temp_q95[h] <- quantile(temp[h, ], 0.95)
  }
    
  if (test1==0) 
  {   tsdata<- cbind(temp_mean, temp_q95)
      test1 <- 1
      
    } else {
      tsdata<- cbind(tsdata, temp_mean, temp_q95)
    } 
  }
  
  names = c("d_bt_mean","d_bt_q95","b_bt_mean","b_bt_q95","bF_bt_mean","bF_bt_q95","bS_bt_mean","bS_bt_q95",
            "bT_bt_mean","bT_bt_q95","bCov_bt_mean","bCov_bt_q95",
            "g_bt_mean","g_bt_q95","r_bt_mean","r_bt_q95", "PrDef_bt_mean","PrDef_bt_q95",
            "P_EDA_bt_mean", "P_EDA_bt_q95","NL_EDA_bt_mean","NL_EDA_bt_q95",
            "L_EDA_bt_mean","L_EDA_bt_q95","b_EDA_bt_mean","b_EDA_bt_q95","R_EDA_bt_mean", 
            "R_EDA_bt_q95","EN_EDA_bt_mean","EN_EDA_bt_q95","EL_EDA_bt_mean","EL_EDA_bt_q95",
            "TR_EDA_bt_mean","TR_EDA_bt_q95")
  j=1
  for (i in names) {
    names[j] = paste(Name,i, sep="_")
    j=j+1
  }
  colnames(tsdata) = names 

  if (test==0) 
  {
   Total_Data_S1<- tsdata
    test <- 1
    
  } else {
    Total_Data_S1<- cbind(Total_Data_S1, tsdata)
  } 
}

### Create Total Debt (b_EDA + L_EDA)

for (Name in MSList[MSList$Simulation=="Y",4])
{
  test1 <- 0
    temp <- list_temp1[[Name]][["b_bt"]] + list_temp1[[Name]][["L_EDA_bt"]]
    temp_mean <- as.xts(array(NA, 21), order.by=dates7)
    temp_q95 <- as.xts(array(NA, 21), order.by=dates7)

  for (h in 1:21){
    temp_mean[h] <- mean(temp[h, ])
    temp_q95[h] <- quantile(temp[h, ], 0.95)}

  if (test1==0)
  { tsdata<- cbind(temp_mean, temp_q95)
    test1 <- 1
  } else {
    tsdata<- cbind(tsdata, temp_mean, temp_q95)
  }
  names = c("total_debt_mean","total_debt_q95")
  j=1
  for (i in names) {
    names[j] = paste(Name,i, sep="_")
    j=j+1
  }
  colnames(tsdata) = names
  Total_Data_S1<- cbind(Total_Data_S1, tsdata)
}

#-------### SCENARIO 2 ###-------

test2 <- 0
for (Name in MSList[MSList$Simulation=="Y",4])
{
  test1 <- 0 
  for (i in 1:18)
  { temp <- list_temp2[[Name]][[i]]
  temp_mean <- as.xts(array(NA, 21), order.by=dates7)
  temp_q95 <- as.xts(array(NA, 21), order.by=dates7)
  
  for (h in 1:21){
    temp_mean[h] <- mean(temp[h, ])
    temp_q95[h] <- quantile(temp[h, ], 0.95)
  }
  
  if (test1==0) 
  {   tsdata<- cbind(temp_mean, temp_q95)
  test1 <- 1
  
  } else {
    tsdata<- cbind(tsdata, temp_mean, temp_q95)
  } 
  }
  
  names = c("b_bt_mean","b_bt_q95","bT_bt_mean","bT_bt_q95",
            "g_bt_mean","g_bt_q95","r_bt_mean","r_bt_q95","PrDef_bt_mean","PrDef_bt_q95",
            "d_bt_mean","d_bt_q95","dT_bt_mean","dT_bt_q95", "GDP_bt_mean", "GDP_bt_q95",
            "IC_mean", "IC_q95","P_EDA_bt_mean", "P_EDA_bt_q95","NL_EDA_bt_mean","NL_EDA_bt_q95",
            "L_EDA_bt_mean","L_EDA_bt_q95","b_EDA_bt_mean","b_EDA_bt_q95","EL_EDA_bt_mean","EL_EDA_bt_q95",
            "R_EDA_bt_mean","R_EDA_bt_q95","EN_EDA_bt_mean","EN_EDA_bt_q95",
            "TR_EDA_bt_mean","TR_EDA_bt_q95","LT_EDA_bt_mean","LT_EDA_bt_q95")
  
  j=1
  for (i in names) {
    names[j] = paste(Name,i, sep="_")
    j=j+1
  }
  colnames(tsdata) = names 
  
  if (test2==0) 
  {
    Total_Data_S2<- tsdata
    test2 <- 1
    
  } else {
    Total_Data_S2<- cbind(Total_Data_S2, tsdata)
  } 
}

rm(temp, temp_mean, temp_q95,tsdata)

write.table(Total_Data_S0, file = "data/Data_S0_Thesis.csv", row.names = F, sep = ";", dec='.')
write.table(Total_Data_S1, file = "data/Data_S1_Thesis.csv", row.names = F, sep = ";", dec='.')
write.table(Total_Data_S2, file = "data/Data_S2_Thesis.csv", row.names = F, sep = ";", dec='.')

# write.table(Total_Data_S2, file = "data/Total_Data_S2_v2.csv", row.names = F, sep = ";", dec='.')

