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

Data_S0 <- read.table("data/Data_S0_Thesis.csv",header = T,sep = ";",dec = ".", stringsAsFactors = FALSE) 
Data_S1 <- read.table("data/Data_S1_Thesis.csv",header = T,sep = ";",dec = ".", stringsAsFactors = FALSE) 
Data_S2 <- read.table("data/Data_S2_Thesis.csv",header = T,sep = ";",dec = ".", stringsAsFactors = FALSE) 
Stochastic_EDA <- read.table("data/Stochastic_EDA_Thesis.csv",header = T,sep = ";",dec = ".", stringsAsFactors = FALSE) 


dates <-seq(as.Date("2021-12-15"),length=21, by="years")
Stochastic_EDA <- as.xts(Stochastic_EDA, order.by=dates)
Data_S0 <- as.xts(Data_S0, order.by=dates)
Data_S1 <- as.xts(Data_S1, order.by=dates)
Data_S2 <- as.xts(Data_S2, order.by=dates)
#-------------------------------------------------------------------------------
### GRAPHS:  
#-------------------------------------------------------------------------------

### plot - CHECK: 
plot(Stochastic_EDA$IR_bt_mean, main = "EDA Rate, Avg Cost, IC - Germany", type = "l", col = "blue",ylim = c(0.00, 0.08))
lines(Stochastic_EDA$IR_bt_q95, col = "blue", lty=2,lwd=2)
lines(Data_S1$DE_r_bt_mean, col = "red",lwd=2)
lines(Data_S1$DE_r_bt_q95,col = "red",lwd=2, lty=2)
lines(Data_S2$DE_IC_mean, col="green", lwd=2)
lines(Data_S2$DE_IC_q95, col="green", lwd=2, lty=2)

addLegend("topleft",
          legend.names = c( "EDA rate - Mean","EDA rate - Upper bound", "Avg Cost - Mean",
                            "Avg Cost - Upper bound", "Idiomatic Cost - Mean", 
                            "Idiomatic Cost - Upper bound"),
          col=c("blue", "blue", "red", "red", "green","green"), lty=c(1,2), lwd=2, text.font=3)


### plot - CHECK: 
plot(Data_S1$DE_g_bt_mean, main = "Growth rate - Germany", type = "l", col = "green",ylim = c(-0.03, 0.06))
lines(Data_S1$DE_g_bt_q95, col = "green", lty=2,lwd=2)


#-------------------------------------------------------------------------------
### DEBT/GDP:  
#-------------------------------------------------------------------------------

#-------- GERMANY-----------

plot(Data_S1$DE_b_bt_mean, main = "Debt/GDP - Germany", type = "l", col = "blue",ylim = c(0, 0.75))
lines(Data_S1$DE_b_bt_q95, col = "blue",lty=2)
lines(Data_S2$DE_b_bt_mean, col = "green")
lines(Data_S2$DE_b_bt_q95, col = "green", lty=2)

addLegend("bottomright",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue", "blue", "green","green"), lty=c(1,2), lwd=2, text.font=3)

#-------- SPAIN-----------

plot(Data_S1$ES_b_bt_mean, main = "Debt/GDP - Spain", type = "l", col = "blue",ylim = c(0, 1.30))
lines(Data_S1$ES_b_bt_q95, col = "blue",lty=2)
lines(Data_S2$ES_b_bt_mean, col = "green")
lines(Data_S2$ES_b_bt_q95, col = "green", lty=2)

addLegend("bottomright",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue", "blue", "green","green"), lty=c(1,2), lwd=2, text.font=3)

#-------- FRANCE-----------

plot(Data_S1$FR_b_bt_mean, main = "Debt/GDP - France", type = "l", col = "blue",ylim = c(0, 1.20))
lines(Data_S1$FR_b_bt_q95, col = "blue",lty=2)
lines(Data_S2$FR_b_bt_mean, col = "green")
lines(Data_S2$FR_b_bt_q95, col = "green", lty=2)

addLegend("bottomright",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue", "blue", "green","green"), lty=c(1,2), lwd=2, text.font=3)


#-------- GREECE-----------

plot(Data_S1$GR_b_bt_mean, main = "Debt/GDP - Greece", type = "l", col = "blue",ylim = c(0, 2.05))
lines(Data_S1$GR_b_bt_q95, col = "blue",lty=2)
lines(Data_S2$GR_b_bt_mean, col = "green")
lines(Data_S2$GR_b_bt_q95, col = "green", lty=2)

addLegend("bottomright",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3)


#-------- ITALY-----------

plot(Data_S1$IT_b_bt_mean, main = "Debt/GDP - Italy", type = "l", col = "blue",ylim = c(0, 2.05))
lines(Data_S1$IT_b_bt_q95, col = "blue",lty=2)
lines(Data_S2$IT_b_bt_mean, col = "green")
lines(Data_S2$IT_b_bt_q95, col = "green", lty=2)

addLegend("bottomright",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3)


#-------- PORTUGAL-----------
plot(Data_S1$PT_b_bt_mean, main = "Debt/GDP - Greece", type = "l", col = "blue",ylim = c(0, 1.55))
lines(Data_S1$PT_b_bt_q95, col = "blue",lty=2)
lines(Data_S2$PT_b_bt_mean, col = "green")
lines(Data_S2$PT_b_bt_q95, col = "green", lty=2)

addLegend("bottomright",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3)


#-------------------------------------------------------------------------------
### Total DEBT/GDP and Primary Surplus:  
#-------------------------------------------------------------------------------

#-------- GERMANY-----------

####DEBT:
plot(Data_S1$DE_d_bt_mean, main = "Total Debt/GDP - Germany", type = "l", col = "blue",ylim = c(0.45, 0.75), lwd=1.5)
lines(Data_S1$DE_d_bt_q95, col = "blue",lty=2)
lines(Data_S1$DE_b_bt_mean, col = "red", lwd=1.5)
lines(Data_S1$DE_b_bt_q95, col = "red",lty=2)
lines(Data_S2$DE_d_bt_mean, col = "green",lwd=1.5)
lines(Data_S2$DE_d_bt_q95, col = "green", lty=2)


addLegend("bottomleft",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3)

####PRIMARY SURPLUS:
Data_S1$horizontal_line <- 0
plot(Data_S1$DE_PrDef_bt_mean, main = "Primary Surplus - Germany", type = "l", 
     col = "blue",ylim = c(-0.08, 0.12),lwd = 1.5)
lines(Data_S1$DE_PrDef_bt_q95, col="blue",lty=2, lwd = 1.5)
lines(Data_S2$DE_PrDef_bt_mean, col = "green",lwd = 1.5)
lines(Data_S2$DE_PrDef_bt_q95, col = "green", lty=2, lwd = 1.5)
lines(Data_S1[, "horizontal_line"], col = "black",lwd=1)


addLegend("bottomright",
          legend.names = c( "Primary Surplus - Mean - case 1","Primary Surplus - Upper bound - case 1", 
                            "Primary Surplus - Mean - case 2","Primary Surplus - Upper bound - case 2"),
          col=c("blue", "blue", "green","green"), lty=c(1,2), lwd=2, text.font=3)

#-------- SPAIN-----------

####DEBT:
plot(Data_S1$ES_total_debt_mean, main = "Total Debt/GDP - Spain", type = "l", col = "blue",ylim = c(0.45, 1.4))
lines(Data_S1$ES_total_debt_q95, col = "blue",lty=2)
lines(Data_S2$ES_d_bt_mean, col = "green")
lines(Data_S2$ES_d_bt_q95, col = "green", lty=2)

addLegend("bottomleft",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3)

####PRIMARY SURPLUS:
plot(Data_S1$ES_PrDef_bt_mean, main = "Primary Surplus - Spain", type = "l", 
     col = "blue",ylim = c(-0.08, 0.18),lwd = 1.5)
lines(Data_S1$ES_PrDef_bt_q95, col="blue",lty=2, lwd = 1.5)
lines(Data_S2$ES_PrDef_bt_mean, col = "green",lwd = 1.5)
lines(Data_S2$ES_PrDef_bt_q95, col = "green", lty=2, lwd = 1.5)
lines(Data_S1[, "horizontal_line"], col = "black",lwd=1)


addLegend("bottomright",
          legend.names = c( "Primary Surplus - Mean - case 1","Primary Surplus - Upper bound - case 1", 
                            "Primary Surplus - Mean - case 2","Primary Surplus - Upper bound - case 2"),
          col=c("blue","blue", "green", "green"), lty=c(1,2), lwd=2, text.font=3)


#-------- FRANCE-----------

####DEBT:
plot(Data_S1$FR_total_debt_mean, main = "Total Debt/GDP - France", type = "l", col = "blue",ylim = c(0.45, 1.4))
lines(Data_S1$FR_total_debt_q95, col = "blue",lty=2)
lines(Data_S2$FR_d_bt_mean, col = "green")
lines(Data_S2$FR_d_bt_q95, col = "green", lty=2)
lines(Data_S0$FR_d_bt_mean, col = "red")
lines(Data_S0$FR_d_bt_q95, col = "red", lty=2)


addLegend("bottomleft",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3)

####PRIMARY SURPLUS:
plot(Data_S1$FR_PrDef_bt_mean, main = "Primary Surplus - France", type = "l", 
     col = "blue",ylim = c(-0.08, 0.12),lwd = 1.5)
lines(Data_S1$FR_PrDef_bt_q95, col="blue",lty=2, lwd = 1.5)
lines(Data_S2$FR_PrDef_bt_mean, col = "green",lwd = 1.5)
lines(Data_S2$FR_PrDef_bt_q95, col = "green", lty=2, lwd = 1.5)
lines(Data_S1[, "horizontal_line"], col = "black",lwd=1)


addLegend("bottomright",
          legend.names = c( "Primary Surplus - Mean - case 1","Primary Surplus - Upper bound - case 1", 
                            "Primary Surplus - Mean - case 2","Primary Surplus - Upper bound - case 2"),
          col=c("blue","blue", "green", "green"), lty=c(1,2), lwd=2, text.font=3)

#-------- GREECE-----------

####DEBT:
plot(Data_S1$GR_total_debt_mean, main = "Total Debt/GDP - Greece", type = "l", col = "blue",ylim = c(0.45, 2.05))
lines(Data_S1$GR_total_debt_q95, col = "blue",lty=2)
lines(Data_S2$GR_d_bt_mean, col = "green")
lines(Data_S2$GR_d_bt_q95, col = "green", lty=2)

addLegend("bottomleft",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3)

####PRIMARY SURPLUS:
plot(Data_S1$GR_PrDef_bt_mean, main = "Primary Surplus - Greece", type = "l", 
     col = "blue",ylim = c(-0.08, 0.24),lwd = 1.5)
lines(Data_S1$GR_PrDef_bt_q95, col="blue",lty=2, lwd = 1.5)
lines(Data_S2$GR_PrDef_bt_mean, col = "green",lwd = 1.5)
lines(Data_S2$GR_PrDef_bt_q95, col = "green", lty=2, lwd = 1.5)
lines(Data_S1[, "horizontal_line"], col = "black",lwd=1)


addLegend("bottomright",
          legend.names = c( "Primary Surplus - Mean - case 1","Primary Surplus - Upper bound - case 1", 
                            "Primary Surplus - Mean - case 2","Primary Surplus - Upper bound - case 2"),
          col=c("blue","blue", "green", "green"), lty=c(1,2), lwd=2, text.font=3)

#-------- ITALY-----------

####DEBT:
plot(Data_S1$IT_total_debt_mean, main = "Total Debt/GDP - Italy", type = "l", 
     col = "blue",ylim = c(0.880, 1.55),lwd=1.5, cex=0.8)
lines(Data_S1$IT_total_debt_q95, col = "blue",lty=2,lwd=1.5)
lines(Data_S2$IT_d_bt_mean, col = "green",lwd=1.5)
lines(Data_S2$IT_d_bt_q95, col = "green", lty=2,lwd=1.5)
lines(Data_S1$IT_b_bt_mean, col="darkred",lwd=1.5)
lines(Data_S1$IT_b_bt_q95, col="darkred",lty=2,lwd=1.5)
lines(Data_S0$IT_d_bt_mean, col = "orange",lwd=1.5)
lines(Data_S0$IT_d_bt_q95, col = "orange",lty=2,lwd=1.5)

# addLegend("bottomleft", legend.names = c("Mean - small EDA","Upper bound - small EDA",
#                   "Mean - large EDA","Upper bound - large EDA"),
# col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3, cex=0.9)


addLegend("bottomleft",
          legend.names = c( "Mean - w/o EDA","Upper bound - w/o EDA","Mean - small EDA","Upper bound - small EDA", 
                            "Mean - large EDA","Upper bound - large EDA","Mean - small EDA w/o accounting for loans",
                            "Upper bound - small EDA w/o accounting for loans"),
          col=c("orange", "orange","blue","blue","green","green","darkred","darkred"), lty=c(1,2), lwd=2, text.font=3, cex=0.9)

# dev.copy2pdf(width = 8.5, out.type = "pdf",file="figures/Figure_debt_IT_r_Thesis.pdf")
dev.copy2pdf(width = 8.5, out.type = "pdf",file="figures/Figure_debt_IT_thesis.pdf")
dev.off()

####PRIMARY SURPLUS:
plot(Data_S1$IT_PrDef_bt_mean, main = "Primary Surplus - Italy", type = "l", 
     col = "blue",ylim = c(-0.10, 0.18),lwd = 1.5, cex=0.8)
lines(Data_S1$IT_PrDef_bt_q95, col="blue",lty=2, lwd = 1.5)
lines(Data_S2$IT_PrDef_bt_mean, col = "green",lwd = 1.5)
lines(Data_S2$IT_PrDef_bt_q95, col = "green", lty=2, lwd = 1.5)
lines(Data_S1[, "horizontal_line"], col = "black",lwd=1)
lines(Data_S0$IT_PrDef_bt_mean, col = "orange",lwd = 1.5)
lines(Data_S0$IT_PrDef_bt_q95, col = "orange", lty=2, lwd = 1.5)


addLegend("bottomright",
          legend.names = c( "Mean - w/o EDA","Upper bound - w/o EDA","Mean - small EDA","Upper bound - small EDA", 
                            "Mean - large EDA","Upper bound - large EDA"),
          col=c("orange", "orange","blue", "blue","green","green"), lty=c(1,2), lwd=2, text.font=3, cex=0.90)

# addLegend("bottomright", legend.names = c("Mean - small EDA","Upper bound - small EDA",
#                    "Mean - large EDA","Upper bound - large EDA"),
#                     col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3, cex=0.9)

# dev.copy2pdf(width = 8.5, out.type = "pdf",file="figures/Figure_surplus_IT_r_thesis.pdf")
dev.copy2pdf(width = 8.5, out.type = "pdf",file="figures/Figure_surplus_IT_thesis.pdf")
dev.off()

#-------- PORTUGAL-----------

####DEBT:
plot(Data_S1$PT_total_debt_mean, main = "Total Debt/GDP - Portugal", type = "l", col = "blue",ylim = c(0.45, 1.4))
lines(Data_S1$PT_total_debt_q95, col = "blue",lty=2)
lines(Data_S2$PT_d_bt_mean, col = "green")
lines(Data_S2$PT_d_bt_q95, col = "green", lty=2)

addLegend("bottomleft",
          legend.names = c( "Mean - case 1","Upper bound - case 1", "Mean - case 2","Upper bound - case 2"),
          col=c("blue","blue","green","green"), lty=c(1,2), lwd=2, text.font=3)

####PRIMARY SURPLUS: 
plot(Data_S1$PT_PrDef_bt_mean, main = "Primary Surplus - Portugal", type = "l", 
     col = "blue",ylim = c(-0.08, 0.16),lwd = 1.5)
lines(Data_S1$PT_PrDef_bt_q95, col="blue",lty=2, lwd = 1.5)
lines(Data_S2$PT_PrDef_bt_mean, col = "green",lwd = 1.5)
lines(Data_S2$PT_PrDef_bt_q95, col = "green", lty=2, lwd = 1.5)
lines(Data_S1[, "horizontal_line"], col = "black",lwd=1)


addLegend("bottomright",
          legend.names = c( "Primary Surplus - Mean - case 1","Primary Surplus - Upper bound - case 1", 
                            "Primary Surplus - Mean - case 2","Primary Surplus - Upper bound - case 2"),
          col=c("blue","blue", "green", "green"), lty=c(1,2), lwd=2, text.font=3)


#-------------------------------------------------------------------------------
### Average Cost:
#-------------------------------------------------------------------------------

#-------- GERMANY-----------
plot(Data_S1$DE_r_bt_mean, main = "Avg Cost and Idiomatic Cost - Germany", type = "l", col = "blue",ylim = c(0, 0.1))
lines(Data_S1$DE_r_bt_q95, col = "blue",lty=2,lwd=1.5)
lines(Data_S2$DE_IC_mean, col = "orange", lwd=2)
lines(Data_S2$DE_IC_q95, col = "orange", lty=2,lwd=1.5)

addLegend("topright",
          legend.names = c( "Avg Cost - Mean","Avg Cost - Upper bound", 
                            "Idiomatic Cost - Mean","Idiomatic Cost - Upper bound"),
          col=c("blue","blue","orange", "orange"), lty=c(1,2), lwd=2, text.font=3)

#-------- SPAIN-----------
plot(Data_S1$ES_r_bt_mean, main = "Avg Cost and Idiomatic Cost - Spain", type = "l", col = "blue",ylim = c(0, 0.1))
lines(Data_S1$ES_r_bt_q95, col = "blue",lty=2,lwd=1.5)
lines(Data_S2$ES_IC_mean, col = "orange", lwd=2)
lines(Data_S2$ES_IC_q95, col = "orange", lty=2,lwd=1.5)

addLegend("topright",
          legend.names = c( "Avg Cost - Mean","Avg Cost - Upper bound", 
                            "Idiomatic Cost - Mean","Idiomatic Cost - Upper bound"),
          col=c("blue","blue","orange", "orange"), lty=c(1,2), lwd=2, text.font=3)

#-------- FRANCE-----------
plot(Data_S1$FR_r_bt_mean, main = "Avg Cost and Idiomatic Cost - France", type = "l", col = "blue",ylim = c(0, 0.1))
lines(Data_S1$FR_r_bt_q95, col = "blue",lty=2,lwd=1.5)
lines(Data_S2$FR_IC_mean, col = "orange", lwd=2)
lines(Data_S2$FR_IC_q95, col = "orange", lty=2,lwd=1.5)

addLegend("topright",
          legend.names = c( "Avg Cost - Mean","Avg Cost - Upper bound", 
                            "Idiomatic Cost - Mean","Idiomatic Cost - Upper bound"),
          col=c("blue","blue","orange", "orange"), lty=c(1,2), lwd=2, text.font=3)

#-------- GREECE-----------
plot(Data_S1$GR_r_bt_mean, main = "Avg Cost and Idiomatic Cost - Greece", type = "l", col = "blue",ylim = c(0, 0.1))
lines(Data_S1$GR_r_bt_q95, col = "blue",lty=2,lwd=1.5)
lines(Data_S2$GR_IC_mean, col = "orange", lwd=2)
lines(Data_S2$GR_IC_q95, col = "orange", lty=2,lwd=1.5)

addLegend("topright",
          legend.names = c( "Avg Cost - Mean","Avg Cost - Upper bound", 
                            "Idiomatic Cost - Mean","Idiomatic Cost - Upper bound"),
          col=c("blue","blue","orange", "orange"), lty=c(1,2), lwd=2, text.font=3)

#-------- ITALY-----------
plot(Data_S1$IT_r_bt_mean, main = "Avg Cost and Idiomatic Cost - Italy", type = "l", col = "blue",ylim = c(0, 0.1))
lines(Data_S1$IT_r_bt_q95, col = "blue",lty=2,lwd=1.5)
lines(Data_S2$IT_IC_mean, col = "orange", lwd=2)
lines(Data_S2$IT_IC_q95, col = "orange", lty=2,lwd=1.5)

addLegend("topright",
          legend.names = c( "Avg Cost - Mean","Avg Cost - Upper bound", 
                            "Idiomatic Cost - Mean","Idiomatic Cost - Upper bound"),
          col=c("blue","blue","orange", "orange"), lty=c(1,2), lwd=2, text.font=3)

#-------- PORTUGAL-----------
plot(Data_S1$PT_r_bt_mean, main = "Avg Cost and Idiomatic Cost - Portugal", type = "l", col = "blue",ylim = c(0, 0.1))
lines(Data_S1$PT_r_bt_q95, col = "blue",lty=2,lwd=1.5)
lines(Data_S2$PT_IC_mean, col = "orange", lwd=2)
lines(Data_S2$PT_IC_q95, col = "orange", lty=2,lwd=1.5)

addLegend("topright",
          legend.names = c( "Avg Cost - Mean","Avg Cost - Upper bound", 
                            "Idiomatic Cost - Mean","Idiomatic Cost - Upper bound"),
          col=c("blue","blue","orange", "orange"), lty=c(1,2), lwd=2, text.font=3)


#-------------------------------------------------------------------------------
### EDA:  
#-------------------------------------------------------------------------------
#Scenario 1 
plot(Stochastic_EDA$R_EDA_1_mean,main = "EDA Assets and Liabilities - S1", 
     type = "l", col = "green", lwd =2, ylim = c(-0.11, 0.14), cex=0.8)
lines(Stochastic_EDA$L_EDA_1_mean, col="green", lwd =2, lty=2)
lines(Stochastic_EDA$B_EDA_1_mean, col="red",lwd =2)
lines(Stochastic_EDA$L_EDA_1_mean+Stochastic_EDA$R_EDA_1_mean - Stochastic_EDA$B_EDA_1_mean,
      col = "red", lty=2, lwd =2)
lines(Stochastic_EDA$R_EDA_1_mean - Stochastic_EDA$B_EDA_1_mean, col = "darkred", lty=2, lwd =2)
addLegend("topleft",
          legend.names = c( "Reserves","Loans", "Bonds",
                            "Solvency Capital", "Solvency Capital - w/o Loans"),
          col=c("green", "green", "red", "red", "darkred"), lty=c(1,2,1,2,2), lwd=2, text.font=3)

dev.copy2pdf(width = 8.5, out.type = "pdf",file="figures/Figure_EDA1.pdf")
dev.off()

#-----------------------------
#Scenario 2
plot(Stochastic_EDA$R_EDA_2_mean,main = "EDA Assets and Liabilities - S2", 
     type = "l", col = "green", lwd =2 ,lty=1, ylim = c(-0.1, 1.55),cex=0.8)
lines(Stochastic_EDA$L_EDA_2_mean, col = "green", lty=2, lwd =2)
lines(Stochastic_EDA$B_EDA_2_mean, col = "red", lty=1, lwd =2)
lines(Stochastic_EDA$TR_EDA_2_mean-Stochastic_EDA$R_EDA_2_mean+Stochastic_EDA$EL_EDA_2_mean,
     col = "red", lty=2, lwd =2)

addLegend("topleft",
          legend.names = c( "Reserves","Loans","Bonds",
                            "Solvency capital"),
          col=c("green", "green","red","red"), lty=c(1,2,1,2), lwd=2, text.font=3)

dev.copy2pdf(width = 8.5, out.type = "pdf",file="figures/Figure_EDA2.pdf")
dev.off()

plot(Stochastic_EDA$L_EDA_2_mean+Stochastic_EDA$R_EDA_2_mean-Stochastic_EDA$B_EDA_2_mean,
      col = "green", lty=2, lwd =2, ylim = c(-0.08, 0.15))
lines(Stochastic_EDA$TR_EDA_2_mean-Stochastic_EDA$R_EDA_2_mean,
      col = "blue", lty=2, lwd =2)
lines(Stochastic_EDA$TR_EDA_2_mean-Stochastic_EDA$R_EDA_2_mean+Stochastic_EDA$EL_EDA_2_mean,
      col = "red", lty=2, lwd =2)

addLegend("topleft",
          legend.names = c( "Expected Loss Provision  - case 2","Endowment - case 2",
                            "Solvency capital - case 2"),
          col=c("green","blue","red"), lty=c(2), lwd=2, text.font=3)




#-------------------------------------------------------------------------------
####Count of Fiscal Savings 
MSList <- read.table("data/MSList2.csv",header = T,sep = ";",dec = ".", stringsAsFactors = FALSE)

k=1
Sum_Diff_PrDef <- as.data.frame(vector(mode ="numeric", length = 6))

for (Name in MSList[MSList$Simulation=="Y",4])
{
PrDef_diff <- Data_S1[,paste(Name, "PrDef_bt_mean", sep="_")] - Data_S2[,paste(Name, "PrDef_bt_mean", sep="_")]
colnames(PrDef_diff)<- "Diff"

DF <- as.data.frame(vector(mode ="numeric", length = 21))
DF[1,] <- 1/(1+Data_S2[1,paste(Name, "IC_mean", sep="_")][[1]])

for (i in 2:21) {
  DF[i,]<- DF[i-1,]*(1/(1+Data_S2[i,paste(Name, "IC_mean", sep="_")][[1]]))}

temp <- as.data.frame(vector(mode ="numeric", length = 21))
for (i in 1:21) {
  temp[i,] <- PrDef_diff[i,][[1]]*DF[i,]}

Sum_Diff_PrDef[k,] <- sum(temp[c(1:21),1])
k= k+1
}

Sum_Diff_PrDef <- cbind(MSList[MSList$Simulation=="Y",4],Sum_Diff_PrDef)
colnames(Sum_Diff_PrDef) <- c("MS","Fiscal Savings")



