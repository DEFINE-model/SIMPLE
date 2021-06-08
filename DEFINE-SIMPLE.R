#Open R and create a new R script (File->New file->R script). Save this file as 'Green' (File->Save as). 

#Clear the workspace and identify how many time periods (T) you wish your model to run

rm(list=ls(all=TRUE))
T<-83

#STEP 1: For each endogenous variable, create a vector that has a length equal to the time periods. (Once you have written the commands, press 'Source'.)

#Endogenous variables
Y_D<- vector(length=T)   
CO<- vector(length=T)   
D<- vector(length=T)     
Y<- vector(length=T)     
TP<- vector(length=T)   
RP<- vector(length=T)   
DP<- vector(length=T)   
I<- vector(length=T)
r<- vector(length=T) 
IG<- vector(length=T)
beta<- vector(length=T)   
IC<- vector(length=T)   
KG<- vector(length=T)   
KC<- vector(length=T)   
K<- vector(length=T)   
LG<- vector(length=T)   
LC<- vector(length=T)   
L<- vector(length=T)   
BP<- vector(length=T)   
D_red<- vector(length=T) 
EMIS_IN<- vector(length=T)
CI<- vector(length=T)
Y_star<- vector(length=T) #auxiliary variable  
u<- vector(length=T)  #auxiliary variable  
g_Y<- vector(length=T) #auxiliary variable  
lev<- vector(length=T) #auxiliary variable  

#STEP 2: Give values to the parameters 
sce=1

for (j in 1:3){
  int_C<- 0.08
  int_G<- 0.08
  beta_0_change<-0
  c_1_change<-0
  alpha_0_change<-0
  
      for (i in 1:T) {
    
  ##########################
  #Parameters per scenario
  ##########################
  
  #########
  #Baseline
  #########
  if (j==1){} 
  ####################################
  #Green investment shock
  ####################################
  if (j==2 & i<5) {}
  if (j==2 & i>=5) {
    int_G<-0.04
    int_C<-0.12
    beta_0_change<-0.3
     }
  ####################################
  #Degrowth shock
  ####################################
    if (j==3 & i<5) {}
    if (j==3 & i>=5) {
     c_1_change<--0.01
     alpha_0_change<--0.01
    }
    
    if (i == 1) {
    for (iterations in 1:10){
      #######################
      #Non-scenario parameters
      #######################
      g_K<- 0.029
      c_2<- 0.0498
      int_D<- 0.025 
      c_1<- ((Y[i]*g_K)/K[i]-(c_2*D[i])/K[i]-g_K+Y[i]/K[i])/(Y_D[i]/K[i])
      s_F<- (g_K-g_K*(L[i]/K[i]))/((TP[i]*(1+g_K))/K[i]) 
      s_W<-0.54 
      v<-Y[i]/(K[i]*u[i])  
      beta_1<-1
      beta_0<-beta[i]+beta_1*(int_G-int_C)
      alpha_1<- 0.1
      alpha_0<- I[i]/(K[i]/(1+g_K))-alpha_1*r[i]
      CI_max<-0.6
      CI_min<-0.05
      ci_1<- 2.451037
      ci_2<- 3.579244
      
#STEP 3: Give initial values to your variables
      
      #Initial values
      Y_D[i]<- s_W*Y[i]+DP[i]+BP[i]+int_D*(D[i]/(1+g_K))
      CO[i]<-Y[i]-I[i]  
      D[i]<-L[i]  
      Y[i]<-85.93
      TP[i]<-Y[i]-s_W*Y[i]-int_C*(LC[i]/(1+g_K))-int_G*(LG[i]/(1+g_K)) 
      RP[i]<-s_F*TP[i] 
      DP[i]<-TP[i]-RP[i] 
      I[i]<-0.24*Y[i]
      r[i]<-TP[i]/K[i]
      IG[i]<- 0.7 
      beta[i]<-IG[i]/I[i] 
      IC[i]<-I[i]-IG[i] 
      KG[i]<-beta[i]*K[i]
      KC[i]<-K[i]-KG[i] 
      K_Y_ratio<-(0.24*(1+g_K))/g_K 
      K[i]<- K_Y_ratio*Y[i]
      LG[i]<-beta[i]*L[i] 
      LC[i]<-L[i]-LG[i] 
      L[i]<-0.914*Y[i] 
      BP[i]<-int_C*(LC[i]/(1+g_K))+int_G*(LG[i]/(1+g_K))-int_D*(D[i]/(1+g_K)) 
      D_red[i]<-D[i]
      EMIS_IN[i]<-36.6
      CI[i]<-EMIS_IN[i]/Y[i] 
      Y_star[i]<-v*K[i]  
      u[i]<-0.72 
      g_Y[i]<-g_K 
      lev[i]<-L[i]/K[i] 
      
    }
  }
  
  #STEP 4: Write down the equations and run the model
  
  #Equations
  else {
    
    for (iterations in 1:10){
      
      #Households
      Y_D[i]<- s_W*Y[i]+DP[i]+BP[i]+int_D*D[i-1]
      CO[i]<-(c_1+c_1_change)*Y_D[i-1]+c_2*D[i-1]
      D[i]<-D[i-1]+Y_D[i]-CO[i]
      
      #Firms
      Y[i]<-CO[i]+I[i]
      TP[i]<-Y[i]-s_W*Y[i]-int_C*LC[i-1]-int_G*LG[i-1]
      RP[i]<-s_F*TP[i]
      DP[i]<-TP[i]-RP[i]
      I[i]<-(alpha_0+alpha_0_change+alpha_1*r[i-1])*K[i-1]
      r[i]<-TP[i]/K[i]
      IG[i]<-beta[i]*I[i]
      beta[i]<-beta_0-beta_1*(int_G-int_C)+beta_0_change
      IC[i]<-I[i]-IG[i]
      KG[i]<-KG[i-1]+IG[i]
      KC[i]<-KC[i-1]+IC[i]
      K[i]<-KC[i]+KG[i]
      LG[i]<-LG[i-1]+IG[i]-beta[i]*RP[i]
      LC[i]<-LC[i-1]+IC[i]+IG[i]-RP[i]-(LG[i]-LG[i-1])
      L[i]<-LC[i]+LG[i]
      
      #Banks
      BP[i]<-int_C*LC[i-1]+int_G*LG[i-1]-int_D*D[i-1]
      D_red[i]<-L[i]
      
      #Emissions
      EMIS_IN[i]<-CI[i]*Y[i]
      CI[i]<-CI_max-((CI_max-CI_min)/(1+ci_1*exp(-ci_2*(KG[i-1]/KC[i-1]))))
      
      #Auxiliary equations
      Y_star[i]<-v*K[i]
      u[i]<-Y[i]/Y_star[i]
      g_Y[i]<-(Y[i]-Y[i-1])/Y[i-1]
      lev[i]<-L[i]/K[i]
      
    }
  }
}

#STEP 5: Create a table to report the following variables: D_red, D, u, r, g_Y, lev, Y, beta, int_CO2, EMIS_IN. Create also 3 graphs for key endogenous variables 
#Table
matrixname<-paste("Table",sce, sep="")
assign (matrixname, (round(cbind(D_red, D, u, r, g_Y, lev, Y, beta, CI, EMIS_IN), digits=4)))
  
sce=sce+1
}

sce1="Baseline"
sce2="Green investment shock"
sce3="Degrowth shock"
col1="#000000"
col2="#0072B2"
col3="#D55E00"

#Graphs
plot(Table1[,c("g_Y")], type="l", col=col1, lwd=1.7, ylab= "Growth rate of output", ylim=c(-0.01, 0.05),  cex.lab=1.2, xlab= "Year", xaxt="n",lty=1)
axis(side=1, at=c(1,23,43,63,83), labels=c("2018","2040","2060","2080","2100"))
lines(Table2[,c("g_Y")], lty=3, col=col2, lwd=1.7)
lines(Table3[,c("g_Y")], lty=2, col=col3, lwd=1.7)
legend("bottomright", legend=c(sce1, sce2, sce3), lty=c(1, 3, 2), col=c(col1, col2, col3), lwd=c(1.7,1.7,1.7), bty="n",cex = 0.8) 


plot(Table1[,c("lev")], type="l", col=col1, lwd=1.7, ylab= "Firm leverage", ylim=c(0.0, 0.16),  cex.lab=1.2, xlab= "Year", xaxt="n",lty=1)
axis(side=1, at=c(1,23,43,63,83), labels=c("2018","2040","2060","2080","2100"))
lines(Table2[,c("lev")], lty=3, col=col2, lwd=1.7)
lines(Table3[,c("lev")], lty=2, col=col3, lwd=1.7)
legend("bottomright", legend=c(sce1, sce2, sce3), lty=c(1, 3, 2), col=c(col1, col2, col3), lwd=c(1.7,1.7,1.7), bty="n",cex = 0.8) 
 

plot(Table1[,c("beta")], type="l", col=col1, lwd=1.7, ylab= "Share of green investment", ylim=c(0, 0.8),  cex.lab=1.2, xlab= "Year", xaxt="n",lty=1)
axis(side=1, at=c(1,23,43,63,83), labels=c("2018","2040","2060","2080","2100"))
lines(Table2[,c("beta")], lty=3, col=col2, lwd=1.7)
lines(Table3[,c("beta")], lty=2, col=col3, lwd=1.7)
legend("topright", legend=c(sce1, sce2, sce3), lty=c(1, 3, 2), col=c(col1, col2, col3), lwd=c(1.7,1.7,1.7), bty="n",cex = 0.8) 


plot(Table1[,c("EMIS_IN")], type="l", col=col1, lwd=1.7, ylab=expression("Industrial CO" [2]*" emissions (Gt)"), ylim=c(0, 500),  cex.lab=1.2, xlab= "Year", xaxt="n",lty=1)
axis(side=1, at=c(1,23,43,63,83), labels=c("2018","2040","2060","2080","2100"))
lines(Table2[,c("EMIS_IN")], lty=3, col=col2, lwd=1.7)
lines(Table3[,c("EMIS_IN")], lty=2, col=col3, lwd=1.7)
legend("topleft", legend=c(sce1, sce2, sce3), lty=c(1, 3, 2), col=c(col1, col2, col3), lwd=c(1.7,1.7,1.7), bty="n",cex = 0.8) 
 

plot(Table1[,c("CI")], type="l", col=col1, lwd=1.7, ylab= expression("CO" [2]*" intensity"), ylim=c(0.1, 0.5),  cex.lab=1.2, xlab= "Year", xaxt="n",lty=1)
axis(side=1, at=c(1,23,43,63,83), labels=c("2018","2040","2060","2080","2100"))
lines(Table2[,c("CI")], lty=3, col=col2, lwd=1.7)
lines(Table3[,c("CI")], lty=2, col=col3, lwd=1.7)
legend("bottomleft", legend=c(sce1, sce2, sce3), lty=c(1, 3, 2), col=c(col1, col2, col3), lwd=c(1.7,1.7,1.7), bty="n",cex = 0.8) 
 


