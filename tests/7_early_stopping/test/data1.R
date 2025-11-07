## Function for predicting least-squares mean of resid
## Code produced by GUIDE 45.2 on 11/7/25 at 23:43
predicted <- function(){
 if(!is.na(nox) & nox <= 0.715500000000 ){
   if(!is.na(dis) & dis <= 2.50055000000 ){
     if(!is.na(tax) & tax <= 420.000000000 ){
       if(!is.na(black) & black <= 393.370000000 ){
         nodeid <- 16
         predict <- 0.354560068433
       } else {
         nodeid <- 17
         predict <- 1.08150265412
       }
     } else {
       if(!is.na(indus) & indus <= 24.8150000000 ){
         nodeid <- 18
         predict <- -0.153583466603
       } else {
         nodeid <- 19
         predict <- -0.731705372025
       }
     }
   } else {
     if(!is.na(rad) & rad <= 1.50000000000 ){
       if(!is.na(crim) & crim <= 0.414450000000E-01 ){
         nodeid <- 20
         predict <- 0.227330884338
       } else {
         nodeid <- 21
         predict <- -2.24912656535
       }
     } else {
       if(!is.na(rad) & rad <= 3.50000000000 ){
         nodeid <- 22
         predict <- 0.115875768840
       } else {
         nodeid <- 23
         predict <- -0.126607250212
       }
     }
   }
 } else {
   if(!is.na(rm) & rm <= 6.65450000000 ){
     if(!is.na(lstat) & lstat <= 15.9650000000 ){
       if(!is.na(nox) & nox <= 0.820500000000 ){
         nodeid <- 24
         predict <- -0.293506917956E-01
       } else {
         nodeid <- 25
         predict <- 1.27013673413
       }
     } else {
       if(!is.na(age) & age <= 98.9000000000 ){
         nodeid <- 26
         predict <- -0.596758400400
       } else {
         nodeid <- 27
         predict <- 0.197610169250
       }
     }
   } else {
     nodeid <- 7
     predict <- -2.72393290917
   }
 }
 return(c(nodeid,predict))
}
## end of function
##
##
## If desired, replace "data1.csv" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.csv("data1.csv",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred contains predicted value of each case
node <- NULL
pred <- NULL
for(i in 1:nrow(newdata)){
    crim <- as.numeric(newdata$crim[i])
    indus <- as.numeric(newdata$indus[i])
    nox <- as.numeric(newdata$nox[i])
    rm <- as.numeric(newdata$rm[i])
    age <- as.numeric(newdata$age[i])
    dis <- as.numeric(newdata$dis[i])
    rad <- as.numeric(newdata$rad[i])
    tax <- as.numeric(newdata$tax[i])
    black <- as.numeric(newdata$black[i])
    lstat <- as.numeric(newdata$lstat[i])
    tmp <- predicted()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}
